# Load required R libraries ----
library(dplyr)
library(readxl)
library(measurements)
library(sp)
library(sf)
library(Nippon) #https://github.com/cran/Nippon
library(here)

# Read Original Database and Process Strings ----

# Read most up-to-date version of the original database (published on 3rd March 2022)
c14raw  <- read.csv(here('raw','c14_raw_v240517.csv'),na.strings = '',skip=2)
# Assign New Columns Names:
colnames(c14raw) <-  c("PrefectureCode", #都道府県コード
                    "Prefecture", #都道府県
                    "SiteName", #遺跡名
                    "SiteLocation", #所在地
                    "SamplingLocation",#サンプル採取地点等
                    "MaterialType",#試料の種類
                    "MaterialCode1",#試料コード1
                    "MaterialCode2",#試料コード2
                    "Period",#時代
                    "Phase",#時期
                    "PeriodCode",#時代コード
                    "LabCode",#試料番号
                    "Method",#β線法/AMS法
                    "CRA",#14C年代
                    "elim1",#
                    "CRAError",#14C年代
                    "UnroundedCRA",#暦年較正用14C年代
                    "elim2",#
                    "UnroundedCRAError",#暦年較正用14C年代
                    "Delta13C",#δ13CAms
                    "elim3",#
                    "Delta13CError",#
                    "Delta13CIRMS",#δ13C（‰）(IR-MS)
                    "AnalysedBy",#分析者（著者）
                    "Laboratory",#測定機関
                    "PubblicationYear",#刊行年
                    "ReportTitle",#報告タイトル
                    "Page",#ページ
                    "Remarks",#備考
                    "Reference",#報告書名
                    "Publisher",#発行者
                    "Latitude",#緯度
                    "Longitude",#経度
                    "Note1",#(非公開）
                    "Note2",#(非公開）
                    "Note3",#(非公開）
                    "Note4",#(非公開）
                    "Note5")#(非公開）
# Eliminate Unnecessary Columns
c14raw  <-  c14raw[,-which(names(c14raw)%in%c("elim1","elim2","elim3","Note1","Note2","Note3","Note4","Note5"))]

# Create boolean field to determine whether date should be kept in the final redacted translation:
c14raw$toKeep  <-  TRUE

# Use row number as a foreign key to link back to the original database (as this one does not contain a unique identifier:
c14raw$originalRow  <- 2:(nrow(c14raw)+1)

# Extract nabunken URL from "Remarks" and create dedicated field:
c14raw$nabunkenURL  <-  as.character(str_match_all(c14raw$Remarks, 'https://sitereports.nabunken.go.jp/\\d+'))
c14raw$nabunkenURL[which(c14raw$nabunkenURL=='character(0)')]=NA

# Remove carriage return, new lines, and white spaces from Site names and references:
c14raw$Reference  <-  gsub("[\r\n]", "", c14raw$Reference)
c14raw$SiteName  <-   gsub("[\r\n]", "", c14raw$SiteName)
c14raw$SiteName  <-   gsub(" ", "", fixed=TRUE, c14raw$SiteName)
c14raw$Phase  <-  gsub("[\r\n]", "", c14raw$Phase)
c14raw$Period  <-  gsub("[\r\n]", "", c14raw$Period)
c14raw$SiteLocation  <-  gsub("[\r\n]", "", c14raw$SiteLocation)
c14raw$LabCode  <- trimws(c14raw$LabCode)

# Handle LabCodes ----

# Identify missing/imcomplete LabCodes:
c14raw$labcode_issues  <-  NA
index.na  <-  which(is.na(c14raw$LabCode)|c14raw$LabCode=="不明")
index.only.letters  <-  which(!grepl("[^A-Za-z]", c14raw$LabCode))
index.only.numbers <- which(!grepl("\\D", c14raw$LabCode))
index.questionmark <- grep("\\?", c14raw$LabCode) #LabCodes with questionmarks
c14raw$labcode_issues[index.only.letters] <- "Labcode does not contain numbers"
c14raw$labcode_issues[index.only.numbers] <- "Labcode does not contain letters"
c14raw$labcode_issues[index.questionmark] <- "Labcode contains a question mark"
c14raw$labcode_issues[index.na] <- "Missing or Unknown"
# Define which entries should be kept
c14raw$toKeep[unique(c(index.only.letters,index.only.numbers,index.questionmark,index.na))] <- FALSE


# Clean 14C and Delta13C Fields ----

# Handle non-numeric characters (tailspaces)
c14raw$UnroundedCRA <- trimws(c14raw$UnroundedCRA)
c14raw$UnroundedCRAError <- trimws(c14raw$UnroundedCRAError)
c14raw$CRA <- trimws(c14raw$CRA)
c14raw$CRAError <- trimws(c14raw$CRAError)
# unique(c14raw$UnroundedCRA[grep('[^0-9.-]',c14raw$UnroundedCRA)]) # Some cases of Japaese blank space:
c14raw$UnroundedCRA[which(c14raw$UnroundedCRA=='　')] = NA
# unique(c14raw$UnroundedCRAError[grep('[^0-9.-]',c14raw$UnroundedCRAError)]) # No issues

# Handle non-numeric symbols:
# unique(c14raw$CRA[grep('[^0-9.-]',c14raw$CRA)]) #Several non-numeric symbols
# unique(c14raw$CRAError[grep('[^0-9.-]',c14raw$CRAError)]) #Several non-numeric symbols
c14raw$c14_issues <- NA
index1 <- unique(c(grep('[^0-9.-]',c14raw$CRA),grep('[^0-9.-]',c14raw$CRAError)))
index2 <- unique(c(grep('[^0-9.-]',c14raw$UnroundedCRA),grep('[^0-9.-]',c14raw$UnroundedCRAError)))
c14raw$c14_issues[index1] <- 'Non numeric values in C14 field'
c14raw$c14_issues[index2] <- 'Non numeric values in C14 field'
c14raw$toKeep[index1] <- FALSE #Exclude dates that have issues with 14C dates
c14raw$toKeep[index2] <- FALSE

# Handle Missing missing dates
index3 <- which(!(!(is.na(c14raw$CRA)|is.na(c14raw$CRAError)) | !(is.na(c14raw$UnroundedCRA)|is.na(c14raw$UnroundedCRAError))))
c14raw$c14_issues[index3] <- 'Missing data for calibration'
c14raw$toKeep[index3] <- FALSE

# Hanlde Dates outside calibration range:
onlyDates <- data.frame(CRA=as.numeric(c14raw$CRA),CRAError=as.numeric(c14raw$CRAError),UnroundedCRA=as.numeric(c14raw$UnroundedCRA),UnroundedCRAError=as.numeric(c14raw$UnroundedCRAError))
onlyDates$C14Age <- onlyDates$UnroundedCRA
onlyDates$C14Age[which(is.na(onlyDates$C14Age))] <- onlyDates$CRA[which(is.na(onlyDates$C14Age))]
onlyDates$C14Error <- onlyDates$UnroundedCRAError
onlyDates$C14Error[which(is.na(onlyDates$C14Error)|onlyDates$C14Error<=0)] <- onlyDates$CRAError[which(is.na(onlyDates$C14Error)|onlyDates$C14Error<=0)]
index4 <- which(as.numeric(onlyDates$C14Age)<0 | as.numeric(onlyDates$C14Age)>55000)
c14raw$c14_issues[index4] <- 'Dates outside calibration range'
c14raw$toKeep[index4] <- FALSE
index4b  <- which(as.numeric(onlyDates$C14Error)<=0) 
c14raw$c14_issues[index4b] <- 'Incorrect c14 error'
c14raw$toKeep[index4b] <- FALSE


# Handle Delta13C Issues
Delta13C <- gsub("[()]","",c14raw$Delta13C) |> as.numeric()
Delta13CError <- gsub("[()]","",c14raw$Delta13CError) |> as.numeric()
Delta13CIRMS <- gsub("[()]","",c14raw$Delta13CIRMS)
Delta13CIRMS <- gsub("‰","",Delta13CIRMS)
Delta13CIRMS[grep(",",Delta13CIRMS)] <- NA
Delta13CIRMS <- as.numeric(Delta13CIRMS)
Delta13C[which(Delta13C>0)] <- Delta13C[which(Delta13C>0)]*(-1)
Delta13CIRMS[which(Delta13CIRMS>0)] <- Delta13CIRMS[which(Delta13CIRMS>0)]*(-1)
Delta13C[which(Delta13C<(-35)|Delta13C>(-5))] <- NA
Delta13CIRMS[which(Delta13CIRMS<(-35)|Delta13CIRMS>(-5))] <- NA
c14raw$Delta13C <- Delta13C
c14raw$Delta13CError <- Delta13CError
c14raw$Delta13CIRMS <- Delta13CIRMS

# Handle Duplicated Dates and References ----
# Duplicates to be removed,
# Different references should be stored into a single aggregated field

# Create an aggregate field for reference:
c14raw$ReferenceCombined <- paste0(c14raw$Publisher,", ",c14raw$PubblicationYear,", ",c14raw$Reference, ", (pp.",c14raw$Page,")") #combine info from multiple fields
c14raw$ReferenceFinal <- NA #place holder

# Identify instances of LabCode duplicates:
# any(duplicated(c14raw$LabCode))
duplicates <- sort(table(c14raw$LabCode)[which(table(c14raw$LabCode)>1)],TRUE)
c14raw$IsDuplicate <- FALSE

# Actual duplicate handling options
## 1. If the labcode is NA or missing, then consider date to be unreliable
## 2. If the labcode is problematic (i.e. character or numbers only), then check whether other key info have also any duplicates. If there are none do nothing. If there are duplicates consider all instances to be unreliable.
## 3. If the labcode is non-problematic check if other key info have duplicates. If these info are different, take not that the labcode is incorrect. 

keycolumns <- c("PrefectureCode","SiteName","MaterialCode1","MaterialCode2","LabCode","Method","CRA","CRAError")

for (i in 1:length(duplicates))
{
  labcodename <- names(duplicates[i])
  index.labcodename <- which(c14raw$LabCode==labcodename)
  number.cases <- as.numeric(duplicates[i])
  
  if(labcodename=="不明") {
	  c14raw$labcode_issues[index.labcodename] <- 'Missing or Unknown'
	  c14raw$toKeep[index.labcodename] <- FALSE
	  next()
  }

  # Case 2: 
  tmp <- c14raw[index.labcodename,keycolumns]
  tmp.number.cases <- nrow(unique(tmp))

  if (tmp.number.cases==1)  # if all duplicates
	  {
		  #Extract and combined reference
		  c14raw$ReferenceFinal[index.labcodename] <-  paste0(unique(c14raw$ReferenceCombined[index.labcodename]),collapse="||")

		  #Select first index as  "original" and the others as duplicates
		  c14raw$IsDuplicate[index.labcodename[-1]] <- TRUE
		  c14raw$toKeep[index.labcodename[-1]] <- FALSE
	  }
  if (tmp.number.cases>1) # if some variations in info
	  {
		  c14raw$labcode_issues[index.labcodename] <- ''
		  if(length(unique(tmp$PrefectureCode))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different Prefecture',sep=';')}
		  if(length(unique(tmp$SiteName))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different SiteName',sep=';')}
		  if(length(unique(tmp$MaterialCode1))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different MaterialCode1',sep=';')}
		  if(length(unique(tmp$MaterialCode2))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different MaterialCode2',sep=';')}
		  if(length(unique(tmp$Method))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different Method',sep=';')}
		  if(length(unique(tmp$CRA))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different CRA',sep=';')}
		  if(length(unique(tmp$CRAError))>1){c14raw$labcode_issues[index.labcodename]<-paste(c14raw$labcode_issues[index.labcodename],'Same labcode, different CRAError',sep=';')}
		  
		  c14raw$toKeep[index.labcodename] <- FALSE
	  }
}


# Move remaining case of references in the `ReferenceFinal` field:
i = which(is.na(c14raw$ReferenceFinal))
c14raw$ReferenceFinal[i] = c14raw$ReferenceCombined[i]

# Check if there are still labcode dubplicates
anyDuplicated(subset(c14raw,toKeep==TRUE)$LabCode)
which(table(subset(c14raw,toKeep==TRUE)$LabCode)>1)

# Handle 14C Rounding Issues ----
# The field `CRA` should contain rounded 14C ages after corrections for isotopic fractionation, whilst the field `UnroundedCRA` contains the same date without rounding. In theory the difference between the two dates should be small, but the list below suggests larger discrepancies suggestive of input error

threshold <- 50 #Acceptable difference
absDiff <- abs(as.numeric(c14raw$CRA)-as.numeric(c14raw$UnroundedCRA))
index.above.threshold <- which(absDiff>threshold)
c14raw$toKeep[index.above.threshold] <- FALSE
c14raw$c14_issues[index.above.threshold] <- paste("difference between unrounded and CRA >",threshold) 

# Handling Geographic Coordinates ----
# Create place holder for storing issues
c14raw$coordinate_issues  <-  NA

# Identify instances of missing coordinates and convert them as NA:
c14raw$Latitude[which(c14raw$Latitude=='不明')] <- NA
c14raw$Longitude[which(c14raw$Longitude=='不明')] <- NA
coordinate.na.index <- which(is.na(c14raw$Latitude)|is.na(c14raw$Longitude))
c14raw$coordinate_issues[coordinate.na.index] <- 'Missing coordinates'
# c14raw$toKeep[coordinate.na.index] = FALSE

# Convert degree minute seconds into degree decimals:
nonNAs <- which(is.na(c14raw$Latitude)|is.na(c14raw$Longitude))
c14raw$Latitude[-nonNAs] <- as.numeric(char2dms(paste0(c14raw$Latitude[-nonNAs],"N"),chd='゜',chm="'"))
c14raw$Longitude[-nonNAs] <- as.numeric(char2dms(paste0(c14raw$Longitude[-nonNAs],"E"),chd='゜',chm="'"))
c14raw$Latitude <- as.numeric(c14raw$Latitude)
c14raw$Longitude <- as.numeric(c14raw$Longitude)

# Translate prefecture names:
## Remove '-ken' and '-fu'
c14raw$Prefecture <- str_remove(c14raw$Prefecture,'府')
c14raw$Prefecture <- str_remove(c14raw$Prefecture,'県')
## Match to Lookup Table
pref.lookup <- read.csv(here('lookup_tables','prefectures_translations.csv'),stringsAsFactors=FALSE)
colnames(pref.lookup) <- c("Prefecture","PrefectureNameEn","Region","NabunkenPrefCode")
c14raw  <- left_join(c14raw,pref.lookup,by=c('Prefecture'))

# Check whether the coordinates fall within the designated prefectures using the utility function `checkPrefecture()`, and store them on designated fields:

source(here('utility','checkPrefecture.R'))
japanMap  <- st_read(dsn=here('gis',layer='polbnda_jpn.shp'))
japanMap$pref <- unlist(lapply(japanMap$nam,function(x){strsplit(x,split=' ')[[1]][1]}))
japanMap$pref[which(japanMap$pref=='Hokkai')]  <- 'Hokkaido'
japanMap  <- st_transform(japanMap,crs=4326)
original.coord.check <- checkPrefecture(lat=c14raw$Latitude,lon=c14raw$Longitude,pref = c14raw$PrefectureNameEn,map=japanMap)
c14raw$coord.prefecture.check <- original.coord.check

#Determine if there are instances of mismatches between coordinates and prefectures:
manual.prefecture.check.index <- which((c14raw$coord.prefecture.check==FALSE|is.na(c14raw$coord.prefecture.check))&!is.na(c14raw$Latitude))
manual.prefecture.check <- c14raw[manual.prefecture.check.index,]
manual.prefecture.check  <- unique(manual.prefecture.check[,c("Prefecture","SiteName","SiteLocation","Latitude","Longitude","coord.prefecture.check")])


# Manual checks
#神奈川   猿島洞穴遺跡 is ok
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[1]))]=TRUE

# 熊本         荘貝塚 (should be Kagoshima)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[2]))]=FALSE
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[2]))]=FALSE
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[2]))[2]]=TRUE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[2]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[2]))[1]]="Site coordinate be in Kagoshima, Prefecture recorded as Kumamoto"

# 熊本       鞠智城跡 (coodinate in Hokkaido)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[3]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[3]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[3]))]="Site coordinate in Hokkaido, Prefecture recored as Kumamoto"

# 北海道     落部１遺跡 (coodinate in Aichi)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[4]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[4]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[4]))]="Site coordinate in Aichi, Prefecture recorded as Hokkaido"

# 北海道      倉知川右岸遺跡 (coodinate in Shizuoka)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[5]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[5]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[5]))]="Site coordinate in Shizuoka but Prefecture recored as Hokkaido"

# 徳島       大谷尻遺跡 (coodinate in Aichi) ... correct coordinate is  34.0566 133.9768
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[6]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[6]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[6]))]='Coordinate in Aichi but Prefecture recorded as Tokushima. Correct coordinate should be 34.0566 133.9768'

# 奈良       観音寺本馬遺跡 (coordinate in Yamagata)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[7]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[7]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[7]))]='Coordinate in Yamagata but Prefecture recored as Nara'

# 島根  湯里天神遺跡 coordinate in Gunma ... correct coordinate is 35.105833 132.380277 (large discrepancy)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[8]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[8]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[8]))]='Coordinate in Gunma, Prefecture recorded as Shimane. Correct coordinate is 35.105833 132.380277'
# c14raw$toKeep[ii] = FALSE #Eliminate site as wrong coordinates

# 鳥取  久見高丸遺跡 coordinate in Tottori ... correct coordinate is 36.330555 133.238888 (small discrepancy)
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[9]))]=TRUE
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[9]))]='Correct coordinate is 36.330555 133.23888'
# 新潟  堂屋敷遺跡 coordinate in Saitama 
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[10]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[10]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[10]))]='Coordinate in Saitama, Prefecture recored as Niigata'

# 長野  東峰遺跡 coordinate in Gunma
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[11]))]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[11]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[11]))]='Coordinate in Gunma, Prefecture recorded as Nagano'

# 長野  旭久保C遺跡 coordinate in Gunma
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[12]))[1:4]]=TRUE
c14raw$coord.prefecture.check[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[12]))[5:6]]=FALSE
ii = which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[12]))
c14raw[ii,c('Prefecture','Latitude','Longitude')]
c14raw$coordinate_issues[which(c14raw$SiteLocation%in%c(manual.prefecture.check$SiteLocation[12]))[5:6]]='Coordinate in Gunma but Prefecture recorded as Nagano'

# Site Duplicates:
# The database does not provide unique identifier for sites. Thus several sites with different names might refer to the same site, but equally because coordinates were often derived from addresses, multiple sites with different names might have the same spatial coordinate as well. In some rare cases the same site might have also different coordinates in different publications. 

# Check all cases where the same coordinates are matched to different site names.

##Assign a temporary unique ID to Coordinate Pairs
c14raw.sitenames  <- select(c14raw,Prefecture,SiteName,Latitude,Longitude)
comb <- do.call(paste, c(as.list(c14raw.sitenames[c("Latitude","Longitude")]), sep = "."))
c14raw.sitenames$coordid <- match(comb, unique(comb))

#If no instances of multiple coordinates there should be no duplicates in id
c14raw.sitenames.unique  <- unique(c14raw.sitenames)
any(duplicated(c14raw.sitenames.unique$id)) #results indicate there are no instances where the same set of coordinates are referring to different sites. 

# Check all cases where the same site name is matched to different coordinates.
## Assign a temporary unique ID to Prefecture-SiteName pairs
c14raw.sitenames <- select(c14raw,Prefecture,SiteName,Latitude,Longitude)
comb <- do.call(paste, c(as.list(c14raw.sitenames[c("Prefecture","SiteName")]), sep = "."))
c14raw.sitenames$id <- match(comb, unique(comb))
#If no instances of multiple coordinates there should be no duplicates in id:
c14raw.sitenames.unique  <- unique(c14raw.sitenames)
any(duplicated(c14raw.sitenames.unique$id)) #Result is TRUE, indicating that we do have cases where the same site name is matched to different coordinates. 

# `dist_threshold` defines the inclusion criteria. In this case all instances where the maximum inter-distance between the sites was above 1km was excluded.  

dist_threshold <- 1
c14raw$same.site.max.d <- NA
index.same.site.diff.coord <- which(duplicated(c14raw.sitenames.unique$id)) #which index are duplicates
dup.ids <- unique(c14raw.sitenames.unique$id[index.same.site.diff.coord])#what are the temporary ID of those duplicates

for (i in 1:length(dup.ids))
{
	tmp.index<-which(c14raw.sitenames$id==dup.ids[i])
	tmp.df<-c14raw[tmp.index,]
	tmp.df<-select(tmp.df,PrefectureNameEn,SiteName,Latitude,Longitude) |> unique()

	if(all(is.na(tmp.df$SiteName)))
	{
		next()
	}


	if (length(unique(tmp.df$PrefectureNameEn))==1)
	{

		tmp.df.sites  <-  tmp.df
		coordinates(tmp.df.sites)  <- c("Longitude","Latitude")
		proj4string(tmp.df.sites)  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
		tmp.dist_mat  <- spDists(tmp.df.sites,longlat=TRUE) #inter-site distance matrix
		c14raw$coordinate_issues[tmp.index] <- paste0(c14raw$coordinate_issues[tmp.index],"; Same site name with different coordinates (dist=",round(max(tmp.dist_mat),2)," km)")
# 		if (max(tmp.dist_mat) > dist_threshold) {c14raw$toKeep[tmp.index] = FALSE}
		c14raw$same.site.max.d <- max(tmp.dist_mat)
	}

	if (length(unique(tmp.df$PrefectureNameEn))>1)
	{
		print(i)
		break()
	}
}

# Site Name Romanisation and Site Typology ----

# Site names have been romanised by first using the Nabunken Site Report Database to automatically extract the _furigana_ of site names in _kanji_, followed by a manual check for fixing potential mismatch and missing _furigana_. Romanisation has been carried out using the `kana2roma()` function from the `Nippon` package. 

library(Nippon)
source(here('utility','siteNameTranscript.R'))

### The following block is executed only in the very first iteration:
#siteList = unique(c14raw$SiteName)
#sitenames=siteNameTranscript(siteList)
##Export for manual editing
#sitenames$matched=(sitenames$SiteName==sitenames$MatchedName)
#sitenames$ToCheck = !sitenames$matched | is.na(sitenames$Furigana)
#sitenames=dplyr::select(sitenames,SiteName,Furigana,ToCheck)
#sitenames$Alternative=NA
#write.csv(sitenames,"./lookup/sitenamesTranscript.csv",row.names = FALSE)

###This block is from second iteration onwards
#Import manually edited file
sitenames  <- read.csv(here('lookup_tables','sitenamesTranscript.csv'),stringsAsFactors = FALSE)
siteList  <-  unique(c14raw$SiteName)

# The following step updates the lookup table in case there are some missing sites
if (!all(unique(siteList)%in%sitenames$SiteName))
{
  missingSites = unique(siteList)[which(!unique(siteList)%in%sitenames$SiteName)]
  additional_sitenames=siteNameTranscript(missingSites)
  sitenames=rbind.data.frame(sitenames,data.frame(SiteName=additional_sitenames$SiteName,
                                                      Furigana=additional_sitenames$Furigana,
                                                      ToCheck=TRUE,
                                                      Alternative=NA))
  write.csv(sitenames,here('lookup_tables','sitenamesTranscript_toprocess.csv'),row.names = FALSE)
}

# The section reads and merges the sitenames to the main data.frame
sitenames  <- read.csv(here('lookup_tables','sitenamesTranscript.csv'),stringsAsFactors = FALSE)

# Define Site Type
sitenames$SiteType<-NA
# 遺跡群
sitenames$SiteType[grep('いせきぐん',sitenames$Furigana)]<-'Site Cluster'
# 遺跡
sitenames$SiteType[which(is.na(sitenames$SiteType)&grepl('いせき',sitenames$Furigana))]<-'Site'
sitenames$SiteType[which(is.na(sitenames$SiteType)&grepl('遺跡',sitenames$SiteName))]<-'Site'
# 貝塚
sitenames$SiteType[grep('かいづか',sitenames$Furigana)]<-'Shell Midden'
sitenames$SiteType[grep('貝塚',sitenames$SiteName)]<-'Shell Midden'
# 窯跡群
sitenames$SiteType[grep('窯跡群',sitenames$SiteName)]<-'Kiln Cluster'
# 窯跡
sitenames$SiteType[which(is.na(sitenames$SiteType)&grepl('窯跡',sitenames$SiteName))]<-'Kiln'
sitenames$SiteType[grep('窯跡群',sitenames$SiteName)]<-'Kiln'
sitenames$SiteType[grep('窯',sitenames$SiteName)]<-'Kiln'
# 館跡
sitenames$SiteType[grep('館跡',sitenames$SiteName)]<-'Fort'
# 城跡
sitenames$SiteType[grep('城跡',sitenames$SiteName)]<-'Castle'
# 古墳
sitenames$SiteType[grep('古墳',sitenames$SiteName)]<-'Kofun'
# 洞窟
sitenames$SiteType[grep('洞窟',sitenames$SiteName)]<-'Cave'
sitenames$SiteType[grep('洞穴',sitenames$SiteName)]<-'Cave'
# 城
sitenames$SiteType[grep('城',sitenames$SiteName)]<-'Castle'
# 横穴
sitenames$SiteType[grep('横穴',sitenames$SiteName)]<-'Side-Hole Burial'
# 水田址
sitenames$SiteType[grep('水田址',sitenames$SiteName)]<-'Paddy Field'
# 寺跡
sitenames$SiteType[grep('寺跡',sitenames$SiteName)]<-'Temple/Shrine'
# 列石
sitenames$SiteType[grep('列石',sitenames$SiteName)]<-'Standing Stones'
# Everything Else
sitenames$SiteType[which(is.na(sitenames$SiteType))]<-'Other'

# Romanise #
sitenames$Romanised <- kana2roma(sitenames$Furigana)
sitenames$Romanised <- stringr::str_to_title(sitenames$Romanised)

#Merge Back to Main DB
c14raw<-left_join(x=c14raw,y=sitenames,by=c("SiteName"="SiteName"))

#Add issues with SiteName
c14raw$siteName_issues  <-  NA
ii = which(c14raw$Alternative!='')
c14raw$siteName_issues[ii]  <-  paste0('Site Name should be',c14raw$Alternative[ii])


# Remove samples with no sitename
i  <-  which(c14raw$SiteName==""|is.na(c14raw$SiteName))
c14raw$toKeep[i]  <-  FALSE
c14raw$siteName_issues[i]  <-  "Site Name Missing"


# Handle Material Fields ----

# Read Lookup tables
matcode1 <- read.csv(here('lookup_tables','materialGeneralCode.csv'))
matcode2 <- read.csv(here('lookup_tables','materialDetailsCode.csv'))

c14raw  <- left_join(c14raw,matcode1,by=c("MaterialCode1"="Code"))
c14raw  <- left_join(c14raw,matcode2,by=c("MaterialCode2"="Code"))
colnames(c14raw)[which(colnames(c14raw)=="Description.x")]  <- "Material"
colnames(c14raw)[which(colnames(c14raw)=="Description.y")]  <- "Material_Details" 

# Generate a lookup table for the field "MaterialType" which will be used to extract the taxa information. 

# Material detail is translated by using a lookup table. First we clean the relevant field
c14raw$MaterialType <- gsub("[\r\n]", "", c14raw$MaterialType) #Remove escapes
c14raw$MaterialType <- gsub("　", " ", c14raw$MaterialType) #Convert to half-width encoding
c14raw$MaterialType <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", c14raw$MaterialType) #remove trail space
# We then create a data.frame with unique instances of `MaterialType`:
material_taxa  <-  data.frame(MaterialType=unique(c14raw$MaterialType),Taxa=NA)
# And export
write.csv(material_taxa,file=here('lookup_tables','material_taxa_todo.csv'),row.names=FALSE)

# `material_taxa_todo.csv` file is then read and merged back to the main database:
material_taxa_import  <- read.csv(here('lookup_tables','material_taxa.csv'))
c14raw  <- left_join(x=c14raw,y=material_taxa_import,by="MaterialType")
c14raw$Taxa[which(c14raw$Taxa=="")] = NA


# Prepare DB to export ----

# Organise database and produce:

## Full Version with Issues for Y.Kudo's check
c14raw$issues  <-   FALSE
c14raw$issues[which(!is.na(c14raw$labcode_issues)|!is.na(c14raw$c14_issues)|!is.na(c14raw$coordinate_issues)|!is.na(c14raw$siteName_issues))]  <-  TRUE
c14db_toCheck = select(c14raw,originalRow,LabCode,PrefectureCode,Prefecture,SiteName,SiteLocation,SamplingLocation,MaterialType,MaterialCode1,MaterialCode2,Period,Phase,PeriodCode,Method,CRA,CRAError,UnroundedCRA,UnroundedCRAError,Delta13C,Delta13CError,Delta13CIRMS,AnalysedBy,Laboratory,PubblicationYear,ReportTitle,Page,Remarks,Reference,Publisher,Latitude,Longitude,nabunkenURL,labcode_issues,c14_issues,coordinate_issues,siteName_issues,IsDuplicate,issues,toKeep)
write.csv(c14db_toCheck,file=here('output','c14db_toCheck.csv'),row.names=FALSE)

# Full Version for Pubblication
## Remove unreliable Lat/Longs
coord.safe  <-  which(!is.na(c14raw$coordinate_issues))
c14raw$Latitude[coord.safe]  <- NA
c14raw$Longitude[coord.safe]  <- NA
## Subset dates samples to retain
c14db  <- subset(c14raw,toKeep==TRUE)
c14db$CRA  <- as.numeric(c14db$CRA)
c14db$CRAError  <- as.numeric(c14db$CRAError)
c14db$UnroundedCRA  <- as.numeric(c14db$UnroundedCRA)
c14db$UnroundedCRAError  <- as.numeric(c14db$UnroundedCRAError)

c14db  <- select(c14db,LabCode,Prefecture=PrefectureNameEn,Region,SiteNameJp=SiteName,SiteNameEn=Romanised,SiteType=SiteType,Latitude,Longitude,CRA,CRAError,UnroundedCRAError,UnroundedCRA,Delta13C,Delta13CError,Delta13CIRMS,DatingMethod=Method,Material,MaterialDetails=Material_Details,MaterialTaxa=Taxa,NabunkenURL=nabunkenURL,Reference=ReferenceFinal)
write.csv(c14db,file=here('output','c14db_1.1.0.csv'),row.names=FALSE)
saveRDS(c14db,file=here('output','c14db_1.1.0.Rds'))

# Full Internal Version ----
c14raw$CRA  <- as.numeric(c14raw$CRA)
c14raw$CRAError  <- as.numeric(c14raw$CRAError)
c14raw$UnroundedCRA  <- as.numeric(c14raw$UnroundedCRA)
c14raw$UnroundedCRAError  <- as.numeric(c14raw$UnroundedCRAError)
write.csv(c14raw,file=here('output','c14raw_1.1.0.csv'))
saveRDS(c14raw,file=here('output','c14raw_1.1.0.Rds'))
