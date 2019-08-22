library(tidyverse)
library(readxl)
library(measurements)
library(sp)
library(NipponMap) 
library(mapdata)
library(maptools)

#### Clean Kudo's Original Table ####

## Read Data
c14raw = read_xlsx( "../raw_data/current.xlsx", skip=1)
c14raw = as.data.frame(c14raw)

## Assign New Columns Names
colnames(c14raw)= c("PrefectureCode", #都道府県コード
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
		    "CorrectedC14Age",#暦年較正用14C年代
		    "elim2",#
		    "CorrectedC14Error",#暦年較正用14C年代
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
		   
## Eliminate Unecessary Columns
c14raw = c14raw[,-which(names(c14raw)%in%c("elim1","elim2","elim3","Note1","Note2","Note3","Note4","Note5"))]


#### PROCESSING ####
c14db = c14raw

#### Convert Latitude/Longitude into Degree Decimals ####
c14db$Latitude=as.numeric(char2dms(paste0(c14db$Latitude,"N"),chd='゜',chm="'"))
c14db$Longitude=as.numeric(char2dms(paste0(c14db$Longitude,"E"),chd='゜',chm="'"))

#### Extract prefecture names 
pref = read.csv("~/gitrepos/ENCOUNTER/encounter_c14db/processed_data/prefectures_translations.csv")
colnames(pref) = c("Prefecture","PrefectureNameEn","Region")

c14db=plyr::join(c14db,pref,type='left')

# Check site coordinates
sitecoord = data.frame(RowN=3:(nrow(c14db)+2),
		       PrefectureCode=c14db$PrefectureCode,
		       PrefectureName=c14db$PrefectureNameEn,
		       SiteName=c14db$SiteName,
		       SiteLocation=c14db$SiteLocation,
		       Latitude=round(c14db$Latitude,4),
		       Longitude=round(c14db$Longitude,4))
sitecoord = unique(sitecoord)

### Are sites within Japan?

jwgs84 = rgdal::readOGR(dsn="~/gitrepos/ENCOUNTER/encounter_c14db/gis",layer='japan_wgs84')
jjgd2000 = rgdal::readOGR(dsn="~/gitrepos/ENCOUNTER/encounter_c14db/gis",layer='japan_jgd2000')

site_wgs84 = sitecoord
site_jgd2000 = sitecoord
coordinates(site_wgs84) <- c("Longitude","Latitude")
coordinates(site_jgd2000) <- c("Longitude","Latitude")
proj4string(site_wgs84) <- proj4string(jwgs84)
proj4string(site_jgd2000) <- proj4string(jjgd2000)

#Extract Prefectures based on coordinates
sitecoord$extractedWGS84=over(site_wgs84,jwgs84)$prefecture
sitecoord$extractedJGD2000=over(site_jgd2000,jjgd2000)$prefecture

#Check if there any mismatch between the extracted coordinates
any(as.character(sitecoord$extractedWGS84)!=as.character(sitecoord$extractedJGD2000),na.rm=T) 

#Check if any site was not assigned to any prefecture
i1 = which(is.na(sitecoord$extractedJGD2000)) # same result
i2 = which(is.na(sitecoord$extractedWGS84)) # same result
all(i1==i2) #are they identical?

#Report:
# * Several sites have coordinates of 0,0 ... these should be replaced with NA
# * SiteLocation of mismatched address is 小笠原村北硫黄島 and 横須賀市猿島. 
# * 横須賀市猿島 coordinates are correct
# * 小笠原村北硫黄島 are the same to 横須賀市猿島 and should be corrected.

#Mismatch between extracted sites and assigned sites

i = which(sitecoord$PrefectureName!=as.character(sitecoord$extractedWGS84)&sitecoord$Latitude!=0&sitecoord$Longitude!=0) #mismatching sites
k = which(is.na(sitecoord$extractedWGS84)) # sites with no coordinates
mismatched=sitecoord[c(i,k),]

write.csv(mismatched,"~/gitrepos/ENCOUNTER/encounter_c14db/problems/mismatchsite.csv")



### Several Sites have different coordinates (due to differently recorded addresses)

c14sites<-select(c14db,Prefecture,SiteName,SiteLocation,Latitude,Longitude) %>% unique()

# Sites with different coordinates
uniqueSitesAndCoords = select(c14sites,Prefecture,SiteName,Latitude,Longitude)%>%unique

dSites = unique(uniqueSitesAndCoords$SiteName[duplicated(uniqueSitesAndCoords$SiteName)])

c14db$RowN=3:(nrow(c14db)+2)
differentCoordinates<-filter(c14db,SiteName%in%dSites)%>%arrange(SiteName)%>%select(RowN,SiteName,SiteLocation,Prefecture,Latitude,Longitude)

#for each site compute distance to the furtherest site
diffCoordSites = unique(data.frame(SiteName=differentCoordinates$SiteName,Prefecture=differentCoordinates$Prefecture)) 
diffCoordSites = diffCoordSites[!is.na(diffCoordSites$SiteName),]
differentCoordinates$maxD = NA

for (i in 1:nrow(diffCoordSites))
{
	ii = which(differentCoordinates$SiteName==diffCoordSites$SiteName[i]&differentCoordinates$Prefecture==diffCoordSites$Prefecture[i])
	ccrd = data.frame(lon=differentCoordinates$Longitude[ii],lat=differentCoordinates$Latitude[ii])
	maxDist=max(spDists(x=as.matrix(ccrd),longlat=TRUE)) 
	differentCoordinates$maxD[ii] = maxDist
}

#Eliminate sites with same name from different prefectures:
differentCoordinates=differentCoordinates[which(differentCoordinates$maxD>0),]



write.csv(differentCoordinates,"~/gitrepos/ENCOUNTER/encounter_c14db/problems/differentCoordinates.csv")


#### Coordinates with Different Sites

# extract coordinates
coords = select(c14db,Latitude,Longitude)%>%filter(Latitude!=0&Longitude!=0)%>%unique()

index <- numeric()

for (i in 1:nrow(coords))
{
	tmp <- filter(c14db,Latitude==coords$Latitude[i]& Longitude==coords$Longitude[i])
	if (length(unique(tmp$SiteName))>1)
	{
		index=c(index,tmp$RowN-2)
	}
}

differentSites <- c14db[index,] %>% select(Latitude,Longitude,SiteName,RowN)

write.csv(differentSites,"~/gitrepos/ENCOUNTER/encounter_c14db/problems/differentSites.csv")




#######################
####  Translation  ####
#######################

# Calibration Method
c14db$Method_En[which(c14db$Method%in%c("β線法","β線","β線法\n"))]="Beta Counting"
c14db$Method_En[which(c14db$Method=="AMS法")]="AMS"


# MatericalCode1 & MaterialCode2
matcode1 = read.csv("./processed_data/materialGeneralCode.csv")
matcode2 = read.csv("./processed_data/materialDetailsCode.csv")

if (!all(c14db$MaterialCode1%in%matcode1$Code))
{
	i=which(!c14db$MaterialCode1%in%matcode1$Code)
	c14db[i,]
	c14db$MaterialCode1[i]
}


if (!all(c14db$MaterialCode2%in%matcode2$Code))
{
	j=which(!c14db$MaterialCode2%in%matcode2$Code)
	c14db[j,]
	c14db$MaterialCode2[j]
}


#### Translate Site Chronology ####

## Period
#  periods = unique(data.frame(Period=c14db$Period,PeriodEN=NA))
#  periods = arrange(periods,Period)
#  write.csv(periods,file="./translation/periods.csv",row.names=F)

periods = read.csv("./translation/periods.csv")
period.tmp=data.frame(Period=unique(as.character(c14db$Period)))
period.tmp=left_join(period.tmp,periods,by="Period")
period.tmp$Period=as.character(period.tmp$Period)
c14db=left_join(x=c14db,y=period.tmp,by="Period")

## Phases:
#  phases = unique(data.frame(Phase=c14db$Phase,PhaseEN=NA))
#  phases = arrange(phases,Phase)
#  write.csv(phases,file="./translation/phases.csv",row.names=F)

phases  = read.csv("./translation/phases.csv")
phases.tmp=data.frame(Phase=unique(as.character(c14db$Phase)))
phases.tmp=left_join(phases.tmp,phases,by="Phase")
phases.tmp$Phase=as.character(phases.tmp$Phase)
c14db=left_join(x=c14db,y=phases.tmp,by="Phase")
 
#### Transliterate Site Names ####
sitesWithFurigana<-read.csv("./translation/sitenames.csv")
c14db=left_join(x=c14db,y=sitesWithFurigana,by="SiteName")





