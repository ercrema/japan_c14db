
library(readxl)
library(measurements)
library(sp)
library(NipponMap) 
library(mapdata)
library(maptools)

setwd("~/gitrepos/ENCOUNTER/encounter_c14db/")

#### Clean Kudo's Original Table ####

## Read Data
c14raw = read_xlsx("./raw_data/★年代測定データベース学術・関東･東北・北陸・中部・鹿児島190423.xlsx",skip=1)
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

# Add Here Scripts for checking site coordinates
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

sitecoord$extractedWGS84=over(site_wgs84,jwgs84)$prefecture
sitecoord$extractedJGD2000=over(site_jgd2000,jjgd2000)$prefecture

#Mismatch sites
# j = which(as.character(sitecoord$extractedWGS84)!=as.character(sitecoord$extractedJGD2000))
# length(j)


i = which(sitecoord$PrefectureName!=as.character(sitecoord$extractedWGS84)) #mismatching sites

k = which(is.na(sitecoord$extractedWGS84)) # sites with no coordinates

mismatched=sitecoord[c(i,k),]

write.csv(mismatched,"~/gitrepos/ENCOUNTER/encounter_c14db/problems/mismatchsite.csv")


### Check Sites with different coordinates



####  Translate Methods
# Method
c14db$Method_En = NA
c14db$Method_En[which(c14db$Method%in%c("β線法","β線","β線法\n"))]="Beta Counting"
c14db$Method_En[which(c14db$Method=="AMS法")]="AMS"


### Check DB codes ####

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







