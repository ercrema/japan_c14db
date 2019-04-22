#### Setup
setwd("~/gitrepos/ENCOUNTER/encounter_c14db")
library(readxl)
library(measurements)
library(sp)

#### Clean Kudo's Original Table ####

## Read Data
c14raw = read_xlsx("./raw_data/★年代測定データベース学術・関東･東北・北陸・中部・鹿児島181031.xlsx",skip=1)
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

# Add Here Scripts for checking site coordinates

####  Translate Methods
# Method
c14db$Method_En = NA
c14db$Method_En[which(c14db$Method%in%c("β線法","β線","β線法\n"))]="Beta Counting"
c14db$Method_En[which(c14db$Method=="AMS法")]="AMS"






