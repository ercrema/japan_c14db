library(tidyverse)
library(readxl)
library(measurements)
library(sp)
library(NipponMap) 
library(mapdata)
library(maptools)

source("./utilities.R")
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
siteList = unique(c14db$SiteName)

# First Export
# sitenames=siteNameTranscript(siteList[1:5])
# sitenames$Completed = as.numeric(!sitenames$partial)
# write.csv(sitenames,"./translation/sitenames.csv",row.names=FALSE)

# read checked sites
sitesWithFurigana<-read.csv("./translation/sitenames.csv")

# check which sies are not present in sitesWithFurigana
i = which(is.na(siteList,sitesWithFurigana$SiteName))

# autodetect site names for the missing sites
sitenamesUpdate=siteNameTranscript(siteList[i])
sitenamesUpdate$Completed = as.numeric(!sitenames$partial)

# bind
sitesWithFurigana = rbind.data.frame(sitesWithFurigana,sitenamesUpdate)

# export updated sitelist
write.csv(sitesWithFurigana,"./translation/sitenames.csv",row.names=FALSE)





