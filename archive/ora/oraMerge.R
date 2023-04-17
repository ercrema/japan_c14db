library(here)
library(dplyr)
c14db  <- readRDS(here('c14db_0.2.1.Rds'))
c14raw=read.csv(here('input','c14_raw_v220303.csv'),na.strings = '',skip=2)
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

# c14raw = c14raw[,-which(names(c14raw)%in%c("elim1","elim2","elim3"))]
subset(c14raw,LabCode=='MTC-08954')
subset(c14raw,LabCode=='MTC-08955')
c14raw$toKeep = TRUE
c14raw$ReferenceID = as.numeric(as.factor(c14raw$Reference))
c14raw$originalRow = 2:(nrow(c14raw)+1)
c14raw <- select(c14raw,originalRow,Remarks)
c14db  <- left_join(c14db,c14raw)
ora  <- subset(c14db,MaterialDetails=='Organic Residue from Pottery')
rekihaku  <- subset(ora,Reference==names(table(ora$Reference)[which.max(table(ora$Reference))]))

