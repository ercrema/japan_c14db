library(here)
library(stringr)
library(dplyr)

options(warn=-1) #suppress warning
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

c14raw$nabunkenURL = as.character(str_match_all(c14raw$Remarks, 'https://sitereports.nabunken.go.jp/\\d+'))
c14raw$nabunkenURL[which(c14raw$nabunkenURL=='character(0)')]=NA

c14raw$SiteName =  gsub("[\r\n]", "", c14raw$SiteName)
c14raw$SiteName =  gsub(" ", "", fixed=TRUE, c14raw$SiteName)


c14raw$Prefecture = str_remove(c14raw$Prefecture,'府')
c14raw$Prefecture = str_remove(c14raw$Prefecture,'県')
# Match to Lookup Table
pref.lookup = read.table(here('input','prefectures_translations.csv'),stringsAsFactors=FALSE,colClasses = c('character','character','character'),header=T,sep=',')
colnames(pref.lookup) = c("Prefecture","PrefectureNameEn","Region",'SetCode')
c14raw=left_join(c14raw,pref.lookup)
c14raw$ReferenceID = as.numeric(as.factor(c14raw$Reference))

c14original = c14raw
c14raw  <- select(c14raw,PrefectureNameEn,SetCode,SiteName,LabCode,Reference,ReferenceID,nabunkenURL)
c14searchset = subset(c14raw,!is.na(nabunkenURL)) |> select(SiteName,PrefectureNameEn,SetCode,Reference,ReferenceID) |> unique()
c14searchset$LabCode = NA
for (i in 1:nrow(c14searchset))
{
	tmp = subset(c14raw,Reference==c14searchset$Reference[i])$LabCode
	a1 = which(is.na(tmp)|tmp=="不明")
	a2 = which(!grepl("[^A-Za-z]", tmp))
	a3 = which(!grepl("\\D", tmp))
	a4 = grep("\\?", tmp)
	a = unique(c(a1,a2,a3,a4))
	if (length(a)== length(tmp)) {c14searchset$LabCode[i]=NA} #no viable labcode
	if (length(a)==0){c14searchset$LabCode[i]=tmp[1]} #if all labcodes are ok, pick first one
	if (length(a)>0)
		{
			tmp = tmp[-a]
			c14searchset$LabCode[i]=tmp[1]
		}
}

c14searchset = subset(c14searchset,ReferenceID!=2883)




### Start Scraping ####
library(rvest)
library(httr)
ua="Contact Email: erc62@cam.ac.uk"
ua = user_agent(ua)
n.attempts=100
delay=1
c14searchset$n1 = NA
c14searchset$n2 = NA
c14searchset$url1 = NA
c14searchset$doi1 = NA
c14searchset$url1 = NA
c14searchset$doi2 = NA

for (i in 1:nrow(c14searchset))
{
	print(i)
	x1  <-  paste0("https://sitereports.nabunken.go.jp/en/search?all=",c14searchset$LabCode[i],"&has_file=x&include_file=include&iseki=",c14searchset$SiteName[i],"&set=",c14searchset$SetCode[i],"&translate=on") #PDF + LabCode Search
	x2  <-  paste0("https://sitereports.nabunken.go.jp/en/search?has_file=x&include_file=exclude&iseki=",c14searchset$SiteName[i],"&set=",c14searchset$SetCode[i],"&translate=on") #SiteName only Search

	# Search 1
	Sys.sleep(delay)
	sessiondata=session(URLencode(x1),ua)
	webpage=read_html(sessiondata)
	n1 = html_text(html_nodes(webpage,'.page-header .text-right'))
	n1 <- as.numeric(regmatches(n1, gregexpr("[[:digit:]]+", n1))) #Number of Cases
	if(length(n1)==0){n1=0}
	c14searchset$n1[i] = n1

	if (n1>0)
	{
		contentlist <- html_nodes(webpage,'.document_list_item')
		links = html_nodes(contentlist,'.list_title a')
		tmpaddress = html_attr(links[1],"href")
		retry=TRUE
		attempt.count=0
		while (retry)
		{ 
			reportpage=NA
			reportpage=tryCatch(read_html(session(URLencode(paste0("https://sitereports.nabunken.go.jp",tmpaddress)),ua)),error=function(i)return(NA))
			if(is.na(reportpage)){
				attempt.count=attempt.count+1
				Sys.sleep(delay)
			}
			else retry=F
		}
		contentOriginal <- html_nodes(reportpage,'td')
		c14searchset$url1[i] = html_text(contentOriginal[[1]],trim = T)
		c14searchset$doi1[i] = html_text(contentOriginal[[3]],trim = T)
	}


	# Search 2
	Sys.sleep(delay)
	sessiondata=session(URLencode(x2),ua)
	webpage=read_html(sessiondata)
	n2 = html_text(html_nodes(webpage,'.page-header .text-right'))
	n2 <- as.numeric(regmatches(n2, gregexpr("[[:digit:]]+", n2))) #Number of Cases
	if(length(n2)==0){n2=0}
	c14searchset$n2[i] = n2

	if (n2>0)
	{
		contentlist <- html_nodes(webpage,'.document_list_item')
		links = html_nodes(contentlist,'.list_title a')
		tmpaddress = html_attr(links[1],"href")
		retry=TRUE
		attempt.count=0
		while (retry)
		{ 
			reportpage=NA
			reportpage=tryCatch(read_html(session(URLencode(paste0("https://sitereports.nabunken.go.jp",tmpaddress)),ua)),error=function(i)return(NA))
			if(is.na(reportpage)){
				attempt.count=attempt.count+1
				Sys.sleep(delay)
			}
			else retry=F
		}
		contentOriginal <- html_nodes(reportpage,'td')
		c14searchset$url2[i] = html_text(contentOriginal[[1]],trim = T)
		c14searchset$doi2[i] = html_text(contentOriginal[[3]],trim = T)
	}
}

c14searchset2merge = select(c14searchset,SiteName,ReferenceID,n1,url1,doi1,n2,url2,doi2)
c14out = left_join(c14original,c14searchset,by=c("SiteName"="SiteName","ReferenceID"="ReferenceID"))
save.image('temp.RData')
write.csv(c14out,file=here('output','nabunkenURLScraped.csv'))
