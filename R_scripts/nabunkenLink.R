#'@title Function for adding a Nabunken Reference link to C14 Database
#'@params x Data.frame with columns 

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
x = c14raw
x = unique(data.frame(ID=x$LabCode,SiteName=x$SiteName,Reference=x$Reference))



refNabunkenLink(x)
{
  require(rvest)
  reports = unique(data.frame(Reference=x$Reference))
  
  reports$nabunken = NA
  reports$pdf = NA
  reports$check = FALSE
  
  for (i in 1:nrow(reports))
  {
  
  webpage <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",as.character(reports$Reference[i]))))
  n = html_text(html_nodes(webpage,'.page-header .text-right'))
  n <- as.numeric(regmatches(n, gregexpr("[[:digit:]]+", n)))
  
  if (length(n)==0) #if no hits, double check with site name
  {
    site.tmp = unique(as.character(x$SiteName[which(as.character(x$Reference)==as.character(reports$Reference[i]))]))
    webpage2 <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",site.tmp)))
    n2 = html_text(html_nodes(webpage2,'.page-header .text-right'))
    n2 <- as.numeric(regmatches(n2, gregexpr("[[:digit:]]+", n)))
    
    if (length(n)>0) #if site name has hits, iterate through
    {
      npages = ceiling(n2/20) #20 items per page
      for (p in 1:npages)
      {
        webpage.tmp <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search/p/",p,"?all=",site.tmp)))
        contentlist <- html_nodes(webpage.tmp,'.document_list_item')
        if (length(contentlist)==0){break()} #break if page is empty
        links = html_nodes(contentlist,'.list_title a')
   
        for (k in 1:length(links)) #iterate through reports
        {
          tmpaddress = html_attr(links[k],"href")
          reportpage = read_html(URLencode(paste0("https://sitereports.nabunken.go.jp",tmpaddress)))
          header <- html_nodes(reportpage,'th')
          header <- as.character(header)
          header<-sapply(strsplit(header,"<th>"),function(x){x[2]})
          header<-sapply(strsplit(header,"</th>"),function(x){x[1]})
          contentOriginal <- html_nodes(reportpage,'td')
          content <- as.character(contentOriginal)
          content<-sapply(strsplit(content,"<td>"),function(x){x[2]})
          content<-sapply(strsplit(content,"</td>"),function(x){x[1]})
          content <- content[-2]
          sitename.JP = content[which(header=='Site Name')]
          if (length(sitename.JP)>0 )
          {
            if (sitename.JP==site.tmp)
            {
              reports$pdf[i]= grepl(".pdf",content[which(header=="File")]) #check if pdf is available
              if (reports$pdf[i])
              {
                pdfLinks = html_attr(html_nodes(reportpage,'.report-attach-file a'),"href") #extract pdf links
                #consider only full version
                pdfLinks=unique(pdfLinks[!grepl("attach_mobile",pdfLinks)])
                #remove repetition
                pdfLinks=paste0("https://sitereports.nabunken.go.jp/",pdfLinks)
                pdfHighlights = textExtractor(x=pdfLinks,x=labcodes)
              }
            }
          }
          
        }
        
      }
    }
    
    
  }
  
  if (length(n)>0) #if there are hits
  {}
  
  
  
  
  }

}