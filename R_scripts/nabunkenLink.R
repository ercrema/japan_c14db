#'@title Function for adding a Nabunken Reference link to C14 Database
#'@params x Data.frame with columns 

library(tidyverse)
library(readxl)
library(measurements)
library(sp)
library(NipponMap) 
library(mapdata)
library(maptools)
library(stringr)
library(rebus)
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
x$SiteName=as.character(x$SiteName)
x$Reference=as.character(x$Reference)


refNabunkenLink(x)
{
  require(rvest)
  require(stringr)
  require(rebus)
  require(tabulizer)
  require(stringr)
  
  # Distinguish Between Single Site Reports and Multi Site Reports ####
  reports = unique(data.frame(SiteName=x$SiteName,Reference=x$Reference,stringsAsFactors = FALSE))
  singleReports =subset(reports,Reference%in%names(which(table(reports$Reference)==1)))
  multiReports = subset(reports,!Reference%in%names(which(table(reports$Reference)==1)))
  multiReportsUnique = unique(multiReports$Reference)
    
  # Extract Series Names from References where possible ####                 
  splitSeries = function(x){
    spl = unlist(str_split(x,pattern = "\\s"))
    if (any(grepl("集",spl)))
    {
      tmp=spl[which(grepl("集",spl))[1]]
      return(unlist(strsplit(tmp,"第"))[1])
    } else {return(NA)}
  }
  
  # Extract Series Number from Full Reference
  getSeriesNumber = function(x){
    spl = unlist(str_split(x,pattern = "\\s"))
    if (any(grepl("集",spl)))
    {
      tmp=spl[which(grepl("集",spl))[1]]
      return(as.numeric(unlist(strsplit(tmp,"第|集"))[2]))
    } else {return(NA)}
  }
  
  
  singleReports$Series = as.character(sapply(singleReports$Reference,splitSeries))
  singleReports$SeriesNumber = sapply(singleReports$Reference,getSeriesNumber)
  singleReports$nabunkenURL=NA
  multiReports$Series = as.character(sapply(multiReports$Reference,splitSeries))
  multiReports$SeriesNumber = sapply(multiReports$Reference,getSeriesNumber)
  multiReports$nabunkenURL=NA
  
  
  #Iterate through single reports
  for (i in 1:nrow(singleReports))
  {
    webpage <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",as.character(singleReports$SiteName[i])," AND 炭素年代")))
    #extract number of hits:
    n = html_text(html_nodes(webpage,'.page-header .text-right'))
    n <- as.numeric(regmatches(n, gregexpr("[[:digit:]]+", n)))
    
    if (length(n)>0) #if any hits
    {
      npages = ceiling(n/20) #20 items per page
      for (p in 1:npages)
      {
        webpage.tmp <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search/p/",p,"?all=",as.character(singleReports$SiteName[i])," AND 炭素年代")))
        contentlist <- html_nodes(webpage.tmp,'.document_list_item')
        if (length(contentlist)==0){break()} #break if page is empty
        links = html_nodes(contentlist,'.list_title a') #extract link to each site
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
          pdfLinks = html_attr(html_nodes(reportpage,'.report-attach-file a'),"href") #extract pdf links
          if (length(pdfLinks)>0)
          {
          pdfLinks=unique(pdfLinks[!grepl("attach_mobile",pdfLinks)])
          pdfLinks=paste0("https://sitereports.nabunken.go.jp/",pdfLinks)
          }
          
          if (any(header=='Site Name',na.rm=T)) #If genuine report
          {
            sitenames = content[which(header=='Site Name')] #extract site name
            if (any(sitenames==singleReports$SiteName[i]))
            {
              series = (content[which(header=='Series')])
              # If site names, series names, and series number is matching assign URL
              if (agrepl(singleReports$Series[i],series)&singleReports$SeriesNumber[i]==as.numeric(content[which(header=='Series Number')]))
              {
                singleReports$nabunkenURL[i]=html_attr(html_nodes(contentOriginal,'a')[1],'href')
                break()
              } else if (length(pdfLinks)>0) #if no match but there is a pdf try scraping 
              {
               extractedText = sapply(pdfLinks,extract_text) #extract text from pdf link
               extractedText = sapply(extractedText, gsub, pattern="[\r\n]",replacement="") #remove end of lines
               lbc = as.character(x$ID[which(x$Reference==singleReports$Reference[i])])
               hh = sapply(extractedText, str_detect,pattern=lbc)
               if (any(hh))
               {
                 singleReports$nabunkenURL[i]=html_attr(html_nodes(contentOriginal,'a')[1],'href')
               }
               rm(extractedText)
              }
            }
          }
        }
      }
    }
  }
  
  #Iterate through multiple site reports
  for (i in 1:length(multiReportsUnique))
  {
    print(i)
    sites = multiReports$SiteName[which(multiReports$Reference==multiReportsUnique[i])]
    for (j in 1:length(sites))
    {
      webpage <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",sites[j]," AND 炭素年代")))
      n = html_text(html_nodes(webpage,'.page-header .text-right'))
      n <- as.numeric(regmatches(n, gregexpr("[[:digit:]]+", n)))
      if (length(n)>0) #if any hits
      {
        npages = ceiling(n/20) #20 items per page
        for (p in 1:npages)
        {
          webpage.tmp <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search/p/",p,"?all=",sites[j]," AND 炭素年代")))
          contentlist <- html_nodes(webpage.tmp,'.document_list_item')
          if (length(contentlist)==0){break()} #break if page is empty
          links = html_nodes(contentlist,'.list_title a') #extract link to each site
          
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
            pdfLinks = html_attr(html_nodes(reportpage,'.report-attach-file a'),"href") #extract pdf links
            if (length(pdfLinks)>0)
            {
              pdfLinks=unique(pdfLinks[!grepl("attach_mobile",pdfLinks)])
              pdfLinks=paste0("https://sitereports.nabunken.go.jp/",pdfLinks)
            }
            
            if (any(header=='Site Name',na.rm=T)) #If genuine report
            {
              sitenames = content[which(header=='Site Name')] #extract site name
              {
                if (all(sites%in%sitenames)) #if all sites are in the list of extracted site names
                {
                  multiReports$nabunkenURL[which(multiReports$Reference==multiReportsUnique[i])]=html_attr(html_nodes(contentOriginal,'a')[1],'href')
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  
  