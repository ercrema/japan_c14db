#' @title Capitalise sting of characters
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}



#' @title Find Furigana and Romanisation of Japanese Site Names using Nabunken Site Report Database
#' @param x A vector of site names in Kanji
#' @return 


siteNameTranscript<-function(x)
{
  
  library(rvest)
  library(utils)
  library(Nippon)
  
  res = data.frame(SiteName=x,MatchedName=NA,partial=NA,Furigana=NA,Romanised=NA,Romanised2=NA)
  
  for (k in 1:length(x))
  {
    webpage <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",x[k])))
    
    #extract first search result URL key
    content <- html_nodes(webpage,'.document_list_item')
    
    if (length(content)>0)
    {
      firstRes = html_nodes(content,'div a')[[1]]
      urlCode <- as.character(firstRes)
      urlCode <- strsplit(urlCode,'"')[[1]][2]
      
      #extract URL of the first search result
      webpage <- read_html(paste0("https://sitereports.nabunken.go.jp",urlCode))
      
      
      header <- html_nodes(webpage,'th')
      header <- as.character(header)
      header<-sapply(strsplit(header,"<th>"),function(x){x[2]})
      header<-sapply(strsplit(header,"</th>"),function(x){x[1]})
      
      content <- html_nodes(webpage,'td')
      content <- as.character(content)
      content<-sapply(strsplit(content,"<td>"),function(x){x[2]})
      content<-sapply(strsplit(content,"</td>"),function(x){x[1]})
      content <- content[-2]
      
      
      nsites = sum(header=="Site Name",na.rm=T)
      SiteName = content[which(header=='Site Name')]
      
      i = which(SiteName==x[k])
      
      if (length(i)==0) #if full matching not possible try partial matching
      {
        i = agrep(x[k],SiteName) #partial matching
        if (length(i)>0){res$partial[k]=TRUE}
      } else {res$partial[k]=FALSE}
      
      if (length(i)==0)
      {
        SiteNameTranscript=EnglishName=EngslishNamewithouSite=NA
      } else {
        res$SiteName[k] = x[k]
        res$MatchedName[k] = SiteName[i]
        res$Furigana[k] = content[which(header=='Site Name Transcription')][i[1]]
        res$Romanised[k] = as.character(sapply(kana2roma(content[which(header=='Site Name Transcription')]),CapStr))[i[1]]
        res$Romanised2[k] = strsplit(res$Romanised[k],"iseki")[[1]][1]
      }
    }
    
  }
  return(res)
}
