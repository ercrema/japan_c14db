#' @title Capitalise sting of characters
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


siteNameTranscript<-function(x)
{
  
  library(rvest)
  library(utils)
  library(Nippon)
  
  res = data.frame(SiteName=x,PartialSiteName=x,MatchedName=NA,partial=NA,Furigana=NA,Romanised=NA,Romanised2=NA)
  
  pb <- txtProgressBar(min=1, max=length(x), style=3)
  
  
  for (k in 1:length(x))
  {
    setTxtProgressBar(pb, k)
    webpage <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",x[k])))
    tmp=NA
    #extract first search result URL key
    content <- html_nodes(webpage,'.document_list_item')
    
    if (length(content)==0)
    {
      tmp=unlist(strsplit(x[k],"遺跡"))[1]
      webpage <- read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",tmp)))
      content <- html_nodes(webpage,'.document_list_item')
    }
    
    if (length(content)>0)
    {
      firstRes = html_nodes(content,'a')
      links = html_attr(firstRes,'href')
      urlCode <- links[agrep("search/item",links)][1] #extract the first
      
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
        i = agrep(tmp,SiteName) #partial matching
        if (length(i)>0){res$partial[k]=TRUE}
      } else {res$partial[k]=FALSE}
      
      if (length(i)==0)
      {
        SiteNameTranscript=EnglishName=EngslishNamewithouSite=NA
      } else {
        res$SiteName[k] = x[k]
        res$PartialSiteName[k] = tmp
        res$MatchedName[k] = SiteName[i]
        res$Furigana[k] = content[which(header=='Site Name Transcription')][i[1]]
        res$Romanised[k] = as.character(sapply(kana2roma(content[which(header=='Site Name Transcription')]),CapStr))[i[1]]
        res$Romanised2[k] = strsplit(res$Romanised[k],"iseki")[[1]][1]
      }
    }
    

    
    
  }
  close(pb) 
  return(res)
}
