#' @title Capitalise sting of characters
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}



#Scenario 1 ... site name is found directly, report SiteName, MatchName, Furigana, and Transliteration
#Scenario 2 ... fuzzy match found with site name wihout -iski, report SiteName, PMatchName, Furigana, and Transliteration
#Scneario 3 ... no match found



siteNameTranscript<-function(x,n.attempts=1000)
{
  
  library(rvest)
  library(utils)
  library(Nippon)
  
  res = data.frame(SiteName=x,MatchedName=NA,Furigana=NA,Romanised=NA, Romanised2=NA)
  pb <- txtProgressBar(min=1, max=length(x), style=3)
  
  
  for (k in 1:length(x))
  {
    print(k)
    candidateSiteName = x[k]
    candidateSiteNameWithoutIseki=ifelse(grepl("遺跡",candidateSiteName),unlist(strsplit(x[k],"遺跡"))[1],NA)
    candidateSiteNameWithoutIseki = gsub("\\[|\\]|\\(|\\)", "", candidateSiteNameWithoutIseki) #remove brackets
    
    scenario = 1
    
    
    # Try First with Scenario 1
    setTxtProgressBar(pb, k)
    retry=TRUE
    attempt.count=0
    while(retry)
    {
      webpage <- try(read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",candidateSiteName))),silent=TRUE)
      if ((attempt.count>n.attempts)|(class(webpage)[1] != "try-error")){retry=FALSE;attempt.count=attempt.count+1}
    }
    
    #extract first search result URL key
    content <- html_nodes(webpage,'.document_list_item')
    
    
    #if no match - try scenario 2
    if (length(content)==0&!is.na(candidateSiteNameWithoutIseki))
    {
      scenario = 2
      retry=TRUE
      attempt.count=0
      while(retry)
      {
        webpage <- try(read_html(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",candidateSiteNameWithoutIseki))),silent=TRUE)
        if ((attempt.count>n.attempts)|(class(webpage)[1] != "try-error")){retry=FALSE;attempt.count=attempt.count+1}
      }
      content <- html_nodes(webpage,'.document_list_item')
    }
    
    if (length(content)==0){scenario = 3} #If no hit scenario 3
    
    # Scenario 1 & 2
    if (length(content)>0)
    {
      #extract URL of the first 'hit'
      firstRes = html_nodes(content,'a')
      links = html_attr(firstRes,'href')
      urlCode <- links[agrep("search/item",links)][1] #extract the first
      retry=TRUE
      attempt.count=0
      while(retry)
      {
        webpage <- try(read_html(paste0("https://sitereports.nabunken.go.jp",urlCode)),silent=TRUE)
        if ((attempt.count>n.attempts)|(class(webpage)[1] != "try-error")){retry=FALSE;attempt.count=attempt.count+1}
      }
      
      
      header <- html_nodes(webpage,'th')
      header <- as.character(header)
      header<-sapply(strsplit(header,"<th>"),function(x){x[2]})
      header<-sapply(strsplit(header,"</th>"),function(x){x[1]})
      
      content <- html_nodes(webpage,'td')
      content <- as.character(content)
      content<-sapply(strsplit(content,"<td>"),function(x){x[2]})
      content<-sapply(strsplit(content,"</td>"),function(x){x[1]})
      content <- content[-2]
      
      #Extract Site Names
      nsites = sum(header=="Site Name",na.rm=T)
      SiteName = content[which(header=='Site Name')]
    }
      if (scenario == 1) #Scenario 1
      {
          i = which(SiteName%in%candidateSiteName) #identify match SiteName
          if ((all(is.na(i))|length(i)==0)&!is.na(candidateSiteNameWithoutIseki)){i = grep(candidateSiteNameWithoutIseki,SiteName)} #fuzzy match
          if (length(i)==0){scenario=3} #if this fails scneario 3
          if (length(i)>1){i[1]} #if more than match, select first
          MatchedName = SiteName[i]
      }
      if (scenario == 2) #Scenario 2
      {
        i = grep(candidateSiteNameWithoutIseki,SiteName) #Start with fuzzy match
        if (length(i)==0|is.na(candidateSiteNameWithoutIseki)){scenario=3} #if this fails scneario 3
        if (length(i)>1){i[1]} #if more than match, select first
        MatchedName = SiteName[i]
      }
      if (scenario == 3)
      {
        res$MatchedName[k] = res$Furigana[k] = res$Romanised[k] = res$Romanised2[k] = NA
      }
        
      
      if (scenario%in%c(1,2))
      {
        res$MatchedName[k] = MatchedName
        res$Furigana[k] = content[which(header=='Site Name Transcription')][i[1]]
        res$Romanised[k] = kana2roma(res$Furigana[k])
        res$Romanised2[k] = strsplit(res$Romanised[k],"iseki")[[1]][1]
      }
}
  close(pb) 
  return(res)
}
