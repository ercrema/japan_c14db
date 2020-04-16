### Nabunken Link
# load("./testSiteNames.RData")
# x = c14raw
# x = unique(data.frame(site=x$SiteName,reference=x$Reference))
# x$SiteName=as.character(x$SiteName)
# x$Reference=as.character(x$Reference)
# site = x$site
# reference = x$reference


refNabunkenLink<-function(site,reference,maxchar=80,delay=1,ua="Contact Email: erc62@cam.ac.uk")
{
  require(rvest)
  require(stringr)
  require(pdftools)
  require(httr)
  require(magrittr)
  ua = user_agent(ua) #Define User Agent
  
  #Generate a search list
  uniqueRef=unique(reference)
  nabunkenURL = DOI = retrievedRef = rep(NA,length=length(reference))
  check = numeric(length(reference))
  
  for (i in 1:length(uniqueRef))
  {
    print(i)
    # Matching Site Name(s) and LabCode(s):
    index = which(reference==uniqueRef[i])
    searchSites = unique(site[index])

    # First Attempt: Search By Full Reference
    Sys.sleep(delay)
    if (nchar(uniqueRef[i])<=maxchar)
    {
      webpage=read_html(html_session(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?all=",uniqueRef[i])),ua))
      n = html_text(html_nodes(webpage,'.page-header .text-right'))
      n = as.numeric(regmatches(n, gregexpr("[[:digit:]]+", n)))
    } else if (nchar(uniqueRef[i])>maxchar){n=0}
    
    if (length(n)==0){n=0}
    if (n==1) #example i=906
    {
      #Single Hit ---> Generally Ok, so retain
      contentlist <- html_nodes(webpage,'.document_list_item')
      links = html_nodes(contentlist,'.list_title a')
      tmpaddress = html_attr(links[1],"href")
  
      Sys.sleep(delay)
      reportpage=read_html(html_session(URLencode(paste0("https://sitereports.nabunken.go.jp",tmpaddress)),ua))
      
      #Extract Key Headers
      header <- html_nodes(reportpage,'th')
      header <- as.character(header)
      header<-sapply(strsplit(header,"<th>"),function(x){x[2]})
      header<-sapply(strsplit(header,"</th>"),function(x){x[1]})
      
      #Extract Contents
      contentOriginal <- html_nodes(reportpage,'td')
      content <- as.character(contentOriginal)
      content<-sapply(strsplit(content,"<td>"),function(x){x[2]})
      content<-sapply(strsplit(content,"</td>"),function(x){x[1]})
      content <- content[-2]
      contentOriginal <- contentOriginal[-2]
      
      if (any(header=='URL',na.rm=TRUE)){nabunkenURL[index]=html_text(contentOriginal[which(header=='URL')],trim=TRUE)}
      if (any(header=='DOI',na.rm=TRUE)){DOI[index]=html_text(contentOriginal[which(header=='DOI')],trim=TRUE)}
      retrievedRef[index]=html_text(html_nodes(reportpage,".copy-clipboard-text"))[1]
      check[index]=1 #Scenario 1
    }
    
    if (n>1) #example i=908 (i.e. more than single hit)
    {
      check[index]=3 #premptively assign the no match class 3
      
      #check if the first hit does actually have the same site name
      contentlist <- html_nodes(webpage,'.document_list_item')
      links = html_nodes(contentlist,'.list_title a')
      tmpaddress = html_attr(links[1],"href")
      
      Sys.sleep(delay)
      reportpage=read_html(html_session(URLencode(paste0("https://sitereports.nabunken.go.jp",tmpaddress)),ua))
      
      #Extract Key Headers
      header <- html_nodes(reportpage,'th')
      header <- as.character(header)
      header<-sapply(strsplit(header,"<th>"),function(x){x[2]})
      header<-sapply(strsplit(header,"</th>"),function(x){x[1]})
      
      #Extract Contents
      contentOriginal <- html_nodes(reportpage,'td')
      content <- as.character(contentOriginal)
      content<-sapply(strsplit(content,"<td>"),function(x){x[2]})
      content<-sapply(strsplit(content,"</td>"),function(x){x[1]})
      content <- content[-2]
      contentOriginal <- contentOriginal[-2]
      
      if (any(header=='Site Name',na.rm=TRUE))
      {
        foundSiteNames=content[which(header=='Site Name')]
        tmp=FALSE
        if (length(foundSiteNames)==1 & length(searchSites)==1)
        {
          tmp=agrepl(searchSites,foundSiteNames)
        }
        
        if (length(foundSiteNames)>1 & length(searchSites)>1)
        {
          test=sapply(searchSites,agrepl,foundSiteNames)
          tmp=all(apply(test,2,sum)==1)
        }
        
        
        if (tmp)
        {
          check[index]=2 # Assign scenario 2 (manual check required)
          if (any(header=='URL',na.rm=TRUE)){nabunkenURL[index]=html_text(contentOriginal[which(header=='URL')],trim=TRUE)}
          if (any(header=='DOI',na.rm=TRUE)){DOI[index]=html_text(contentOriginal[which(header=='DOI')],trim=TRUE)}
          retrievedRef[index]=html_text(html_nodes(reportpage,".copy-clipboard-text"))[1]
        }
      }
    }
    
    if (n==0)
    {
      retrievedRef[index]=NA
      DOI[index]=NA
      nabunkenURL[index]=NA
    }
  }
  return(data.frame(site=site,reference=reference,retrievedRef=retrievedRef,nabunkenURL=nabunkenURL,DOI=DOI))    
}

    
  