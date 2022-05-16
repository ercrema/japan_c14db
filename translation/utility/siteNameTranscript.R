
#'@title siteListExtractor
#'@description get a list of report containing certain keyword
#'@param URL
#'@param keyword Keyword to be searched
#'@param delay 
#'@param n.attempts 
#'@param ua 
#'@param lim.sites
siteNameTranscript <- function(sitename=NULL,delay=1,n.attempts=100,ua="Contact Email: erc62@cam.ac.uk")
{
	require(rvest)
	require(httr)
	ua = user_agent(ua) #Define User Agent
	res = data.frame(SiteName=sitename,Furigana=NA)

	for (s in 1:length(sitename))
	{
		print(paste(s,'of',length(sitename)))
		retry=TRUE
		attempt.count=0
		while (retry)
		{ 
			if(attempt.count>0){print(paste0('Retrying Connection; Attempt No.: ',attempt.count))}
			sessiondata=session(URLencode(paste0("https://sitereports.nabunken.go.jp/en/search?has_file=x&include_file=exclude&iseki=",sitename[s],"&translate=on")),ua)
			Sys.sleep(delay)
			if (attempt.count<=n.attempts)
			{
				if(sessiondata$response$status_code==200)
				{
					webpage=read_html(sessiondata)
					x = sitename[s]
					retry=FALSE 
				} else if (sessiondata$response$status_code!=200)
				{
					attempt.count=attempt.count+1
				}
			}
			if (attempt.count>=n.attempts)
			{
				warning('Repeated failure to access website, returning NULL values')
			}
		}


		#Extract Number of Cases
		n = html_text(html_elements(webpage,'.page-header .text-right'))
		n <- as.numeric(regmatches(n, gregexpr("[[:digit:]]+", n)))
		if (length(n)==0){next()}

		#Extract Info from first hit
		contentlist <- html_elements(webpage,'.document_list_item')
		links = html_elements(contentlist,'.list_title a')
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

		curreport=reportToList(reportpage,log=F)
		index = grep('^Site Name',names(curreport))
		if (length(index)>2)
		{    
			index.transcript = index[grep('Site Name Transcription',names(curreport)[index])]
			index.sitename = index.transcript - 1
			candidate.names = curreport[index.sitename]
			res$Furigana[s] = as.character(unlist(curreport[index.transcript][agrep(sitename[s],unlist(candidate.names))[1]]))
		}
		if (length(index)==2)
		{
			res$Furigana[s] = as.character(unlist(curreport[grep('Site Name Transcription',names(curreport))]))
		}


	}

	return(res)
}



reportToList <- function(report,log=F){
    maintable=html_element(report,'.table_data')
    if(log)print(paste("retriving info from",html_attr(html_elements(reportpage,"link")[3],"href")))
    alldata=list()
    all_elmts=html_elements(maintable,"tr")
    for(v in all_elmts){
        th=html_element(v,"th")
        td=html_element(v,"td")
        dtitle = html_text2(th)
        if(is.na(dtitle) || length(dtitle)==0 || dtitle=="") #missing or empty headers
            dtitle=paste0("Untitled",as.character(length(alldata)+1))
        if(length(dtitle)>1 )dtitle=dtitle[1]
        cpt=1
        btitle=dtitle
        while(!is.null(alldata[[dtitle]])){
            cpt=cpt+1
            dtitle=paste0(btitle," ",cpt)
        }
        if(length(html_elements(td,"table"))>0) #check for embedded tables and ignore them
            alldata[[dtitle]]="NA"
        else if(length(html_children(td))>1 && length(html_elements(td,"br"))<length(html_children(td))) #rvest consider <br> as full child wich destroy the dataslot utility
            alldata[[dtitle]]=lapply(html_children(td),dataslot)
        else
            alldata[[dtitle]]=dataslot(td)
    }
    if(log)print(paste("done.",length(alldata),"fields retrieved"))
    return(alldata)
}

## from a final td html node extract link and text
dataslot <- function(td_countainer){
    results=c()
    links=html_elements(td_countainer,"a")
    if(length(links)>0){
        l=sapply(links,html_attr,"href")
        t=sapply(links,html_text2)
        results=c(l,t)
        #results=paste(l,t) #the oldschool way
    }
    results=c(results,html_text2(td_countainer))
    return(unique(results))
}
