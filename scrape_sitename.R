library(Nippon)
source("./utility/siteNameTranscript.R")
load('temp_c14raw.RData')


sitenames=read.csv("./lookup/sitenamesTranscript.csv",stringsAsFactors = FALSE)
siteList = unique(c14raw$SiteName)
missingSites = unique(siteList)[which(!unique(siteList)%in%sitenames$SiteName)]
additional_sitenames=siteNameTranscript(missingSites)
sitenames=rbind.data.frame(sitenames,data.frame(SiteName=additional_sitenames$SiteName,
                                                Furigana=additional_sitenames$Furigana,
                                                ToCheck=TRUE))
write.csv(sitenames,"./lookup/sitenamesTranscript_todo.csv",row.names = FALSE)