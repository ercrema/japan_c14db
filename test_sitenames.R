load("./runc14raw.RData")
source("./utility/siteNameTranscript.R")
siteList = unique(c14raw$SiteName)
sitenames=siteNameTranscript(siteList)
#Export for manual editing
sitenames$matched=(sitenames$SiteName==sitenames$MatchedName)
sitenames$ToCheck = !sitenames$matched | is.na(sitenames$Furigana)
sitenames=dplyr::select(sitenames,SiteName,Furigana,ToCheck)
write.csv(sitenames,"./lookup/sitenamesTranscript.csv",row.names = FALSE)