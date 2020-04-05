load("./testSiteNames.RData")
x=siteList
source("./utility/siteNameTranscript.R")
sitenames=siteNameTranscript(x)
save(sitenames,file="./testSiteNames.RData")

#troubleshoote k=56
