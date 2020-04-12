load("./runc14raw.RData")
siteList = unique(c14raw$SiteName)
x=siteList
source("./utility/siteNameTranscript.R")
sitenames=siteNameTranscript(x)
save(sitenames,file="./siteNames.RData")

