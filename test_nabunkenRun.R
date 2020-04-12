### Nabunken Link
load("./runc14raw.RData")
x = c14raw
x = unique(data.frame(site=x$SiteName,reference=x$Reference))
x$site=as.character(x$site)
x$reference=as.character(x$reference)
site = x$site
reference = x$reference
source("./utility/nabunkenLink.R")
nabunkenRes=refNabunkenLink(site,reference)
save(nabunkenRes,file="./nabunkenLink.RData")