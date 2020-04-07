### Nabunken Link
load("./testSiteNames.RData")
x = c14raw
x = unique(data.frame(site=x$SiteName,reference=x$Reference))
x$site=as.character(x$site)
x$reference=as.character(x$reference)
site = x$site
reference = x$reference
save("./nabunkenLink.RData")