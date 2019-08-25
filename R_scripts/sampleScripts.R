#Sample Text Mining Scripts for Analysis

# Extract all texts containing the the terms in searchTerms
#e.g search all dates on rice
searchTerms = c("コメ","米")
result=c14db[grep(paste(searchTerms,collapse="|"),c14db$MaterialType),]
#e.g All dates from residential units
searchTerms = c("住居","柱","SI","炉","住")
result=c14db[grep(paste(searchTerms,collapse="|"),c14db$SamplingLocation),]
#e.g. All Itazuke Style Pottery
searchTerms = c("板付")
result=c14db[grep(paste(searchTerms,collapse="|"),c14db$Phase),]

