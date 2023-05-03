library(here)
c14db  <- readRDS(here('output','c14db_1.0.0.Rds'))
# Total sample
nrow(c14db)

# Sample before 30k 14C age
sum(c14db$CRA>30000,na.rm=T)
nrow(c14db)

44425-nrow(c14db)

table(c14db$Material)
table(c14db$Material)/nrow(c14db)

sum(is.na(c14db$Latitude))
sum(is.na(c14db$Latitude))/nrow(c14db)
