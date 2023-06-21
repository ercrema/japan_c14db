library(here)
library(dplyr)
x  <- read.csv('c14db_1.0.0.csv')

# Total number of samples
nrow(x)

# Check Dates All Dates
# Dates should have reliable value either in CRA or Unrounded CRA
dd  <- select(x,CRA,CRAError,UnroundedCRA,UnroundedCRAError)
no.cra  <- which(is.na(dd$CRA)|is.na(dd$CRAError))
all(!is.na(dd$UnroundedCRA[no.cra])&!is.na(dd$UnroundedCRAError[no.cra]))

