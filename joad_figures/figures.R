library(rcarbon)
library(here)
c14db  <- readRDS(here('output','c14db_1.0.0.Rds'))
table(c14db$Material)/nrow(c14db)
c14db.terrestrial  <- subset(c14db,Material=='Terrestrial')
c14db.marine  <- subset(c14db,Material=='Marine')
c14db.other  <- subset(c14db,Material=='Other')


c14db.terrestrial$C14Age = c14db.terrestrial$UnroundedCRA
i = which(is.na(c14db.terrestrial$C14Age))
c14db.terrestrial$C14Age[i] = c14db.terrestrial$CRA[i]
c14db.terrestrial$C14Error = c14db.terrestrial$UnroundedCRAError
i = which(is.na(c14db.terrestrial$C14Error))
c14db.terrestrial$C14Error[i] = c14db.terrestrial$CRAError[i]

c14db.terrestrial  <- subset(c14db.terrestrial,C14Age<54000&C14Age>100)
calibrated  <- calibrate(c14db.terrestrial$C14Age,c14db.terrestrial$C14Error)
spd.calibrated  <- spd(calibrated,timeRange=c(55000,100))




