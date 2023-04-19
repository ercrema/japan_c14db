library(rcarbon)
library(here)
c14db  <- readRDS(here('output','c14db_1.0.0.Rds'))
c14db$C14Age = c14db$UnroundedCRA
i = which(is.na(c14db$C14Age))
c14db$C14Age[i] = c14db$CRA[i]
c14db$C14Error = c14db$UnroundedCRAError
i = which(is.na(c14db$C14Error))
c14db$C14Error[i] = c14db$CRAError[i]

table(c14db$Material)/nrow(c14db)
1-(sum(c14db$C14Age>30000)/nrow(c14db))


c14db.terrestrial  <- subset(c14db,Material=='Terrestrial')
c14db.marine  <- subset(c14db,Material=='Marine')
c14db.other  <- subset(c14db,Material=='Other')


c14db.terrestrial$C14Age = c14db.terrestrial$UnroundedCRA
i = which(is.na(c14db.terrestrial$C14Age))
c14db.terrestrial$C14Age[i] = c14db.terrestrial$CRA[i]
c14db.terrestrial$C14Error = c14db.terrestrial$UnroundedCRAError
i = which(is.na(c14db.terrestrial$C14Error))
c14db.terrestrial$C14Error[i] = c14db.terrestrial$CRAError[i]


c14db.terrestrial$Region2  <- c14db.terrestrial$Region
c14db.terrestrial$Region2[which(c14db.terrestrial$Prefecture%in%c("Ishikawa","Fukui","Niigata","Toyama"))] <- 'Hokuriku'
c14db.terrestrial$Region2[which(c14db.terrestrial$Prefecture%in%c("Shizuoka","Aichi","Gifu","Mie"))] <- 'Tokai'
c14db.terrestrial$Region2[which(c14db.terrestrial$Region=="Kansai")]  <- 'Kinki'



c14db.terrestrial  <- subset(c14db.terrestrial,C14Age<54000&C14Age>100)
calibrated  <- calibrate(c14db.terrestrial$C14Age,c14db.terrestrial$C14Error)
spd.calibrated  <- spd(calibrated,timeRange=c(55000,100))
spd.regional  <- stackspd(calibrated,timeRange=c(55000,100),group=c14db.terrestrial$Region2)

save(spd.calibrated,spd.regional,file=here('joad_figures','spds.RData'))

