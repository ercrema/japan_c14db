# Load R libraries and C14 DB ####
library(spatstat)
library(sparr)
library(maptools)
library(rgdal)
library(leastcostpath)
library(gdistance)
library(rcarbon)
library(quantreg)
library(quokar)
library(qgam)
library(movecost)
library(McSpatial)
load('../japanc14db_v02(200130).RData')


# Data Cleaning ####

# 1) Eliminate Marine Dates (reservoir correction available only for Tokyo Bay and Easter/Western Hokkaido)
# See http://calib.org/marine/index.html?npoints=20&clat=36.001934568299895&clon=139.82833512681754
# 2) Eliminate Non-reliable dates

c14db = subset(c14db, retain==TRUE & Material=='Terrestrial' & CRA>0)


# Create Spatial Data ####

# Create SPDF
c14db.spatial = c14db
coordinates(c14db.spatial) = c("Longitude","Latitude")
proj4string(c14db.spatial) <- CRS("+proj=longlat +datum=WGS84")

# Read Shapefile for window of analysis
window <- readOGR(dsn="../shpfiles",layer="window_jpn")
window <- spTransform(window,proj4string(c14db.spatial))

# Source Point [Itazuke Site] #
itazuke = c14db[grep('板付遺跡',c14db$SiteName),c('Latitude','Longitude')]
coordinates(itazuke) = c("Longitude","Latitude") 
proj4string(itazuke) <- CRS("+proj=longlat +datum=WGS84")

# Create Rice DataSet #
# index = which(grepl(paste(c("コメ","米"),collapse="|"),c14db$MaterialType)|grepl('水田',c14db$SamplingLocation))
index = which(grepl(paste(c("コメ","米"),collapse="|"),c14db$MaterialType))
rice = c14db[index,]
coordinates(rice) =  c("Longitude","Latitude") 
proj4string(rice) = CRS("+proj=longlat +datum=WGS84")

# Convert to UTM
itazuke.utm = spTransform(itazuke,CRS("+proj=utm +zone=54 ellps=WGS84"))
window.utm = spTransform(window,CRS("+proj=utm +zone=54 ellps=WGS84"))
rice.utm = spTransform(rice,CRS("+proj=utm +zone=54 ellps=WGS84"))
  
# Compute Euclidean Distance #
rice.utm$distanceFromItazuke=spDists(rice.utm,itazuke.utm,longlat = FALSE)/1000

# Cost Distance #
# jpnDEM= get_elev_raster(window,prj="+proj=longlat +datum=WGS84",z=7,clip='locations')
# jpnDEM=projectRaster(from=jpnDEM,crs="+proj=utm +zone=54 ellps=WGS84")
# accumCost = movecost(dtm=jpnDEM,origin=itazuke.utm,time='h',destin=rice.utm)

# Compute Median Calibrated Dates ####
rice.dates = calibrate(rice.utm$CRA,rice.utm$CRAError)
rice.utm$medDate = -medCal(rice.dates)
rice.utm = subset(rice.utm,medDate>= -3500)

# Quantile Regression ####
fit = rq(medDate~distanceFromItazuke,data=rice.utm,tau=0.01)
1/coefficients(fit)[2] #Average speed (km per year)

# Plot
pdf(file='quantileReg.pdf',width = 6,height = 6)
plot(rice.utm$distanceFromItazuke,rice.utm$medDate,pch=20,axes=FALSE,xlab='Distance From Itazuke (km)',ylab='Median cal BP',type='n')
axis(1)
axis(2,at=seq(-3500,0,500),labels=c(seq(3500,0,-500)))
abline(fit,lty=2,lwd=1.5)
group1 = which(rice.utm$distanceFromItazuke<=300)
group2 = which(rice.utm$distanceFromItazuke>=924)
group4 = which(rice.utm$distanceFromItazuke>700&rice.utm$distanceFromItazuke<900 &rice.utm$medDate <c(-2146))
rice.utm$col=brewer.pal(4, 'Set2')[3]
rice.utm$col[group1]=brewer.pal(4, 'Set2')[1]
rice.utm$col[group2]=brewer.pal(4, 'Set2')[2]
rice.utm$col[group4]=brewer.pal(4, 'Set2')[4]
points(rice.utm$distanceFromItazuke,rice.utm$medDate,col=rice.utm$col,pch=20)
fit2 <- qgam(medDate~s(distanceFromItazuke, k=30, bs="ad"), data = rice.utm@data, qu = 0.01)
fit2.predicted=predict(fit2,newdata=data.frame(distanceFromItazuke=0:1300),se=TRUE)
lines(0:1300,fit2.predicted$fit,lty=3,lwd=1.5)
#lines(0:1300, fit2.predicted$fit + 2*fit2.predicted$se.fit, lwd = 1, col = 2)
#lines(0:1300, fit2.predicted$fit - 2*fit2.predicted$se.fit, lwd = 1, col = 2)   


legend('topleft',legend=c('Quantile Regression', 'Smoothed Additive Model'),lwd=1.5,lty=c(2,3),cex=0.8)


outliers=rice.utm@data[which(rice.utm$col==brewer.pal(4, 'Set2')[4]),]
rect(xleft=775.6748-20,xright=775.6748+20,ybottom=-3023 - 50,ytop=-2322+50,border='lightgrey')
text(x=775.6748+100,y=-3023 - 150,labels='Chikaraishijori cluster',cex=0.8)
rect(xleft=827.6437-20,xright=827.6437+20,ybottom=-2595 - 50,ytop=-2463+50,border='lightgrey')
text(x=827.6437+150,y=-2685.555,labels='Nakayashiki',cex=0.8)
rect(xleft=828.1691-20,xright=828.1691+20,ybottom=-2339 - 50,ytop=-2223+50,border='lightgrey')
text(x=828.1691+140,y=-2223,labels='Nakazato',cex=0.8)

dev.off()


par(mar=c(0,0,0,0))
plot(window.utm)
points(rice.utm,pch=20,col=rice.utm@data$col)



















#fit2 = lprq(y=rice$medDate,x=rice$distanceFromItazuke,tau=0.01,m=50,h=200)
#lines(fit2$xx,fit2$fv,lty=3)



# Early Kanto Dates
kantoDates=subset(rice,Region=='Kanto')
kantoDates[order(kantoDates$medDate,decreasing = FALSE)[1:4],]


# Identify outliers using quokar (https://cran.r-project.org/web/packages/quokar/vignettes/quokar.html)
prob <- frame_bayes(y=rice$medDate, x=rice$distanceFromItazuke, tau=0.01, M =  500, burn = 100, method = 'bayes.prob')
rice$outlierProb = prob$value
rice$residuals=resid$residuals
rice$residualsGAM=fit2$residuals
coordinates(rice) =  c("Longitude","Latitude") 
proj4string(rice) <- CRS("+proj=longlat +datum=WGS84")
spplot(rice,zcol='outlierProb',sp.layout=window)
spplot(rice,zcol='residuals',sp.layout=window)
spplot(rice,zcol='residualsGAM',sp.layout=window)

#earlier than expected have smaller residuals, later has positive residual


# Ideally using Bayesian Version using JAGS (cf. https://stats.stackexchange.com/questions/17672/quantile-regression-in-jags)



# Grid Based Earliest Date
source('gridder.R')
window.utm = spTransform(window,CRS("+proj=utm +zone=54 ellps=WGS84"))
rice.utm = spTransform(rice,CRS("+proj=utm +zone=54 ellps=WGS84"))
hex50km = gridderEarlyDates(rice.utm,win=window.utm,cell_diameter=50000,clip=TRUE,method='hex')



