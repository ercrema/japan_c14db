# Load Library ----
library(here)
library(rcarbon)
library(rnaturalearth)
library(dplyr)
library(elevatr)
library(raster)
library(sf)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(grDevices)

# Load SPDs ----
c14db  <- readRDS(here('output','c14db_1.1.0.Rds'))
load(file=here('joad_figures','spds.RData'))


# Create Spatial Data ----

# Site Locations
c14db.locations <- dplyr::select(c14db,Longitude,Latitude) |> unique() |> na.omit()
locations <- st_as_sf(c14db.locations,coords=c('Longitude','Latitude'),crs=4326)

# Sampling Intensity
datecounts <- aggregate(LabCode~Prefecture,length,data=c14db) |> left_join(x=_,y=unique(dplyr::select(c14db,Prefecture,Region)))
datecounts$Region2  <- datecounts$Region
datecounts$Region2[which(datecounts$Prefecture%in%c("Ishikawa","Fukui","Niigata","Toyama"))] <- 'Hokuriku'
datecounts$Region2[which(datecounts$Prefecture%in%c("Shizuoka","Aichi","Gifu","Mie"))] <- 'Tokai'
datecounts$Region2[which(datecounts$Region=="Kansai")]  <- 'Kinki'
win <- ne_states(country = "japan",returnclass = 'sf')
win$woe_name[which(win$woe_name=='Hyōgo')] <- 'Hyogo'
win  <- left_join(win,datecounts,by=c('woe_name'='Prefecture')) |> dplyr::select(woe_name,region=Region2,n=LabCode)
win$area  <- st_area(win)
win$dens  <- as.numeric(win$n/(win$area/1e+6))
win2  <- dplyr::select(win,region) |> group_by(region) |> summarise()

# Background Map
world <- ne_countries(scale = "large", returnclass = "sf")
# Hillshade Japan
elevation <- get_elev_raster(locations = win, z = 5,clip = "locations",src='aws')
slope  <- terrain(elevation,opt='slope')
aspect  <- terrain(elevation,opt='aspect')
hill  <- hillShade(slope,aspect,40,270)
cropped_elev <- crop(elevation,win)
cropped_hill  <- crop(hill,win)
elevate <- as.data.frame(cropped_elev,xy = TRUE)
hs  <- as.data.frame(cropped_hill,xy = TRUE)
colnames(elevate)[3] = "elevation_value"
colnames(hs)[3] = "hs_value"
elevate <- elevate[complete.cases(elevate),]
hs <- hs[complete.cases(hs),]
shade <- ggplot(hs, aes(x, y)) +
	geom_raster(aes(x=x,y=y,fill = hs_value), alpha = 0.5) +
	scale_fill_gradient2(low = "white", high = "white", mid = "black", midpoint = 0.6, guide = "none") +
	xlim(124,146) + 
	ylim(24.5,46)
grob.shade <- ggplotGrob(shade)
grob.shade <- grob.shade$grobs[[6]]$children[[3]]


# Figure 1 ----
g0 <-   ggplot() +
	geom_sf(data=world,aes(),fill='grey65',show.legend=FALSE,lwd=0) +
	annotation_custom(grob = grob.shade) +
	geom_sf(data=locations,size=0.6,col='black',pch=21,fill='tomato') + 
	xlab("Longitude") + 
	ylab("Latitude") + 
	xlim(124,146) + 
	ylim(24.5,46)


g1 <- 	ggplot()  + 
	geom_sf(data=world,fill='lightgrey')+
	geom_sf(data=win,aes(fill=dens),lwd=0) +
	scale_fill_distiller(palette = "Spectral") +
	geom_sf(data=win2,fill=NA,lwd=0.6) +
	geom_text(data = data.frame(x = 138.38, y = 44.58, label = "Hokkaido"),mapping = aes(x = x, y = y, label = label),  inherit.aes = FALSE) +
	geom_text(data = data.frame(x = 143.8, y = 38.50, label = "Tohoku"), mapping = aes(x = x, y = y, label = label), inherit.aes = FALSE) +
	geom_curve(data = data.frame(x = 136.07, y = 38.67, xend = 138.04, yend = 37.55),mapping = aes(x = x, y = y, xend = xend, yend = yend),angle = 0L, arrow = arrow(30L, unit(0L, "inches"), "last", "closed"),inherit.aes = FALSE) +  
	geom_text(data = data.frame(x = 135.68, y = 39.06, label = "Hokuriku"),mapping = aes(x = x, y = y, label = label),inherit.aes = FALSE) +
	geom_text(data = data.frame(x = 142.27, y = 35.50, label = "Kanto"),mapping = aes(x = x, y = y, label = label),inherit.aes = FALSE) +
	geom_curve(data = data.frame(x = 140.77, y = 33.89, xend = 138.29, yend = 35.85),mapping = aes(x = x, y = y, xend = xend, yend = yend),angle = 0L, arrow = arrow(30L, unit(0L, "inches"), "last", "closed"),inherit.aes = FALSE) + 
	geom_text(data = data.frame(x = 141.41, y = 33.33, label = "Chubu"),mapping = aes(x = x, y = y, label = label),inherit.aes = FALSE) +
	geom_curve(data = data.frame(x = 137.52, y = 34.94, xend = 137.10, yend = 33.33),mapping = aes(x = x, y = y, xend = xend, yend = yend),angle = 0L, arrow = arrow(30L, unit(0L, "inches"), "last", "closed"),inherit.aes = FALSE) + 
	geom_text(data = data.frame(x = 137.05, y = 32.77, label = "Tokai"),mapping = aes(x = x, y = y, label = label),inherit.aes = FALSE) +
	geom_curve(data = data.frame(x = 133.08, y = 37.10, xend = 135.30, yend = 35.15),mapping = aes(x = x, y = y, xend = xend, yend = yend),angle = 0L, arrow = arrow(30L, unit(0L, "inches"), "last", "closed"),inherit.aes = FALSE) + 
	geom_text(data = data.frame(x = 132.52, y = 37.45, label = "Kinki"),mapping = aes(x = x, y = y, label = label),inherit.aes = FALSE) +
	geom_text(data = data.frame(x = c(128.03, 130.95, 133.931),y = c(31.31, 35.92, 31.52),label = c("Kyushu", "Chugoku", "Shikoku")),mapping = aes(x = x, y = y, label = label), inherit.aes = FALSE) +
	xlab("Longitude") + 
	ylab("Latitude") + 
	labs(fill = 'Sampling Density \n (dates/sq.km)')+
	xlim(124,146) + 
	ylim(24.5,46) +
	theme(legend.position = c(0.8, 0.2))

png(file = 'figure1.png',width=10,height=7,units='in',res=500)
grid.arrange(g0,g1,ncol=2)
dev.off()



# Figure 2 ----
# Prepare SPD
spd.whole  <- spd.calibrated$grid
spd.whole$PrDens  <- runMean(spd.whole$PrDens,100)
spd.whole  <- subset(spd.whole,!is.na(PrDens))

# Plot
png(file = 'figure2.png',width=10,height=5,units='in',res=500)
plot(spd.calibrated$grid[,1],runMean(spd.calibrated$grid[,2],100),type='n',xlim=c(20000,100),axes=FALSE,xlab='',ylab='Summed Probability',xaxs='i')
rect(xleft=20000,xright=16000,ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[1],alpha.f=0.5))
rect(xleft=16000,xright=BCADtoBP(-950),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[2],alpha.f=0.5))
rect(xleft=BCADtoBP(-950),xright=BCADtoBP(250),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[3],alpha.f=0.5))
rect(xleft=BCADtoBP(250),xright=BCADtoBP(1185),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[4],alpha.f=0.5))
rect(xleft=BCADtoBP(1185),xright=BCADtoBP(1573),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[5],alpha.f=0.5))
rect(xleft=BCADtoBP(1573),xright=BCADtoBP(1868),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[6],alpha.f=0.5))
polygon(c(spd.whole$calBP,rev(spd.whole$calBP)),c(rep(-1,nrow(spd.whole)),rev(spd.whole$PrDens)),border=NA,col='grey47')
axis(1,at=seq(20000,0,-1000),padj=-2,cex.axis=0.7,tck=-.015)
axis(side=1,at=seq(20000,0,-500),labels=NA,tck = -.01)
axis(3,at=c(BCADtoBP(c(seq(-20000,-1000,1000),1,1000,1950))),labels=abs(c(seq(-20000,-1000,1000),1,1000,1950)),padj=2,cex.axis=0.7,tck=-.015)
axis(side=3,at=c(BCADtoBP(c(seq(-20000,-500,500),1,500,1000,1500))),labels=NA,tck = -.01)
axis(2)
mtext('Cal BP',1,line=2)
mtext('BCE/CE',3,line=2)
legend("topleft",legend=c('Palaeolithic','Jomon','Yayoi','Kodai','Early Modern','Modern'),fill=adjustcolor(brewer.pal('Set2',n=6),alpha.f=0.5),bg='white')
dev.off()


# Figure 3 ----
# Koyama Estimates (From Koyama and Sugito 1984)
Tohoku = c(2000,19200,46700,43800,39500,33400)
Kanto = c(9700,42800,95400,51600,7700,99000)
Hokuriku =c(400,4200,24600,15700,5100,20700)
Chubu = c(3000,25300,71900,22000,6000,84200)
Tokai = c(2200,5000,13200,7600,6600,55300)
Kinki = c(300,1700,2800,4400,2100,108300)
Chugoku = c(400,1300,1200,2400,2000,58800)
Shikoku = c(200,400,200,2700,500,30100)
Kyushu = c(1900,5600,5300,10100,6300,105100)

# Plot Function
JomonYayoi <- function(x,ylim=c(0,max(x)))
{
	plot(NULL,xlim=c(11500,1700),ylim=ylim,axes=FALSE,xlab='',ylab='')
	rect(xleft=11500,xright=7000,ybottom=0,ytop=x[1],border=NA,col='lightgrey')
	rect(xleft=7000,xright=5500,ybottom=0,ytop=x[2],border=NA,col='lightgrey')
	rect(xleft=5500,xright=4400,ybottom=0,ytop=x[3],border=NA,col='lightgrey')
	rect(xleft=4400,xright=3200,ybottom=0,ytop=x[4],border=NA,col='lightgrey')
	rect(xleft=3200,xright=2900,ybottom=0,ytop=x[5],border=NA,col='lightgrey')
	rect(xleft=2900,xright=1700,ybottom=0,ytop=x[6],border=NA,col='lightgrey')
	axis(4,cex.axis=0.8,padj=-0.8)
}


# Plot Figure 3
png(file = 'figure3.png',width=11,height=10,units='in',res=500)
layout(matrix(1:9,3,3))
layout.show(9)
Regions = c('Kyushu','Shikoku','Chugoku','Kinki','Tokai','Chubu','Kanto','Hokuriku','Tohoku')

for (i in 1:length(Regions))
{
	par(mar=c(5,4,4,4))
	JomonYayoi(get(Regions[i]))
	par(new=T)
	ii <- which(names(spd.regional$spds)==Regions[i])
	tmp.spd  <- spd.regional$spds[ii][[1]][[2]]
	tmp.spd$PrDens  <- runMean(tmp.spd$PrDens,100)
	tmp.spd  <- subset(tmp.spd,calBP<11500&calBP>1700&!is.na(PrDens))
	plot(NULL,xlim=c(11500,1700),ylim=c(0,max(tmp.spd$PrDens)),axes=F,xlab='',ylab='')
	lines(tmp.spd$calBP,tmp.spd$PrDens,lwd=2)
	axis(2,cex.axis=0.8)
	axis(1,at=seq(12000,1000,-1000),padj=-0.8,cex.axis=0.8)
	axis(side=1,at=seq(12000,1000,-200),labels=NA,tck = -.01)
	axis(3,at=c(BCADtoBP(c(seq(-11000,-1000,1000),1,1000))),labels=abs(c(seq(-11000,-1000,1000),1,1000)),padj=0.8,cex.axis=0.8)
	axis(side=3,at=c(BCADtoBP(c(seq(-11000,-200,200),1,200))),labels=NA,tck = -.01)
	mtext('Cal BP',1,line=2,cex=0.8)
	mtext('BCE/CE',3,line=2,cex=0.8)
	mtext('Est.Pop.Size(Koyama 1984)',4,line=2,cex=0.8)
	mtext('Summed Probability',2,line=2,cex=0.8)
	box()
	legend('topleft',legend=Regions[i],bty='n',cex=2)
}
dev.off()
