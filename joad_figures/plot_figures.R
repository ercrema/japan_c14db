# Load Library ----
library(here)
library(rcarbon)

# Load SPDs ----
load(file=here('joad_figures','spds.RData'))

# Figure 1 (All dates across time)
library(RColorBrewer)
library(grDevices)
spd.whole  <- spd.calibrated$grid
spd.whole$PrDens  <- runMean(spd.whole$PrDens,100)
spd.whole  <- subset(spd.whole,!is.na(PrDens))
plot(spd.calibrated$grid[,1],runMean(spd.calibrated$grid[,2],100),type='n',xlim=c(16000,100),axes=FALSE,xlab='',ylab='Summed Probability',xaxs='i')
rect(xleft=20000,xright=14000,ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[1],alpha.f=0.5))
rect(xleft=14000,xright=BCADtoBP(-950),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[2],alpha.f=0.5))
rect(xleft=BCADtoBP(-950),xright=BCADtoBP(250),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[3],alpha.f=0.5))
rect(xleft=BCADtoBP(250),xright=BCADtoBP(1185),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[4],alpha.f=0.5))
rect(xleft=BCADtoBP(1185),xright=BCADtoBP(1573),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[5],alpha.f=0.5))
rect(xleft=BCADtoBP(1573),xright=BCADtoBP(1868),ybottom=-100,ytop=1000,border=NA,col=adjustcolor(brewer.pal('Set2',n=6)[6],alpha.f=0.5))
polygon(c(spd.whole$calBP,rev(spd.whole$calBP)),c(rep(-1,nrow(spd.whole)),rev(spd.whole$PrDens)),border=NA,col='grey47')
axis(1,at=seq(20000,0,-1000),padj=-0.8,cex.axis=0.8)
axis(side=1,at=seq(20000,0,-500),labels=NA,tck = -.01)
axis(3,at=c(BCADtoBP(c(seq(-20000,-1000,1000),1,1000,1950))),labels=abs(c(seq(-20000,-1000,1000),1,1000,1950)),padj=0.8,cex.axis=0.8)
axis(side=3,at=c(BCADtoBP(c(seq(-20000,-500,500),1,500,1000,1500))),labels=NA,tck = -.01)
axis(2)
mtext('Cal BP',1,line=2)
mtext('BCE/CE',3,line=2)
legend("topleft",legend=c('Palaeolithic','Jomon','Yayoi','Kodai','Early Modern','Modern'),fill=adjustcolor(brewer.pal('Set2',n=6),alpha.f=0.5),bg='white')


# Regional Comparison

# Koyama Estimate
Tohoku = c(2000,19200,46700,43800,39500,33400)
Kanto = c(9700,42800,95400,51600,7700,99000)
Hokuriku =c(400,4200,24600,15700,5100,20700)
Chubu = c(3000,25300,71900,22000,600,84200)
Tokai = c(2200,5000,13200,7600,6600,55300)
Kinki = c(300,1700,2800,4400,2100,108300)
Chugoku = c(400,1300,1200,2400,2000,58800)
Shikoku = c(200,400,200,2700,500,30100)
Kyushu = c(1900,5600,5300,10100,6300,105100)

JomonYayoi <- function(x)
{
	plot(NULL,xlim=c(11500,1700),ylim=c(0,max(x)),axes=FALSE,xlab='',ylab='')
	rect(xleft=11500,xright=7000,ybottom=0,ytop=x[1],border=NA,col='lightgrey')
	rect(xleft=7000,xright=5500,ybottom=0,ytop=x[2],border=NA,col='lightgrey')
	rect(xleft=5500,xright=4400,ybottom=0,ytop=x[3],border=NA,col='lightgrey')
	rect(xleft=4400,xright=3200,ybottom=0,ytop=x[4],border=NA,col='lightgrey')
	rect(xleft=3200,xright=2900,ybottom=0,ytop=x[5],border=NA,col='lightgrey')
	rect(xleft=2900,xright=1700,ybottom=0,ytop=x[6],border=NA,col='lightgrey')
	axis(4,cex.axis=0.8,padj=-0.8)
}



layout(matrix(1:9,3,3))
layout.show(9)
Regions = c('Kyushu','Shikoku','Chugoku','Kinki','Tokai','Chubu','Kanto','Hokuriku','Tohoku')

for (i in 1:length(Regions))
{
	par(mar=c(5,4,4,4))
	JomonYayoi(get(Regions[i]))
	par(new=T)
	i <- which(names(spd.regional$spds)==Regions[i])
	tmp.spd  <- spd.regional$spds[i][[1]][[2]]
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












