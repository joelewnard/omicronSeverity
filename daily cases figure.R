
pdf(file='cases over time.pdf',width=6,height=1.5)

layout(matrix(c(1,1,1,3,3,3,5,5,5,
                1,2,1,3,4,3,5,6,5,
                1,1,1,3,3,3,5,5,5),nrow=3,byrow=T),widths=rep(c(5.5,2.5,0.25),3),heights=c(1,3,2))

par(mgp=c(3,0.3,0)); par(lwd=0.5)

monthlabs = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

daylabs = paste(day(days),' ',monthlabs[month(days)],sep='')


par(mar=c(3,2.5,1.5,0.5)); par(tck=-0.02); 
plot(1,type='n',axes=F,ann=F,xlim=c(31,132),ylim=c(0,5e4))
for (i in 31:137){
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(0,tests[i]),each=2),col=rgb(0.5,0.49,0.51,0.25),lty=0)
  #lines(x=i+c(-0.5,-0.5,0.5,0.5,-0.5),y=c(0,tests[i],tests[i],0,0),col='dark blue')
}
xs = (c(rep(31:137,each=2)))+0.5
ys = (c(tests[31],rep(tests[32:137],each=2),0))

lines(x=xs,y=ys,col='black')
box(bty='l')
axis(1,at=seq(31,137,7),lwd=0,lwd.ticks=0.5,labels=NA)
axis(2,at=seq(0,5e4,1e4),labels=seq(0,50,10),lwd=0,lwd.ticks=0.5,las=T,cex.axis=0.65)
mtext(side=2,line=1,'Total tests\n(in thousands)',cex=0.5)
mtext(side=1,line=1.75,'Day',cex=0.5)
mtext(side=3,line=0.25,'A. Testing volume',cex=0.5,font=2,adj=0,at=5)
text(x=seq(31,137,7),y=-0.08*5e4,adj=1,srt=45,cex=0.65,xpd=T,daylabs[seq(31,137,7)])
abline(v=62)

par(mar=c(2,1,0,0)); par(tck=-0.06)
plot(1,type='n',axes=F,ann=F,xlim=c(31,132),ylim=c(0,0.4))
lines(pos7,col='red')
box(bty='l')
axis(1,at=which(substr(daylabs,1,2)=='1 '),lwd=0,lwd.ticks=0.5,labels=NA)
axis(2,at=seq(0,0.4,0.1),labels=seq(0,0.4,0.1),lwd=0,lwd.ticks=0.5,las=T,cex.axis=0.5)
mtext(side=2,line=1,'Prop. positive\n(7d avg.)',cex=0.4)
#mtext(side=1,line=1.75,'Day',cex=0.5)
text(x=which(daylabs%in%c('1 Dec','1 Jan','1 Feb','1 Mar')),y=-0.06,
     adj=1,srt=45,xpd=T,c('Dec','Jan','Feb','Mar'),cex=0.45)
abline(v=62)






par(mar=c(3,2.5,1.5,0.5)); par(tck=-0.02); 
plot(1,type='n',axes=F,ann=F,xlim=c(31,132),ylim=c(0,2e4))
for (i in 31:137){
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(sgtfOutpt[i],totOutpt[i]),each=2),col=rgb(1,0,0,0.25),lty=0)
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(0,sgtfOutpt[i]),each=2),col=rgb(0,0,1,0.25),lty=0)
  #lines(x=i+c(-0.5,-0.5,0.5,0.5,-0.5),y=c(0,tests[i],tests[i],0,0),col='dark blue')
}

xs = (c(rep(31:137,each=2)))+0.5
ys = (c(totOutpt[31],rep(totOutpt[32:137],each=2),0))
lines(x=xs,y=ys,col='dark red')
ys = (c(sgtfOutpt[31],rep(sgtfOutpt[32:137],each=2),0))
lines(x=xs,y=ys,col='dark blue')
box(bty='l')
axis(1,at=seq(31,137,7),lwd=0,lwd.ticks=0.5,labels=NA)
axis(2,at=seq(0,2e4,5e3),labels=seq(0,20,5),lwd=0,lwd.ticks=0.5,las=T,cex.axis=0.65)
mtext(side=2,line=1,'Outpatient cases\n(in thousands)',cex=0.5)
mtext(side=3,line=0.25,'B. Outpatient cases identified (TF testing)',cex=0.5,font=2,adj=0,at=5)
mtext(side=1,line=1.75,'Day',cex=0.5)
text(x=seq(31,137,7),y=-0.08*2e4,adj=1,srt=45,cex=0.65,xpd=T,daylabs[seq(31,137,7)])
abline(v=62)

polygon(x=c(30,40,40,30),y=c(2e4,2e4,18e3,18e3),col=rgb(1,0,0,0.25),lty=0)
polygon(x=c(30,40,40,30),y=c(17e3,17e3,15e3,15e3),col=rgb(0,0,1,0.25),lty=0)
text(x=rep(41,2),y=c(19e3,16e3),c('No SGTF','SGTF'),cex=0.65,font=3,adj=0)
for (i in c(15e3,17e3,18e3,2e4)){lines(y=rep(i,2),x=c(30,40),col=ifelse(i>=18e3,'dark red','dark blue'))}

par(mar=c(2,1,0,0)); par(tck=-0.06)
plot(1,type='n',axes=F,ann=F,xlim=c(109,137),ylim=c(0,400))

for (i in 109:137){
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(sgtfOutpt[i],totOutpt[i]),each=2),col=rgb(1,0,0,0.25),lty=0)
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(0,sgtfOutpt[i]),each=2),col=rgb(0,0,1,0.25),lty=0)
  #lines(x=i+c(-0.5,-0.5,0.5,0.5,-0.5),y=c(0,tests[i],tests[i],0,0),col='dark blue')
}
xs = (c(108,rep(109:137,each=2)))+0.5
ys = (c(rep(totOutpt[109:137],each=2),0))
lines(x=xs,y=ys,col='dark red')
ys = (c(rep(sgtfOutpt[109:137],each=2),0))
lines(x=xs,y=ys,col='dark blue')

box(bty='l')
axis(1,at=which(daylabs%in%c('17 Feb','24 Feb','3 Mar','10 Mar','17 Mar')),lwd=0,lwd.ticks=0.5,labels=NA)
axis(2,at=seq(0,400,100),labels=seq(0,400,100),lwd=0,lwd.ticks=0.5,las=T,cex.axis=0.5)
mtext(side=2,line=1,'Outpatient\ncases',cex=0.4)
#mtext(side=1,line=1.75,'Day',cex=0.5)
text(x=which(daylabs%in%c('17 Feb','24 Feb','3 Mar','10 Mar','17 Mar')),y=-50,
     adj=1,srt=45,xpd=T,c('17 Feb','24 Feb','3 Mar','10 Mar','17 Mar'),cex=0.45)






par(mar=c(3,2.5,1.5,0.5)); par(tck=-0.02); 
plot(1,type='n',axes=F,ann=F,xlim=c(31,132),ylim=c(0,300))
for (i in 31:137){
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(outptHosps[i],totHosps[i]),each=2),col=rgb(1,0,1,0.25),lty=0)
  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=rep(c(0,outptHosps[i]),each=2),col=rgb(0.13,0.55,0.13,0.25),lty=0)
  #lines(x=i+c(-0.5,-0.5,0.5,0.5,-0.5),y=c(0,tests[i],tests[i],0,0),col='dark blue')
}

xs = (c(30,rep(31:137,each=2)))+0.5
ys = (c(rep(totHosps[31:137],each=2),0))
lines(x=xs,y=ys,col='darkorchid4')
ys = (c(rep(outptHosps[31:137],each=2),0))
lines(x=xs,y=ys,col='forestgreen')
box(bty='l')
axis(1,at=seq(31,137,7),lwd=0,lwd.ticks=0.5,labels=NA)
axis(2,at=seq(0,300,50),labels=seq(0,300,50),lwd=0,lwd.ticks=0.5,las=T,cex.axis=0.65)
mtext(side=2,line=1.5,'Positive admissions',cex=0.5)
mtext(side=3,line=0.25,'C. New inpatient admissions',cex=0.5,font=2,adj=0,at=5)
mtext(side=1,line=1.75,'Day',cex=0.5)
text(x=seq(31,137,7),y=-0.08*300,adj=1,srt=45,cex=0.65,xpd=T,daylabs[seq(31,137,7)])
abline(v=62)

j = 60
polygon(x=j+c(30,40,40,30),y=c(300,300,270,270),col=rgb(1,0,1,0.25),lty=0)
polygon(x=j+c(30,40,40,30),y=c(190,190,220,220),col=rgb(0.13,0.5,0.13,0.25),lty=0)
text(x=j+rep(41,2),y=c(285,205),c('New\ndetections','Outpatient\ncases'),cex=0.65,font=3,adj=0)
for (i in c(190,220,270,300)){lines(y=rep(i,2),x=j+c(30,40),col=ifelse(i>=270,'darkorchid4','forestgreen'))}




dev.off()



