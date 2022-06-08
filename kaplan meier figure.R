
q95fn = function(x){return(quantile(x,c(0.025,0.975),na.rm=T))}
plot.fn = function(y,samps,x,yub,xub,mar1,mar2,by,sub,lab,othcol=0,extra=0){
  
  if (othcol==0){
    cols = c('dark red','dark blue')    
  } else{
    cols = c('darkorange4','dark blue')
  }
  
  
  par(mar=mar1); par(tck=-0.02)
  plot(1,type='n',xlim=c(0,xub),ylim=c(0,yub),axes=F,ann=F)
  mainY = cbind(rep(y[,1],each=2),rep(y[,2],each=2)); mainY = mainY[1:(dim(mainY)[1]-1),]
  ciLb = cbind(rep(apply(samps[,,1],2,q95fn)[1,],each=2),
               rep(apply(samps[,,2],2,q95fn)[1,],each=2)); ciLb = ciLb[1:(dim(ciLb)[1]-1),]
  ciUb = cbind(rep(apply(samps[,,1],2,q95fn)[2,],each=2),
               rep(apply(samps[,,2],2,q95fn)[2,],each=2)); ciUb = ciUb[1:(dim(ciUb)[1]-1),]
  xs = c(x[1],rep(x[2:length(x)],each=2))
  
  mainY = mainY[which(xs<=xub),]
  ciLb = ciLb[which(xs<=xub),]
  ciUb = ciUb[which(xs<=xub),]
  xs = xs[which(xs<=xub)]
  
  if (othcol==0){
    polygon(y=c(ciLb[,1],rev(ciUb[,1])),x=c(xs,rev(xs)),col=rgb(1,0,0,0.25),lty=0)
  } else{
    polygon(y=c(ciLb[,1],rev(ciUb[,1])),x=c(xs,rev(xs)),col=rgb(1,1,0,0.5),lty=0)
  }
  polygon(y=c(ciLb[,2],rev(ciUb[,2])),x=c(xs,rev(xs)),col=rgb(0,0,1,0.25),lty=0)
  
  for (i in 1:2){
    lines(y=mainY[,i],x=xs,col=cols[i])
    lines(y=ciLb[,i],col=cols[i],x=xs,lty='dotted')
    lines(y=ciUb[,i],col=cols[i],x=xs,lty='dotted')
  }
  box(bty='l')
  axis(side=2,at=seq(0,yub,by),labels=seq(0,yub,by)*100,las=1,cex.axis=0.65,lwd.ticks=0.5,lwd=0)
  axis(side=1,at=0:xub,labels=F,lwd.ticks=0.5,lwd=0)
  text(y=-0.08*yub,x=seq(0,xub,5),seq(0,xub,5),srt=45,cex=0.65,xpd=T,adj=1)
  mtext(side=2,'Cumulative prob., %',cex=0.5,line=1.75)
  mtext(side=1,'Days',cex=0.5,line=1)
  mtext(side=3,lab,cex=0.5,font=3,adj=0,at=-16,line=0.25)
  
  
}


pdf(file='kaplan meier plots.pdf',width=6,height=3)
par(mgp=c(3,0.3,0)); par(lwd=0.5)

layout(matrix(c(rep(11,5),
                1:5,
                rep(12,5),
                6:10),byrow=T,nrow=4,ncol=5),heights=rep(c(0.1,1),2))


mar1 = c(2.5,3,1.5,0.5); mar2 = c(1,1.5,0,0.5)
plot.fn(y=cumHosp[,,1],samps=cumHospSamp[,,,1],x=xvals,xub=30,yub=0.02,by=0.005,mar1=mar1,mar2=mar2,sub=0,
        lab='A: Any admission')
plot.fn(y=cumSymptHosp[,,1],samps=cumSymptHospSamp[,,,1],x=xvals,xub=30,yub=0.02,by=0.005,mar1=mar1,mar2=mar2,sub=0,
        lab='B: Symptomatic admission')
plot.fn(y=cumIcu[,,1],samps=cumIcuSamp[,,,1],x=xvals,xub=30,yub=0.003,by=0.001,mar1=mar1,mar2=mar2,sub=0,
        lab='C: ICU admission')
plot.fn(y=cumVent[,,1],samps=cumVentSamp[,,,1],x=xvals,xub=30,yub=0.003,by=0.001,mar1=mar1,mar2=mar2,sub=0,
        lab='D: Mechanical ventilation')
plot.fn(y=cumDeath[,,1],samps=cumDeathSamp[,,,1],x=xvals,xub=30,yub=0.003,by=0.001,mar1=mar1,mar2=mar2,sub=0,
        lab='E: Mortality')

plot.fn(y=cumHosp[,,2],samps=cumHospSamp[,,,2],x=xvals,xub=30,yub=0.02,by=0.005,mar1=mar1,mar2=mar2,sub=0,
        lab='F: Any admission',othcol=1)
plot.fn(y=cumSymptHosp[,,2],samps=cumSymptHospSamp[,,,2],x=xvals,xub=30,yub=0.02,by=0.005,mar1=mar1,mar2=mar2,sub=0,
        lab='G: Symptomatic admission',othcol=1)
plot.fn(y=cumIcu[,,2],samps=cumIcuSamp[,,,2],x=xvals,xub=30,yub=0.003,by=0.001,mar1=mar1,mar2=mar2,sub=0,
        lab='H: ICU admission',othcol=1)
plot.fn(y=cumDeath[,,2],samps=cumDeathSamp[,,,2],x=xvals,xub=30,yub=0.003,by=0.001,mar1=mar1,mar2=mar2,sub=0,
        lab='I: Mortality',othcol=1)


par(mar=c(0,0,1.5,0))
plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0.4,1.2))
polygon(y=c(0.8,0.8,0.9,0.9),x=c(0.4,0.6,0.6,0.4),col=rgb(1,0,0,0.25),lty=0,xpd=T)
polygon(y=c(0.65,0.65,0.75,0.75),x=c(0.4,0.6,0.6,0.4),col=rgb(0,0,1,0.25),lty=0,xpd=T)
for (i in c(0.65,0.7,0.75,0.8,0.85,0.9)){lines(x=c(0.4,0.6),y=rep(i,2),col=ifelse(i<0.8,'dark blue','dark red'),
                                    lty=ifelse(i%in%c(0.7,0.85),'solid','dotted'))}
text(x=0.65,y=c(0.7,0.85),c('Omicron (SGTF)','Delta (No SGTF)'),cex=0.65,adj=0,xpd=T)

polygon(y=c(0.8,0.8,0.9,0.9)-0.5,x=c(0.4,0.6,0.6,0.4),col=rgb(1,1,0,0.5),lty=0,xpd=T)
polygon(y=c(0.65,0.65,0.75,0.75)-0.5,x=c(0.4,0.6,0.6,0.4),col=rgb(0,0,1,0.25),lty=0,xpd=T)
for (i in c(0.65,0.7,0.75,0.8,0.85,0.9)){lines(x=c(0.4,0.6),y=rep(i,2)-0.5,col=ifelse(i<0.8,'dark blue','darkorange4'),
                                               lty=ifelse(i%in%c(0.7,0.85),'solid','dotted'))}
text(x=0.65,y=c(0.7,0.85)-0.5,c('BA.1* Omicron (SGTF)','BA.2 Omicron (No SGTF)'),cex=0.65,adj=0,xpd=T)

text(x=0.4,y=c(1,0.5),adj=0,
     c('Delta/Omicron cases (A-E)','BA.1*/BA.2 cases (F-I)'),
     cex=0.7,font=2)

par(mar=c(0,0,0,0))
plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1))
text(x=-0.035,y=0.35,'Comparison of Delta and Omicron variant detections, 15 December, 2021 to 17 January, 2022',
     adj=0,cex=0.85,font=2,xpd=T)

plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1))
text(x=-0.035,y=0.35,'Comparison of BA.1* and BA.2 Omicron subvariant detections, 3 February to 17 March, 2022',
     adj=0,cex=0.85,font=2,xpd=T)

dev.off()
