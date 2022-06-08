
xvals = c(0:30)


q95fn = function(x){return(quantile(x,c(0.025,0.975),na.rm=T))}
plot.fn = function(y,samps,x,yub,xub,mar1,mar2,by,sub,lab,othcol=0){
  
  if (othcol==0){
    cols = c('dark red','dark blue')    
  } else{
    cols = c('darkorange4','dark blue')
  }
  
  
  par(mar=mar1); par(tck=-0.02)
  plot(1,type='n',xlim=c(0,xub),ylim=c(0,yub),axes=F,ann=F)
  mainY = cbind(c(0,0,rep(y[,1],each=2)),c(0,0,rep(y[,2],each=2))); mainY = mainY[1:(dim(mainY)[1]-1),]
  ciLb = cbind(c(0,0,rep(apply(samps[,,1],2,q95fn)[1,],each=2)),
               c(0,0,rep(apply(samps[,,2],2,q95fn)[1,],each=2))); ciLb = ciLb[1:(dim(ciLb)[1]-1),]
  ciUb = cbind(c(0,0,rep(apply(samps[,,1],2,q95fn)[2,],each=2)),
               c(0,0,rep(apply(samps[,,2],2,q95fn)[2,],each=2))); ciUb = ciUb[1:(dim(ciUb)[1]-1),]
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
    lines(y=mainY[,i],x=xs,col=cols[i],lwd=0.5)
    lines(y=ciLb[,i],col=cols[i],x=xs,lty='dotted')
    lines(y=ciUb[,i],col=cols[i],x=xs,lty='dotted')
  }
  box(bty='l')
  axis(side=2,at=seq(0,yub,by),labels=seq(0,yub,by)*100,las=1,cex.axis=0.65,lwd.ticks=0.5,lwd=0)
  axis(side=1,at=0:xub,labels=F,lwd.ticks=0.5,lwd=0)
  text(y=-0.09*yub,x=seq(0,xub,5),seq(0,xub,5),srt=45,cex=0.65,xpd=T,adj=1)
  mtext(side=2,'Cumulative prob., %',cex=0.5,line=1.25)
  mtext(side=1,'Days from admission',cex=0.5,line=1)
  mtext(side=3,lab,cex=0.5,font=3,adj=0,at=-8,line=0.25)

}

r = c(1,1,0); g = c(0,1,0); b = c(0,0,1); alpha = c(0.25,0.5,0.25)
col = c('dark red','darkorange4','dark blue')

hist.fn = function(tobj,yub,xub,j,ymax,yby,print=F,top){
  
  plot(1,type='n',axes=F,ann=F,xlim=c(0,xub+3),ylim=c(0,yub))
  for (i in 1:xub){
    polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(min(c(mean(tobj==i,na.rm=T),yub)),2)),lty=0,col=rgb(r[j],g[j],b[j],alpha[j]))
    lines(x=i+c(-0.5,0.5,0.5,-0.5,-0.5),y=c(0,0,rep(min(c(mean(tobj==i,na.rm=T),yub)),2),0),col=col[j])
  }
  polygon(x=xub+2.5+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(mean(tobj>xub,na.rm=T),2)),lty=0,col=rgb(r[j],g[j],b[j],alpha[j]))
  lines(x=xub+2.5+c(-0.5,0.5,0.5,-0.5,-0.5),y=c(0,0,rep(mean(tobj>xub,na.rm=T),2),0),col=col[j])
  
  box(bty='l')
  axis(side=1,at=c(1:xub,xub+2.5),labels=NA,lwd=0,lwd.ticks=0.5)
  axis(side=2,at=c(seq(0,ymax,yby),yub),
       labels=round(100*c(seq(0,ymax,yby),mean(tobj==1,na.rm=T))),
       lwd=0,lwd.ticks=0.5,cex.axis=0.65,las=1)
  axis.break(axis=2,breakpos=mean(c(ymax,yub)),style='slash',brw=0.06)
  mtext(side=2,'Prob., %',cex=0.5,line=1.25)
  axis.break(axis=1,breakpos=xub+1.25,style='zigzag',brw=0.08)
  if (print==T){
    text(x=seq(1,xub,3),y=-0.15*yub,seq(1,xub,3),srt=45,adj=1,cex=0.65,xpd=T) 
    text(x=xub+2.5,y=-0.15*yub,'>15',srt=45,adj=1,cex=0.65,xpd=T) 
    mtext(side=1,'Length of stay (days)',cex=0.5,line=1.25)
  }
  mtext(side=3,top,cex=0.5,font=3,adj=0,at=-4.5,line=0.25)
}

densObj = list()
for (i in 1:2){
  densObj[[i]] = list()
  for (j in 1:4){
    obj = density(hospRatios[,j,i])
    densObj[[i]][[j]] = cbind(obj$x,obj$y)
  }
}



pdf(file='hosp duration plots.pdf',width=4.5,height=5)
par(mgp=c(3,0.3,0)); par(lwd=0.5)

layout(matrix(c(rep(13,3),
                1,2,3,
                7,8,11,
                rep(14,3),
                4,5,6,
                9,10,12),nrow=6,byrow=T),heights=rep(c(0.15,1,0.75),2))


mar1 = c(2.5,2,1.5,0.5); mar2 = c(1,1.5,0,0.5)
plot.fn(y=cumDischHosp[,,1],samps=cumDischHospSamp[,,,1],x=xvals,xub=30,yub=1,by=0.2,mar1=mar1,mar2=mar2,sub=0,
        lab='A: To home')
plot.fn(y=cumCareHosp[,,1],samps=cumCareHospSamp[,,,1],x=xvals,xub=30,yub=0.5,by=0.1,mar1=mar1,mar2=mar2,sub=0,
        lab='B: To skilled care/home against advice')
plot.fn(y=cumDeathHosp[,,1],samps=cumDeathHospSamp[,,,1],x=xvals,xub=30,yub=0.1,by=0.02,mar1=mar1,mar2=mar2,sub=0,
        lab='C: Death/to hospice')

plot.fn(y=cumDischHosp[,,2],samps=cumDischHospSamp[,,,2],x=xvals,xub=30,yub=1,by=0.2,mar1=mar1,mar2=mar2,sub=0,othcol=1,
        lab='G: To home')
plot.fn(y=cumCareHosp[,,2],samps=cumCareHospSamp[,,,2],x=xvals,xub=30,yub=0.5,by=0.1,mar1=mar1,mar2=mar2,sub=0,othcol=1,
        lab='H: To skilled care/home against advice')

plot(1,type='n',axes=F,ann=F,xlim=c(0,30),ylim=c(0,1))

polygon(x=c(1,6,6,1),y=c(0.9,0.9,0.8,0.8),col=rgb(1,0,0,0.25),lty=0)
polygon(x=c(1,6,6,1),y=c(0.7,0.7,0.6,0.6),col=rgb(0,0,1,0.25),lty=0)
for (i in seq(0.6,0.9,0.1)){lines(x=c(1,6),y=rep(i,2),col=ifelse(i>=0.8,'dark red','dark blue'),lty='dotted')}
for (i in c(0.65,0.85)){lines(x=c(1,6),y=rep(i,2),col=ifelse(i>=0.8,'dark red','dark blue'),lwd=0.5)}
text(x=rep(7,2),y=c(0.85,0.65),adj=0,cex=0.65,font=3,
     c('Delta variant admissions',
       'Omicron variant admissions'))

polygon(x=c(1,6,6,1),y=c(0.3,0.3,0.2,0.2),col=rgb(1,1,0,0.5),lty=0)
polygon(x=c(1,6,6,1),y=c(0.1,0.1,0,0),col=rgb(0,0,1,0.25),lty=0)
for (i in c(0,0.1,0.2,0.3)){lines(x=c(1,6),y=rep(i,2),col=ifelse(i>=0.2,'darkorange4','dark blue'),lty='dotted')}
for (i in c(0.05,0.25)){lines(x=c(1,6),y=rep(i,2),col=ifelse(i>=0.2,'darkorange4','dark blue'),lwd=0.5)}
text(x=rep(7,2),y=c(0.25,0.05),adj=0,cex=0.65,font=3,
     c('BA.2 subvariant admissions',
       'BA.1* subvariant admissions'))
text(x=rep(1,2),y=c(1,0.4),c('Delta/Omicron comparison',
                             'BA.2/BA.1* comparison'),
     cex=0.65,font=2,adj=0,xpd=T)

par(mar=c(3,2,1.5,0.5)); par(tck=-0.05)
hist.fn(tobj=tdisch[which(tf==1&thosp>=1&testDate%in%daterange[[1]]&hospDate%in%daterange[[1]]&sgtf==0&dischDispo!=0&symptHosp==1)],
        yub=0.33,xub=15,j=1,ymax=0.26,yby=0.1,top='D. Delta variant admissions',print=T)

hist.fn(tobj=tdisch[which(tf==1&thosp>=1&testDate%in%daterange[[1]]&hospDate%in%daterange[[1]]&sgtf==1&dischDispo!=0&symptHosp==1)],
        yub=0.33,xub=15,j=3,ymax=0.26,yby=0.1,top='E. Omicron variant admissions',print=T)


hist.fn(tobj=tdisch[which(tf==1&thosp>=1&testDate%in%daterange[[2]][1:31]&hospDate%in%daterange[[2]][1:31]&sgtf==0&dischDispo!=0&symptHosp==1)],
        yub=0.33,xub=15,j=2,ymax=0.26,yby=0.1,print=T,top='I. BA.2 subvariant admissions')
hist.fn(tobj=tdisch[which(tf==1&thosp>=1&testDate%in%daterange[[2]][1:31]&hospDate%in%daterange[[2]][1:31]&sgtf==1&dischDispo!=0&symptHosp==1)],
        yub=0.37,xub=15,j=3,ymax=0.3,yby=0.1,print=T,top='J. BA.1* subvariant admissions')

cols = c('blue','red','darkgoldenrod4','forestgreen')
top = c('F. Delta vs. Omicron length of stay','K. BA.2 vs. BA.1* length of stay')
durs = c('>5 days','>10 days','>15 days','>20 days')
for (k in 1:2){
  plot(1,type='n',axes=F,ann=F,xlim=c(-1,1),ylim=c(0,4))
  abline(v=0,col='black')
  for (i in 1:4){
    lines(densObj[[k]][[i]][,2],x=log(densObj[[k]][[i]][,1]),col=cols[i])
      text(x=-1,y=3.8-0.7*(i-1),durs[i],adj=0,cex=0.65,font=3,col=cols[i])
  }  
  box(bty='l')
  axis(side=2,0:4,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5)
  axis(side=1,log10(c(seq(0.1,0.9,0.1),1:10)),labels=NA,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5)
  mtext(side=2,'Density',cex=0.5,line=0.75)
    text(x=log10(c(0.1,0.2,1,2,10)),c(0.1,0.2,1,2,10),cex=0.65,srt=45,adj=1,xpd=T,y=-0.65)
    mtext(side=1,'Likelihood ratio',cex=0.5,line=1.25)
  mtext(top[k],cex=0.5,at=-1.45,side=3,font=3,line=0.25,adj=0)
}

par(mar=rep(0,4))
plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1))
text(x=-0.03,y=0.5,xpd=T,font=2,adj=0,cex=0.75,
     'Comparison of cases admitted following outpatient Delta and Omicron variant detection,\n15 December, 2021 to 17 January, 2022')

plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1))
text(x=-0.03,y=0.5,xpd=T,font=2,adj=0,cex=0.75,
     'Comparison of cases admitted following outpatient BA.2 and BA.1* Omicron subvariant detection,\n3 February to 17 March, 2022')


dev.off()
