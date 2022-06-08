
pdf(file='era analysis.pdf',height=4.5,width=5)

layout(matrix(c(rep(1:3,each=3),
                1,10,1, 2,11,2, 3,12,3,
                rep(1:3,each=3),
                rep(4:6,each=3),
                4,13,4, 5,14,5, 6,6,6,
                rep(4:6,each=3),
                rep(7:9,each=3)),byrow=T,nrow=7),heights=c(rep(c(0.1,0.3,0.6),2),1),widths=rep(c(0.4,0.57,0.03),3))
       

par(mgp=c(3,0.3,0))
par(lwd=0.5)


plot.fn = function(obj,obj7,cpObj,lab){
  
  par(mar=c(2.75,2.5,1.5,2.5))
  par(tck=-0.02)
  cpObj[is.nan(cpObj)] = 0
  
  plot(1,type='n',axes=F,ann=F,ylim=c(0,0.5),xlim=c(1,76))
  polygon(y=c(cpObj,0)/sum(cpObj),x=c(1:76,1),col=rgb(0,0,1,0.25),lty=0)
  if (sum(cpObj)>0){
    axis(side=4,at=seq(0,0.15,0.05),col='blue',lwd=0.5,lwd.ticks=0.5,las=1,cex.axis=0.65) 
    text(x=77,y=0.22,'Probability\ndensity',adj=0,cex=0.6,font=3,col='blue',xpd=T)
  }
    
  par(new=T)
  plot(1,type='n',axes=F,ann=F,ylim=c(-4,-0.5),xlim=c(1,76))
  for (i in 1:75){
    lines(quantile(log10(obj[,i]),c(0.025,0.975)),x=rep(i,2),col=rgb(0.5,0.49,0.51,0.75))
  }
  lines(log10(obj7),col='red',lwd=1)
  box(bty='l')
  axis(side=2,at=-4:-1,label=c('0.01','0.1','1','10'),cex.axis=0.65,las=1,lwd=0,lwd.ticks=0.5)
  mtext(side=2,'Outcome probability, %',cex=0.5,line=1.45)
  axis(side=1,at=seq(1,76,15),labels=NA,lwd=0,lwd.ticks=0.5)
  text(c('1 Nov','16 Nov','1 Dec','16 Dec','31 Dec','15 Jan'),
       srt=45,adj=1,xpd=T,cex=0.65,y=-4.3,x=seq(1,76,15))
  mtext(side=3,adj=0,at=-26,cex=0.5,font=2,text=lab,line=0.5)
  mtext(side=1,'Testing date',cex=0.5,line=1.65)
}

objList = list(pHosp,pSymptHosp,pIcu,pVent,pDeath)
obj7List = list(pHosp7,pSymptHosp7,pIcu7,pVent7,pDeath7)
cpObjList = list(prAdj[1,],prAdj[2,],prAdj[3,],rep(0,76),prAdj[5,])
labs = c('A. Any hospital admission',
         'B. Symptomatic hospital admission',
         'C. Intensive care unit admission','D. Mechanical ventilation','E. Mortality')

for (i in 1:5){plot.fn(obj=objList[[i]],obj7=obj7List[[i]],cpObj=cpObjList[[i]],lab=labs[i])}



par(mar=c(0,0,0.5,0))
plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1))
for (i in seq(0.02,0.3,0.01)){
  lines(x=rep(i,2),y=c(0.9,0.8)+runif(2,-0.02,0.02),col=rgb(0.5,0.49,0.51,0.75))
}
lines(x=c(0,0.32),y=rep(0.85,2),lwd=1,col='red')
text(x=0.35,y=0.88,'Daily est. (bootstrap 95% CI)',col='black',adj=0,cex=0.65,font=3,xpd=T)
text(x=0.35,y=0.82,'7-day moving average',col='red',adj=0,cex=0.65,font=3,xpd=T)
polygon(x=c(seq(0,0.32,0.01),0),y=c(seq(0.6,0.675,0.075/16),seq(0.675,0.6,-0.075/15),0.6)+runif(34,-0.005,0.005),
        col=rgb(0,0,1,0.25),lty=0)
text(y=0.6375,x=0.35,col='blue',adj=0,cex=0.65,font=3,'Distribution of changepoint\npositions (inferred)')
polygon(y=c(0.4,0.45,0.45,0.4),x=c(0.04,0.04,0.28,0.28),col=rgb(1,0,0,0.25),lty=0)
lines(y=rep(0.4,2),x=c(0.04,0.28),col='red',lwd=0.25)
lines(y=rep(0.45,2),x=c(0.04,0.28),col='red',lwd=0.25)
lines(y=rep(0.425,2),x=c(0.04,0.28),col='red',lwd=0.5)
text(y=0.425,x=0.35,'Adjusted hazard ratio (95% CI)\nfitted via changepoint models',cex=0.65,font=3,adj=0,xpd=T,col='red')

plot.fn = function(obj,obj7,lab,ylb,yub,ylabs,ypos,yax,breakpos=NA,note){
  
  par(mar=c(2.75,2.25,1.5,0.25))

  plot(1,type='n',axes=F,ann=F,ylim=c(ylb,yub),xlim=c(1,76))
  for (i in 1:75){
    lines(quantile((obj[,i]),c(0.025,0.975),na.rm=T),x=rep(i,2),col=rgb(0.5,0.49,0.51,0.75))
  }
  lines((obj7),col='red',lwd=1)
  box(bty='l')
  axis(side=2,at=ypos,label=ylabs,cex.axis=0.65,las=1,lwd=0,lwd.ticks=0.5)
  if (is.na(breakpos)==F){
    axis.break(breakpos=breakpos,axis=2,style='zigzag',brw=0.05)
  }
  mtext(side=2,yax,cex=0.5,line=1.3)

  axis(side=1,at=seq(1,78,7),labels=NA,lwd=0,lwd.ticks=0.5)
  text(c('1 Nov','8 Nov','15 Nov','22 Nov','29 Nov','6 Dec','13 Dec','20 Dec','27 Dec','3 Jan','10 Jan','17 Jan'),
       srt=45,adj=1,xpd=T,cex=0.65,y=ylb-0.08*(yub-ylb),x=seq(1,78,7))
  
  mtext(side=3,adj=0,at=-20,cex=0.5,font=2,text=lab,line=0.5)
  text(x=76,y=ylb+0.075*(yub-ylb),adj=1,cex=0.65,note,font=3,xpd=T)
  mtext(side=1,'Testing date',cex=0.5,line=1.65)
}

objList = list(pSympt,-tSymptTest,tTestHosp)
obj7List = list(pSympt7,-tSymptTest7,tTestHosp7)
lab = c('F. Symptoms at testing','G. Time from symptoms to testing','H. Time from testing to admission')
yax = c('Proportion, %','Days','Days')
ylbs = c(0.63,2.25,0); yubs = c(0.92,5.5,21)
notes = c('Among all cases tested\nin outpatient settings',
          'Among all symptomatic cases\ntested in outpatient settings',
          'Among all hospitalized cases\nfirst tested in outpatient settings')

ylabs = list(c(0,seq(70,90,5)),
             c(0,3,4,5),
             seq(0,21,7))
ypos = list(c(0.63,seq(0.7,0.9,0.05)),
            c(2.25,3:5),
            seq(0,21,7))
breakpos = c(0.66,2.625,NA)
for (i in 1:3){plot.fn(obj=objList[[i]],obj7=obj7List[[i]],lab=lab[i],ylb=ylbs[i],yub=yubs[i],
                       ylabs=ylabs[[i]],ypos=ypos[[i]],yax=yax[i],breakpos=breakpos[i],note=notes[i])}


plot.fn = function(mergedObj){
  par(mar=c(1,3.25,0.5,0))
  par(mgp=c(3,0.2,0))
  q95fn = function(x){return(quantile(x,c(0.025,0.975),na.rm=T))}
  plot(1,type='n',axes=F,ann=F,ylim=c(-2,0.5),xlim=c(1,76))
  if (sum(mergedObj)!=0){
    abline(h=0,lty='dotted')
    polygon(y=log10(c(apply(mergedObj,2,q95fn)[1,],rev(apply(mergedObj,2,q95fn)[2,]))),
            x=c(0:76,76:0),col=rgb(1,0,0,0.25),lty=0)
    lines(y=apply(log10(mergedObj),2,q95fn)[1,],x=0:76,col='red',lwd=0.25)
    lines(y=apply(log10(mergedObj),2,q95fn)[2,],x=0:76,col='red',lwd=0.25)
    lines(y=apply(log10(mergedObj),2,mean),x=0:76,col='red')
    box(bty='l')   
    for (j in c(0.5)){lines(y=c(-1e5,1e5),x=rep(min(which(pSgtf>=j)),2),col='blue')}
    axis(1,at=c(1,31,62),lwd=0,lwd.ticks=0.5,tck=-0.075,labels=F)
    text(x=c(11,41,65),y=-2.5,adj=0,c('Nov','Dec','Jan'),cex=0.5,xpd=T)
    axis(side=2,at=c(-2,-1,0),labels=c(0.01,0.1,1),cex.axis=0.5,lwd=0,lwd.ticks=0.5,las=1,tck=-0.075)
    text(x=-22,y=-0.5,adj=1,'Adjusted\nhazard\nratio',cex=0.6,xpd=T,font=3,col='red')
  }
}

mergedList = list(mergedAdj[[1]],mergedAdj[[2]],mergedAdj[[3]],rep(0,76),mergedAdj[[5]])
for (i in 1:5){
  plot.fn(mergedObj=mergedList[[i]])
}





dev.off()
