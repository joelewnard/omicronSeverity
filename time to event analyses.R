
########################################################################################
########################################################################################
###### analysis here --- times to key events ###########################################
########################################################################################
########################################################################################

testWeek = floor(testDate/7); testWeek[testWeek<0] = 0

tsymptA = tsympt; tsymptA[tsymptA<=0] = 0.5
thospA = thosp; thospA[thosp<=0] = 0.5
ticuA = ticu; ticuA[ticu<=0] = 0.5
tventA = tvent; tventA[tvent<=0] = 0.5
tdeathA = tdeath; tdeathA[tdeath<=0] = 0.5


symptHospMod = hospMod = symptHospModOutpt = hospModOutpt = icuMod = icuModOutpt = ventMod = ventModOutpt = deathMod = deathModOutpt = symptMod = symptModAsx = symptModOutpt = symptModAsxOutpt = symptModHosp = symptModAsxHosp = list()
hospModAsxOutpt = symptHospModAsxOutpt = icuModAsxOutpt = ventModAsxOutpt = deathModAsxOutpt = list()
hospModAsx = symptHospModAsx = icuModAsx = ventModAsx = deathModAsx = list()
##### restrict primary analysis for date range of 15 Dec to 17 Jan
for (i in 1:5){

  hospModOutpt[[i]] = coxph(Surv(thosp,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  hospMod[[i]] = coxph(Surv(thospA,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))

  symptHospModOutpt[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  symptHospMod[[i]] = coxph(Surv(thospA,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  icuModOutpt[[i]] = coxph(Surv(ticu,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  icuMod[[i]] = coxph(Surv(ticuA,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  ventModOutpt[[i]] = coxph(Surv(tvent,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  ventMod[[i]] = coxph(Surv(tventA,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  deathModOutpt[[i]] = coxph(Surv(tdeath,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  deathMod[[i]] = coxph(Surv(tdeathA,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  symptModAsxOutpt[[i]] = coxph(Surv(tsympt,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&thosp>=1&tf==1&testDate%in%daterange[[1]]))
  hospModAsxOutpt[[i]] = coxph(Surv(thosp,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&thosp>=1&tf==1&testDate%in%daterange[[1]]))
  symptHospModAsxOutpt[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&thosp>=1&tf==1&testDate%in%daterange[[1]]))
  
  icuModAsxOutpt[[i]] = coxph(Surv(ticu,symptIcu)~sgtf+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&thosp>=1&tf==1&testDate%in%daterange[[1]]))
  ventModAsxOutpt[[i]] = coxph(Surv(tvent,symptVent)~sgtf+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&thosp>=1&tf==1&testDate%in%daterange[[1]]))
  deathModAsxOutpt[[i]] = coxph(Surv(tdeath,symptDeath)~sgtf+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&thosp>=1&tf==1&testDate%in%daterange[[1]]))
  
  symptModAsx[[i]] = coxph(Surv(tsymptA,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  hospModAsx[[i]] = coxph(Surv(thospA,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  symptHospModAsx[[i]] = coxph(Surv(thospA,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  
  icuModAsx[[i]] = coxph(Surv(ticuA,symptIcu)~sgtf+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  ventModAsx[[i]] = coxph(Surv(tventA,symptVent)~sgtf+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  deathModAsx[[i]] = coxph(Surv(tdeathA,symptDeath)~sgtf+strata(testDate),data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  
  print(i)
}




symptHospModOutptEn1 = symptHospModOutptAlt = list()
for (i in 1:5){
  
  symptHospModOutptAlt[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+incCat+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  
  symptHospModOutptEn1[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+incCat+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]&enroll==1))

  print(i)
}



symptHospMod2 = hospMod2 = symptHospModOutpt2 = hospModOutpt2 = icuMod2 = icuModOutpt2 = ventMod2 = ventModOutpt2 = deathMod2 = deathModOutpt2 = list()
##### restrict primary analysis for date range of 15 Dec to 17 Jan
for (i in 1:5){
  
  hospModOutpt2[[i]] = coxph(Surv(thosp,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[2]]))
  hospMod2[[i]] = coxph(Surv(thospA,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[2]]))
  
  symptHospModOutpt2[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[2]]))
  symptHospMod2[[i]] = coxph(Surv(thospA,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[2]]))
  
  icuModOutpt2[[i]] = coxph(Surv(ticu,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[2]]))
  icuMod2[[i]] = coxph(Surv(ticuA,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[2]]))
  
  ventModOutpt2[[i]] = coxph(Surv(tvent,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[2]]))
  ventMod2[[i]] = coxph(Surv(tventA,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[2]]))
  
  deathModOutpt2[[i]] = coxph(Surv(tdeath,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[2]]))
  deathMod2[[i]] = coxph(Surv(tdeathA,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[2]]))

  print(i)
}




hospModOutpt1 =symptHospModOutpt1 = icuModOutpt1 = ventModOutpt1 = deathModOutpt1 = list()
hospMod1 =symptHospMod1 = icuMod1 = ventMod1 = deathMod1 = list()
for (i in 1:5){

  anyVax = impDat[[i]]$vaxStatTime!='0'

  hospModOutpt1[[i]] = coxph(Surv(thosp,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  symptHospModOutpt1[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  icuModOutpt1[[i]] = coxph(Surv(ticu,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+anyVax*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  ventModOutpt1[[i]] = coxph(Surv(tvent,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+anyVax*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  deathModOutpt1[[i]] = coxph(Surv(tdeath,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+anyVax*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  
  hospMod1[[i]] = coxph(Surv(thospA,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  symptHospMod1[[i]] = coxph(Surv(thospA,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  icuMod1[[i]] = coxph(Surv(ticuA,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+anyVax*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  ventMod1[[i]] = coxph(Surv(tventA,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+anyVax*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  deathMod1[[i]] = coxph(Surv(tdeathA,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+anyVax*sgtf+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  print(i)
}


symptModAsxU = symptModAsxOutptU = symptModAsx = symptModAsxOutpt = symptModAsxVax = list()
symptModAsx0 = symptModAsx1 = symptModAsxOutpt0 = symptModAsxOutpt1 = list()
symptModAsx0U = symptModAsx1U = symptModAsxOutpt0U = symptModAsxOutpt1U = list()
for (i in 1:5){

  symptModAsx0[[i]] = coxph(Surv(tsymptA,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid,data=impDat[[i]],subset=(tsympt>=1&tf==1&vaxStatTime=='0'&testDate%in%daterange[[1]]))
  symptModAsx1[[i]] = coxph(Surv(tsymptA,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid+vaxStatTime,data=impDat[[i]],subset=(tsympt>=1&tf==1&vaxStatTime!='0'&testDate%in%daterange[[1]]))
  
  symptModAsxOutpt0[[i]] = coxph(Surv(tsympt,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid,data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&vaxStatTime=='0'&testDate%in%daterange[[1]]))
  symptModAsxOutpt1[[i]] = coxph(Surv(tsympt,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid+vaxStatTime,data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&vaxStatTime!='0'&testDate%in%daterange[[1]]))
  
  symptModAsx0U[[i]] = coxph(Surv(tsymptA,sympt)~sgtf,data=impDat[[i]],subset=(tsympt>=1&tf==1&vaxStatTime=='0'&testDate%in%daterange[[1]]))
  symptModAsx1U[[i]] = coxph(Surv(tsymptA,sympt)~sgtf,data=impDat[[i]],subset=(tsympt>=1&tf==1&vaxStatTime!='0'&testDate%in%daterange[[1]]))
  
  symptModAsxOutpt0U[[i]] = coxph(Surv(tsympt,sympt)~sgtf,data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&vaxStatTime=='0'&testDate%in%daterange[[1]]))
  symptModAsxOutpt1U[[i]] = coxph(Surv(tsympt,sympt)~sgtf,data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&vaxStatTime!='0'&testDate%in%daterange[[1]]))
  
  symptModAsxU[[i]] = coxph(Surv(tsymptA,sympt)~sgtf,data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  symptModAsxOutptU[[i]] = coxph(Surv(tsymptA,sympt)~sgtf,data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  
  symptModAsx[[i]] = coxph(Surv(tsymptA,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid+vaxStatTime,data=impDat[[i]],subset=(tsympt>=1&tf==1&testDate%in%daterange[[1]]))
  symptModAsxOutpt[[i]] = coxph(Surv(tsympt,sympt)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid+vaxStatTime,data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&testDate%in%daterange[[1]]))

  symptModAsxVax[[i]] = coxph(Surv(tsympt,sympt)~sgtf*(vaxStatTime!='0')+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tsympt>=1&tf==1&testDate%in%daterange[[1]]))

  
  print(i)
}





#####################################################################################
#### Complete case analysis and leaving out pars anaylsis ###########################
#####################################################################################

hospModCCOutpt = coxph(Surv(thosp,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
hospModCC = coxph(Surv(thospA,hosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(tf==1&testDate%in%daterange[[1]]))

symptHospModCCOutpt = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
symptHospModCC = coxph(Surv(thospA,symptHosp)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(tf==1&testDate%in%daterange[[1]]))

icuModCCOutpt = coxph(Surv(ticu,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
icuModCC = coxph(Surv(ticuA,symptIcu)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(tf==1&testDate%in%daterange[[1]]))

ventModCCOutpt = coxph(Surv(tvent,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
ventModCC = coxph(Surv(tventA,symptVent)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(tf==1&testDate%in%daterange[[1]]))

deathModCCOutpt = coxph(Surv(tdeath,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
deathModCC = coxph(Surv(tdeathA,symptDeath)~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=forImp,subset=(tf==1&testDate%in%daterange[[1]]))


for (i in 1:5){
  
  hospModLoOutpt[[i]] = coxph(Surv(thosp,hosp)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  hospModLo[[i]] = coxph(Surv(thospA,hosp)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))

  symptHospModLoOutpt[[i]] = coxph(Surv(thosp,symptHosp)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  symptHospModLo[[i]] = coxph(Surv(thospA,symptHosp)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  icuModLoOutpt[[i]] = coxph(Surv(ticu,symptIcu)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  icuModLo[[i]] = coxph(Surv(ticuA,symptIcu)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  ventModLoOutpt[[i]] = coxph(Surv(tvent,symptVent)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  ventModLo[[i]] = coxph(Surv(tventA,symptVent)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  
  deathModLoOutpt[[i]] = coxph(Surv(tdeath,symptDeath)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  deathModLo[[i]] = coxph(Surv(tdeathA,symptDeath)~sgtf+ed+inpt+outpt+sex+race+as.factor(chargrp)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+strata(testDate),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
}



##########################################################################################
##########################################################################################
####### alternative not time-to-event for omicron/delta ##################################
##########################################################################################
##########################################################################################

hospBnModOutpt1 =symptHospBnModOutpt1 = icuBnModOutpt1 = ventBnModOutpt1 = deathBnModOutpt1 = list()
hospBnMod1 =symptHospBnMod1 = icuBnMod1 = ventBnMod1 = deathBnMod1 = list()
#### neeeded starting values soomewhere in i=2 so used i=1 fits for starting values
starts = list(coef(hospBnModOutpt1[[1]]),
              coef(symptHospBnModOutpt1[[1]]),
              coef(icuBnModOutpt1[[1]]),
              coef(ventBnModOutpt1[[1]]),
              coef(deathBnModOutpt1[[1]]),
              coef(hospBnMod1[[1]]),
              coef(symptHospBnMod1[[1]]),
              coef(icuBnMod1[[1]]),
              coef(ventBnMod1[[1]]),
              coef(deathBnMod1[[1]]))

for (i in 1:5){

  hospBnModOutpt1[[i]] = glm(hosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                             start=starts[[1]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
print(c(i,'hosp outpt'))
  symptHospBnModOutpt1[[i]] = glm(symptHosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                                  start=starts[[2]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'sympt hosp outpt'))
  icuBnModOutpt1[[i]] = glm(symptIcu~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                            start=starts[[3]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'icu outpt'))
  ventBnModOutpt1[[i]] = glm(symptVent~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                             start=starts[[4]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'vent outpt'))
  deathBnModOutpt1[[i]] = glm(symptDeath~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                              start=starts[[5]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'death outpt'))
  
  hospBnMod1[[i]] = glm(hosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                        start=starts[[6]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'hosp'))
  symptHospBnMod1[[i]] = glm(symptHosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                             start=starts[[7]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'sympt hosp'))
  icuBnMod1[[i]] = glm(symptIcu~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                       start=starts[[8]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'icu'))
  ventBnMod1[[i]] = glm(symptVent~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                        start=starts[[9]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'vent'))
  deathBnMod1[[i]] = glm(symptDeath~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime*sgtf+priorCovid+as.factor(testWeek),
                         start=starts[[10]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'death'))
  
  print(i)
}



hospBnModOutpt =symptHospBnModOutpt = icuBnModOutpt = ventBnModOutpt = deathBnModOutpt = list()
hospBnMod =symptHospBnMod = icuBnMod = ventBnMod = deathBnMod = list()
#### neeeded starting values soomewhere in i=2 so used i=1 fits for starting values
starts = list(coef(hospBnModOutpt1[[1]])[1:48],
              coef(symptHospBnModOutpt1[[1]])[1:48],
              coef(icuBnModOutpt1[[1]])[1:48],
              coef(ventBnModOutpt1[[1]])[1:48],
              coef(deathBnModOutpt1[[1]])[1:48],
              coef(hospBnMod1[[1]])[1:48],
              coef(symptHospBnMod1[[1]])[1:48],
              coef(icuBnMod1[[1]])[1:48],
              coef(ventBnMod1[[1]])[1:48],
              coef(deathBnMod1[[1]])[1:48])

for (i in 1:5){
  
  hospBnModOutpt[[i]] = glm(hosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                             start=starts[[1]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'hosp outpt'))
  symptHospBnModOutpt[[i]] = glm(symptHosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                                  start=starts[[2]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'sympt hosp outpt'))
  icuBnModOutpt[[i]] = glm(symptIcu~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                            start=starts[[3]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'icu outpt'))
  ventBnModOutpt[[i]] = glm(symptVent~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                             start=starts[[4]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'vent outpt'))
  deathBnModOutpt[[i]] = glm(symptDeath~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                              start=starts[[5]],family=binomial(link='log'),data=impDat[[i]],subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  print(c(i,'death outpt'))
  
  hospBnMod[[i]] = glm(hosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                        start=starts[[6]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'hosp'))
  symptHospBnMod[[i]] = glm(symptHosp~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                             start=starts[[7]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'sympt hosp'))
  icuBnMod[[i]] = glm(symptIcu~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                       start=starts[[8]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'icu'))
  ventBnMod[[i]] = glm(symptVent~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                        start=starts[[9]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'vent'))
  deathBnMod[[i]] = glm(symptDeath~sgtf+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid+as.factor(testWeek),
                         start=starts[[10]],family=binomial(link='log'),data=impDat[[i]],subset=(tf==1&testDate%in%daterange[[1]]))
  print(c(i,'death'))
  
  print(i)
}


############################################################################################################################
############################################################################################################################
####### Era analysis #######################################################################################################
############################################################################################################################
############################################################################################################################


setwd('~/Google drive (jlewnard@berkeley.edu)/CAP KP study/omicron/data code/data update/revision analysis')
#### analysis hits peopel tested through day 76 (jan 16, 2022)

dates1 = c(15:61)
dates2 = c(22:68)
cps = cp1 = cp2 = is = js = c()
parsHosp = parsSymptHosp = parsIcu = parsVent = parsDeath = c()
bicHosp = bicSymptHosp = bicIcu = bicVent = bicDeath = c()
vcovHosp = vcovSymptHosp = vcovIcu = vcovVent = vcovDeath = array(NA,dim=c(length(dates1),length(dates2),3,3))
for (i in 1:length(dates1)){
  for (j in (which(dates2>=(dates1[i]+7)))){
    
    cps = c(cps,paste(dates1[i],dates2[j],sep='-'))
    cp1 = c(cp1,dates1[i])
    cp2 = c(cp2,dates2[j])
    is = c(is,i)
    js = c(js,j)
    
    t1 = testDate-dates1[i]; t1[t1<=0] = 0
    t2 = testDate-dates2[j]; t2[t2<=0] = 0


    mod = coxph(Surv(thosp,hosp)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
    parsHosp = cbind(parsHosp,coef(mod))
    vcovHosp[i,j,,] = vcov(mod)[1:3,1:3]
    bicHosp = c(bicHosp,BIC(mod))
    
    mod = coxph(Surv(thosp,symptHosp)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
    parsSymptHosp = cbind(parsSymptHosp,coef(mod))
    vcovSymptHosp[i,j,,] = vcov(mod)[1:3,1:3]
    bicSymptHosp = c(bicSymptHosp,BIC(mod))
    
    mod = coxph(Surv(ticu,icu)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
    parsIcu = cbind(parsIcu,coef(mod))
    vcovIcu[i,j,,] = vcov(mod)[1:3,1:3]
    bicIcu = c(bicIcu,BIC(mod))
    
    mod = coxph(Surv(tvent,vent)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
    parsVent = cbind(parsVent,coef(mod))
    vcovVent[i,j,,] = vcov(mod)[1:3,1:3]
    bicVent = c(bicVent,BIC(mod))
    
    mod = coxph(Surv(tdeath,death)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
    parsDeath = cbind(parsDeath,coef(mod))
    vcovDeath[i,j,,] = vcov(mod)[1:3,1:3]
    bicDeath = c(bicDeath,BIC(mod))

    print(c(i,j))
  }
}


tsUnadjPars = list(parsHosp,parsSymptHosp,parsIcu,parsVent,parsDeath)
tsUnadjVcov = list(vcovHosp,vcovSymptHosp,vcovIcu,vcovVent,vcovDeath)
tsUnadjBic = list(bicHosp,bicSymptHosp,bicIcu,bicVent,bicDeath)


#### models with one changepoint

parsHosp = parsSymptHosp = parsIcu = parsVent = parsDeath = c()
bicHosp = bicSymptHosp = bicIcu = bicVent = bicDeath = c()
vcovHosp = vcovSymptHosp = vcovIcu = vcovVent = vcovDeath = array(NA,dim=c(length(dates1),2,2))

for (i in 1:length(dates1)){
  
  t1 = testDate-dates1[i]; t1[t1<=0] = 0
  
  mod = coxph(Surv(thosp,hosp)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsHosp = cbind(parsHosp,coef(mod))
  vcovHosp[i,,] = vcov(mod)[1:2,1:2]
  bicHosp = c(bicHosp,BIC(mod))
  
  mod = coxph(Surv(thosp,symptHosp)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsSymptHosp = cbind(parsSymptHosp,coef(mod))
  vcovSymptHosp[i,,] = vcov(mod)[1:2,1:2]
  bicSymptHosp = c(bicSymptHosp,BIC(mod))
  
  mod = coxph(Surv(ticu,icu)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
  parsIcu = cbind(parsIcu,coef(mod))
  vcovIcu[i,,] = vcov(mod)[1:2,1:2]
  bicIcu = c(bicIcu,BIC(mod))
  
  mod = coxph(Surv(tvent,vent)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
  parsVent = cbind(parsVent,coef(mod))
  vcovVent[i,,] = vcov(mod)[1:2,1:2]
  bicVent = c(bicVent,BIC(mod))
  
  mod = coxph(Surv(tdeath,death)~testDate+t1+t2,subset=(thosp>=1&testDate<=76))
  parsDeath = cbind(parsDeath,coef(mod))
  vcovDeath[i,,] = vcov(mod)[1:2,1:2]
  bicDeath = c(bicDeath,BIC(mod))
  
  print(i)
}

tsUnadjPars1cp = list(parsHosp,parsSymptHosp,parsIcu,parsVent,parsDeath)
tsUnadjVcov1cp = list(vcovHosp,vcovSymptHosp,vcovIcu,vcovVent,vcovDeath)
tsUnadjBic1cp = list(bicHosp,bicSymptHosp,bicIcu,bicVent,bicDeath)


######## adjusted models with one and two changepoints
parsHosp = parsSymptHosp = parsIcu = parsVent = parsDeath = c()
bicHosp = bicSymptHosp = bicIcu = bicVent = bicDeath = c()
vcovHosp = vcovSymptHosp = vcovIcu = vcovVent = vcovDeath = array(NA,dim=c(length(dates1),length(dates2),3,3))
for (i in 1:length(dates1)){
  for (j in (which(dates2>=(dates1[i]+7)))){
    
    cps = c(cps,paste(dates1[i],dates2[j],sep='-'))
    cp1 = c(cp1,dates1[i])
    cp2 = c(cp2,dates2[j])
    is = c(is,i)
    js = c(js,j)
    
    t1 = testDate-dates1[i]; t1[t1<=0] = 0
    t2 = testDate-dates2[j]; t2[t2<=0] = 0
    
    mod = coxph(Surv(thosp,hosp)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
    parsHosp = cbind(parsHosp,coef(mod))
    vcovHosp[i,j,,] = vcov(mod)[1:3,1:3]
    bicHosp = c(bicHosp,BIC(mod))
    
    mod = coxph(Surv(thosp,symptHosp)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
    parsSymptHosp = cbind(parsSymptHosp,coef(mod))
    vcovSymptHosp[i,j,,] = vcov(mod)[1:3,1:3]
    bicSymptHosp = c(bicSymptHosp,BIC(mod))
    
    mod = coxph(Surv(ticu,icu)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
    parsIcu = cbind(parsIcu,coef(mod))
    vcovIcu[i,j,,] = vcov(mod)[1:3,1:3]
    bicIcu = c(bicIcu,BIC(mod))
    
    mod = coxph(Surv(tvent,vent)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
    parsVent = cbind(parsVent,coef(mod))
    vcovVent[i,j,,] = vcov(mod)[1:3,1:3]
    bicVent = c(bicVent,BIC(mod))
    
    mod = coxph(Surv(tdeath,death)~testDate+t1+t2+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
    parsDeath = cbind(parsDeath,coef(mod))
    vcovDeath[i,j,,] = vcov(mod)[1:3,1:3]
    bicDeath = c(bicDeath,BIC(mod))
    
    print(c(i,j))
  }
}
tsAdjPars = list(parsHosp,parsSymptHosp,parsIcu,parsVent,parsDeath)
tsAdjVcov = list(vcovHosp,vcovSymptHosp,vcovIcu,vcovVent,vcovDeath)
tsAdjBic = list(bicHosp,bicSymptHosp,bicIcu,bicVent,bicDeath)


parsHosp = parsSymptHosp = parsIcu = parsVent = parsDeath = c()
bicHosp = bicSymptHosp = bicIcu = bicVent = bicDeath = c()
vcovHosp = vcovSymptHosp = vcovIcu = vcovVent = vcovDeath = array(NA,dim=c(length(dates1),2,2))
for (i in 1:length(dates1)){

  t1 = testDate-dates1[i]; t1[t1<=0] = 0
  
  mod = coxph(Surv(thosp,hosp)~testDate+t1+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsHosp = cbind(parsHosp,coef(mod))
  vcovHosp[i,,] = vcov(mod)[1:2,1:2]
  bicHosp = c(bicHosp,BIC(mod))
  
  mod = coxph(Surv(thosp,symptHosp)~testDate+t1+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsSymptHosp = cbind(parsSymptHosp,coef(mod))
  vcovSymptHosp[i,,] = vcov(mod)[1:2,1:2]
  bicSymptHosp = c(bicSymptHosp,BIC(mod))
  
  mod = coxph(Surv(ticu,icu)~testDate+t1+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsIcu = cbind(parsIcu,coef(mod))
  vcovIcu[i,,] = vcov(mod)[1:2,1:2]
  bicIcu = c(bicIcu,BIC(mod))
  
  mod = coxph(Surv(tvent,vent)~testDate+t1+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsVent = cbind(parsVent,coef(mod))
  vcovVent[i,,] = vcov(mod)[1:2,1:2]
  bicVent = c(bicVent,BIC(mod))
  
  mod = coxph(Surv(tdeath,death)~testDate+t1+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
  parsDeath = cbind(parsDeath,coef(mod))
  vcovDeath[i,,] = vcov(mod)[1:2,1:2]
  bicDeath = c(bicDeath,BIC(mod))
  
  print(i)
}
tsAdjPars1cp = list(parsHosp,parsSymptHosp,parsIcu,parsVent,parsDeath)
tsAdjVcov1cp = list(vcovHosp,vcovSymptHosp,vcovIcu,vcovVent,vcovDeath)
tsAdjBic1cp = list(bicHosp,bicSymptHosp,bicIcu,bicVent,bicDeath)



#########################################################
#########################################################

### objects for era figure
######## bootstrap pr(outcome), t(outcome)
pHosp = pSymptHosp = pIcu = pVent = pDeath = pSympt = tSymptTest = tTestHosp = pIcuHosp = pDeathHosp = array(NA,dim=c(1e3,76))
for (i in 1:1e3){
  samps = sample(which(testDate<=76&thosp>=1),length(which(testDate<=76&thosp>=1)),replace=T)
  sampsHospTest = sample(which(testDate<=76&hosp==1&thosp>=1),length(which(testDate<=76&hosp==1&thosp>=1)),replace=T)
  sampsHosp = sample(which(testDate<=76&hosp==1),length(which(testDate<=76&hosp==1)),replace=T)
  for (t in 1:76){
    sel = which(testDate[samps]==t)
    pHosp[i,t] = mean(hosp[samps][sel])
    pSymptHosp[i,t] = mean(symptHosp[samps][sel])
    pIcu[i,t] = mean(icu[samps][sel])
    pVent[i,t] = mean(vent[samps][sel])
    pDeath[i,t] = mean(death[samps][sel])
    
    pSympt[i,t] = mean(sympt[samps][sel]==1&tsympt[samps][sel]<=0)
    
    sel = which(testDate[samps]==t&sympt[samps]==1&tsympt[samps]<=0)
    tSymptTest[i,t] = mean(tsympt[samps][sel])
    
    sel = which(testDate[sampsHospTest]==t)
    tTestHosp[i,t] = mean(thosp[sampsHospTest][sel])
    print(c(i,t))
  }
}

thresh = 13
pHosp7 = pSymptHosp7 = pIcu7 = pVent7 = pDeath7 = pSympt7 = tSymptTest7 = tTestHosp7 = pIcuHosp7 = pDeathHosp7 = rep(NA,76)
for (i in 4:73){
  sel = which(testDate%in%(i+c(-3:3))&thosp>=1)
  pHosp7[i] = mean(hosp[sel],na.rm=T); if (sum(hosp[sel],na.rm=T)<=thresh){pHosp7[i] = NA}
  pSymptHosp7[i] = mean(symptHosp[sel],na.rm=T); if (sum(symptHosp[sel],na.rm=T)<=thresh){pSymptHosp7[i] = NA}
  pIcu7[i] = mean(icu[sel],na.rm=T); if (sum(icu[sel],na.rm=T)<=thresh){pIcu7[i] = NA}
  pVent7[i] = mean(vent[sel],na.rm=T); if (sum(vent[sel],na.rm=T)<=thresh){pVent7[i] = NA}
  pDeath7[i] = mean(death[sel],na.rm=T); if (sum(death[sel],na.rm=T)<=thresh){pDeath7[i] = NA}
  
  pSympt7[i] = mean(sympt[sel]==1&tsympt[sel]<=0,na.rm=T)
  
  sel = which(testDate%in%(i+c(-3:3))&sympt==1&tsympt<=0)
  tSymptTest7[i] = mean(tsympt[sel],na.rm=T)
  
  sel = which(testDate%in%(i+c(-3:3))&hosp==1&thosp>=1)
  tTestHosp7[i] = mean(thosp[sel],na.rm=T)
  
}




#####################################################################################################
#####################################################################################################
##### applying weights to model estimates from slope models and for change point probabilities ###### 
#####################################################################################################
#####################################################################################################


mod0cpHosp = coxph(Surv(thosp,hosp)~testDate+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
mod0cpSymptHosp = coxph(Surv(thosp,symptHosp)~testDate+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
mod0cpIcu = coxph(Surv(ticu,icu)~testDate+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))
mod0cpDeath = coxph(Surv(tdeath,death)~testDate+ed+inpt+outpt+sex+smoke+race+as.factor(chargrp)+as.factor(bmiClassA)+as.factor(agegrp)+logInc+vaxStatTime+priorCovid,subset=(thosp>=1&testDate<=76))

log10(exp(-(BIC(mod0cpDeath) - min(tsAdjBic[[5]]))/2))

wtAdj = wtUnadj = wtAdj1cp = wtUnadj1cp = list()
for (i in 1:5){
  wtAdj[[i]] = exp(-(tsAdjBic[[i]]-min(tsAdjBic[[i]]))/2)
  wtAdj1cp[[i]] = exp(-(tsAdjBic1cp[[i]]-min(tsAdjBic1cp[[i]]))/2)
  wtUnadj[[i]] = exp(-(tsUnadjBic[[i]]-min(tsUnadjBic[[i]]))/2)
  wtUnadj1cp[[i]] = exp(-(tsUnadjBic1cp[[i]]-min(tsUnadjBic1cp[[i]]))/2)
}


set.seed(1)
sampsAdj = sampsUnadj = sampsAdj1cp = sampsUnadj1cp = list()
for (i in 1:5){
  sampsAdj[[i]] = sample(1:length(wtAdj[[i]]),1e4,replace=T,prob=wtAdj[[i]])
  sampsAdj1cp[[i]] = sample(1:length(wtAdj1cp[[i]]),1e4,replace=T,prob=wtAdj1cp[[i]])
  sampsUnadj[[i]] = sample(1:length(wtUnadj[[i]]),1e4,replace=T,prob=wtUnadj[[i]])
  sampsUnadj1cp[[i]] = sample(1:length(wtUnadj1cp[[i]]),1e4,replace=T,prob=wtUnadj1cp[[i]])
}


cpAdj = cpUnadj = cpAdj1cp = cpUnadj1cp = list()
for (i in 1:5){
  cpAdj[[i]] = cbind(dates1[is[sampsAdj[[i]]]],dates2[js[sampsAdj[[i]]]])
  cpUnadj[[i]] = cbind(dates1[is[sampsUnadj[[i]]]],dates2[js[sampsUnadj[[i]]]])
  
  cpAdj1cp[[i]] = dates1[sampsAdj1cp[[i]]]
  cpUnadj1cp[[i]] = dates1[sampsUnadj1cp[[i]]]
}


##### pr (changepoints) accounting for uniform prob of 1 vs 2 changepoints
prAdj = prUnadj = array(NA,dim=c(5,76))
for (i in 1:5){
  for (t in 1:76){
    prAdj[i,t] = sum(wtAdj[[i]][which(dates1[is]==t|dates2[js]==t)],na.rm=T)/sum(dates1[is]==t|dates2[js]==t) + sum(wtAdj1cp[[i]][which(dates1==t)],na.rm=T)
    prUnadj[i,t] = sum(wtUnadj[[i]][which(dates1[is]==t|dates2[js]==t)],na.rm=T)/sum(dates1[is]==t|dates2[js]==t) + sum(wtUnadj1cp[[i]][which(dates1==t)],na.rm=T)
  }  
}

####### make fitted model outputs for each model
####### then select among them accourding to respective model weights with 1 or 2 changepoints
set.seed(1)
outAdj = outUnadj = outAdj1cp = outUnadj1cp = list()
for (i in 1:5){
  outAdj[[i]] = outUnadj[[i]] = outAdj1cp[[i]] = outUnadj1cp[[i]] = array(NA,dim=c(1e4,77))
  for (j in 1:1e4){
    selI = is[sampsAdj[[i]][j]]; selJ = js[sampsAdj[[i]][j]]
    par = mvrnorm(1,tsAdjPars[[i]][1:3,sampsAdj[[i]][j]],tsAdjVcov[[i]][selI,selJ,1:3,1:3]) 
    date0 = 0:76
    date1 = 0:76 - dates1[selI]; date1[date1<0] = 0
    date2 = 0:76 - dates2[selJ]; date2[date2<0] = 0
    outAdj[[i]][j,] = exp(par[1]*date0+par[2]*date1+par[3]*date2)
    
    selI = is[sampsUnadj[[i]][j]]; selJ = js[sampsUnadj[[i]][j]]
    par = mvrnorm(1,tsUnadjPars[[i]][1:3,sampsUnadj[[i]][j]],tsUnadjVcov[[i]][selI,selJ,1:3,1:3]) 
    date0 = 0:76
    date1 = 0:76 - dates1[selI]; date1[date1<0] = 0
    date2 = 0:76 - dates2[selJ]; date2[date2<0] = 0
    outUnadj[[i]][j,] = exp(par[1]*date0+par[2]*date1+par[3]*date2)
    
    selI = sampsAdj1cp[[i]][j]
    par = mvrnorm(1,tsAdjPars1cp[[i]][1:2,sampsAdj1cp[[i]][j]],tsAdjVcov1cp[[i]][selI,1:2,1:2]) 
    date0 = 0:76
    date1 = 0:76 - dates1[selI]; date1[date1<0] = 0
    outAdj1cp[[i]][j,] = exp(par[1]*date0+par[2]*date1)
    
    selI = sampsUnadj1cp[[i]][j]
    par = mvrnorm(1,tsUnadjPars1cp[[i]][1:2,sampsUnadj1cp[[i]][j]],tsUnadjVcov1cp[[i]][selI,1:2,1:2]) 
    date0 = 0:76
    date1 = 0:76 - dates1[selI]; date1[date1<0] = 0
    outUnadj1cp[[i]][j,] = exp(par[1]*date0+par[2]*date1)
    
    print(c(i,j))
  }
}


###################### sampling across 1cp and 2cp models
set.seed(1)
mergedAdj = mergedUnadj = list()
for (i in 1:5){

  sumWt1cp = sum(wtAdj1cp[[i]])
  sumWt2cp = sum(wtAdj[[i]])*length(wtAdj1cp[[i]])/length(wtAdj[[i]])
  draw = sample(c(1,2),1e4,prob=c(sumWt1cp,sumWt2cp),replace=T)
  mergedAdj[[i]] = array(NA,dim=c(1e4,77))
  mergedAdj[[i]][which(draw==1),] = outAdj1cp[[i]][which(draw==1),]
  mergedAdj[[i]][which(draw==2),] = outAdj[[i]][which(draw==2),]
  
  sumWt1cp = sum(wtUnadj1cp[[i]])
  sumWt2cp = sum(wtUnadj[[i]])*length(wtUnadj1cp[[i]])/length(wtUnadj[[i]])
  draw = sample(c(1,2),1e4,prob=c(sumWt1cp,sumWt2cp),replace=T)
  mergedUnadj[[i]] = array(NA,dim=c(1e4,77))
  mergedUnadj[[i]][which(draw==1),] = outUnadj1cp[[i]][which(draw==1),]
  mergedUnadj[[i]][which(draw==2),] = outUnadj[[i]][which(draw==2),]
  
}



############################################################################################
############################################################################################
######## cox model for time to end of hospital stay ########################################
############################################################################################
############################################################################################

for (i in 1:5){
  impDat[[i]]$agegrpAlt = NA
  impDat[[i]]$agegrpAlt[impDat[[i]]$agegrp%in%1:4] = 1
  impDat[[i]]$agegrpAlt[impDat[[i]]$agegrp%in%5:6] = 2
  impDat[[i]]$agegrpAlt[impDat[[i]]$agegrp%in%7:8] = 3
  impDat[[i]]$agegrpAlt[impDat[[i]]$agegrp%in%9:10] = 4
  impDat[[i]]$agegrpAlt[impDat[[i]]$agegrp%in%11] = 5
}

dischOutcome = dischDispo; dischOutcome[dischDispo!=0] = 1
modUnadj = modAdj = modAdj1 = list()
for (i in 1:5){
  modUnadj[[i]] = coxph(Surv(tdisch,dischOutcome)~sgtf+strata(hospDate),
                 subset=(symptHosp==1&tf==1&thosp>=1&testDate%in%daterange[[1]]&hospDate%in%daterange[[1]]),data=impDat[[i]]) 
  modAdj[[i]] = coxph(Surv(tdisch,dischOutcome)~sgtf+as.factor(ed!='0')+as.factor(inpt!='0')+outpt+sex+smoke+race+as.factor(chargrp!='0')+as.factor(bmiClassA)+as.factor(agegrpAlt)+logInc+as.factor(vaxStatSimp!='0')+priorCovid+strata(hospDate),
              subset=(symptHosp==1&tf==1&thosp>=1&testDate%in%daterange[[1]]&hospDate%in%daterange[[1]]),data=impDat[[i]])
  modAdj1[[i]] = coxph(Surv(tdisch,dischOutcome)~sgtf*as.factor(vaxStat!='0')+as.factor(ed!='0')+as.factor(inpt!='0')+outpt+sex+smoke+race+as.factor(chargrp!='0')+as.factor(bmiClassA)+as.factor(agegrpAlt)+logInc+priorCovid+strata(hospDate),
                      subset=(symptHosp==1&tf==1&thosp>=1&testDate%in%daterange[[1]]&hospDate%in%daterange[[1]]),data=impDat[[i]])
}







\