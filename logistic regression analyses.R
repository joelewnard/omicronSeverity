

modSgtf1 = modSgtf1En = modSgtf2 = list()
for (i in 1:5){
  modSgtf1[[i]] = glm(sgtf~agegrp+sex+race+incCat+smoke+bmiClassA+outpt+ed+inpt+chargrp+
                       as.factor(testDate)+priorCovid*vaxStatTime,data=impDat[[i]],family='binomial',subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]))
  
  modSgtf1En[[i]] = glm(sgtf~agegrp+sex+race+incCat+smoke+bmiClassA+outpt+ed+inpt+chargrp+
                        as.factor(testDate)+priorCovid+vaxStatTime,data=impDat[[i]],family='binomial',subset=(thosp>=1&tf==1&testDate%in%daterange[[1]]&enroll==1))
  
  modSgtf2[[i]] = glm(sgtf~agegrp+sex+race+incCat+smoke+bmiClassA+outpt+ed+inpt+chargrp+
                        as.factor(testDate)+priorCovid*vaxStatTime,data=impDat[[i]],family='binomial',subset=(thosp>=1&tf==1&testDate%in%daterange[[2]]))

  print(i)
}


