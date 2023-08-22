## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, results="hide")
library(survival)
library(survminer)
library(fitdistrplus)
#setwd("C:/Users/Yaya5/Desktop/M2/Biostatistique/Analyse survie/Cours 2")


## ---- results="hide"-------------------------------------------------------
data(lung)
?lung
str(lung)
lung$sex=factor(lung$sex,  labels=c("m", "f"))


## ---- result="hide"--------------------------------------------------------
model1=coxph(Surv(time, status)~age+sex+ph.ecog+ph.karno+meal.cal+wt.loss, data=lung)
summary(model1)


## ---- result="hide"--------------------------------------------------------
lung1=lung[complete.cases(lung),]
model2=step(coxph(Surv(time, status)~age+sex+ph.ecog+ph.karno+meal.cal+wt.loss, data=lung1))
summary(model2)


## ---- result="hide", eval=FALSE--------------------------------------------
model2.sex = survfit(Surv(time, status)~sex, data=lung1)
ggsurvplot(model2.sex,data = lung1)
plot(model2.sex)


## --------------------------------------------------------------------------
res.schoenf=residuals(model2, type="schoenfeld")
test.prop.hasard = cox.zph(model2)
model2.sex = survfit(Surv(time, status)~sex, data=lung1)


## ---- eval=FALSE-----------------------------------------------------------
S_f=survfit(Surv(time, status)~1, data=lung1[lung1$sex=="f",])
S_m=survfit(Surv(time, status)~1, data=lung1[lung1$sex=="m",])
plot(log(S_f$time), log(-log(S_f$surv)), type="l", col="red")
lines(log(S_m$time), log(-log(S_m$surv)), type="l", col="blue")
plot(survfit(Surv(time, status)~sex, data=lung1), fun="cloglog")


## ---- result="hide", eval=FALSE--------------------------------------------
res.deviance=residuals(model2, type="deviance")
plot(res.deviance)
text(1:nrow(lung1),residuals(model2,type="deviance"),names(residuals(model2,type="deviance"))) # pôur savoir les numéros 
# des individus pour savoir ceux qui ont un mauvais résidus et on regarde la ligne comme ça:
lung1[rownames(lung1)=='37',]
res.dfbetas=residuals(model2, type="dfbetas") # résidus par individu par variable
plot(res.dfbetas[,1])
text(res.dfbetas[,1],rownames(res.dfbetas[,1]))
ggcoxdiagnostics(model2, type="deviance") # 


## ---- eval=FALSE-----------------------------------------------------------
res.marting = residuals(model2, type="martingale")
par(mfrow=c(2,2))
# residus de martingale par rapport aux differentes variables
plot(lung1$age, res.marting)
lines(lowess(lung1$age, res.marting), col="red")
abline(h=0,col="blue")
plot(lung1$ph.karno, res.marting)
lines(lowess(lung1$ph.karno, res.marting), col="red")
abline(h=0,col="blue") # au debut on a une tendance -> moyenne des res pas 
# nulle -> faudrait modele stratifié
plot(lung1$ph.ecog, res.marting)
lines(lowess(lung1$ph.ecog, res.marting), col="red")
abline(h=0,col="blue") # cest pareil à la fin maisi ici cest pas des 
plot(lung1$wt.loss, res.marting)
lines(lowess(lung1$wt.loss, res.marting), col="red")
abline(h=0,col="blue")

# Choix de la forme de lien possible :
ggcoxfunctional(Surv(time, status)~age+log(age)+age^2, data=lung1)
ggcoxfunctional(Surv(time, status)~ph.karno, data=lung1)
ggcoxfunctional(Surv(time, status)~ph.ecog, data=lung1)



