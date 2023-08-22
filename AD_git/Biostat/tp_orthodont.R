
#Q1
library(nlme)

data("Orthodont")

?Orthodont
summary(Orthodont)
str(Orthodont)
plot(Orthodont)

#Q2?
plot(Orthodont,outer=1)
plot(Orthodont,outer=~Sex)

#Q3
fit.lme<- lme(distance~age,data=Orthodont,random=~age)

fit.lme
summary(fit.lme)
plot(fit.lme)

(0.7752460/16.761111)*100

#Q4
fit.lme2<- lme(distance~age,data=Orthodont,random=~age,method="ML")
#ou
fit.lme2<- update(fit.lme,method="ML")

fit.lme2
summary(fit.lme2)
plot(fit.lme2)

#Q5
plot(Orthodont,outer=~Sex)

fit.sex<- lme(distance~age*Sex,data=Orthodont,random=~age,method="ML")
#ou
# fit.sex<-update(fit.lme2,fixed=distance*Sex)

summary(fit.sex)

anova(fit.lme2,fit.sex)

#Q6
fit.mat<- lme(distance~age*Sex,data=Orthodont,random=pdDiag(~age),method="ML")
summary(fit.mat)
anova(fit.sex,fit.mat)

#Q7
fit1<-update(fit.mat,weights=varPower(fixed=list(power=1)))
fit2<-update(fit.mat,weights=varConstPower(fixed = list(power=1)))
summary(fit1)
summary(fit2)
anova(fit1,fit2)


resid(fit.lme,level=1,type= "pearson")













