library(ggplot2)

diabete<-read.csv("C:/Users/Sandro/Documents/R/Meth_app/diabetes.csv",header=TRUE)

summary(diabete)
str(diabete)


log.diab<-glm(Outcome ~ .,family="binomial", data=diabete)
log.diab

summary(log.diab)


coef.reg <- coef(log.diab) #Pour extraire la colonne "estimate" de
# glm (i.e. Beta chapeau)

ci.reg<-confint(log.diab)

exp(coef.reg) # récupère les odds-ratio
# exemple: le fait d'être enceinte multiplie le
# risque par 1.13

exp(ci.reg) #on cherche les IC qui ne contiennent pas 1

cbind(exp(coef.reg),exp(ci.reg))

# Effet d'une augmentation de l'âge de 10 (ans)
coef.age<- coef.reg["Age"]
coef.age.plus10 <- 10*coef.age
exp(coef.age.plus10)

ci.age.plus10<-10*ci.reg["Age",]
exp(ci.age.plus10)


#print("Odds-R pour l'âge qui augmente de 10 ans :", exp(coef.age.plus10),
      #"Odds-R intervalle de conf à 95%:[",exp(ci.age.plus10)[1], ";",exp(ci.age.plus10)[2]"]")


library("broom")
tidy(log.diab)

library("gtsummary")
tbl_regression(log.diab)

library("forestmodel")
forest_model(log.diab)

library("GGally")
ggcoef(log.diab)

#ggplot(tidy(log.diab))


summary(log.diab)
library(MASS)
aic.diab<-stepAIC(log.diab)
summary(aic.diab)


new_log.diab<-glm(Outcome ~ . -SkinThickness,family="binomial", data=diabete)
summary(new_log.diab)

new_log.diab<-glm(Outcome ~ . -SkinThickness -Insulin,family="binomial", data=diabete)
summary(new_log.diab)


## Nouveau odds-ratio

coef.reg <- coef(new_log.diab) #Pour extraire la colonne "estimate" de
# glm (i.e. Beta chapeau)

ci.reg<-confint(new_log.diab)

exp(coef.reg) # récupère les odds-ratio
# exemple: le fait d'être enceinte multiplie le
# risque par 1.13

exp(ci.reg) #on cherche les IC qui ne contiennent pas 1

cbind(exp(coef.reg),exp(ci.reg))


#print("Odds-R pour l'âge qui augmente de 10 ans :", exp(coef.age.plus10),
#"Odds-R intervalle de conf à 95%:[",exp(ci.age.plus10)[1], ";",exp(ci.age.plus10)[2]"]")


tidy(new_log.diab)


tbl_regression(new_log.diab)


forest_model(new_log.diab)


ggcoef(new_log.diab)

## 1er ggeffect
library("ggeffects")
ggeff<-ggeffect(log.diab)
plot(ggeff)

##2e ggeffect
ggeff<-ggeffect(new_log.diab)
plot(ggeff)

## Roc1
library("gplots")
library("ROCR")
P<-diabete$Pregnancies + diabete$Glucose + diabete$BloodPressure +diabete$SkinThickness
    + diabete$Insulin + diabete$BMI + diabete$DiabetesPedigreeFunction + diabete$Age
Y<-diabete$Outcome
pred<- prediction(P,Y)
perf <- performance(pred, "tpr", "fpr")

plot(perf)




library(pROC)
PROC<-plot.roc(Y,P,main="", percent=TRUE,ci=TRUE)
SE<-ci.se(PROC,specificities=seq(0, 100, 5))
plot(SE, type="shape", col="light blue")

## Roc2

P<-diabete$Pregnancies + diabete$Glucose + diabete$BloodPressure
   + diabete$BMI + diabete$DiabetesPedigreeFunction + diabete$Age
Y<-diabete$Outcome
pred<- prediction(P,Y)
perf <- performance(pred, "tpr", "fpr")

plot(perf)


PROC<-plot.roc(Y,P,main="", percent=TRUE,ci=TRUE)
SE<-ci.se(PROC,specificities=seq(0, 100, 5))
plot(SE, type="shape", col="light blue")




## Exo 2
cmc<- read.table("C:/Users/Sandro/Documents/R/Meth_app/cmc.data", sep=",",header =  T)
str(cmc)

data<-cmc
#en recode les variables qualitatives en facteurs
cmc$education<-as.factor(cmc$education)
cmc$husband_education<-as.factor(cmc$husband_education)
cmc$religion<-as.factor(cmc$religion)
cmc$working<-as.factor(cmc$working)
cmc$husband_occupation<-as.factor(cmc$husband_occupation)
cmc$standard_of_living<-as.factor(cmc$standard_of_living)
cmc$media<-as.factor(cmc$media)
cmc$contraceptive<-as.factor(cmc$contraceptive)
str(cmc)

library("nnet")
# on renomme les niveaux

levels(cmc$contraceptive)<-c("aucune","court-terme","long-terme")
summary(cmc)
str(cmc)

log.cmc<-multinom(contraceptive~., data=cmc)

summary(log.cmc)



log.cmc2<-multinom(as.factor(contraceptive)~., data=data)
summary(log.cmc2)



coef.reg <- coef(log.cmc) #Pour extraire la colonne "estimate" de
# glm (i.e. Beta chapeau)

ci.reg<-confint(log.cmc)

exp(coef.reg) # récupère les odds-ratio
# exemple: le fait d'être enceinte multiplie le
# risque par 1.13

exp(ci.reg) #on cherche les IC qui ne contiennent pas 1

cbind(exp(coef.reg),exp(ci.reg))

tidy(log.cmc)


tbl_regression(log.cmc)

forest_model(log.cmc)


ggcoef(log.cmc)

#ggplot(tidy(log.diab))

ggeff<-ggeffect(log.cmc)
plot(ggeff)


coef.reg <- coef(log.cmc2) #Pour extraire la colonne "estimate" de
# glm (i.e. Beta chapeau)

ci.reg<-confint(log.cmc2)

coef.reg

ci.reg

forest_model(log.cmc2)


predict(log.cmc2,type="probs")
library("gplots")
library("ROCR")
P<-data$age+data$education+data$husband_education+data$nbchildren+data$media
    +data$religion+data$working+data$husband_occupation+data$standard_of_living
Y<-data$contraceptive
#pred<- prediction(P,Y)
#perf <- performance(pred, "tpr", "fpr")
#
#plot(perf, colorize=T)


library(pROC)
PROC<-plot.roc(Y,P,main="", percent=TRUE,ci=TRUE)
SE<-ci.se(PROC,specificities=seq(0, 100, 5))
plot(SE, type="shape", col="light blue")


predict(log.cmc,type="probs")
library("gplots")
library("ROCR")
P<-cmc$age+cmc$education+cmc$husband_education+cmc$nbchildren+cmc$media
+cmc$religion+cmc$working+cmc$husband_occupation+cmc$standard_of_living
Y<-cmc$contraceptive
#pred<- prediction(P,Y)
#perf <- performance(pred, "tpr", "fpr")
#
#plot(perf, colorize=T)


library(pROC)
PROC<-plot.roc(Y,P,main="", percent=TRUE,ci=TRUE)
SE<-ci.se(PROC,specificities=seq(0, 100, 5))
plot(SE, type="shape", col="light blue")

