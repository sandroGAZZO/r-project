setwd("C:/Users/Sandro/Documents/R/Biostat/TP_biostat")



load("divorce.RData")
head(divorce)
summary(divorce)
str(divorce)


### premiere analyse:
# On a une variable id, concernant les couples de l'etude selectionnes certainement.
# On a heduc qui correspond a la duree du mariage pour l'homme avant fin de l'etude ou divorce
# on distingue 3 types de heduc: inferieur a 12, entre 12 et 15, superieur a 16 ans de mariage.
# On a la variable hebalck correspondant a la couleur de peau de l'homme marie: 
# s'il est non blanc ou autre.
# On a la variable mixed qui nous dit si le couple est de la meme ethnie ou non.
# On a la variable years qui correspond aux nombres d'annees de mariage .
# On a la variable div qui correspond au fait de s'il y a eu divorce ou non (1 divorce, 0 pas divorce).

## On va s'intéresser a qui divorce, donc aux evenements divorce


# MODELE AG
# on a ici des intervalles de temps correspondant à chaque evenement contrairement au modele de Cox
# où c'est un ti par evemenement.

ag.semipar = coxph(Surv(div) ~ heduc + years +mixed + heblack +cluster(id), data=divorce)

summary(ag.semipar) # pour interpréter les RR il faut connaitre la référence 

## on a 1.07 fois plus de risque de divorcer entre 12 et 15 ans qu'en moins de 12 ans
# on a 1.16 fois plus de risque de divorcer en plus de 16 ans qu'en moins de 12 ans
# Plus on est age, plus on a de chance de divorcer (1.02 fois plus par an)
# venir d'une ethnie differente obtient un risque 1.11 fois moins que si l'on vient de la meme ethnie
# si l'homme est noir, il y a 1.11 fois plus de risque de divorce que si l'on est autre

plot(survfit(Surv(years) ~  div +cluster(id), data=divorce)
     ,col=c("red", "blue"))

# la moitie des couples qui divorcent le font avant 10 ans
# la moitie des couples qui n'ont pas divorce ont deja 20 ans de mariage
library(ggplot2)
ggplot(data=divorce, aes(x=years,fill=as.factor(div)))+geom_density() # on voit pas grand chose 

# on a beaucoup de divorce lorsqu'on est jeune alors que plus l'on veillit, moins il y a de divorce

## MODELE PWP GT
pwp = coxph(Surv(div) ~ mixed + years +heduc + strata(heblack) +cluster(id), data=divorce)
summary(pwp)

# nous obtenons sensiblement la meme chose qu'avant


## MODELE Fragilite
frailty=coxph(Surv(div) ~ heduc + years +mixed + heblack +frailty(id), data=divorce)
summary(frailty)
# nous avons encore une fois des résultats sensiblement identiques


#Analyse des residus martingale
# Residus du modele Anderson-Gill
res_ag = resid(ag.semipar, type="martingale")
plot(res_ag,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")
# les résidus ne sont pas centrés en 0, ce n'est pas un bon modèle



# Residus du modele de fragilite
res_fr = resid(frailty,
               type="martingale")
plot(res_fr,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")
# meme constat


# Residus du modele PWP
# (sans interactions)
res_pwp = resid(pwp,type="martingale")
plot(res_pwp,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")
# meme constat

## on peut voir que l'on a des donnees sensiblement identique et des residus
# qui ont la meme forme, et qui nous disent que l'on a de mauvais modèles
# on doit donc certainement faire une autre etude, une autre analyse car il y a une erreur
# soit il faut proceder d'une autre facon, soit nous avons fait une erreur dans les parametres
# et les choses a estimer


# Analyse residu  test avec hypothese nulle coeff est constant dans le temps
# Test
cox.zph(frailty)

# Graphique
plot(cox.zph(frailty))

## on a un bon residu sur age, et un qui pourrait s'ameliorer pour mixed
# essayons avec ces deux varaiables
frailty=coxph(Surv(div) ~ years +mixed  +frailty(id), data=divorce)
summary(frailty)
# nous avons encore une fois des résultats sensiblement identiques
cox.zph(frailty)
# mixed a un trop grand residu



frailty=coxph(Surv(div) ~ years +frailty(id), data=divorce)
summary(frailty)
cox.zph(frailty)
