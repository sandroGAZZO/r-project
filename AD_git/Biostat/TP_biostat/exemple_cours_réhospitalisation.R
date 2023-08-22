library(fitdistrplus)
library(ggplot2)
setwd("C:/Users/Sandro/Documents/R/Biostat/TP_biostat")
load(file="readmission.RData", .GlobalEnv) # Lire les donnees
head(readmission) # Premieres lignes
str(readmission) # Structure de donnees
par(mfrow=c(1,1))

# MODELE AG
# on a ici des intervalles de temps correspondant à chaque evenement contrairement au modele de Cox
# où c'etait un ti par evemenement.

ag.semipar = coxph(Surv(t.start, t.stop, event) ~ chemo + sex+ dukes+cluster(id), data=readmission)

summary(ag.semipar) # pour interpréter les RR il faut connaitre la référence 

# Estimation de survie S(t)
plot(survfit(Surv(t.start, t.stop, event) ~ chemo+cluster(id) ,
             data=readmission),col=c("red", "blue"))
# Durée entre les observations 

ggplot(data=readmission, aes(x=time,fill=as.factor(enum)))+geom_density() # on voit pas grand chose 
# trop de enum, on fait un subset pour prendre que les premiers 
ggplot(data=subset(readmission, enum<=4), aes(x=time,fill=as.factor(enum)))+geom_density(alpha=0.5)
# alpha parametre de transparence

# Modele PWP - GT sans interaction
pwp = coxph(Surv(t.start, t.stop, event) ~ chemo + sex+ dukes+cluster(id)+strata(enum), data=readmission)
summary(pwp)
# interprétaion etre une femme ou etre traité le coeff associé est négatif -> exp < 1 où
# exp(coeff)= RR=Rfemme/Rhomme (homme référence)
# facteur protecteur les autres facteurs de risque
# estimation de S(t)

plot(survfit(pwp)[1:3], col=c("red", "blue", "green"))
legend(x=1500,y=1, legend=c("1 ere réhospistalisation", "2 eme rehospitalisation","3 eme rehospitalisation"),col=c("red", "blue","green"), lty=1, cex=0.8)
# mediane avec abline
#On considere que les 5 re-hospitalisations
readmission1=subset(readmission,enum<=5)
pwp1 = coxph(Surv(t.start, t.stop, event)~dukes*strata(enum)+cluster(id)+strata(enum),data=readmission1)
summary(pwp1)

# Modele avec fragilite

frailty=coxph(Surv(t.start, t.stop, event)~ chemo + sex + dukes + frailty(id),
                data=readmission)
summary(frailty)
# Modele sans fragilite
ag=coxph(Surv(t.start, t.stop, event)~chemo + sex + dukes + cluster(id),data=readmission)
summary(ag)
# on enleve la variable fragilite donc le coeff sex est surestimé pour compenser d'ou la difference.

#Analyse des residus martingale
# Residus du modele Anderson-Gill
res_ag = resid(ag.semipar, type="martingale")
plot(res_ag,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")
# on voit une tendance 
#ggcoxdiagnostics(res_ag,type="martingale")

# Residus du modele de fragilite
res_fr = resid(frailty,
               type="martingale")
plot(res_fr,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")

# Residus du modele PWP
# (avec interactions)
res_pwp_int = resid(pwp1,type="martingale")
plot(res_pwp_int,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")

# Residus du modele PWP
# (sans interactions)
res_pwp = resid(pwp,type="martingale")
plot(res_pwp,cex=0.7,col="darkgray",pch=19)
lines(lowess(res_ag),col='red')
abline(h=0,col="blue")

# Analyse residu  test avec hypothese nulle coeff est constant dans le temps
# Test
cox.zph(frailty)

# Graphique
plot(cox.zph(frailty))
# courbe noir est le coeff estimé, si constant alors pas de tendance, 
# si pas constant dans le temps tendance 

# on a un pb pour dukesD il se comporte pas comme les autres, on pourrait penser 
# a faire un notre modele pour cette variable
# le modele se regle pour le modele avec interaction car on traite la variable séparement
# avec modele stratifié
# mais ce modele est compliqué (bcp de param à estimer)

