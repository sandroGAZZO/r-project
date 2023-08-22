library(fitdistrplus)
# on a que une covariable pour voir comment ça se passe dans cas simple
setwd("C:/Users/Sandro/Documents/R/Biostat/TP_biostat")
load(file="FlowDataAnalyse.RData", .GlobalEnv) # Lire les donnees
head(data) # Premieres lignes
str(data) # Structure de donnéees
summary(data)

#Durée d'observation pour chaque patiente
dur_obs = (data$End_obs-data$Start_obs)

# Durée d'observation en heures

dur_obs = as.numeric((data$End_obs-data$Start_obs))/(60*60)

# Distribution empirique de la durée d'observation
#hist(dur_obs,probability=TRUE,col="blue")
#ggplot(data=data, aes(x=dur_obs))+geom_density()

# Durée d'accouchement pour chaque patiente

dur_acc=(data$End_lab-data$Start_lab)

# Durée d'accouchement en heures 
dur_acc=as.numeric((data$End_lab-data$Start_lab))/(60*60)
                   
# Analyse de "séjour" en observation                 
hist(dur_obs,probability=TRUE,col="blue")                  

ff_exp=fitdist(data = dur_obs, distr="exp") # Ajustement de loi exponentielle
ff_exp

ff_weib=fitdist(data = dur_obs, distr="weibull") # Ajustement de loi de Weibull
ff_weib

# Calcul des moyennes

rate=ff_exp$estimate  # temps moyen d'arrivée devenement pour loi exponentielle 
shape=ff_weib$estimate[1]
scale=ff_weib$estimate[2]

# Moyenne observee # temps moyen dans la salle d'observation
mean_obs = mean(dur_obs)
# Esperance de la loi exponentielle
mean_exp=1/rate

# Esperance de Weibull
scale*gamma(1+shape)

# Mediane observee # la moitié des patientes sortent au bout de la médiane en heures.
med_dur=median(dur_obs)
# Mediane de loi exponentielle
med_exp=log(2)/rate
# Mediane de Weibull
med_weib=scale*log(2)^(1/shape)

# Remarque la médiane est plus petite que la moyenne car la moyenne est influencé par les grandes valeurs
# alors que la médiane se calcule par les premieres valeurs donc elle est 
# plus robuste aux valeurs extremes et aux asymétries

# Ajustement du modele paramétrique  
# cf p.70 pour explications
# catégorie de référence est -25ans

cox.par.dur.obs = survreg(Surv(dur_obs)~ data$Age, dist="weibull")
summary(cox.par.dur.obs)

# calcul des paramètres de weibull par survregx
scale=exp(cox.par.dur.obs$coefficients[1]) # scale estim
shape=1/cox.par.dur.obs$scale # shape estim

# ou avec une meilleur fonction plus flexible avec directement le calcul des parametres
# et qui donne de plus les Risque Relatif RR exp(est)

library(flexsurv)
cox.par.dur.obs.flex = flexsurvreg(Surv(dur_obs)~ data$Age, dist="weibull")
cox.par.dur.obs.flex


data$dur_acc= dur_acc
ggplot(data=data, aes(x=dur_acc,fill=Age))+geom_density() # aes cest notre variables x variable d'étude
# fill covariable
# plus l'age augmente plus la durée d'accouchement est courte
# on va modéliser cette dépendance par un modèle de cox semi-parametrique 

cox.npar.dur_acc = coxph(Surv(dur_acc, Ces_yn)~Age, data=data)
summary(cox.npar.dur_acc)

# Fonction survfit estimation de S(t) # fonction de survie avec accouchement comme evenement
plot(survfit(Surv(dur_acc, Ces_yn)~Age, data=data),
     col=c("red", "blue", "black"))

