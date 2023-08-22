rm(list=ls())
graphics.off()

library(fields)
library(spdep)

setwd("C:/Users/Sandro/Documents/R/Stats_spatiales")

traceStrates = function(titre="",couleur='white'){
               if (length(couleur)==1) couleur=rep(couleur,38)
               plot(range(longitude),range(latitude),type="n",xlim=c(-66,-60),ylim=c(45,50),
                    main=titre,xlab="Longitude",ylab="Latitude")
               rect(-66,45,-60,50,col='brown')
               for(i in 401:439){
                   polygon(bornes[which(bornes[,1]==i),2:3],col=couleur[i-400])
               }
}


# Lecture des donn?es
#--------------------

donnees = read.csv(file = "StLaurent.csv",sep=";")
summary(donnees)
attach(donnees)
BH = as.factor(BH)
coords = cbind(longitude,latitude)

bornes = read.table(file = "boundaries38s.dat")
summary(bornes)
bornes = as.matrix(bornes)
summary(bornes)

# Trace de la carte des strates
#------------------------------
traceStrates(titre='Strates')
points(longitude,latitude)
traceStrates(titre='Strates')
text(longitude,latitude,strate,cex=0.5)

# Etude de la corr?lation sur la pr?sence de Bernards-l'hermites
#---------------------------------------------------------------

x11()
titre = "pr?sence de bernards l'hermites "
couleur = rep('white',38)
couleur[BH==1]='lightblue'
traceStrates(titre,couleur)

# systemes de voisinage
#---------------------

titre = 'Centres de strates'
traceStrates(titre)

########par triangulation on calcule les poids de proximite wij

voisinage1 <- tri2nb(coords,row.names=strate) # par triangulation
titre = 'Voisinages par triangulation'
traceStrates(titre)
plot(voisinage1,coords,add=TRUE,col='red')

ppv <- knearneigh(coords, k=4)
voisinage2 = knn2nb(ppv)
titre = 'Voisinages par plus proches voisins'
traceStrates(titre)
plot(voisinage2,coords,add=TRUE,col='red')

voisinage3 <- dnearneigh(coords, d1=0, d2=0.85 )
titre = 'Voisinages par plus petites distances'
traceStrates(titre)
plot(voisinage3,coords,add=TRUE,col='red')

# matrice de voisinage
#---------------------
voisinage = voisinage3
poids.vois <- nb2listw(voisinage,style="W")

#les poids
poids.vois$weights

#les voisins
poids.vois$neighbours

mat.vois <- nb2mat(voisinage,style ="W")
mat.vois

poids.vois.sym = listw2U(poids.vois)  # pour symetriser W (pb avec plus proches voisins)

print(joincount.mc(BH,poids.vois.sym,nsim=100))

print(joincount.test(BH,poids.vois.sym))

# sur l'indice de consommation totale
#------------------------------------
x11() 
titre = paste("Indice de pr?dation")
paletteBleue = colorRampPalette(c("white", "blue"))
couleur = paletteBleue(100)[findInterval(totconsum,seq(min(totconsum),max(totconsum),l=100))]
traceStrates(titre,couleur)
text(longitude,latitude,round(totconsum,2),cex=0.5)

######indice de Moran########

print(moran(totconsum,poids.vois,length(totconsum),Szero(poids.vois)))

print(moran.mc(totconsum,poids.vois,nsim=99))
print(moran.test(totconsum,poids.vois))
print(moran.test(totconsum,poids.vois,randomisation=F))
print(sp.correlogram(voisinage,totconsum,order=5,method="I",zero.policy=T))
plot(sp.correlogram(voisinage,totconsum,order=5,method="I",zero.policy=T))

# Test de Geary
#--------------
print(geary(totconsum,poids.vois,length(totconsum),length(totconsum)-1,Szero(poids.vois)))
print(geary.mc(totconsum,poids.vois,nsim=99))
print(geary.test(totconsum,poids.vois))
print(geary.test(totconsum,poids.vois,randomisation=F))

#====================
# Regression spatiale
#====================
# 1. Modele lineaire standard
#----------------------------
x11()
titre = paste("Profondeur")
paletteVerte = colorRampPalette(c("white", "green"))
couleur = paletteVerte(100)[findInterval(depth,seq(min(depth),max(depth),l=100))]
traceStrates(titre,couleur)
text(longitude,latitude,round(depth,2),cex=0.5)
x11()
titre = paste("Temperature")
paletteRouge = colorRampPalette(c("white", "red"))
couleur = paletteRouge(100)[findInterval(temperature,seq(min(temperature),max(temperature),l=100))]
traceStrates(titre,couleur)
text(longitude,latitude,round(temperature,2),cex=0.5)

pairs(cbind(totconsum,temperature,depth))

totcons.lm <- lm(totconsum ~  temperature + depth) 
summary(totcons.lm)
anova(totcons.lm)
totcons.lm <- lm(totconsum ~  temperature) 
summary(totcons.lm)
anova(totcons.lm)

#test d'autocorr?lation des residus 

lm.morantest(totcons.lm,poids.vois)

paletteRes = colorRampPalette(c("yellow", "cyan"))

###sans hypoth?se de normalit?

res0 <- residuals(totcons.lm)
moran.test(res0,poids.vois)

#####graphique des r?sidus 
x11()
couleur = paletteRes(100)[findInterval(res0,seq(-0.3,0.3,l=100))]
titre = paste("Residus modele lineaire")
traceStrates(titre,couleur)
text(longitude,latitude,round(res0,2),cex=0.5)

# 2. Modele SAR sur les erreurs
#------------------------------
totcons.errorsarlm <- errorsarlm(totconsum ~  temperature,data=donnees,poids.vois) 
summary(totcons.errorsarlm)
res1 <- residuals(totcons.errorsarlm)
moran.test(res1,poids.vois)
titre = "Residus modele SAR sur les erreurs"
x11()
couleur = paletteRes(100)[findInterval(res1,seq(-0.3,0.3,l=100))]
traceStrates(titre,couleur)
text(longitude,latitude,round(res1,2),cex=0.5)


# 3. Modele SAR a retard
#-----------------------
totcons.lagsarlm <- lagsarlm(totconsum ~  temperature ,data=donnees,poids.vois) 
summary(totcons.lagsarlm)
res2 <- residuals(totcons.lagsarlm)
moran.test(res2,poids.vois)
titre = "Residus modele SAR a retard"
x11()
couleur = paletteRes(100)[findInterval(res2,seq(-0.3,0.3,l=100))]
traceStrates(titre,couleur)
text(longitude,latitude,round(res2,2),cex=0.5)

# 4. Modele mixte SAR a retard
#-----------------------------
totcons.mixte <- lagsarlm(totconsum ~  temperature ,data=donnees,poids.vois,type="mixed") 
summary(totcons.mixte)
res3 <- residuals(totcons.mixte)
moran.test(res3,poids.vois)
titre = "Residus modele mixte SAR a retard"
x11()
couleur = paletteRes(100)[findInterval(res3,seq(-0.3,0.3,l=100))]
traceStrates(titre,couleur)
text(longitude,latitude,round(res3,2),cex=0.5)

lm.LMtests(totcons.lm, poids.vois, test=c("LMerr", "LMlag", "RLMerr",
       "RLMlag", "SARMA"))
     lm.LMtests(totcons.lm, poids.vois)
     lm.LMtests(residuals(totcons.lm), poids.vois)

# Comparaison des modeles
#------------------------
print(LR.sarlm(totcons.mixte,totcons.errorsarlm))
print(anova(totcons.mixte,totcons.errorsarlm,totcons.lagsarlm,totcons.lm))
print(AIC(totcons.mixte,totcons.errorsarlm,totcons.lagsarlm,totcons.lm))
