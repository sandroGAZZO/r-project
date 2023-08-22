#   TP sur le krigeage
#     


rm(list=ls())
graphics.off()

#Exercice 1
#--------------------------------------------
# chargement des packages et fonctions utilises 
#--------------------------------------------

library(geoR)
library(fields)
setwd("C:/Users/Sandro/Documents/R/Stats_spatiales")
#---------------------------------
# chargement du fichier de donnees
#--------------------------------- 
aniso = 0
donnees = read.table("jeu1.dat")
summary(donnees)
donnees.simu = read.table("simu1.dat")
x=donnees$V1
y=donnees$V2
#----graphique du champs--------
grx = seq(0,100)
gry = seq(0,100)

x11()
Z = matrix(donnees.simu$V3,nrow=length(grx),ncol=length(gry),byrow=F)
titre = paste("Gaussian field")

image.plot(grx,gry,Z,main=titre,asp=1)
points(x,y,pch=19)

####
# variogramme
#------------
# 1. Variogramme empirique
#-------------------------
geodata = as.geodata(donnees)#convertir les observations en geodata
m.d = 50                    # distance maximale
interv = seq(0,m.d,by=5)    # intervalles
p.m = 10                   # nombre minimal de paire

vario.c = variog(geodata,op="cloud")
plot(vario.c,main = "",pch='+')
vario.b = variog(geodata,max.dist=m.d,pairs.min=p.m,breaks=interv)
x11()
plot(vario.b,main = "")
vario.bc = variog(geodata,max.dist=m.d,pairs.min=p.m,breaks=interv,bin.cloud=TRUE)
x11()
plot(vario.bc,main = "Box-plot sur le variogramme empirique",bin.cloud=TRUE)

# 2. Ajustement du variogramme
#-----------------------------
c.m = "exponential"

i.c = c(1.5,10)
varioest = variofit(vario.b,cov.model = c.m,fix.kappa=TRUE, ini.cov.pars=i.c,fix.nugget=T)
x11()
titre = paste("modele ",c.m,", portee =",round(varioest$cov.pars[2],2),", palier =",round(varioest$cov.pars[1],2),"nu = ",round(varioest$kappa,2))
plot(vario.b, main=titre)
lines(varioest)


# 3. Krigeage
#------------
grx = seq(0,100)
gry = seq(0,100)
grille = expand.grid(grx,gry)# l'ensemble S

Kcontrol = krige.control(type.krige="ok",obj.model=varioest)#krigeage ordinaire
Ocontrol = output.control(n.pred=100,simul=TRUE,thres=2)
K = krige.conv(geodata,loc=grille,krige=Kcontrol)
# l'échantillon est geodata (les 100 donnees de jeu1, l'ensemble S de prediction est la grille et la fonction de variogramme est l'exponentiel avec les parametres estimes par variofit

#####le resultat du krigeage

x11()
Zkrige = matrix(K$predict,nrow=length(grx),ncol=length(gry),byrow=F)
titre = paste("Krigeage avec un modele",c.m)
image.plot(grx,gry,Zkrige,zlim=c(-5,5),main=titre,asp=1)
contour(grx,gry,Zkrige,levels=seq(-5,5),add=TRUE)
points(x,y,pch=19)


#####variance de l'erreur de prévision

x11()
s=apply(cbind(K$krige.var,rep(0,length(K$krige.var))),1,max)
Sigma = sqrt(matrix(s,nrow=length(grx),ncol=length(gry),byrow=F))
titre = "Ecart-type de krigeage"
image.plot(grx,gry,Sigma,zlim=c(0,2),main=titre)
contour(grx,gry,Sigma,levels=seq(0,2,0.5),add=TRUE)
points(x,y,pch=19)


### On peut aussi automatiser le choix du variogramme avec Automap

library(automap)


### choix du variogramme automatisé

colnames(donnees)=c("x","y","z")
coordinates(donnees) = ~x+y
# Ici le modèle optimal est de Sphérique de paramètres de portee 40.20053, portee  1.5887879 et pepite 

variogram = autofitVariogram(z~1,donnees)
variogram
plot(variogram)

###Krigeage avec choix automatique du variogramme
grille2=as.data.frame(grille)
colnames(grille2)=c("x","y")
coords=SpatialPoints(grille2)
kriging_result = autoKrige(z~1,donnees, coords)
plot(kriging_result)
#Sans grille

kriging_result = autoKrige(z~1,donnees)
plot(kriging_result)