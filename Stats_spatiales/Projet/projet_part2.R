rm(list=ls())

traceStrates = function(titre="",couleur='white'){
  if (length(couleur)==1) couleur=rep(couleur,38)
  plot(range(longitude),range(latitude),type="n",xlim=c(-66,-60),ylim=c(45,50),
       main=titre,xlab="Longitude",ylab="Latitude")
  rect(-66,45,-60,50,col='brown')
  for(i in 401:439){
    polygon(bornes[which(bornes[,1]==i),2:3],col=couleur[i-400])
  }
}

#--------------------------------------------
# chargement des packages et fonctions utilises 
#--------------------------------------------

library(fields)
library(spdep)
library(spatialreg)


#### OUVERTURE FICHIER #####
## Option 1
library(rgdal)
library(sp)
mais <- readOGR(dsn = 'C:/Users/Sandro/Documents/R/Stats_spatiales/Projet',
               layer = 'rosas1999')
proj4string(mais)
coordinates(mais)

mais@data
# ou
mais<-data.frame(mais)

## Option 2
library(sf)
champ <- st_read(dsn = 'C:/Users/Sandro/Documents/R/Stats_spatiales/Projet',
               layer = 'rosas1999')

st_crs(champ)
st_coordinates(champ)
champ<-data.frame(champ)


summary(mais)
summary(champ)

str(mais)


attach(mais)
TOP2=as.factor(TOP2)
TOP3=as.factor(TOP3)
TOP4=as.factor(TOP4)
TOPO=as.factor(TOPO)
#OBS=as.factor(OBS) ???


YIELD100<- 100*mais$YIELD

donnees<-cbind(mais,YIELD100)


donnees[0,35]<-colnames(donnees[0,35],prefix = "YIELD100")

library(foreign)
poids<-read.dbf("C:/Users/Sandro/Documents/R/Stats_spatiales/Projet/rosas1999.dbf")



lineaire<-lm(formula = YIELD100~N + N2 + TOP2 +TOP3 +TOP4)
summary(lineaire)

voisin<-cbind(poids$TOP2,poids$TOP3,poids$TOP4,poids$N,poids$N2)
voisin[0,1]<-colnames(voisin[0,1],prefix="TOP2")
voisin[0,2]<-colnames(voisin[0,2],prefix="TOP3")
voisin[0,3]<-colnames(voisin[0,3],prefix="TOP4")
voisin[0,4]<-colnames(voisin[0,4],prefix="N")
voisin[0,5]<-colnames(voisin[0,5],prefix="N2")
SAR<-lagsarlm(formula=lineaire,listw = poids, type = "lag",
              method = "eigen") # = lag spatial geoda
summary(SAR)
