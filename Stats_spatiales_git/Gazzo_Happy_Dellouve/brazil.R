######################
## Gazzo Sandro  #####
##      &        #####
## Dellouve Théo #####
##      &        #####
## Happy Hugo    #####
######################

## chemin à changer
setwd("C:/Users/Sandro/Documents/R/Stats_spatiales/Projet")

##  Librairies utilisées
library(fields)
library(spdep)
library(geoR)

## fonction permettant de tracer les frontières de la carte du Parana
trace.front <- function(titre="",couleur="white"){
  if (length(couleur)==1) couleur=rep(couleur,38)
  plot(range(station$east),range(station$north),type="n",xlim=c(130,820),ylim=c(40,520),
       main=titre,xlab="Longitude",ylab="Latitude")
  rect(80,560,900,0,col='green')
  polygon(front[,1:2],col="white")
}


## ouverture des fichiers
station<-read.table("parana.txt",header = TRUE)
front<-read.table("parana-borders.txt", header =  TRUE)

## analyse descriptive
summary(station)
summary(front)

front<- as.matrix(front)

## création de lde la carte du Parana
trace.front(titre='Etat du Parana, Brésil')
points(station$east,station$north)


trace.front(titre='Etat du Parana, Brésil')
text(station$east,station$north,cex=0.5)


donnees<-station



####
# variogramme
#------------
# 1. Variogramme empirique
#-------------------------
geodata = as.geodata(donnees)#convertir les observations en geodata
m.d = 680                    # distance maximale
interv = seq(0,m.d,by=5)    # intervalles
p.m = 1                   # nombre minimal de paire

vario.c = variog(geodata,op="cloud")
plot(vario.c,main = "",pch='+')
vario.b = variog(geodata,max.dist=m.d,pairs.min=p.m,breaks=interv)

plot(vario.b,main = "")
vario.bc = variog(geodata,max.dist=m.d,pairs.min=p.m,breaks=interv,bin.cloud=TRUE)

plot(vario.bc,main = "Box-plot sur le variogramme empirique",bin.cloud=TRUE)

### Variogramme pluri-directionnelle

vario.b4 = variog4(geodata)

plot(vario.b4,main = "")


# 2. Ajustement du variogramme
#-----------------------------

c.m= "spherical"

i.c = c(0.5,700)
varioest = variofit(vario.b,cov.model = c.m,fix.kappa=T, ini.cov.pars=i.c,fix.nugget=T)

titre = paste("modele ",c.m,", portee =",round(varioest$cov.pars[2],2),", palier =",round(varioest$cov.pars[1],2),"nu = ",round(varioest$kappa,2))
plot(vario.b, main=titre)
lines(varioest)



x<-donnees$east
y<-donnees$north
#----graphique du champs--------
grx <- seq(140,780,by=2)
gry <- seq(60,470,by=2)

grille = expand.grid(grx,gry)# l'ensemble S

Kcontrol = krige.control(type.krige="ok",obj.model=varioest)#krigeage ordinaire
Ocontrol = output.control(n.pred=100,simul=TRUE,thres=2)
K = krige.conv(geodata,loc=grille,krige=Kcontrol)
# l'échantillon est geodata 

#####le resultat du krigeage
Zkrige = matrix(K$predict,nrow=length(grx),ncol=length(gry),byrow=F)
titre = paste("Krigeage avec un modele",c.m)
image.plot(grx,gry,Zkrige,zlim=c(162,414),main=titre,asp=1)
contour(grx,gry,Zkrige,levels=seq(160,414,30),add=TRUE)
points(x,y,pch=19)


#####variance de l'erreur de prévision

s=apply(cbind(K$krige.var,rep(0,length(K$krige.var))),1,max)
Sigma = sqrt(matrix(s,nrow=length(grx),ncol=length(gry),byrow=F))
titre = "Ecart-type de krigeage"
image.plot(grx,gry,Sigma,zlim=c(0,65),main=titre)
contour(grx,gry,Sigma,levels=seq(0,65,10),add=TRUE)
points(x,y,pch=19)



x= station[,1]
y= station[,2]
z=station[,3]
summary(lm(z~x+y))
#===> préférable krigeage universel


geodata = as.geodata(cbind(x,y,z))
m.d = 100                    # distance maximale
interv = seq(10,m.d,by=20)    # intervalles
p.m = 2                    # nombre minimal de paire


vario.c = variog(geodata,op="cloud")
plot(vario.c,main = "",pch='+')

vario.b = variog(geodata,max.dist=m.d,pairs.min=p.m,
                 uvec=interv,messages.screen=FALSE)

plot(vario.b,main = "Variogramme empirique")

# 2. Ajustement du variogramme
#-----------------------------
c.m = "exponential"
i.c = c(500,50)
varioest = variofit(vario.b,cov.model = c.m,
                    minimisation.function = "nls",ini.cov.pars=i.c,fix.nugget=T,
                    fix.kappa=TRUE,max.dist=vario.b$max.dist)

titre = paste("modele ",c.m,", portee =",
              round(varioest$cov.pars[2]*100)/100,", palier =",
              round(varioest$cov.pars[1]*100)/100)
plot(vario.b,main=titre)
lines(varioest)


# 3. Krigeage
#------------
grxy = grille[,1:2]
grx <- seq(140,780,by=2)
gry <- seq(60,470,by=2)
grille = expand.grid(grx,gry)# l'ensemble S

K = krige.conv(geodata,loc=grxy,
               krige=krige.control(type.krige="ok",obj.model=varioest))

## prediction combinée=krigeage de l'erreur+modele deterministe

Zkrige = matrix(K$predict,nrow=length(grx),
                ncol=length(gry),byrow=FALSE)
titre = paste("Krigeage avec un modele",c.m)
image.plot(grx,gry,Zkrige,zlim=c(162,414),main=titre,
           col=tim.colors(64))
contour(grx,gry,Zkrige,levels=seq(160,414,30),add=TRUE)
points(x,y,pch=19)



s=apply(cbind(K$krige.var,rep(0,length(K$krige.var))),1,max)
Sigma = sqrt(matrix(s,nrow=length(grx),ncol=length(gry),byrow=FALSE))
titre = "Ecart-type de krigeage"
image.plot(grx,gry,Sigma,zlim=c(0,ceiling(max(Sigma))),
           main=titre,col =tim.colors(64))
contour(grx,gry,Sigma,levels=seq(0,ceiling(max(Sigma)),10),add=TRUE)
points(x,y,pch=19)




donnees<-station
library(automap)


### choix du variogramme automatisé

colnames(donnees)=c("x","y","z")
coordinates(donnees) = ~x+y

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



