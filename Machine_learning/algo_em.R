#install.packages("mclust")
#install.packages("Rmixmod")

library(mclust)
library(Rmixmod)

#Exo1
p<-1/4

mu1<- c(0,1)
mu2<- c(1,1)
mu3<- c(0,-1)
mu4<- c(0,-2)

sigma<-matrix(c(0.2,0,0,0.1),ncol=2)

n=100
x<-matrix(0,nr=n,nc=2)

comp<-sample(1:4,n,replace=T)
for (i in 1:n)
{
  if (comp[i]==1) # simulation pour les données de la composante 1
  {
    x[i,] <- c(rnorm(1,0,sqrt(0.2)),rnorm(1,1,sqrt(0.1)))
  }else if (comp[i]==2) # simulation pour les données de la composante 2
  {
    x[i,] <- c(rnorm(1,1,sqrt(0.2)),rnorm(1,1,sqrt(0.1)))
  }
  else if (comp[i]==3) # simulation pour les données de la composante 2
  {
    x[i,] <- c(rnorm(1,0,sqrt(0.2)),rnorm(1,-1,sqrt(0.1)))
  }
  else{ # simulation pour les données de la composante 3
    x[i,] <- c(rnorm(1,0,sqrt(0.2)),rnorm(1,-2,sqrt(0.1)))
  }
}

mu_vrai<-matrix(c(0,1,1,1,0,-1,0,-2),nr=4,byrow=T)
sigma_vrai<-matrix(rep(c(0.2,0,0,0.1),4),nr=4,byrow=T)

# Nuage de points, par la fonction plot de R ou par le package ggplot
plot(x,type="p")
require(ggplot2)
x<-data.frame(x)
ggplot(data=x,aes(x[,1],x[,2],color=as.factor(comp))) + geom_point()


#### mclust ########
# avec reglage par defaut
mc<-Mclust(x)
table(comp)
summary(mc)

plot(mc,what="BIC")
summary(mc$BIC)

plot(mc,what="classification")
summary(mc$classification)

plot(mc,what="uncertainty")
summary(mc$uncertainty)

mcICL<-mclustICL(x)
summary(mcICL)
plot(mcICL)
# pour faire classification
str(mcICL)



mc15<-Mclust(x,G=1:5)
summary(mc15)
plot(mc15,what="BIC")
summary(mc15$BIC)


mcICL15<-mclustICL(x,G=1:5)
summary(mcICL15)
plot(mcICL15)


mc1<-Mclust(x,G=1,modelNames = "EEI")
plot(mc1,what="BIC")
summary(mc1$BIC)
plot(mc1,what="classification")
summary(mc1)


mc2<-Mclust(x,G=2,modelNames = "EEI")
plot(mc2,what="BIC")
summary(mc2$BIC)
plot(mc2,what="classification")
summary(mc2)


mc3<-Mclust(x,G=3,modelNames = "EEI")
plot(mc3,what="BIC")
summary(mc3$BIC)
plot(mc3,what="classification")
summary(mc3)


mc4<-Mclust(x,G=4,modelNames = "EEI")
plot(mc4,what="BIC")
summary(mc4$BIC)
plot(mc4,what="classification")
summary(mc4)


mc5<-Mclust(x,G=5,modelNames = "EEI")
plot(mc5,what="BIC")
summary(mc4$BIC)
plot(mc5,what="classification")
summary(mc5)


## sur plusieurs directement, mais on ne voit pas 
# comment focntionne étape par étape l'algorithme
mctest<-Mclust(x,G=1:5,modelNames = "EEI")
plot(mctest,what="BIC")
summary(mctest$BIC)
plot(mctest,what="classification")



##### Rmixmod #######
rmm<-mixmodCluster(x,nbCluster=1:5)
summary(rmm)
plot(rmm)
plotCluster(rmm["bestResult"],data=x)

rmm4<-mixmodCluster(x,nbCluster=4)
summary(rmm4)
plot(rmm4)
plotCluster(rmm4["bestResult"],data=x)


rmmICL<-mixmodCluster(x,nbCluster=1:5,criterion="ICL")
summary(rmmICL)
plot(rmmICL)
plotCluster(rmmICL["bestResult"],data=x)

rmmICL4<-mixmodCluster(x,nbCluster=4,criterion="ICL")
summary(rmmICL4)
plot(rmmICL4)
plotCluster(rmmICL4["bestResult"],data=x)



rmmEM<-mixmodCluster(x,nbCluster=1:5,strategy=mixmodStrategy("EM"))
rmmCEM<-mixmodCluster(x,nbCluster=1:5,strategy=mixmodStrategy("CEM"))

summary(rmmEM)
summary(rmmCEM)
plot(rmmEM)
plot(rmmCEM)
plotCluster(rmmEM["bestResult"],data=x)
plotCluster(rmmCEM["bestResult"],data=x)


## Exo 2
data(birds)
head(birds)
summary(birds)

rmm_birds<-mixmodCluster(birds,nbCluster=1:5)
summary(rmm_birds)
plot(rmm_birds)
#plotCluster(rmm_birds["bestResult"],data=birds) ==> pas possible ici
barplot(rmm_birds)





