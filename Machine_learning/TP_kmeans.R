ar#TP6


source("C:/Users/Sandro/Documents/R/Meth_app/datasets2.R")

d1<-circles(200)
d2<-difftaille(200)
d3 <- carres(200)
d4<-losanges(200)


require(ggplot2)
d<-rbind(d1,d2,d3,d4)

d$data<-c(rep(c("Cercles","Tailles différentes","Carrés","Losanges"),each=400))


ggplot(data=d, aes(x1,x2))+ geom_point() + facet_wrap(~data,scales="free")+  theme(legend.position = "none")



### k-means ### (i.e.) centre de gravités

## Cercle
# nstart = 1
clust_d1<-kmeans(d1,centers=2,nstart=1)
clust_d1
clust_d1$centers
res<-cbind(d1,clust=clust_d1$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust),size=1))+geom_point()

d1_centres<-as.data.frame(clust_d1$centers)
ggplot(data=d1,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d1_centres,col="blue",size=4)

# nstart = 2
clust_d1<-kmeans(d1,centers=2,nstart=2)
clust_d1
clust_d1$centers
res<-cbind(d1,clust=clust_d1$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust),size=1))+geom_point()






## Différente taille
d2_centres<-as.data.frame(clust_d1$centers)
ggplot(data=d2,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d2_centres,col="blue",size=4)

# nstart= 1
clust_d2<-kmeans(d2,centers=2,nstart=1)
clust_d2
clust_d2$centers
res<-cbind(d2,clust=clust_d2$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust)))+geom_point()

d2_centres<-as.data.frame(clust_d2$centers)
ggplot(data=d1,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d1_centres,col="blue",size=4)

#nstart=2
clust_d2<-kmeans(d2,centers=2,nstart=2)
clust_d2
clust_d2$centers
res<-cbind(d2,clust=clust_d2$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust)))+geom_point()

d2_centres<-as.data.frame(clust_d2$centers)
ggplot(data=d2,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d2_centres,col="blue",size=4)





## Carres
# nstart=1
clust_d3<-kmeans(d3,centers=2,nstart=1)
clust_d3
clust_d3$centers
res<-cbind(d3,clust=clust_d3$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust)))+geom_point()

d3_centres<-as.data.frame(clust_d3$centers)
ggplot(data=d3,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d3_centres,col="blue",size=4)


#nstart=2
clust_d3<-kmeans(d3,centers=2,nstart=2)
clust_d3
clust_d3$centers
res<-cbind(d3,clust=clust_d3$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust)))+geom_point()

d3_centres<-as.data.frame(clust_d3$centers)
ggplot(data=d3,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d3_centres,col="blue",size=4)




## Losanges
#nstart=1
clust_d4<-kmeans(d4,centers=2,nstart=1)
clust_d4
clust_d4$centers
res<-cbind(d4,clust=clust_d4$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust)))+geom_point()

d4_centres<-as.data.frame(clust_d4$centers)
ggplot(data=d4,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d4_centres,col="blue",size=4)

#nstart=2
clust_d4<-kmeans(d4,centers=2,nstart=2)
clust_d4
clust_d4$centers
res<-cbind(d4,clust=clust_d4$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust)))+geom_point()

d4_centres<-as.data.frame(clust_d4$centers)
ggplot(data=d4,aes(x=x1,y=x2)) + geom_point() +geom_point(data=d4_centres,col="blue",size=4)



### CAH ###

## Cercles ##
## Saut minimum
dist_d1<-dist(d1)
hclust_1<-hclust(dist_d1,method = "single") #critère du saut minimun "single linkage"
plot(rev(hclust_1$height),type="b",xlim=c(0,20)) # "critère du coude"
# si coude il y a , ça peut-etre une bonne classification
# alternative à la reprsentation graphique des points dans les deux classes


plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d1))
cah_clust[clusters[[2]]]<-2
d1$cah_clust<- cah_clust
ggplot(data=d1,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Saut moyen
dist_d1<-dist(d1)
hclust_1<-hclust(dist_d1,method = "average") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d1))
cah_clust[clusters[[2]]]<-2
d1$cah_clust<- cah_clust
ggplot(data=d1,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()

## Saut maximum
dist_d1<-dist(d1)
hclust_1<-hclust(dist_d1,method = "complete") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d1))
cah_clust[clusters[[2]]]<-2
d1$cah_clust<- cah_clust
ggplot(data=d1,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v1
dist_d1<-dist(d1)
hclust_1<-hclust(dist_d1,method = "ward.D2") #critère de Ward
plot(rev(hclust_1$height),type="b",xlim=c(0,20))


plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d1))
cah_clust[clusters[[2]]]<-2
d1$cah_clust<- cah_clust
ggplot(data=d1,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v2
dist_d1<-dist(d1)
hclust_1<-hclust(dist_d1,method = "ward.D") #critère de Ward

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d1))
cah_clust[clusters[[2]]]<-2
d1$cah_clust<- cah_clust
ggplot(data=d1,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


##  Différentes tailles ##
## Saut minimum
dist_d2<-dist(d2)
hclust_1<-hclust(dist_d2,method = "single") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d2))
cah_clust[clusters[[2]]]<-2
d2$cah_clust<- cah_clust
ggplot(data=d2,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Saut moyen
dist_d2<-dist(d2)
hclust_1<-hclust(dist_d2,method = "average") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d2))
cah_clust[clusters[[2]]]<-2
d2$cah_clust<- cah_clust
ggplot(data=d2,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()

## Saut maximum
dist_d2<-dist(d2)
hclust_1<-hclust(dist_d2,method = "complete") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d2))
cah_clust[clusters[[2]]]<-2
d2$cah_clust<- cah_clust
ggplot(data=d2,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v1
dist_d2<-dist(d2)
hclust_1<-hclust(dist_d2,method = "ward.D2") #critère de Ward
plot(rev(hclust_1$height),type="b",xlim=c(0,20)) # "critère du coude"
# si coude il y a , ça peut-etre une bonne classification
# alternative à la reprsentation graphique des points dans les deux classes

plot(hclust_1)






















clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d2))
cah_clust[clusters[[2]]]<-2
d2$cah_clust<- cah_clust
ggplot(data=d2,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v2
dist_d2<-dist(d2)
hclust_1<-hclust(dist_d2,method = "ward.D") #critère de Ward

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d2))
cah_clust[clusters[[2]]]<-2
d2$cah_clust<- cah_clust
ggplot(data=d2,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Carrés ##
## Saut minimum
dist_d3<-dist(d3)
hclust_1<-hclust(dist_d3,method = "single") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d3))
cah_clust[clusters[[2]]]<-2
d3$cah_clust<- cah_clust
ggplot(data=d3,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Saut moyen
dist_d3<-dist(d3)
hclust_1<-hclust(dist_d3,method = "average") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d3))
cah_clust[clusters[[2]]]<-2
d3$cah_clust<- cah_clust
ggplot(data=d3,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()

## Saut maximum
dist_d3<-dist(d3)
hclust_1<-hclust(dist_d3,method = "complete") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d3))
cah_clust[clusters[[2]]]<-2
d3$cah_clust<- cah_clust
ggplot(data=d3,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v1
dist_d3<-dist(d3)
hclust_1<-hclust(dist_d3,method = "ward.D2") #critère de Ward

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d3))
cah_clust[clusters[[2]]]<-2
d3$cah_clust<- cah_clust
ggplot(data=d3,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v2
dist_d3<-dist(d3)
hclust_1<-hclust(dist_d3,method = "ward.D") #critère de Ward

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d3))
cah_clust[clusters[[2]]]<-2
d3$cah_clust<- cah_clust
ggplot(data=d3,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()



## Losanges ##
## Saut minimum
dist_d4<-dist(d4)
hclust_1<-hclust(dist_d4,method = "single") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d4))
cah_clust[clusters[[2]]]<-2
d4$cah_clust<- cah_clust
ggplot(data=d4,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Saut moyen
dist_d4<-dist(d4)
hclust_1<-hclust(dist_d4,method = "average") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d4))
cah_clust[clusters[[2]]]<-2
d4$cah_clust<- cah_clust
ggplot(data=d4,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()

## Saut maximum
dist_d4<-dist(d4)
hclust_1<-hclust(dist_d4,method = "complete") #critère du saut minimun "single linkage"

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d4))
cah_clust[clusters[[2]]]<-2
d4$cah_clust<- cah_clust
ggplot(data=d4,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v1
dist_d4<-dist(d4)
hclust_1<-hclust(dist_d4,method = "ward.D2") #critère de Ward

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d4))
cah_clust[clusters[[2]]]<-2
d4$cah_clust<- cah_clust
ggplot(data=d4,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()


## Ward v2
dist_d4<-dist(d4)
hclust_1<-hclust(dist_d4,method = "ward.D") #critère de Ward

plot(hclust_1)


clusters <- rect.hclust(hclust_1,k=2)

cah_clust<-rep(1,nrow(d4))
cah_clust[clusters[[2]]]<-2
d4$cah_clust<- cah_clust
ggplot(data=d4,aes(x1,x2,color=as.factor(cah_clust),size=1)) + geom_point()




##### EXERCICE 2 #####

temper<-read.csv("C:/Users/Sandro/Documents/R/Meth_app/temperatures.csv")

summary(temper)


summary(temper[temper$kmeancl3==1,2:7])


##. CAH
dist_temper<-dist(temper[,-c(1,18)])
temper_cah<-hclust(dist_temper,method = "ward.D2")
plot(temper_cah,labels=temper$X)
plot(rev(temper_cah$height),type='b')

plot(temper_cah,labels=temper$X)
rect.hclust(temper_cah,k=3)






temper<-temper[,-c(1,18)]



### k-means ###
clust_temper<-kmeans(temper,centers=2,nstart=1)
clust_temper
clust_temper$centers
res<-cbind(temper,clust=clust_temper$cluster)
ggplot(data = res,aes(x1,x2,color=as.factor(clust),size=1))+geom_point()

temper_centres<-as.data.frame(clust_temper$centers)
ggplot(data=temper,aes(x=x1,y=x2)) + geom_point() +geom_point(data=temper_centres,col="blue",size=4)




### CAH ###
## Ward v2
dist_temper<-dist(temper)
hclust_temper<-hclust(dist_temper,method = "ward.D") #critère de Ward
plot(rev(hclust_temper$height),type="b",xlim=c(0,20)) # "critère du coude"
# si coude il y a , ça peut-etre une bonne classification
# alternative à la reprsentation graphique des points dans les deux classes

plot(hclust_temper)

clusters <- rect.hclust(hclust_temper,k=2)

cah_clust<-rep(1,nrow(temper))
cah_clust[clusters[[2]]]<-2
temper$cah_clust<- cah_clust
ggplot(data=temper,aes(x=x1,y=x2,color=as.factor(cah_clust),size=1)) + geom_point()



tempe <- read.csv("C:/Users/Sandro/Documents/R/Meth_app/temperatures.csv")

summary(tempe)

# k-means
ix <- rep(0,10)
for (k in 1:10){
  tempe_km <- kmeans(tempe[,-c(1,18)],centers=k,nstart=5)
  ix[k] <- tempe_km$tot.withinss
}
plot(1:10,ix,type="b")

cl2 <- kmeans(tempe[,-c(1,18)],centers=2,nstart=5)
cl3 <- kmeans(tempe[,-c(1,18)],centers=3,nstart=5)

tempe$kmeanscl2 <- cl2$cluster
tempe$kmeanscl3 <- cl3$cluster

# 2 clusters
tempe$X[tempe$kmeanscl2 == 1]
tempe$X[tempe$kmeanscl2 == 2]

# 3 clusters
tempe$X[tempe$kmeanscl3 == 1]
tempe$X[tempe$kmeanscl3 == 2]
tempe$X[tempe$kmeanscl3 == 3]



# CAH
dist_tempe <- dist(tempe[,-c(1,18)])
tempe_cah <- hclust(dist_tempe,method="ward.D2")
plot(tempe_cah,labels=tempe$X)
plot(rev(tempe_cah$height),type="b")

plot(tempe_cah,labels=tempe$X)
rect.hclust(tempe_cah,k=3)






