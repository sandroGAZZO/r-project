library(jpeg)
library(imager)
library(dbscan)
source("C:/Users/Sandro/Documents/R/Meth_app/color_utils.R")
setwd("C:/Users/Sandro/Documents/R/Meth_app")

plante<-readJPEG("arabidopsis.jpeg",native = FALSE)

plante

dim(plante)

display(plante)


plante[115:120,115:120,1]
plante[115:120,115:120,2]
plante[115:120,115:120,3]

# mat<-c()
# mat<-matrix(ncol=(221*228),nrow=3)
# 
# for (i in 1:3){
#   for (j in 1:221){
#     for (k in 1:228){
#       mat[i,k+(j-1)*228]=plante[j,k,i]
#       
#     }
#   }
# }
# 
# dim(mat)
# mat
# mat[1,120+118*128]




var1<-as.vector(plante[,,1])
var2<-as.vector(plante[,,2])
var3<-as.vector(plante[,,3])


plante_data<-cbind(var1,var2,var3)
dim(plante_data)


dev.off()
kNNdistplot(plante_data,10)

dknn <- kNNdist(plante_data,k=10)
plot(sort(dknn,decreasing = T),type='l',ylim=c(0,0.05))



kNNdistplot(plante_data,100)

dev.off()
dknn <- kNNdist(plante_data,k=100)
plot(sort(dknn,decreasing = T),type='l',ylim=c(0,0.05))

dev.off()
dknn <- kNNdist(plante_data,k=200)
plot(sort(dknn,decreasing = T),type='l',ylim=c(0,0.05))




eps<-0.015
dbplante<- dbscan(plante_data,eps=eps,minPts = 100)
unique(dbplante$cluster)
length(dbplante$cluster)

matcl<-matrix(dbplante$cluster,nrow=dim(plante)[1],ncol=dim(plante)[2],byrow=F)

image(matcl)


eps<-0.03
dbplante<- dbscan(plante_data,eps=eps,minPts = 100)
unique(dbplante$cluster)
length(dbplante$cluster)

matcl<-matrix(dbplante$cluster,nrow=dim(plante)[1],ncol=dim(plante)[2],byrow=F)

image(matcl)





eps<-0.02
dbplante<- dbscan(plante_data,eps=eps,minPts = 20)
unique(dbplante$cluster)
length(dbplante$cluster)

matcl<-matrix(dbplante$cluster,nrow=dim(plante)[1],ncol=dim(plante)[2],byrow=F)

image(matcl)



dbplante$cluster[1] # classe du premier pixel en haut à gauche ->1
#je définis la plante comme tout ce qui n'est pas dans la classe 1

prop.table(table(dbplante$cluster))


#surface foliaire
(1-prop.table(table(dbplante$cluster))[2])*105



### Exo 2

source("datasets2.R")


d1<- circles(200)
d2<-difftaille(200)
d3<-carres(200)
d4<-losanges(200)


library(ggplot2)
d<-rbind(d1,d2,d3,d4)
d$data<- c(rep(c("Cercles","Taille différentes","Carrés","Losanges"),each=400))

ggplot(data=d,aes(x1,x2))+geom_point() +facet_wrap(~data,scales="free")+ theme(legend.position = "none")



clust_d2<-kmeans(d2,centers=2,nstart = 1)
res<- cbind(d2,clust=clust_d2$cluster)
ggplot(data = res, aes(x1,x2,color=as.factor(clust)))+ geom_point()



sse <- numeric()
for (i in 1:10){
  sse[i]=sum(kmeans(d3,centers=i,nstart = 20)$withinss)
}
plot(1:10,sse,pch=20,type="b")
clust_d3<-kmeans(d3,centers=3,nstart=1)
res<- cbind(d3,clust=clust_d3$cluster)
ggplot(data = res, aes(x1,x2,color=as.factor(clust)))+ geom_point()



# cah_clust[clusters[[2]]]<-2
# d4$cah_clustw-cah_clust
# ggplot(data=d4,aes(x1,x2,color=as.factor(cah_clust))+geom_point()




## DBSCAN
m<-5
dknn<-kNNdistplot(d1[,1:2],k=m)
eps<-0.5
dbclust1<-dbscan(d1[,1:2],eps=eps,minPts=m)
d1$db_clust<-dbclust1$cluster
ggplot(data = d1,aes(x1,x2,color=as.factor(db_clust)))+geom_point()




