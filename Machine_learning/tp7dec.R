getwd()

source("color_utils.R")
library(dbscan)
require(jpeg)

img<-readJPEG("arabidopsis.jpeg")
dim(img)
display(img)

stock<-matrix(ncol=684,nrow=221)
stock[,1:228]<-img[,1:228,1]
stock[,229:(2*228)]<-img[,1:228,2]
stock[,(457):(684)]<-img[,1:228,3]
dim(stock)
stock

#------------------- ou

var1<-as.vector(img[,,1])
var2<-as.vector(img[,,2])
var3<-as.vector(img[,,3])

img_data<-cbind(var1,var2,var3)

#-----------
dev.off()
dknn<-kNNdist(img_data,k=50)
plot(sort(dknn,decreasing = T),ylim=c(0,0.1),type='l')
eps<-0.02 #coude

dbimg<-dbscan(img_data,eps=eps,minPts=50)
unique(dbimg$cluster)
length(dbimg$cluster)

matcl<-matrix(dbimg$cluster,nrow=dim(img)[1],ncol=dim(img)[2],byrow=F)

image(matcl, col=c("darkgreen","white","green"))


#-------

prop.table(table(dbimg$cluster)) # classe en haut a gauceh 1er pixel=1
#donc surface = ce qui est pas = a 1
105*0.45 #47.25 cm2


#--------

source("datasets2.R")

d1<-circles(200)
d2<-difftaille(200)
d3<-carres(200)
d4<-losanges(200)

library(ggplot2)
d<-rbind(d1,d2,d3,d4)

#----
dtest<-d4
#---


m<-5
dk<-kNNdist(dtest,k=m)
plot(sort(dk,decreasing = T),ylim=c(0,0.3),type='l')
eps<-0.18 #coude

dbclust2<-dbscan(dtest[,1:2],eps=eps,minPts=m)
dtest$db_clust<-dbclust2$cluster
ggplot(data=dtest,aes(x1,x2,color=as.factor(db_clust))) + geom_point()

