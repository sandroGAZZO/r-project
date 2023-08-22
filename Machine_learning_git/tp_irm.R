# visualiser graphe du 
# partionnement spectral

library(igraph)

# Exo 1
# Simulation de données
n <- 100
x <- matrix(0,nr=n,nc=2)



# On génère un vecteur de taille n contenant le numéro
# de la composante de chaque observation
p1<-1/3
p2<-1/3
p3<-1/3

m1<-c(-3,0)
m2<-c(0,1)
m3<-c(2,-1)


Sigma<-diag(c(0.2,0.1))


library(mvtnorm)

for (i in 1:100){
 #tirer au sort le groupe
  u<- runif(1)
  #1ere meth
  num_grp<- ifelse(u<1/3,1,ifelse(u<2/3,2,3))
  #2eme meth
  num_grp<-sample(c(1,2,3),size=1,prob=c(1/3,1/3,1/3))
  
  # tirer selon la loi Gaussienne de la composante
  xi<-rmvnorm(1,mean=c(0,0),sigma=Sigma)
  
  if (num_grp==1){
    xi<-xi+m1
  }
  else if (num_grp==2){
    xi<-xi+m2
  }
  else{
    xi<-xi+m3
  }
  
  x[i,]<-xi
}


# Nuage de points
plot(x,type="p")
require(ggplot2)
ggplot(data=data.frame(x),aes(x[,1],x[,2])) + geom_point()


# matrice de similarité
d <- as.matrix(dist(x))
sigma<-1
s <- exp(-d^2/(2*sigma^2))


#graphe dense
Wfull<-s
Dfull<- apply(s,1,sum) # diag de la matrice des degrès
# si on augmente sigma, on augmente la moyenne
# si on diminue sigma, on diminue la moyenne
mean(Dfull) # degré moyen



p<-graph.adjacency(s,weighted = T, mode="undirected")
p<-simplify(p)
plot(p)
E(p)$width<-E(p)$weight
plot(p)




# Graphe du epsilon voisinage
seuil<- quantile(s,0.75)
W_eps<-s
W_eps[s<seuil]<-0
W_eps[s>=seuil]<- 1 #graphe binaire


p<-graph.adjacency(W_eps,weighted = T, node="undirected")
p<-simplify(p)
plot(p)

E(p)$width<-E(p)$weight
plot(p)

Deps<-apply(W_eps, 1,sum)
mean(Deps)

# Graphe des kppv

k<-2*floor(log(n))


# On construit d'abord les indices des (k+1) plus 
# proches voisins de chaque point
# On garde (k+1) voisins car le plus proche sera
# forcément le point lui-même
knn_indices<-matrix(0,nr=n,nc=(k+1))
for (i in 1:n)
{
  knn_indices[i,]<-order(d[i,])[1:(k+1)]
}


# Pour obtenir les voisins mutuels, on doit vérifier que 
# si j est un voisin de i, alors
knn_natural<-list() #on créé une liste plutôt qu'une matrice
# car chaque élément peut
for (i in 1:n){
  
}






Id <- diag(n)

MD_eps<-diag(Deps^(-0.5))

Lsym_eps<- Id - MD_eps %*% W_eps %*% MD_eps
Lsym_eps
speps<-eigen(Lsym_eps)


MDfull<-diag(Dfull^(-0.5))

Lsymfull<- Id - MDfull %*% Wfull %*% MDfull
Lsymfull
spfull<-eigen(Lsymfull)


# MDknn<-diag(Dknn^(-0.5))
# 
# Lsymknn<- Id - MDknn %*% Wknn %*% MDknn
# Lsymknn
# spknn<-eigen(Lsymknn)


ggplot(data = data.frame(values=speps$values[91:100], order=10:1),aes(x=order,y=values)) + geom_point() + ggtitle("Graphe des epsilon voisinages")

ggplot(data = data.frame(values=spfull$values[91:100], order=10:1),aes(x=order,y=values)) + geom_point() + ggtitle("Graphe complet")

# + faire graphe knn

Ueps<-speps$vectors[,98:100]
Ufull<-spfull$vectors[,98:100]


rownorm <- function(v){
  return(v/sqrt(sum(v^2)))
}


Vfull<-t(apply(Ufull,1,rownorm))
Veps<-t(apply(Ueps,1,rownorm))

clfull<-kmeans(Vfull,3)
cleps<-kmeans(Veps,3)


data<-data.frame(x)
data$clfull<- as.factor(clfull$cluster)
data$cleps<-as.factor(cleps$cluster)


ggplot(data=data, aes(X1,X2,color=clfull)) + geom_point()
ggplot(data=data, aes(X1,X2,color=cleps)) + geom_point()


plot(x,col=clfull$cluster,pch=19)
plot(x,col=cleps$cluster,pch=19)


## Exo 2 
require(jpeg)
img<-readJPEG("C:/Users/Sandro/Documents/R/Meth_app/irm_small.jpeg")
image(img)


# Matrice de similarité
# On transforme d'abord l'image, stockée sous forme de matrice,
# en vecteur
img_v<-as.vector(img)
d<-as.matrix(dist(img_v))


# On choisit sigma=0.5 mais on peut tester d'autres valeurs
sigma<-0.5
W <- exp(-d^2/(2*sigma^2))






