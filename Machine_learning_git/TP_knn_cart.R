# TP knn et CART

# Exercice 1 : knn

library(tidyr)
library(ggplot2)

# import data
wines <- read.csv("C:/Users/Sandro/Documents/R/Meth_app/wines.csv")
summary(wines)

# On s'assure que la variable Classe est bien considérée comme un facteur
wines$Classe <- as.factor(wines$Classe)

# Boxplot avec la fonction "plot" de R
boxplot(wines) # toute sles variables sur le même graphe

# un graphe par variable
variables <- names(wines)[-1]
par(mfrow=c(4,4))
for (v in variables){
  boxplot(wines[,v] ~ wines$Classe,main=v,col=2:4)
}


# 1. Boxplot -- à l'aide de ggplot
# On créé une table "en longueur", qui contient 3 colonnes : une colonne avec les noms de variables, et une autre avec la valeur correspondante, et
# une troisième avec les valeurs de class
# Cette fonction sert à tracer sur le même graphe plusieurs courbes. Par exemple, si on a la table :
# y1    y2    y3    y4    x
# 1.1   5.2   8.1   0.2   1
# 0.7   4     1.4   1.8   2
# 0.9   3.6   0.8   4.1   3
# 2     1     1.7   7.6   4
# 3.5   1.2   1.1   0.2   5
# et que l'on souhaite tracer y1 en fonction de x, et y2 en fonction de x, ...
# on peut soit utiliser les fonctions "plot" et "points", soit utiliser le package "reshape2"
# La fonction "melt" de ce package permet de transformer la table ci-dessus en :
# x   variable    value
# 1   y1          1.1
# 2   y1          0.7
# 3   y1          0.9
# 4   y1          2
# 5   y1          3.5
# 1   y2          5.2
# 2   y2          4
# 3   y2          3.6
# ...
# Ceci permet de tracer la variable "value" en fonction de la variable "x", en utilisant une seule expression
# Dans l'exemple ci-dessus, au lieu de faire :
# plot(x,y1,col="red")
# points(x,y2,col="blue")
# points(x,y3,col="green")
# points(x,y4,col="orange")
# on peut écrire directement
# plot(x,value,col=variable)

# Dans le cas de notre exercice, on veut tracer des boxplots, on peut utiliser le package ggplot2 en plus, qui permet 
# d'afficher un boxplot par variable 
# fonction gather du package "tidyr"
d_long <- wines %>% gather(variable, value, -Classe) 
# ici on lui indique d'appliquer la fonction gather à la base wines,
# de combiner en une seule colonne toutes les variables sauf "Classe", d'appeler "variable"
# la colonne qui va indiquer le nom des variables qui ont été transformées, et d'appeler
# "value" la colonne qui va contenir les valeurs des variables transformées

# Pour utiliser la fonction ggplot, on précise d'abord le jeu de données, puis dans un bloc aes() on précise la variable x et la variable y
# On précise ensuite que l'on veut tracer les boxplot, puis l'option "facet_wrap" permet de créer un grille, où chaque case de la grille
# correspond à une valeur différente de "variable". L'option "scales=free" permet d'avoir une échelle des ordonnées différentes dans chaque case
p <- ggplot(data=d_long,aes(x=Classe,y=value,fill=Classe)) + geom_boxplot() + facet_wrap(~variable,scales="free")
p + theme(legend.position = "none")


# Echantillon test et apprentissage
N <- nrow(wines)
ntrain <- floor(0.75*N)
ntest <- N - ntrain

indices <- sample(1:N,ntrain,replace = FALSE)

wines_train <- wines[indices,]
wines_test <- wines[-indices,]
# standardisation des données
wines_train <- data.frame(Classe=wines_train$Classe,scale(wines_train[,-1]))
apply(wines_train,2,sd) # réduit
apply(wines_train[,-1],2,mean) # centré

wines_test <- data.frame(Classe=wines_test$Classe,scale(wines_test[,-1]))

library(dplyr)
d_train <- wines_train %>% select(!Classe)
labels_train <- wines_train$Classe
d_test <- wines_test %>% select(!Classe)

na <- nrow(d_test)
n <- nrow(d_train)


# Fonction knn, prend en argument :
# - k le nb de voisins
# - train la base d'apprentissage (uniquement les variables explicatives)
# - test la base de test (uniquement les variables explicatives)
# - class la classe des observations de la base d'apprentissage
knn <- function(k,train,test,class){
  n <- nrow(test)
  predict <- rep(0,n) # vecteur de prédictions

  train <- as.matrix(train)
  test <- as.matrix(test)
  
  ncl <- max(unclass(class))
  
  probs <- t(sapply(1:n,FUN = function(i){
    dist <- apply(train,1,FUN = function(x){sqrt(sum((x-test[i,])^2))})
    d_max <- sort(dist)[k] 
    temp <- as.data.frame(cbind(train,class,dist))
    dk <- temp[temp$dist <= d_max,]
    
    # Remarque : on peut aussi utiliser la fonction "order" qui renvoie les indices des éléments rangés par ordre croissant
    # Par exemple, on ferait :
    # indices_ordonnes <- order(dist)
    # class_kppv <- class[indices_ordonnes[1:k]]
    
    proba <- numeric()
    for (c in 1:ncl)
    {
      proba <- c(proba,mean(dk$class==c))
    }
    return(proba)
  })) # fin de la boucle sapply
  
  # RQ : on fait les calculs sur toute la matrice de classement, mais on aurait aussi pu faire les calculs à chaque
  # itération de la boucle sur i
  classement <- apply(probs,1,FUN = function(x){y<-which(x==max(x));if(length(y)>1){y<-sample(y,1,replace = FALSE)};return(y)})
  
  predict <- data.frame(cbind(test,probs,classement))
  
  return(predict$classement)
}


# Validation croisée leave-one-out
Kmax <- nrow(wines_train)-1
kseq <- seq(1,Kmax,1) # valeurs de k à tester
err <- rep(0,length(kseq)) # vecteur qui va contenir les erreurs de mauvais classement pour chaque valeur de k

for (k in 1:length(kseq)){
  for (i in 1:nrow(wines_train)){
    train <- wines_train[-i,-1]
    cl_train <- wines_train[-i,1]
    test <- wines_train[i,-1]
    cl_test <- wines_train[i,1]
    
    pred <- knn(train[,1:2],test[,1:2],cl_train,k=kseq[k])
    err[k] <- err[k] + (pred!=cl_test)
  }
  err[k] <- err[k]/nrow(wines_train)
}

derr <- data.frame(k=kseq,error=err)
ggplot(data=derr,aes(x=k,y=error)) + geom_line() #+ xlim(0,50)

kopt <- kseq[which.min(err)]

# Application sur la base de test
knn_test <- knn(wines_train[,-1],wines_test[,-1],wines_train$Classe,k=kopt)
mc <- table(wines_test$Class,knn_test)
mc


# Avec les knn pondérés
# knn pondérés
distance <- function(x,y,nom,q=1){
  if (nom == "euclidienne")
  {
    distance <- sqrt(sum((x-y)^2))
  }else if (nom == "minkowski")
  {
    distance <- sum((x-y)^q)^(1/q)
  }else if (nom == "max")
  {
    distance <- max(abs(x-y))
  }else if (nom == "manhattan")
  {
    distance <- sum(abs(x-y))
  }else if (nom == "canberra")
  {
    distance <- sum( (abs(x-y))/(abs(x) + abs(y)) )
  }else
  {
    print("Nom de distance incorrect")
  }
}

noyau <- function(x,nom)
{
  if (nom == "rect"){noyau <- 0.5*(-1 <= x & x <= 1)}
  if (nom == "triang"){noyau <- (1-abs(x))*(-1 <= x & x <= 1)}
  if (nom == "epan"){noyau <- 0.75*(1-x^2)*(-1 <= x & x <= 1)}
  if (nom == "bipoids"){noyau <- (15/16)*(1-x^2)^2*(-1 <= x & x <= 1)}
  if (nom == "tripoids"){noyau <- (35/32)*(1-x^2)^3*(-1 <= x & x <= 1)}
  if (nom == "cos"){noyau <- (pi/4)*cos((pi/2)*x)*(-1 <= x & x <= 1)}
  if (nom == "gauss"){noyau <- dnorm(x)}
  if (nom == "inv"){noyau <- 1/abs(x)}
  if (nom == "exp"){noyau <- (1/2)*exp(-abs(x))}
  return(noyau)
}

knn_pond <- function(k,noyau_name,dist_name,train,test,class){
  n <- nrow(test)
  predict <- rep(0,n)
  proba <- rep(0,n)
  
  train <- as.matrix(train)
  test <- as.matrix(test)
  
  ncl <- max(unclass(class))
  
  affectation <- t(sapply(1:n,FUN = function(i){
    dist <- apply(train,1,FUN = function(x){sqrt(sum((x-test[i,])^2))})
    d_max <- sort(dist)[k] 
    temp <- as.data.frame(cbind(train,class,dist))
    dk <- temp[temp$dist <= d_max,]
    
    if(noyau_name %in% c("rect","triang","epan","bipoids","tripoids","cos")) # on doit renormaliser les distances
    {
      dk$dist <- dk$dist/temp$dist[k+1]
    }
    weight <- noyau(dk$dist,noyau_name)    
    
    proba <- numeric()
    for (c in 1:ncl)
    {
      proba <- c(proba,mean(dk$class==c))
    }
    return(proba)}))  
  
  classement <- apply(affectation,1,FUN = function(x){y<-which(x==max(x));if(length(y)>1){y<-sample(y,1,replace = FALSE)};return(y)})
  predict <- as.data.frame(cbind(test,affectation,classement))
  
  return(predict)
}


Kmax <- nrow(wines_train)-10
kseq <- seq(1,Kmax,1) # valeurs de k à tester
err <- rep(0,length(kseq)) # vecteur qui va contenir les erreurs de mauvais classement pour chaque valeur de k

for (k in 1:length(kseq)){
  for (i in 1:nrow(wines_train)){
    train <- wines_train[-i,]
    test <- wines_train[i,]

    pred <- knn_pond(kseq[k],"epan","max",train[,-1],test[,-1],train$Classe)
    err[k] <- err[k] + (pred$fitted.values!=test$Classe)
  }
  err[k] <- err[k]/nrow(wines_train)
}

derr <- data.frame(k=kseq,error=err)
ggplot(data=derr,aes(x=k,y=error)) + geom_line()# + xlim(0,50)

kopt <- kseq[which.min(err)]

# Application sur la base de test
knn_test <- kknn(as.factor(Classe) ~ ., wines_train, wines_test,distance=2, kernel="rectangular")
mc <- table(wines_test$Class,knn_test)
mc



# Exo 2
# Base de données SPAM
library(kernlab)
data(spam)

library(rpart)
library(rpart.plot)

# On définit les tailles des échantillons d'apprentissage et de test
ntot <- nrow(spam)
ntrain <- floor(0.75*ntot)
ntest <- ntot-ntrain

# On tire au sort parmi toutes les lignes de la base, celles qui feront partie de l'échantillon d'apprentissage
ind_train <- sample(1:ntot,ntrain)
spam_train <- spam[ind_train,]
spam_test <- spam[-ind_train,]

# On utilise la fonction rpart pour construire l'arbre CART
# On fixe cp=0 pour obtenir toute la séquence de sous-arbres emboîtés
par(mfrow=c(1,1))
spam_cart <- rpart(type~.,data=spam_train, cp = 0)
rpart.plot(spam_cart)
printcp(spam_cart)
plotcp(spam_cart)

# On retient la valeur de cp à partir de laquelle l'erreur se stabilise (cp=0.0037 sur cet exemple, mais cela dépend du tirage de l'échantillon ...)
spam_cart_opt <- prune(spam_cart,cp=0.0037)
rpart.plot(spam_cart_opt,extra=3,cex=1)


# Evaluation sur données test
pred_test <- predict(spam_cart_opt,newdata=spam_test,type="class")

# matrice de confusion
mc <- table(pred_test,spam_test$type,dnn=c("Prédit","Vrai"))
# en pourcentages
prop.table(mc)

# taux de mauvais classement
taux_erreur <- 1 - sum(diag(mc))/sum(mc)  # 9.2%
  

