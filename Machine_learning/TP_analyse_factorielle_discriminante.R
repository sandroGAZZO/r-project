# TP AFD

# Exo 1
summary(iris)
library(MASS)
library(dplyr)

# Création d'une base d'apprentissage et d'une base de test.
# Normalement on fait plutôt 80% apprentissage et 20% test (ou 75-25), mais ici
# comme la base de données est petite et que l'on veut une taille suffisante pour
# le test on prend 50%-50%. On pourrait aussi faire de la validation croisée.
n <- nrow(iris) # nb de lignes et donc d'observations
id.appr <- sample.int(n,0.50*n) # tirage aléatoire de numéros de ligne pour constituer l'échantillon d'apprentissage
data.appr <- iris[id.appr,] # sélection des lignes pour l'échantillon d'apprentissage
data.test <- iris[-id.appr,] # sélection des lignes pour l'échantillon test

# autre méthode en utilisant le package dplyr
iris <- iris %>% mutate(id = row_number()) # création d'une variable id égale au numéro de ligne
data.appr <- iris %>% sample_frac(0.50) # échantillonne 50% de la base
data.test  <- anti_join(iris, data.appr, by = 'id') # sélectionne les éléments de iris qui ne correspondent à aucune valeur de id dans data.appr
# on supprime la variable id des deux bases
data.test$id <- NULL
data.appr$id <- NULL

### Analyse factorielle discriminante
# à l'aide de la fonction lda du package MASS
iris.afd <- lda(Species ~ ., data=data.appr) # on veut expliquer Species à l'aide de toutes les autres variables
str(iris.afd) # pour obtenir la structure de l'objet renvoyé par la fonction lda
# on obtient notamment scaling, qui est la matrice contenant les coordonées des facteurs discriminants en fonction
# des variables initiales
fact.disc <- iris.afd$scaling 

# La fonction predict permet d'appliquer le modèle construit précédemment et stocké dans l'objet "iris.afd"
# à une base de données, ici à la base de test
test.pred <- predict(iris.afd,data.test)
# on obtient une liste contenant entre autres la classe prédite
test.pred.classes <- test.pred$class
# pour construire la matrice de confusion, il suffit de croiser les classes prédites avec les vraies classes
confusion.matrix <- table(data.test$Species,test.pred.classes) # table permet de croiser deux variables factorielles
# le taux de bien classés correspond à la proportion d'observations sur la diagonale
tx.bc <- sum(diag(confusion.matrix))/sum(confusion.matrix)
tx.mc <- 1-tx.bc # taux de mal classés


library(ggplot2)
# On veut visualiser les axes discriminants. Pour cela on commence par sélectionner un plan formé par deux
# variables (par exemple Sepal.Width et Petal.Width)
coord.facteurs <- as.data.frame(t(fact.disc[row.names(fact.disc)%in%c("Sepal.Width","Petal.Width"),]))
# on récupère les coordonnées des facteurs discriminants selon ces deux variables
# on a alors les coordonnées des vecteurs directeurs des axes discriminants et on veut en déduire
# l'équation des droites

# à partir du vecteur directeur v de coordonnées (a,b) on récupère l'équation cartésienne de la droite correspondante
# bx - ay + c = 0
# on a alors la droite d'équation y = (b/a)x + (c/a) et on connaît a et b
# pour trouver c, on utilise le fait que la droite passe par le centre de gravité G du nuage de points
# on en déduit c/a = y_G - (b/a)*x_G
coord.g <- c(x=mean(data.appr$Sepal.Width),y=mean(data.appr$Petal.Width))

# la droite a pour équation y = slope*x + intercept avec :
slope.axe1 <- coord.facteurs$Petal.Width[1]/coord.facteurs$Sepal.Width[1]
intercept.axe1 <- coord.g["y"] - slope.axe1 * coord.g["x"]
slope.axe2 <- coord.facteurs$Petal.Width[2]/coord.facteurs$Sepal.Width[2]
intercept.axe2 <- coord.g["y"] - slope.axe2 * coord.g["x"]

# Nuage de points et axes discriminants
ggplot(data=data.appr,aes(x=Sepal.Width,Petal.Width,color=Species)) + geom_point() + 
  geom_abline(data=NULL, slope = slope.axe1, intercept = intercept.axe1, linetype=2, size=1) +
  geom_abline(data=NULL, slope = slope.axe2, intercept = intercept.axe2, linetype=2, size=0.5)


# Exo 2 
# Importation des données (disponibles également dans le package amap)
insect <- read.table("data/insectes.txt")

summary(insect) # 21 dans groupe A, 22 dans groupe B et 31 dans groupe C
apply(insect,2,sd) # variable la plus dispersée : V1, la moins dispersée : V5

# Centres de gravité
g_classes <- sapply(c("A","B","C"),FUN = function(char){apply(insect[insect$V7==char,1:6],2,mean)})
# RQ : on peut faire autrement ici en utilisant une boucle ...
g_tot <- apply(insect[,1:6],2,mean)


# Base d'apprentissage et base de test
n <- nrow(insect)
rows <- sample.int(n,0.80*n,replace = FALSE)
insect.train <- insect[rows,]
insect.test <- insect[-rows,]


# AFD : deux fonctions possibles sous R
# Premiers tests
require(MASS)
afd <- lda(V7~V1+V2+V3+V4+V5+V6,data=insect.train)

require(ade4) # penser à installer ce package avant de le charger !
# discrimin a besoin en entrée d'une matrice de variables orthogonales
# pour obtenir cela on utilise une ACP sur les données, afin de transformer nos variables initiales
# en variables orthogonales
insect.acp <- dudi.pca(insect.train[,1:6], scannf = F, nf = 6) # ACP uniquement sur les 6 variables explicatives
afd2 <- discrimin(insect.acp,as.factor(insect.train$V7), scannf = F, nf = 2) # ajouter "as.factor" pour s'assurer que 
# la variable de classe ets bien prise en compte comme une variable qualitative
plot(afd2) # outil de visualisation


# Différences entre lda et discrimin
afd$svd # "singular-value decomposition" (généralisation de la décomposition en valeurs propres pour des amtrices non carrées)
afd2$eig # valeurs propres
# lda utilise la normalisation "anglo-saxonne" évoquée dans le cours -> valeurs propres dans R+
# discrimin utilise la normalisation du cours -> valeurs propres dans [0,1]. On peut alors utiliser ces valeurs
# comme des mesures (pessimistes) du pouvoir discriminant des axes. Ici c'est élevé donc le pouvoir du discriminant des
# axes est bon. L'interprétation est plus difficile avec lda.


# Performance sur la base de test
class_test <- predict(afd,insect.test)
mc <- table(class_test$class,insect.test$V7) # matrice de confusion


# Exercice 3
# Sur les chiffres MNIST
digits <- readRDS("digits.rds")

# Tracé de quelques images
par(mfrow=c(2,5)) # découpage de la fenêtre graphique en un tableau de 2 lignes et 10 colonnes : on va donc tracer 20 graphes
id <- sample(1:3000,10) # tirage aléatoire de 20 numéros entre 1 et 3000 (on tire au sort des numéros de ligne de la matrice digits$x)
# on va maintenant tracer chacune de ces images
for (i in id)
{
  m <- matrix(digits$x[i,],nr=28,byrow = TRUE) # on extrait la ligne i de la matrice digits$x, puis on transforme le  vecteur obtenu en une matrice de taille 28x28 (voir énoncé)
  m2 <- apply(m, 2, rev) # on inverse les coordonnées, c'est-à-dire que la 1ère ligne devient la dernière, la 2ème devient l'avant-dernière, ...
  image(t(m2),col=grey.colors(255)) # on affiche la transposée de la matrice obtenue, en échelle de gris
}


# AFD sur échantillon apprentissage
require(MASS)
afd_digits <- lda(digits$x,digits$y,scores=TRUE) # ne fonctionne pas -> message d'erreur lié à la colinéarité de certaines variables (correspondant aux bordures noires communes à tous les individus)

require(ade4)
afd_digits <- discrimin(dudi.pca(digits$x, scannf = FALSE), as.factor(digits$y)) # permet de mieux gérer et de contourner le problème précédent
plot(afd_digits)

# Coordonnées pour passer du repère initial au repère formé par les axes discriminants
scores <- afd_digits$fa

# Calcul des coordonnées des points de l'échantillon d'apprentissage dans le plan discriminant
# la matrice "fa" produite par la fonction discrimin nous donne les coefficients de la combinaison linéaire des variables initiales normalisées 
# permettant d'obtenir les variables discriminantes
# fa peut être vue comme une matrice de passage
# Pour passer des coordonnées dans le système des 784 variables initiales aux coordonnées dans le système des 2 variables discriminantes,
# on applique la matrice fa aux variables normalisées (via la fonction scalewt, mais on peut aussi les normaliser à la main)
coord_x_new <- apply(t(scalewt(digits$x)),2,function(v){as.vector(t(afd_digits$fa)%*%v)})
coord_x_new <- t(coord_x_new) # pour avoir une matrice de dimension 3000*2

# rappel : la fonction apply avec l'argument "2" fait une boucle sur les colonnes de la matrice.


# Calcul des coordonnées des points de l'échantillon test dans le plan discriminant
coord_xt_new <- apply(t(scalewt(digits$xt)),2,function(v){as.vector(t(afd_digits$fa)%*%v)})
coord_xt_new <- t(coord_xt_new) # pour avoir une matrice de dimension 3000*2


# Coordonnées des centres de gravité des classes, dans l'ancien système (celui des 784 variables) et dans le nouveau (celui des 2 variables discriminantes)
g1 <- apply(digits$x[digits$y==1,],2,mean)
g2 <- apply(digits$x[digits$y==7,],2,mean)
g3 <- apply(digits$x[digits$y==8,],2,mean)
g_x <- rbind(g1,g2,g3)

g_x_new <- apply(t(scalewt(g_x)),2,function(v){as.vector(t(afd_digits$fa)%*%v)})


# Distance entre chaque point de l'échantillon test et les centres de gravité
# on calcule d'abord la différence entre la coordonnée 1 de xt et la coordonnée 1 de chaque centre de gravité, puis on fait de 
# même pour la coordonnée 2
dist_to_g1 <- cbind(coord_xt_new[,1]-g_x_new[1,1],coord_xt_new[,2]-g_x_new[2,1])
dist_to_g2 <- cbind(coord_xt_new[,1]-g_x_new[1,2],coord_xt_new[,2]-g_x_new[2,2])
dist_to_g3 <- cbind(coord_xt_new[,1]-g_x_new[1,3],coord_xt_new[,2]-g_x_new[2,3])
# on obtient une matrice de taille 1500x2, où 1500 est le nombre d'individus dans l'échantillon test

# pour chaque individu, c'est-à-dire pour chaque ligne de la matrice précédente, on élève au carré les élèments du vecteur ainsi obtenu
# puis on calcule la somme -> on obtient un vecteur de taille 1500 qui contient la distance entre chaque individu et le centre de gravité de la classe
dist2_to_g1 <- apply(dist_to_g1^2,1,sum)
dist2_to_g2 <- apply(dist_to_g2^2,1,sum)
dist2_to_g3 <- apply(dist_to_g3^2,1,sum)

# on regroupe dans une même matrice ces trois vecteurs, que l'on transform ensuite en data frame
dist <- cbind(dist2_to_g1,dist2_to_g2,dist2_to_g3)
dist <- as.data.frame(dist)
# on calcule la distance minimale : pour chaque ligne, on regarde dans quelle colonne se trouve le minimum
# si c'est en colonne 1 -> on est plus près du groupe 1, si c'est en colonne 2, on est plus près du groupe 2, ...
dist$min_dist <- apply(dist[,1:3],1,min) # calcul du minimum par ligne (apply avec argument "1")
dist$class <- rep(0,nrow(dist))
for (i in 1:nrow(dist))
{
  if (dist$min_dist[i]==dist$dist2_to_g1[i]){
    dist$class[i] <- 1
  }else if (dist$min_dist[i]==dist$dist2_to_g2[i]){
    dist$class[i] <- 7
  }else if (dist$min_dist[i]==dist$dist2_to_g3[i]){
    dist$class[i] <- 8
  }
}

# tableau croisé des variables dist$class et digits$yt : la première est le numéro de classe prédit par notre méthode, et
# la deuxième est la vraie classe 
table(dist$class,digits$yt,dnn=c("Prédiction","Vraie classe"))

# Graphes
plot(coord_x_new,pch=19,col=digits$y) # échantillon apprentissage

plot(coord_xt_new,col=digits$yt,pch=dist$class) # échantillon test -> la couleur représente la vraie classe et le symbole représente celle estimée par notre méthode
legend(x=-5.5,y=7,legend=unique(as.factor(digits$yt)),lty=1,col=unique(digits$yt),bty="n",title="Vraie classe")
legend(x=-4,y=7,legend=unique(as.factor(sort(dist$class))),pch=unique(sort(dist$class)),bty="n",title="Classe prédite")


# Exemple de chiffres mal classés
mal_classes <- cbind(digits$xt[digits$yt!=dist$class,],digits$yt[digits$yt!=dist$class],dist$class[digits$yt!=dist$class])
indices <- sample(1:dim(mal_classes)[1],20)
par(mfrow=c(4,5))
for (i in indices)
{
  m <- matrix(mal_classes[i,1:784],nr=28,byrow = TRUE) # on extrait la ligne i de la matrice digits$x, puis on transforme le  vecteur obtenu en une matrice de taille 28x28 (voir énoncé)
  m2 <- apply(m, 2, rev) # on inverse les coordonnées, c'est-à-dire que la 1ère ligne devient la dernière, la 2ème devient l'avant-dernière, ...
  image(t(m2),col=grey.colors(255), main=paste("Vraie classe : ",mal_classes[i,785]," prédiction : ",mal_classes[i,786],sep="")) # on affiche la transposée de la matrice obtenue, en échelle de gris
}


