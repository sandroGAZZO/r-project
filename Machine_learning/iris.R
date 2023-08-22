data("iris")
summary(iris)

n <- nrow(iris)
id.appr <- sample.int(n,0.50*n)  #sample les entiers et en garde la moitié
# identifiants des numéros de ligne de l'échantillon d'apprentissage

data.appr <- iris[id.appr,]
data.test <- iris[-id.appr,]

summary(data.appr)
summary(data.test)


library("dplyr")
# autre méthode en utilisant le package dplyr
iris <- iris %>% mutate(id = row_number())
data.appr <- iris %>% sample_frac(0.50) # échantillonne 50% de la base
data.test  <- anti_join(iris, data.appr, by = 'id') # sélectionne les éléments de iris qui ne correspondent à aucune valeur de id dans data.appr

summary(data.appr)
summary(data.test)

library("MASS")
iris.afd <- lda(Species ~ ., data=data.appr)
fact.disc <- iris.afd$scaling
fact.disc

test.pred <- predict(iris.afd,data.test)
test.pred.classes <- test.pred$class
confusion.matrix <- table(data.test$Species,test.pred.classes)
tx.bc <- sum(diag(confusion.matrix))/sum(confusion.matrix)
tx.mc <- 1-tx.bc
confusion.matrix



library(ggplot2)
axes=fact.disc

# dans le plan (Sepal.Width,Petal.Width)
coord.facteurs <- as.data.frame(t(axes[row.names(axes)%in%c("Sepal.Width","Petal.Width"),]))
# à partir du vecteur directeur v de coordonnées (a,b) on récupère l'équation cartésienne de la droite correspondante
# bx - ay + c = 0
# on a alors la droite d'équation y = (b/a)x + (c/a) et on connaît a et b
# pour trouver c, on utilise le fait que la droite passe par le centre de gravité du nuage de points
coord.g <- c(x=mean(data.appr$Sepal.Width),y=mean(data.appr$Petal.Width))
# on calcule directement c/a
slope.axe1 <- coord.facteurs$Petal.Width[1]/coord.facteurs$Sepal.Width[1]
intercept.axe1 <- coord.g["y"] - slope.axe1 * coord.g["x"]
slope.axe2 <- coord.facteurs$Petal.Width[2]/coord.facteurs$Sepal.Width[2]
intercept.axe2 <- coord.g["y"] - slope.axe2 * coord.g["x"]


ggplot(data=data.appr,aes(x=Sepal.Width,Petal.Width,color=Species)) + geom_point() + 
  geom_abline(data=NULL, slope = slope.axe1, intercept = intercept.axe1, linetype=2, size=1) +
  geom_abline(data=NULL, slope = slope.axe2, intercept = intercept.axe2, linetype=2, size=0.5)

# on peut ensuite faire de même dans les autres plans formés par les autres combinaisons de variables







