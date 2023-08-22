## Gazzo Sandro
## Théo Dellouve
getwd()

d <- read.table("C:/Users/Sandro/Documents/R/Méth App/1625Data.txt",sep=',',header=TRUE)

head(d)
summary(d)

# char<-"ABCDEF"
# strsplit(char,"")
# 
# resultat<-strsplit(char,"")[[1]]
# resultat<-unlist(strsplit(char,""))
# 
# resultat



## séparation des chaines de caractère
# on rend les données exploitables

d$Octamer<-as.character(d$Octamer)

variables<-t(sapply(1:nrow(d),FUN=function(i){unlist(strsplit(d[i,1],""))}))


d<-cbind(d$Clived,variables)
d<-as.data.frame(d)
summary(d)
d$V1<-as.factor(as.numeric(d$V1)-1)
names(d)<-c("Clived",paste0("Octamer",1:8))
summary(d)

# création des bases de données test et apprentissage
n <- nrow(d)
id.appr <- sample.int(n,0.80*n)  


data.appr <- d[id.appr,]
data.test <- d[-id.appr,]

summary(data.appr)
summary(data.test)

# on regarde les proportions pour voir si elles sont respectées
prop.table(table(data.appr$Clived))
prop.table(table(data.test$Clived))


# on refait la même chose mais on oblige la proportion
# à avoir dans la variable Clived en fonction du pourcentage
# sélectionnée pour créer la base test et 
# la base d'apprentissage (ici 80%)
# nous aurons ainsi la même proportion dans la base
# d'apprentissage et la abse de test
library(caret)
indtrain<-createDataPartition(d$Clived,p=0.8,list=F)
dtrain<-d[indtrain,]
dtest<-d[-indtrain,]
summary(dtrain)
summary(dtest)
# on regarde les proportions pour voir si elles sont respectées
prop.table(table(dtrain$Clived))
prop.table(table(dtest$Clived))
# ici, nous avons le mêm taux de proportion



# Classificateur constant
# avec proportion identique
table_clived<-table(dtrain$Clived)
mcst<-names(table_clived)[which.max(table_clived)]
te_cst<-mean(dtest$Clived!=mcst)
te_cst

# avec les différentes proportions
table_clived<-table(data.appr$Clived)
mcst<-names(table_clived)[which.max(table_clived)]
te_cst<-mean(data.test$Clived!=mcst)
te_cst



# fonction qui calcule le taux d'erreur
tx_er<- function(pred,vrais){
  mc<-table(pred,vrais)
  l<-1-(sum(diag(mc))/sum(mc))
}

# importation des librairies rpart et rpart.plot
library(rpart)
library(rpart.plot)

#### Arbre de décision CART ####

## Construction des arbres:

# par defaut
mcart<-rpart(Clived~.,data=dtrain)
plotcp(mcart)
predcart<-predict(mcart,newdata=dtest,type="class")
te_cart<-tx_er(predcart,dtest$Clived)
te_cart
rpart.plot(mcart)

confusionMatrix(dtest$Clived,predcart)


#.arbre maximal
mcartmax<-rpart(Clived~.,data=dtrain,cp=0,minbucket=1,maxdepth=30)
plotcp(mcartmax)
predcartmax<-predict(mcartmax,newdata=dtest,type="class")
te_cartmax<-tx_er(predcartmax,dtest$Clived)
te_cartmax
rpart.plot(mcartmax)

# en élaguant selon le compromis coût complexité
mcartcomp<-prune(mcartmax,cp=0.01333)
rpart.plot(mcartcomp)
predcartcomp<-predict(mcartcomp,newdata=dtest,type="class")
te_cartcomp<-tx_er(predcartcomp,dtest$Clived)
te_cartcomp

#arbre à 1 noeud
mdecstump<-rpart(Clived~.,data=dtrain,cp=0,maxdepth=1)
rpart.plot(mdecstump)
preddecstump<-predict(mdecstump,newdata=dtest,type="class")
te_decstump<-tx_er(preddecstump,dtest$Clived)
te_decstump




#### Bagging ####

library(adabag)

## Construction des arbres:

#par défaut
mbag<-bagging(Clived~.,data=dtrain,mfinal = 20)
predbag20<-predict(mbag,newdata=dtest,type="class")
te_bag<-tx_er(predbag20$class,dtest$Clived)
te_bag

# arbre à 1 noeud
mstump<-bagging(Clived~.,data=dtrain,mfinal = 20,control=rpart.control(cp=0,maxdepth = 1, minbucket = 1))
predstump<-predict(mstump,newdata=dtest,type="class")
te_stump<-tx_er(predstump$class,dtest$Clived)
te_stump


# arbre maximal
mdeep<-bagging(Clived~.,data=dtrain,mfinal = 20,control=rpart.control(cp=0,maxdepth = 30, minbucket = 1))
preddeep<-predict(mdeep,newdata=dtest,type="class")
te_deep<-tx_er(preddeep$class,dtest$Clived)
te_deep


## Variation du mfinal sur les 3 arbres précédents
nombre<-c(1,2,5,10,20,50)

# initialisation stockage
stock1<-c()
stock2<-c()
stock3<-c()
indice<-1

# boucle de 10 itérations
for (N in nombre){
  som1<-0
  som2<-0
  som3<-0
  for (i in 1:10){
  mbag<-bagging(Clived~.,data=dtrain,mfinal = N)
  predbagN<-predict(mbag,newdata=dtest,type="class")
  te_bag<-tx_er(predbagN$class,dtest$Clived)
  som1<- som1 + te_bag[1]
  mstump<-bagging(Clived~.,data=dtrain,mfinal = N,control=rpart.control(cp=0,maxdepth = 1, minbucket = 1))
  predstump<-predict(mstump,newdata=dtest,type="class")
  te_stump<-tx_er(predstump$class,dtest$Clived)
  som2<-som2+te_stump[1]
  mdeep<-bagging(Clived~.,data=dtrain,mfinal = N,control=rpart.control(cp=0,maxdepth = 30, minbucket = 1))
  preddeep<-predict(mdeep,newdata=dtest,type="class")
  te_deep<-tx_er(preddeep$class,dtest$Clived)
  som3<-som3+te_deep[1]
  }
  stock1[indice]<-som1/10
  stock2[indice]<-som2/10
  stock3[indice]<-som3/10
  print(c(N,som1/10,som2/10,som3/10))
  indice<-indice+1
}

## Graphe
plot(nombre,stock1,type="b",col="blue",xlab="Nombre de mfinal",ylab="Taux d'erreur",ylim=c(0.05,0.24))
points(nombre,stock2,type="b",col="red")
points(nombre,stock3,type="b",col="green")
nom<-c("Par défaut","A 1 noeud", "Maximal")
legend(x=40,y=0.2,legend = nom,col = c("blue","red","green"), lty=1, lwd=1)


# Effet du nombre d'arbres
effetnbTreesBag <- function(m){
  err <- sapply(1:10, FUN=function(i){
  bag <- bagging(Clived~., data=dtrain, mfinal=m,control=rpart.control(cp=0,maxdepth=30,minbucket=1))
  predbag<- predict(bag,newdata=dtest)
  return(tx_er(dtest$Clived,predbag$class))
  })
}

# boxplot et évolution du taux d'erreur
mval<-c(1,2,5,10,20,50)
err_fn_m_bag<-sapply(mval,FUN=function(m)(effetnbTreesBag(m)))
dmBag<-as.data.frame(err_fn_m_bag)
names(dmBag)<-paste0("m=",mval)
boxplot(dmBag)

plot(mval,apply(err_fn_m_bag,2,mean),type="b")


#### Forest model ####

library(randomForest)

# arbre maximal
mrf<-randomForest(Clived~.,data=dtrain,method="class",ntree=20,mtry=8)
predrf<-predict(mrf,newdata=dtest,type="class")
te_rf<-tx_er(predrf,dtest$Clived)
te_rf


# détermination du choix optimal de mtry
nbmtry<-c(1,2,5,8)

# intialisation stockage
indice<-1
stock<-c()

# 100 itérations
for (N in nbmtry){
  som<-0
  for (i in 1:100){
  mrf<-randomForest(Clived~.,data=dtrain,method="class",ntree=20,mtry=N)
  predrf<-predict(mrf,newdata=dtest,type="class")
  te_rf<-tx_er(predrf,dtest$Clived)
  som<-som+te_rf[1]
  }
  stock[indice]<-som/100
  print(c(N,som/100))
  indice<-indice+1
}

## Graphe
plot(nbmtry,stock,type="b",col="blue",xlab="Nombre de mtry",ylab="Taux d'erreur",ylim=c(0.05,0.1))


# détermination optimal du nombre de ntree par rapport au choix optimal de 
# mtry effectué précédemment
nbntrees<-c(1,2,5,10,20,50,100,200,500)

# initialisation stockage
indice<-1
stock<-c()

# 100 itérations
for (N in nbntrees){
  som<-0
  for (i in 1:100){
    mrf<-randomForest(Clived~.,data=dtrain,method="class",ntree=N,mtry=2)
    predrf<-predict(mrf,newdata=dtest,type="class")
    te_rf<-tx_er(predrf,dtest$Clived)
    som<-som+te_rf[1]
  }
  stock[indice]<-som/100
  print(c(N,som/100))
  indice<-indice+1
}

## Graphe
plot(nbntrees,stock,type="b",col="blue",xlab="Nombre de ntree",ylab="Taux d'erreur",ylim=c(0.04,0.16))


#### Boosting ####

## Adaboost

# arbre à 1 noeud
mbooststump<-boosting(Clived~.,data=dtrain,mfinal=20,control = rpart.control(cp=0,maxdepth = 1,minbucket = 1))
predbooststump<-predict(mbooststump,newdata=dtest,type="class")
te_booststump<-tx_er(predbooststump$class,dtest$Clived)
te_booststump

# arbre maximal
mboostdeep<-boosting(Clived~.,data=dtrain,mfinal=20,control = rpart.control(cp=0,maxdepth = 30,minbucket = 1))
predboostdeep<-predict(mboostdeep,newdata=dtest,type="class")
te_boostdeep<-tx_er(predboostdeep$class,dtest$Clived)
te_boostdeep


## XGBoost
library(xgboost)
library(ade4)

## Réorganisation des données
# transforme les données en dummy variables
class <- as.numeric(d$Clived)
class<-class-1
#ddumy<-cbind(class,model.matrix(~.,data=d)[,-c(1,2)])
ddumy<-cbind(class,model.matrix(~. -1- Clived, data=d))

ddumtrain<-ddumy[indtrain,]
ddumtest<-ddumy[-indtrain,]


dtrainxg <- xgb.DMatrix(as.matrix(ddumtrain[,-1]),label=as.matrix(ddumtrain[,1]))
dtestxg<-xgb.DMatrix(as.matrix(ddumtest[,-1]),label=as.matrix(ddumtest[,1]))
watchlist<-list(train=dtrainxg,test=dtestxg)

# pour les prochains eta, ce sera d'abord arbre à 1 noeud puis arbre maximal:

## eta =0.75
modxgb<- xgb.train(params=list(max_depth=1, eta=0.75, objective="binary:logistic"),data=dtrainxg,nrounds=50,watchlist = watchlist)
modxgb<- xgb.train(params=list(max_depth=30, eta=0.75, objective="binary:logistic"),data=dtrainxg,nrounds=50,watchlist = watchlist)

## eta =0.5
modxgb<- xgb.train(params=list(max_depth=1, eta=0.5, objective="binary:logistic"),data=dtrainxg,nrounds=50,watchlist = watchlist)
modxgb<- xgb.train(params=list(max_depth=30, eta=0.5, objective="binary:logistic"),data=dtrainxg,nrounds=50,watchlist = watchlist)

## eta =0.25
modxgb<- xgb.train(params=list(max_depth=1, eta=0.25, objective="binary:logistic"),data=dtrainxg,nrounds=50,watchlist = watchlist)
modxgb<- xgb.train(params=list(max_depth=30, eta=0.25, objective="binary:logistic"),data=dtrainxg,nrounds=50,watchlist = watchlist)



### avec la base de validation
d1 <- read.table("C:/Users/Sandro/Documents/R/Méth App/746Data.txt",sep=',',header=TRUE)

head(d1)
summary(d1)

# séparation des variables
# comme en début de code
d1$Octamer<-as.character(d1$Octamer)

variables<-t(sapply(1:nrow(d1),FUN=function(i){unlist(strsplit(d1[i,1],""))}))

d1<-cbind(d1$Clived,variables)
d1<-as.data.frame(d1)
summary(d1)
d1$V1<-as.factor(as.numeric(d1$V1)-1)
names(d1)<-c("Clived",paste0("Octamer",1:8))
summary(d1)


# base de validation
d1train<-dtrain
d1valid<-d1
summary(d1train)
summary(d1valid)


### choix du modèle efefctué avec la première base de données
# arbre maximal et bagging
mdeep<-bagging(Clived~.,data=d1train,mfinal = 20,control=rpart.control(cp=0,maxdepth = 30, minbucket = 1))
preddeep<-predict(mdeep,newdata=d1valid,type="class")
te_deep<-tx_er(preddeep$class,d1valid$Clived)
te_deep





