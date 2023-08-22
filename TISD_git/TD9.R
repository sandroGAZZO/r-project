###############
###############
##  Exo 1    ##
###############
###############


library(ridge)
library(glmnet)

chemin<-"G:\\Dropbox\\Portable\\Enseignements\\TISD\\Corrections TDs\\"

donnees<-read.table(paste(chemin,"ozone.dta",sep=""),header = TRUE)

head(donnees)
plot(donnees$maxO3, type="l")
dim(donnees)

##############
#  Ridge     #
##############
## Question 1

resmco<-lm(donnees$maxO3 ~ donnees$T6 + donnees$T9 + donnees$T12 + donnees$T15 
            + donnees$T18 + donnees$Ne6 + donnees$Ne9 + donnees$Ne12 + donnees$Ne15 + donnees$Ne18 
            + donnees$Vx)
summary.lm(resmco)

beta_mco=coef(resmco)

reg1<-lm(donnees$maxO3 ~ donnees$T6 
            + donnees$T18 + donnees$Ne6+ donnees$Vx + donnees$maxO3v)
summary(reg1)
#MSE
var(residuals(reg1))
sum(residuals(reg1)^2)

## Question 2


beta_ridge<-function(X,Y,lambda)
{
  if(length(dim(X))==2)
  {
    X=as.matrix(cbind(rep(1,nrow(X)),X))
  }
  if(length(dim(X))==0)
  {
    X=as.matrix(cbind(rep(1,length(X)),X))
  }
  Y=as.vector(Y)
  return(c(solve(t(X)%*%X+ diag(lambda,ncol(X)))%*% t(X)%*% Y))
}

## Question 3

beta_ridge(donnees$T12,donnees$maxO3,0)
summary(lm(donnees$maxO3 ~ donnees$T12))


## Question 4

beta_ridge(donnees[,4:15],donnees$maxO3,10)

# alpha=0 : ridge
resglmnet=cv.glmnet(as.matrix(donnees[,4:15]),donnees$maxO3, alpha=0,family="gaussian")
print(resglmnet)
beta_glmnet=coef(resglmnet)
resglmnet2=glmnet(as.matrix(donnees[,4:15]),c(donnees$maxO3), alpha=0,lambda=resglmnet$lambda.min,family="gaussian")
coef(resglmnet2)
print(resglmnet2)

plot(beta_ridge(donnees[,4:15],donnees$maxO3,0),type="l")
lines(beta_mco,col="red",lwd=3)
lines(beta_ridge(donnees[,4:15],donnees$maxO3,5),col="blue")
lines(beta_ridge(donnees[,4:15],donnees$maxO3,10),col="orange")
lines(beta_ridge(donnees[,4:15],donnees$maxO3,1),col="purple")
lines(beta_glmnet, col="pink",lwd=2)


#

resridge<-linearRidge(donnees$maxO3 ~ donnees$T6 + donnees$T9 + donnees$T12 + donnees$T15 
            + donnees$T18 + donnees$Ne6 + donnees$Ne9 + donnees$Ne12 + donnees$Ne15 + donnees$Ne18 
            + donnees$Vx , lambda="automatic")

summary(resridge)
beta_fonctionridge=coef(resridge)
lines(beta_fonctionridge, col="green",lwd=2)

## Question 5

dim(donnees)
echappr=donnees[1:80,]
echtest=donnees[81:91,]

resridgeappr<-linearRidge(maxO3 ~ T6 + T9 + T12 + T15 + T18 + Ne6 + Ne9 + Ne12 + Ne15 
                      + Ne18 + Vx , data=echappr, lambda="automatic")
summary(resridgeappr)
coef(resridgeappr)

plot(echappr$maxO3, type="l",lwd=2,xlim=c(0,91))
lines(80:91,echtest$maxO3, type="l",lwd=1)
lines(predict(resridgeappr), col="blue")
lines(80:91,predict(resridgeappr, echtest),col="red")

# MSE
sum((donnees$maxO3-predict(resridge))^2)


## Question 6

# alpha=1, LASSO
resglmnet=cv.glmnet(as.matrix(donnees[,4:15]),donnees$maxO3, alpha=1,family="gaussian")
summary(resglmnet)
lambda=resglmnet$lambda.min
resglmnet2=glmnet(as.matrix(donnees[,4:15]),c(donnees$maxO3), alpha=1,family="gaussian",
                  lambda=lambda)

coef(resglmnet2)


## Question 7
resglmnet3=glmnet(as.matrix(echappr[,4:15]),c(echappr$maxO3), alpha=1,family="gaussian",lambda=lambda)

pred=predict(resglmnet3,as.matrix(echtest[,4:15]),s="lambda.min")

plot(echappr$maxO3, type="l",lwd=2,xlim=c(0,91))
lines(80:91,echtest$maxO3, type="l",lwd=1)
lines(80:91,pred, col="blue")

