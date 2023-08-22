


microarray.data.url <- "http://pedagogix-tagc.univ-mrs.fr/courses/ASG1/data/marrays"
expr.url <- file.path(microarray.data.url, "GSE13425_Norm_Whole.txt")
pheno.url<-file.path(microarray.data.url,"phenoData_GSE13425.tab")
expr.matrix<- read.table(expr.url,sep="\t", header = T, row.names = 1)
head(expr.matrix)
pheno.matrix <- read.table(pheno.url,sep="\t", header = T, row.names = 1)
head(pheno.matrix)



sample.labels <- pheno.matrix$sample.labels 
mat<-expr.matrix[,which(sample.labels %in% c("Bh","Bo"))]
cond<-as.factor(as.vector(sample.labels[which(sample.labels %in% c("Bh","Bo"))]))


library(glmnet)
res<-glmnet(t(mat),cond,family="binomial",alpha=1) #LASSO
plot(res,xvar="lambda", main="LASSO")

#alpha=0 ==> RIDGE 
## ATTENTION: on ne peut pas l'utiliser avec les mêmes commandes
# qu'au dessus (avec LASSO)

set.seed(123)

cvres<-cv.glmnet(t(mat),cond,family="binomial",alpha=1,type.measure = "class",nfolds=5,keep=TRUE)
names(cvres)
cvresmat<-cbind(cvres$lambda,cvres$cvm,cvres$nzero)
cvresmat
plot(cvres)
cvres$lambda.min
cvres$lambda.1se

library(gplots)
nonnuls<-which(coef(cvres,lambda=cvres$lambda.1se)!=0) 


library(limma)
groups <- as.factor(pheno.matrix$sample.labels)
design.matrix<-model.matrix(~0+groups)
colnames(design.matrix)<-levels(groups)
fit<-lmFit(expr.matrix,design.matrix)
contrast.matrix<-makeContrasts(Bh-Bo,levels=groups)
fit.contrast<-contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit.contrast)


namesnonnuls<-rownames(expr.matrix)[nonnuls]


adjpval<-p.adjust(fit2$p.value,method="BH")
de<-rownames(expr.matrix)[which(abs(fit2$coefficients>=1)&adjpval<=0.05)]
length(de)
length(which(de %in% namesnonnuls))
de[which(de %in% namesnonnuls)]


depval<-rownames(expr.matrix)[which(adjpval<=0.05)]
length(depval)
length(which(depval %in% namesnonnuls))
depval[which(depval %in% namesnonnuls)]


matde<-as.matrix(expr.matrix[de,])
x11()
heatmap.2(matde,col = redgreen(100)) # library(gplots)
# faut zoomer
heatmap(matde)
heatmap(matde,col=redgreen(100))









