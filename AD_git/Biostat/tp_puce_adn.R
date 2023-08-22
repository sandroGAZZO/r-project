library(limma)
#limmaUsersGuide() ## détails de limma

microarray.data.url <- "http://pedagogix-tagc.univ-mrs.fr/courses/ASG1/data/marrays"
expr.url <- file.path(microarray.data.url, "GSE13425_Norm_Whole.txt")
pheno.url<-file.path(microarray.data.url,"phenoData_GSE13425.tab")
expr.matrix<- read.table(expr.url,sep="\t", header = T, row.names = 1)
head(expr.matrix)
pheno.matrix <- read.table(pheno.url,sep="\t", header = T, row.names = 1)
head(pheno.matrix)




library(limma)
groups <- pheno.matrix$sample.labels
class(groups)#on vérifie que c'est un facteur
table(groups)
levels(groups)
design.matrix<-model.matrix(~0+groups)
colnames(design.matrix)<-levels(groups)
design.matrix
fit<-lmFit(expr.matrix,design.matrix)
contrast.matrix<-makeContrasts(Bh-Bo,levels=groups)
fit.contrast<-contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit.contrast)
names(fit2)


hist(fit2$p.value,nclass = 100)
hist(fit2$p.value,nclass = 100, main="Histogramme des p-valeurs",xlab = "p-valeur",ylab="Fréquence")

adjpval<-p.adjust(fit2$p.value,method="BH")
length(which(adjpval<=0.05))

library(ggplot2)

de<-rep("NO",length(adjpval))
de[which(fit2$coefficients>=1&adjpval<=0.05)]<-"UP"
de[which(fit2$coefficients<=-1&adjpval<=0.05)]<-"DOWN"
res<-data.frame(fit2$coefficients,fit2$p.value,de)
colnames(res)<-c("LogRatio","PValue","DE")
g <- ggplot(data<-res,
            aes(x=LogRatio,y=-log10(PValue),col=DE)) +geom_point()+theme_minimal()
g + scale_color_manual(values=c("red", "blue", "green"))






set.seed(123)
ngenes = 10000
nDE = 500
simulstat <- rnorm(ngenes,mean=0,sd=1)
indDE <- sample(1:ngenes,nDE)
simulstat[indDE] <- rnorm(nDE,mean=2,sd=1)
plot(density(simulstat),main="simulated data")
points(sort(simulstat[-indDE]),(1-nDE/ngenes)*dnorm(sort(simulstat[-indDE]),mean=0,sd=1),
       type="l",col=2)
points(sort(simulstat[indDE]),nDE/ngenes*dnorm(sort(simulstat[indDE]),mean=2,sd=1),
       type="l",col=3)
legend("topright",c("simulstat", "N(0,1)", "N(2,1)"), col=1:3, lty=1)


rawpval<-2*(1-pnorm(abs(simulstat)))
DEfound<-which(rawpval<=0.05)
length(DEfound)
TP<-length(which(DEfound%in%indDE))
TP
FP<-length(DEfound)-TP
FP

adjpval<-p.adjust(rawpval,method="BH")
DEfound<-which(adjpval<=0.05)
length(DEfound)
FP<-length(which(!DEfound%in%indDE))
FP

adjpvalbonf<-p.adjust(rawpval,method="bonferroni")
DEfound<-which(adjpvalbonf<=0.05)
length(DEfound)
FP<-length(which(!DEfound%in%indDE))
FP
