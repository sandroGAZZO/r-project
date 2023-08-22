
data("ToothGrowth")
t<-ToothGrowth
summary(t)
# variable 1 et 3 quantitative
# variable 2 = qualitative


attach(ToothGrowth)
supp<-as.factor(supp)
dose<-as.factor(dose)

interaction.plot(dose,supp,len)

# reg mixte

L_2<-lm(len~supp+dose-1, contrasts=list(supp='contr.sum',dose='contr.sum'))
summary(L_2)

L<-lm(len~supp*dose-1, contrasts=list(supp='contr.sum',dose='contr.sum'))
summary(L)



anova(L)

#----------------------




data("PlantGrowth")
p<-PlantGrowth
summary(p) # var 1 = quanti, var 2 = quali

#Y= weight = quanti
#X=group =quali

boxplot(p)

rlm<-glm(p$weight~p$group)
summary(rlm)

lm(p$weight~p$group-1)
# -1 enleve intercept, coef = esti mean
