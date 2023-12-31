############## 3�me ETAPE #############

#On charge d'abord la table
library(FactoMineR)

###### ATTENTION #####
## Le chemin doit �tre chang� pour pouvoir fonctionner
table<-read.table("C:/Users/Sandro/Documents/R/AD/Projet/donnees_ocde_modif.csv",header=T,sep=',')

# meme raison qu'expliqu�e pour l'AFD pour le param�tre ind.sup
pca <- PCA(table, graph = FALSE,ind.sup = c(10,22,23))

#Premier Shiny
#---------------------------
library(explor)
explor(pca)
#---------------------------
#Deuxi�me Shiny (attention, il faut cliquer sur 
# "Quitter l'application" puis fermer la fen�tre ensuite.
library(Factoshiny)
resshiny = PCAshiny(pca)


# Vous pouvez également télécharger l'applucation et la modifier
#setwd("Interactive_PCA_Explorer-master") 
#runApp(Interactive_PCA_Explorer) # exécution de  l'App 
