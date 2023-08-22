############## 3ème ETAPE #############

#On charge d'abord la table
library(FactoMineR)

###### ATTENTION #####
## Le chemin doit être changé pour pouvoir fonctionner
table<-read.table("C:/Users/Sandro/Documents/R/AD/Projet/donnees_ocde_modif.csv",header=T,sep=',')

# meme raison qu'expliquée pour l'AFD pour le paramètre ind.sup
pca <- PCA(table, graph = FALSE,ind.sup = c(10,22,23))

#Premier Shiny
#---------------------------
library(explor)
explor(pca)
#---------------------------
#Deuxième Shiny (attention, il faut cliquer sur 
# "Quitter l'application" puis fermer la fenêtre ensuite.
library(Factoshiny)
resshiny = PCAshiny(pca)


# Vous pouvez Ã©galement tÃ©lÃ©charger l'applucation et la modifier
#setwd("Interactive_PCA_Explorer-master") 
#runApp(Interactive_PCA_Explorer) # exÃ©cution de  l'App 
