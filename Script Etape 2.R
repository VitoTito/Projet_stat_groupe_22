

load("P:/PROJET STAT 2A/R/3_tables_filtres.RData")


# Selection de la table
###------------------------------------
#Data <- Data_Repro
#Data <- Data_PC
Data <- Data_NE
###------------------------------------

#################################################################
### Détection des types des variables ###
#################################################################

colonnes_numeriques <- names(Data)[sapply(Data, is.numeric)]
colonnes_factor <- names(Data)[sapply(Data, is.factor)]


#################################################################
###   Test des variables numériques ###
#################################################################

variable_a_supprimer <- c()


for (var in colonnes_numeriques) {
  if(t.test(x=Data[var], y= Data[,2])$p.value >= 0.2){
    variable_a_supprimer <- unique(c(variable_a_supprimer, var))
  }
}

variable_a_supprimer


#################################################################
###   Test des variables factor ###
#################################################################

# En travaux /!\ :::: .?.???.....

for (var in colonnes_factor) {
  if(chisq.test(x=Data[var], y= Data[,2])$p.value >= 0.2){
    variable_a_supprimer <- unique(c(variable_a_supprimer, var))
  }
}


fisher.test(Data["X02x1_NBANDE_class"])

