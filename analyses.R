#### Importation du fichier ####

library(readxl)
library(dplyr)

setwd("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A")

repro <- read_excel("base_Repro_X_varY_BEA.xlsx")

#### Etape 1 : Nettoyage de la base #### 

# Liste des variables

str(repro, list.len=ncol(repro))

# Mise en format FACTOR pour les variables CHR

colonnes_chr <- sapply(repro, is.character)
repro[, colonnes_chr] <- lapply(repro[, colonnes_chr], as.factor)

# Remise en chr de la variable CODE_ELEVAGE

repro$CODE_ELEVAGE <- as.character(repro$CODE_ELEVAGE)

# Suppression des données > 15% de valeurs manquantes (NA) :

na_counts <- colSums(is.na(repro)*100/92)
na_counts[na_counts > 15]

# Deux variables avec plus de 15% de valeurs manquantes : on les supprime :

repro <- repro[, -which(names(repro) %in% c("X15x1x2_DEPI1_MAT_rec", "X15x1x2_DEPI1_GEST_rec"))]

# Nombre de modalités par variables : 

resultats_tableau <- sapply(repro, function(x) table(x))
resultats_tableau

str(resultats_tableau)

for (i in seq_along(resultats_tableau)) {
  # Diviser chaque valeur par 92 et multiplier par 100
  resultats_tableau[[i]] <- (resultats_tableau[[i]] / 92) * 100
}

str(resultats_tableau)

for (col_name in names(resultats_tableau)) {
  if (is.table(resultats_tableau[[col_name]])) {
    values <- unlist(resultats_tableau[[col_name]])
    if (any(values >= 85)) {
      print(paste("Supprimer la variable:", col_name))
      resultats_tableau[[col_name]] <- NULL
    }
  }
}


