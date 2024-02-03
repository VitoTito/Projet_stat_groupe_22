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

resultats_tableau

check_threshold <- function(table) {
  any(table >= 85)
}

resultats_tableau <- resultats_tableau %>%
  select(-where(~check_threshold(.)))

# Afficher le résultat
print(resultats_tableau)
