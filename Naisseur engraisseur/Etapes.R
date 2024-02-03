#### Importation des données et préparation ####

# Package #

library(openxlsx)
library(dplyr)
library(tidyr)

# Données 

setwd("C:/Users/nsdk1/Desktop/R/Projet_stat/Source")

base_NE_BEA <- read.xlsx("base_NE_X_varY_BEA.xlsx")

#### Etape 1 : Verification des données ####

# 1.1 #
# Hormis l’identifiant (CODE_ELEVAGE = character), toutes les données doivent être de type Factor ou Numeric

# str(base_NE_BEA, list.len =ncol(base_NE_BEA))

# Convertir les colonnes caractères en facteurs
colonnes_caracteres <- sapply(base_NE_BEA, is.character)
base_NE_BEA[, colonnes_caracteres] <- lapply(base_NE_BEA[, colonnes_caracteres], as.factor)

# str(base_NE_BEA, list.len =ncol(base_NE_BEA))

#Remettre en caractère CODE_ELEVAGE

base_NE_BEA$CODE_ELEVAGE <- as.character(base_NE_BEA$CODE_ELEVAGE)

# str(base_NE_BEA, list.len =ncol(base_NE_BEA))

# 1.2 #
# Suppression de données >15% données manquantes(NA)

na_counts <- colSums(is.na(base_NE_BEA))
na_counts <- as.data.frame(na_counts)
na_counts <- na_counts %>% 
  arrange(desc(na_counts))
#Le max est 11 : 13,75% des lignes

# Supprimer les colonnes ayant plus de 15% de données manquantes
seuil <- nrow(base_NE_BEA) * 0.15 #12
base_NE_BEA <- base_NE_BEA[, colMeans(is.na(base_NE_BEA)) <= seuil]
# Y en a pas

#1.3#
# Suppression de données avec 1 modalité >=85% répondants

occurrences <- sapply(base_NE_BEA, function(x) table(x))
seuil2 <- nrow(base_NE_BEA) * 0.85

# Filtrer les variables qui ont une modalité dépassant le seuil
variables_a_supprimer <- names(occurrences)[sapply(occurrences, function(x) any(x >= seuil2))]

# Supprimer les variables de la base de données
base_NE_BEA <- base_NE_BEA[, !(names(base_NE_BEA) %in% variables_a_supprimer)]


