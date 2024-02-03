#### Importation du fichier ####

library(readxl)
library(dplyr)
library(corrplot)
library(vcd)
library(FactoMineR)

set.seed(3011)

setwd("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A")

repro <- read_excel("base_Repro_X_varY_BEA.xlsx")

#### Etape 1 : Nettoyage de la base #### 

# Liste des variables

str(repro, list.len=ncol(repro))

# Mise en format FACTOR pour les variables CHR

colonnes_chr <- sapply(repro, is.character)
repro[, colonnes_chr] <- lapply(repro[, colonnes_chr], as.factor)

# Suppression des données > 15% de valeurs manquantes (NA) :

na_counts <- colSums(is.na(repro)*100/92)
na_counts[na_counts > 15]

# Deux variables avec plus de 15% de valeurs manquantes : on les supprime :

repro <- repro[, -which(names(repro) %in% c("X15x1x2_DEPI1_MAT_rec", "X15x1x2_DEPI1_GEST_rec"))]

# Nombre de modalités par variables : 

occurrences <- sapply(repro, function(x) table(x))
seuil2 <- nrow(repro) * 0.85

# Filtrer les variables qui ont une modalitÃ© dÃ©passant le seuil
variables_a_supprimer <- names(occurrences)[sapply(occurrences, function(x) any(x >= seuil2))]

# Supprimer les variables de la base de donnÃ©es
repro <- repro[, !(names(repro) %in% variables_a_supprimer)]

# Variables  > 2 modalités et suppression potentielle

base_factor_repro <- repro %>% 
  select_if(is.factor)

occurrences2 <- sapply(base_factor_repro, function(x) table(x))
seuil <- 13.8

variables_a_supprimer <- names(occurrences2)[sapply(occurrences2, function(x) any(x <= seuil))]
variables_a_supprimer

# Pas de variables avec une modalité inférieure à 15%

#### Etape 2 : Etude univariée du lien entre la variable Y et les variables X #### 

str(repro, list.len=ncol(repro))

variable_y <- as.factor(repro$y12_BEA_Repro)

# Récupérez le nom des variables de la table
toutes_les_variables <- names(repro)

# Initialisez un dataframe pour stocker les résultats
liste_var <- data.frame(Variable_X = character(), Type_Variable = character(), P_Value = numeric())

# Boucle pour tester chaque variable
for (variable_x in toutes_les_variables) {
  # Excluez la variable Y elle-même
  if (variable_x != "y12_BEA_Repro") {
    # Séparez les variables factorielles et numériques
    if (is.factor(repro[[variable_x]])) {
      # Test du Chi² pour les variables factorielles
      contingency_table <- table(repro[[variable_x]], variable_y)
      chi_square_test <- chisq.test(contingency_table, simulate.p.value = TRUE)
      
      # Vérification de la p-value
      if (chi_square_test$p.value < 0.20) {
        liste_var <- rbind(liste_var, data.frame(Variable_X = variable_x, Type_Variable = "Factorielle", P_Value = chi_square_test$p.value))
      }
    } else if (is.numeric(repro[[variable_x]])) {
      # Test de comparaison de moyennes pour les variables numériques avec une variable Y multivariée
      anova_result <- aov(repro[[variable_x]] ~ variable_y)
      
      # Vérification de la p-value
      if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.20) {
        liste_var <- rbind(liste_var, data.frame(Variable_X = variable_x, Type_Variable = "Numérique", P_Value = summary(anova_result)[[1]][["Pr(>F)"]][1]))
      }
    }
  }
}

# Afficher les variables à conserver
liste_var_etape_2 <- liste_var

repro <- repro[, (names(repro) %in% c(liste_var$Variable_X, 'y12_BEA_Repro'))]


#### Etape 3 : Attribution des données manquantes (NA) ####

nombre_na_par_variable <- colSums(is.na(repro))
nombre_na_par_variable

remplacer_na <- function(col) {
  if (is.factor(col)) {
    # Si la variable est factorielle, attribuez des valeurs en respectant la distribution des réponses
    col[is.na(col)] <- sample(levels(col), sum(is.na(col)), replace = TRUE)
  } else {
    # Si la variable est numérique, attribuez la moyenne
    col[is.na(col)] <- mean(col, na.rm = TRUE)
  }
  return(col)
}

repro <- repro %>% 
  mutate_all(remplacer_na)

#### Etape 4 : Etude univariée du lien entre la variable Y et les variables X après affectation des NA #### 

str(repro, list.len=ncol(repro))

variable_y <- as.factor(repro$y12_BEA_Repro)

# Récupérez le nom des variables de la table
toutes_les_variables <- names(repro)

# Initialisez un dataframe pour stocker les résultats
liste_var <- data.frame(Variable_X = character(), Type_Variable = character(), P_Value = numeric())

# Boucle pour tester chaque variable
for (variable_x in toutes_les_variables) {
  # Excluez la variable Y elle-même
  if (variable_x != "y12_BEA_Repro") {
    # Séparez les variables factorielles et numériques
    if (is.factor(repro[[variable_x]])) {
      # Test du Chi² pour les variables factorielles
      contingency_table <- table(repro[[variable_x]], variable_y)
      chi_square_test <- chisq.test(contingency_table, simulate.p.value = TRUE)
      
      # Vérification de la p-value
      if (chi_square_test$p.value < 0.20) {
        liste_var <- rbind(liste_var, data.frame(Variable_X = variable_x, Type_Variable = "Factorielle", P_Value = chi_square_test$p.value))
      }
    } else if (is.numeric(repro[[variable_x]])) {
      # Test de comparaison de moyennes pour les variables numériques avec une variable Y multivariée
      anova_result <- aov(repro[[variable_x]] ~ variable_y)
      
      # Vérification de la p-value
      if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.20) {
        liste_var <- rbind(liste_var, data.frame(Variable_X = variable_x, Type_Variable = "Numérique", P_Value = summary(anova_result)[[1]][["Pr(>F)"]][1]))
      }
    }
  }
}

# Afficher les variables à conserver
liste_var_etape_4 <- liste_var

setdiff(liste_var_etape_2$Variable_X, liste_var_etape_4$Variable_X)

repro <- repro[, (names(repro) %in% c(liste_var$Variable_X, 'y12_BEA_Repro'))]

#### Etape 5 : Etude des corrélations entre les variables X retenues à p < 0,2 ####

# Récupérer le nom des variables de la table
toutes_les_variables <- names(repro)

# Initialiser une liste pour stocker les résultats
resultats_corr <- list()

# Boucle pour calculer les corrélations
for (variable_x in toutes_les_variables) {
  for (variable_y in toutes_les_variables) {
    # Exclure la variable elle-même
    if (variable_x != variable_y) {
      # Calculer la corrélation de Pearson pour les variables numériques
      if (is.numeric(repro[[variable_x]]) && is.numeric(repro[[variable_y]])) {
        correlation <- cor(repro[[variable_x]], repro[[variable_y]], use = "complete.obs")
        
        # Vérifier la p-value
        if (abs(correlation) > 0.05) {
          resultats_corr[[paste(variable_x, variable_y, sep="_")]] <- list(Variable_X = variable_x, Variable_Y = variable_y, Correlation = correlation)
        }
      }
      # Test du Chi² pour les variables catégorielles
      else if (is.factor(repro[[variable_x]]) && is.factor(repro[[variable_y]])) {
        contingency_table <- table(repro[[variable_x]], repro[[variable_y]])
        chi_square_test <- chisq.test(contingency_table)
        
        # Vérifier la p-value
        if (chi_square_test$p.value < 0.05) {
          resultats_corr[[paste(variable_x, variable_y, sep="_")]] <- list(Variable_X = variable_x, Variable_Y = variable_y, PValue = chi_square_test$p.value)
        }
      }
    }
  }
}

# Compter les occurrences des variables
occurrences <- table(unlist(lapply(resultats_corr, function(x) x$Variable_X)))
sort(occurrences)

quantitative_data <- repro[, sapply(repro, is.numeric)]  # Select only numeric columns
results_acp <- PCA(quantitative_data, scale.unit = TRUE, graph = TRUE)

# Nous observons des variables très fortements corrélées entre elles. On ne garde que quelques variables numériques saillantes

# Préparation des données (ACM)
# data_cat <- repro[, sapply(repro, is.factor)]  # Sélectionne les variables catégorielles
# 
# # Analyse
# results_acm <- MCA(data_cat, graph = TRUE)
# 
# # Interpréter les résultats
# summary(results_acm)
# get_eigenvalue(results_acm)
# dimdesc(results_acm)
# plot.MCA(results_acm)

repro <- repro[, c('X17x1_DELRET_rec', 'Biosec_clust_4levels')]