#### Importation des données et préparation ####

set.seed(3011)

## Package ##

library(rcompanion)
library(Hmisc)
library(openxlsx)
library(dplyr)
library(tidyr)

## Données ##

setwd("C:/Users/nsdk1/Desktop/R/Projet_stat/Source")

base_NE_BEA <- read.xlsx("base_NE_X_varY_BEA.xlsx")

#### Etape 1 : Verification des données ####

# 1.1 #
# Hormis l’identifiant (CODE_ELEVAGE = character), toutes les données doivent être de type Factor ou Numeric

# str(base_NE_BEA, list.len =ncol(base_NE_BEA))

# Convertir les colonnes caractères en facteurs
colonnes_caracteres <- sapply(base_NE_BEA, is.character)
base_NE_BEA[, colonnes_caracteres] <- lapply(base_NE_BEA[, colonnes_caracteres], as.factor)

str(base_NE_BEA, list.len =ncol(base_NE_BEA))

#Remettre en caractère CODE_ELEVAGE

base_NE_BEA$CODE_ELEVAGE <- as.character(base_NE_BEA$CODE_ELEVAGE)

#Mettre en facteur y13_BEA_NE

base_NE_BEA$y13_BEA_NE <- as.factor(base_NE_BEA$y13_BEA_NE)

# str(base_NE_BEA, list.len =ncol(base_NE_BEA))

# 1.2 #
# Suppression de données >15% données manquantes(NA)

# Verification
# na_counts <- colSums(is.na(base_NE_BEA))
# na_counts <- as.data.frame(na_counts)
# na_counts <- na_counts %>% 
#   arrange(desc(na_counts))
#Le max est 11 : 13,75% des lignes

# Supprimer les colonnes ayant plus de 15% de données manquantes
seuil <- nrow(base_NE_BEA) * 0.15 # Seuil : 12
base_NE_BEA <- base_NE_BEA[, colMeans(is.na(base_NE_BEA)) <= seuil]
# Y en a pas

#1.3#
# Suppression de données avec 1 modalité >=85% répondants

occurrences <- sapply(base_NE_BEA, function(x) table(x))
seuil2 <- nrow(base_NE_BEA) * 0.85 # Seuil : 68

# Filtrer les variables qui ont une modalité dépassant le seuil
variables_a_supprimer <- names(occurrences)[sapply(occurrences, function(x) any(x >= seuil2))]

#Verification
# supp <- base_NE_BEA[, (names(base_NE_BEA) %in% variables_a_supprimer)]
# occurrences_supp <- sapply(supp, function(x) table(x))

# Supprimer les variables 
base_NE_BEA <- base_NE_BEA[, !(names(base_NE_BEA) %in% variables_a_supprimer)]

#1.4 
# Vérification si existence de modalité avec <=15% de répondants
# Si seulement 2 modalités 🡪 supprimer la variable
# Si >2 modalités, étudier si regroupement de modalité est possible : 
#   consulter le tuteur pour son expertise métier pour ce point. 
# Puis réaliser le regroupement en créant une nouvelle variable/ 
#   supprimer la variable avant regroupement de la nouvelle base de données.

base_facteurs <- base_NE_BEA %>%
  select_if(is.factor)

occurrences2 <- sapply(base_facteurs, function(x) table(x))
variables_a_potent_regrouper <- names(occurrences2)[sapply(occurrences2, function(x) any(x <= seuil))]

# base_variable_regroup <- base_NE_BEA[, (names(base_NE_BEA) %in% variables_a_potent_regrouper)]
# occurrences3 <- sapply(base_variable_regroup, function(x) table(x))

# Donner les variables à supprimer et les variables où y aura potentiellement des regroupements

rm(occurrences, occurrences2, colonnes_caracteres, seuil, seuil2)

#### Etape 2 : Etude univariée du lien entre la variable Y et les variables X ####

#2.1 Test chi-deux variable catégorielle

base_facteurs <- base_NE_BEA %>%
  select_if(is.factor)
variable_cible <- base_facteurs$y13_BEA_NE
autres_variables <- base_facteurs[, -1]
variables_significatives_p <- list()

# Effectuez le test du chi-deux pour chaque variable 
for (i in seq_along(autres_variables)) {
  variable = autres_variables[[i]]
  chi_squared_result <- chisq.test(table(variable_cible, variable))
  # Vérifiez si la p-valeur est inférieure à 0.2
  if (chi_squared_result$p.value < 0.2) {
    variables_significatives_p[[names(autres_variables)[i]]] <- chi_squared_result$p.value
  }
}

variables_sign_fact <- names(variables_significatives_p)

#2.2 Test chi-deux exact variable catégorielle

variables_significatives_exact_p <- list()

# Effectuez le test du chi-deux exact pour chaque variable 
for (i in seq_along(autres_variables)) {
  variable = autres_variables[[i]]
  chi_squared_result_exact <- chisq.test(table(variable_cible, variable), simulate.p.value = TRUE)
  # Vérifiez si la p-valeur est inférieure à 0.2
  if (chi_squared_result_exact$p.value < 0.2) {
    variables_significatives_exact_p[[names(autres_variables)[i]]] <- chi_squared_result_exact$p.value
  }
}

variables_sign_fact_2 <- names(variables_significatives_exact_p)

rm(variables_sign_fact_2)
rm(variable_cible, variable, i, autres_variables, chi_squared_result, chi_squared_result_exact, variables_significatives_p, variables_significatives_exact_p)

#2.3

base_numeric <- base_NE_BEA %>%
  mutate(y13_BEA_NE = as.numeric(y13_BEA_NE)) %>% 
  select_if(is.numeric)

## Test de student

# variable_cible2 <- base_numeric$y13_BEA_NE
# 
# resultats_tests_moyenne <- list()
# 
# # Effectuez le test de comparaison de moyenne pour chaque variable numérique
# for (nom_variable in names(base_numeric)) {
#   if ( nom_variable != "y13_BEA_NE") {
#     t_test_result <- t.test(variable_cible2, base_numeric[[nom_variable]])
# 
#     # Vérifiez si la p-valeur est inférieure à 0.2
#     if (t_test_result$p.value < 0.2) {
#       resultats_tests_moyenne[[nom_variable]] <- t_test_result
#     }
#   }
# }
# 
# variables_sign_num <- names(resultats_tests_moyenne)

## Test Anova

variable_cible2 <- base_numeric$y13_BEA_NE

resultats_anova <- list()

# Effectuez le test ANOVA pour chaque variable numérique
for (nom_variable in names(base_numeric)) {
  if (is.numeric(base_numeric[[nom_variable]]) && nom_variable != "y13_BEA_NE") {
    anova_result <- aov(variable_cible2 ~ base_numeric[[nom_variable]])
    
    # Vérifiez si la p-valeur est inférieure à 0.2
    if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.2) {
      resultats_anova[[nom_variable]] <- anova_result
    }
  }
}

variables_sign_num <- names(resultats_anova)

# Resultat très différent en fonction de quel test on utilise, y a qqc de bizarre

rm(resultats_anova, nom_variable, variable_cible2, anova_result)

base_NE_BEA <- base_NE_BEA %>% 
  select(y13_BEA_NE, variables_sign_fact, variables_sign_num)

variables_etape_2 <- names(base_NE_BEA)

rm(base_facteurs, base_numeric)
rm(variables_sign_fact, variables_sign_num)


#### Etape 3 : Attribution des données manquantes ####



nombre_na_par_variable <- colSums(is.na(base_NE_BEA))

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

base_NE_BEA <- base_NE_BEA %>% 
  mutate_all(remplacer_na)

rm(nombre_na_par_variable)

#### Etape 4 : Etude univariée du lien entre la variable Y et les variables X après affectation de na ####

#4.1 Test chi-deux variable catégorielle

base_facteurs <- base_NE_BEA %>%
  select_if(is.factor)
variable_cible <- base_facteurs$y13_BEA_NE
autres_variables <- base_facteurs[, -1]
variables_significatives_p <- list()

# Effectuez le test du chi-deux pour chaque variable 
for (i in seq_along(autres_variables)) {
  variable = autres_variables[[i]]
  chi_squared_result <- chisq.test(table(variable_cible, variable))
  # Vérifiez si la p-valeur est inférieure à 0.2
  if (chi_squared_result$p.value < 0.2) {
    variables_significatives_p[[names(autres_variables)[i]]] <- chi_squared_result$p.value
  }
}

variables_sign_fact <- names(variables_significatives_p)

#4.2 Test chi-deux exact variable catégorielle

variables_significatives_exact_p <- list()

# Effectuez le test du chi-deux exact pour chaque variable 
for (i in seq_along(autres_variables)) {
  variable = autres_variables[[i]]
  chi_squared_result_exact <- chisq.test(table(variable_cible, variable), simulate.p.value = TRUE)
  # Vérifiez si la p-valeur est inférieure à 0.2
  if (chi_squared_result_exact$p.value < 0.2) {
    variables_significatives_exact_p[[names(autres_variables)[i]]] <- chi_squared_result_exact$p.value
  }
}

variables_sign_fact_2 <- names(variables_significatives_exact_p)

rm(variables_sign_fact_2)
rm(variable_cible, variable, i, autres_variables, chi_squared_result, chi_squared_result_exact, variables_significatives_p, variables_significatives_exact_p)

#4.3

base_numeric <- base_NE_BEA %>%
  mutate(y13_BEA_NE = as.numeric(y13_BEA_NE)) %>% 
  select_if(is.numeric) 

## Test de student

# variable_cible2 <- base_numeric$y13_BEA_NE
# 
# resultats_tests_moyenne <- list()
# 
# # Effectuez le test de comparaison de moyenne pour chaque variable numérique
# for (nom_variable in names(base_numeric)) {
#   if (is.numeric(base_numeric[[nom_variable]]) && nom_variable != "y13_BEA_NE") {
#     t_test_result <- t.test(variable_cible2, base_numeric[[nom_variable]])
#     
#     # Vérifiez si la p-valeur est inférieure à 0.2
#     if (t_test_result$p.value < 0.2) {
#       resultats_tests_moyenne[[nom_variable]] <- t_test_result
#     }
#   }
# }

## Test Anova

variable_cible2 <- base_numeric$y13_BEA_NE

resultats_anova <- list()

# Effectuez le test ANOVA pour chaque variable numérique
for (nom_variable in names(base_numeric)) {
  if (is.numeric(base_numeric[[nom_variable]]) && nom_variable != "y13_BEA_NE") {
    anova_result <- aov(variable_cible2 ~ base_numeric[[nom_variable]])
    
    # Vérifiez si la p-valeur est inférieure à 0.2
    if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.2) {
      resultats_anova[[nom_variable]] <- anova_result
    }
  }
}

variables_sign_num <- names(resultats_anova)

# Resultat très différent en fonction de quel test on utilise, y a qqc de bizarre

rm(resultats_anova, nom_variable, variable_cible2, anova_result)

base_NE_BEA <- base_NE_BEA %>% 
  select(y13_BEA_NE, variables_sign_fact, variables_sign_num)

variables_etape_4 <- names(base_NE_BEA)
variables_diff <- variables_etape_2[!(variables_etape_2 %in% variables_etape_4)]

rm(base_facteurs, base_numeric)
rm(variables_sign_fact, variables_sign_num)
rm(variables_etape_2, variables_etape_4)


#### Etape 5 : Etude des corrélations entre les variables X retenues à p<0.20 ####

#5.1 Numérique

base_numeric <- base_NE_BEA %>%
  select_if(is.numeric) 

#Matrice corr
correlation_matrix <- rcorr(as.matrix(base_numeric))

# Extraire les p-values
p_values <- correlation_matrix$P

#Extraire couple variable, corrélation < 0.05
significant_variables <- which(p_values < 0.05, arr.ind = TRUE)
significant_variables <- data.frame(
  Variable1 = rownames(p_values)[significant_variables[, 1]],
  Variable2 = colnames(p_values)[significant_variables[, 2]],
  P_Value = p_values[significant_variables]
)

variables_num_corr <- significant_variables %>%
  filter(P_Value != 0) %>% 
  distinct(Variable1, .keep_all = TRUE)

variables_num_corr <- variables_num_corr$Variable1

rm(base_numeric, correlation_matrix, significant_variables, p_values)

#5.2 Factorielle

base_facteurs <- base_NE_BEA %>%
  select_if(is.factor) %>% 
  select(-y13_BEA_NE)

# Initialiser un data.frame pour stocker les résultats des tests
results_corr <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  ChiSquare = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Boucle pour les tests de corrélation entre variables catégorielles
for (var1 in names(base_facteurs)) {
  for (var2 in names(base_facteurs)) {
    if (var1 != var2) {
      contingency_table <- table(base_NE_BEA[[var1]], base_NE_BEA[[var2]])
      correlation_test <- chisq.test(contingency_table)
      
      if (correlation_test$p.value < 0.05) {
        results_corr <- rbind(results_corr, data.frame(
          Variable1 = var1,
          Variable2 = var2,
          ChiSquare = correlation_test$statistic,
          P_Value = correlation_test$p.value
        ))
      }
    }
  }
}

variables_fact_corr <- results_corr %>%
  filter(P_Value != 0) %>% 
  distinct(Variable1, .keep_all = TRUE)

variables_fact_corr <- variables_fact_corr$Variable1

rm(base_facteurs, correlation_test, contingency_table, var1, var2, results_corr)