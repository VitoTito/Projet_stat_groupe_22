#### Preparation ####

rm(list = ls())
set.seed(3011)

## CHARGEMENT DES PACKAGES ##

library(ade4)
library(adegraphics)
library(Hmisc)
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(vcd)
library(openxlsx)

## CHARGEMENT DES DONNEES ##

# setwd("O:/Annee2/stats/Groupe22/Donnees") # Chemin VM
setwd("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/1.Donnees") # mon dossier (Vito)
# setwd("C:/Users/nsdk1/Desktop/R/Projet_stat/Source") # Chemin perso Nathan

base_NE_BEA <- readRDS(file="base_NE_X_varY_BEA.RData")
base_PC_BEA <- readRDS(file="base_PC_X_varY_BEA.RData")
base_Repro_BEA <- readRDS(file="base_Repro_X_varY_BEA.RData")

## CHOIX DE LA BASE ## 

Data <- base_NE_BEA
Data_name <- "base_NE_BEA"

# Data <- base_PC_BEA
# Data_name <- "base_PC_BEA"

# Data <- base_Repro_BEA
# Data_name <- "base_Repro_BEA"

setwd("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22")


p_values_fact <- read_excel("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22/p_values_fact_base_NE_BEA.xlsx")
p_values_num <- read_excel("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22/p_values_num_base_NE_BEA.xlsx")

# # BEA
# p_values_fact <- read_excel("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22/p_values_fact_base_PC_BEA.xlsx")
# p_values_num_BEA <- read_excel("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22/p_values_num_base_PC_BEA.xlsx")
# 
# # Repro
# p_values_fact <- read_excel("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22/p_values_fact_base_Repro_BEA.xlsx")
# p_values_num <- read_excel("C:/Users/Vito/Desktop/Dépôt Projet Statistique 2A/Projet_stat_groupe_22/p_values_num_base_Repro_BEA.xlsx")

str(p_values_fact)

non_na_counts_fact <- rowSums(!is.na(p_values_fact[, -1])) 
count_occurrences_fact <- data.frame(variable = p_values_fact[, 1], non_na_counts_fact = non_na_counts_fact)
count_occurrences_fact <- count_occurrences_fact[order(-count_occurrences_fact$non_na_counts_fact), ]
count_occurrences_fact

non_na_counts_num <- rowSums(!is.na(p_values_num[, -1])) 
count_occurrences_num <- data.frame(variable = p_values_num[, 1], non_na_count = non_na_counts_num)
count_occurrences <- count_occurrences[order(-count_occurrences$non_na_counts_num), ]
count_occurrences

