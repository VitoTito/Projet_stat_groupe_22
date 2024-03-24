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

## Chemin ##

# setwd("O:/Annee2/stats/Groupe22/Donnees") # Chemin VM
# setwd("C:/Users/Vito/Desktop/D?p?t Projet Statistique 2A/1.Donnees") # mon dossier (Vito)
setwd("C:/Users/nsdk1/Desktop/R/Projet_stat/Source") # Chemin perso Nathan

## CHOIX DE LA BASE ## 

# Data_name <- "base_NE_BEA"

# Data_name <- "base_PC_BEA"

Data_name <- "base_Repro_BEA"

## Import des bases script harmonisé 1

# NE
if (Data_name == "base_NE_BEA") {
  base_NE_BEA <- read_excel("base_NE_BEA.xlsx")
p_values_fact <- read_excel("p_values_fact_base_NE_BEA.xlsx")
p_values_num <- read_excel("p_values_num_base_NE_BEA.xlsx")
Data <- base_NE_BEA
}

# PC
if (Data_name == "base_PC_BEA") {
  base_PC_BEA <- read_excel("base_PC_BEA.xlsx")
  p_values_fact <- read_excel("p_values_fact_base_PC_BEA.xlsx")
  p_values_num_BEA <- read_excel("p_values_num_base_PC_BEA.xlsx")
  Data <- base_PC_BEA
}

# Repro
if (Data_name == "base_Repro_BEA") {
  base_Repro_BEA <- read_excel("base_Repro_BEA.xlsx")
  p_values_fact <- read_excel("p_values_fact_base_Repro_BEA.xlsx")
  p_values_num <- read_excel("p_values_num_base_Repro_BEA.xlsx")
  Data <- base_Repro_BEA
}



#### 2 Compte des p-values significatives ####

## 2 .1 Compte des p-values significatives (variables factorielles)
non_na_counts_fact <- rowSums(!is.na(p_values_fact[, -1])) 
count_occurrences_fact <- data.frame(variable = p_values_fact[, 1], non_na_counts_fact = non_na_counts_fact)
count_occurrences_fact <- count_occurrences_fact[order(-count_occurrences_fact$non_na_counts_fact), ]

## 2.2 Compte des p-values significatives (variables num?riques)
non_na_counts_num <- rowSums(!is.na(p_values_num[, -1])) 
count_occurrences_num <- data.frame(variable = p_values_num[, 1], non_na_counts_num = non_na_counts_num)
count_occurrences_num <- count_occurrences_num[order(-count_occurrences_num$non_na_counts_num), ]

#### 3 Data tri?e selon les meilleurs corr?lations ####

columns_to_keep_fact <- count_occurrences_fact[1: 20, 1]
columns_to_keep_num <- count_occurrences_num[1: 15, 1]

Data_filtered_fact <- Data[, colnames(Data) %in% columns_to_keep_fact ]
Data_filtered_fact <- lapply(Data_filtered_fact, factor)
Data_filtered_num <- Data[, colnames(Data) %in% columns_to_keep_num ]
Data_filtered_num_list <- list(Data_filtered_num)

dudiY <- dudi.pca(Data_filtered_num, center = TRUE,scale = TRUE, scannf = FALSE)
ktabX.data <- ktab.list.df(Data_filtered_num_list)

resmbpls <- mbpls(dudiY, ktabX.data, scale = TRUE, option = "uniform", scannf = FALSE)
summary(resmbpls)
plot(resmbpls)

test <- testdim(resmbpcaiv, nrepet = 10)
plot(test)

test <- randboot(resmbpcaiv, optdim = 4, nrepet = 10)
plot(test$bipc)
