#### Préparation ####

rm(list = ls())

## CHARGEMENT DES PACKAGES ##

library(readxl)
library(dplyr)

## CHARGEMENT DES DONNEES ##

setwd("O:/Annee2/stats/Groupe22/Données")

base_NE_BEA <- readRDS(file="base_NE_X_varY_BEA.RData")
base_PC_BEA <- readRDS(file="base_PC_X_varY_BEA.RData")
base_Repro_BEA <- readRDS(file="base_Repro_X_varY_BEA.RData")

## CHOIX DE LA BASE ## 

# Data <- base_NE_BEA
# Data <- base_PC_BEA
Data <- base_Repro_BEA

##### Etape 1 : Verification donnees #####

# str(Data, list.len =ncol(Data))

# 1.1  Mettre les variables au bon format

# Data$y13_BEA_NE <- as.factor(Data$y13_BEA_NE)

## 1.2 Supprimer donnees NA > 0.15 ##

seuil <- nrow(Data) * 0.15
Data_NA <- colSums(is.na(Data)) 

# Pour regarder les NA en détail
# Data_NA <- as.data.frame(Data_NA) 
# Data_NA <- Data_NA %>% 
#    arrange(desc(Data_NA))

var_a_suppr <- c(names(Data_NA[Data_NA > seuil]))

Data <- Data %>% # Filtrage
  select(-all_of(var_a_suppr))

rm(Data_NA, seuil)

## 1.3 Supprimer donnees modalite > 0.85 ##

# Ne concerne que les facteurs : 
Data_fact <- Data %>% 
  select_if(is.factor)

occurrences <- sapply(Data_fact, function(x) table(x))
seuil2 <- nrow(Data) * 0.85 
var_a_suppr2 <- names(occurrences)[sapply(occurrences, function(x) any(x >= seuil2))]

# Pour regarder les var supprime en  détail
# supp <- base_NE_BEA[, (names(base_NE_BEA) %in% variables_a_supprimer)]
# occurrences_supp <- sapply(supp, function(x) table(x)) 

Data <- Data %>% # Filtrage
  select(-all_of(var_a_suppr2))

rm(Data_fact,occurrences, seuil2)

## 1.4 Potentiel regroupements ##

Data_fact <- Data %>% 
  select_if(is.factor)

seuil <- nrow(Data) * 0.15
occurrences2 <- sapply(Data_fact, function(x) table(x))
var_a_potent_regroup <- names(occurrences2)[sapply(occurrences2, function(x) any(x <= seuil))]
base_var_regroup <- Data[, (names(Data) %in% var_a_potent_regroup)] 
occurrences3 <- sapply(base_var_regroup, function(x) table(x))

rm(Data_fact, seuil, occurrences2, base_var_regroup)

#### Etape 1.5 : Regroupement variables

# Base PC

Data$T13_ENG_AlimPot_type <- factor(ifelse(is.na(Data$T13_ENG_AlimPot_type), NA, 
                                           ifelse(Data$T13_ENG_AlimPot_type %in% c(0, 1), "0/1", "2")))

Data$T10_PS_EauNbPopPo_1 <- factor(ifelse(is.na(Data$T10_PS_EauNbPopPo_1), NA, 
                                          ifelse(Data$T10_PS_EauNbPopPo_1 %in% c(0, 1), "0/1", "2/3/4")))

Data$T10_PS_EauNbPopPo_3 <- factor(ifelse(is.na(Data$T10_PS_EauNbPopPo_3), NA, 
                                          ifelse(Data$T10_PS_EauNbPopPo_3 %in% c(0, 1), "0/1", "2")))

Data$T13_ENG_EauNbPo_1 <- factor(ifelse(is.na(Data$T13_ENG_EauNbPo_1), NA, 
                                        ifelse(Data$T13_ENG_EauNbPo_1 %in% c(0, 1), "0/1", "2/3/4")))

Data$T13_ENG_NbPoBd_1 <- factor(
  ifelse(
    is.na(Data$T13_ENG_NbPoBd_1),                # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$T13_ENG_NbPoBd_1 %in% c(0, 1),       # Si la valeur est 0 ou 1, la recoder en "0/1"
      "0/1",
      ifelse(
        Data$T13_ENG_NbPoBd_1 %in% c(2, 3),     # Si la valeur est 2 ou 3, la recoder en "2/3"
        "2/3",
        "4/5"                                    # Sinon, recoder les valeurs 4 et 5 ensemble
      )
    )
  )
)


Data$X07x1_AN_CONST5_mean_1 <- factor(ifelse(is.na(Data$X07x1_AN_CONST5_mean_1), NA, 
                                             ifelse(Data$X07x1_AN_CONST5_mean_1 %in% c(0, 1), "0/1", "2/3/4")))

Data$Label <- factor(ifelse(is.na(Data$Label), NA, 
                            ifelse(Data$Label %in% c(2, 3), "2/3", "1")))

Data <- subset(Data, select = -c(Biosec_clust_PSE_5levels, T01_T_EXT_3, T01_T_EXT_2, T10_PS_EauDebi_1))

# Base Naisseur Engraisseur



#### Etape 2 : Etude lien var Y et var X ####
#### Etape 6 : Presentation Resultats ####

print("variables avec plus de 15% de NA")
print(var_a_suppr)

print("variables factorielles avec modalité representant plus de 85% donnees")
print(var_a_suppr2)

print("variables factorielles avec plus de 3 modalité et dont une modalité représente moins de 15% des donnees")
print(occurrences3)