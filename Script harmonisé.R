#### Preparation ####

rm(list = ls())
set.seed(3011)

## CHARGEMENT DES PACKAGES ##

library(readxl)
library(dplyr)

## CHARGEMENT DES DONNEES ##

setwd("O:/Annee2/stats/Groupe22/Donn?es")

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

##### Etape 1 : Verification donnees #####

# str(Data, list.len =ncol(Data))

# 1.1  Mettre les variables au bon format

# Data$y13_BEA_NE <- as.factor(Data$y13_BEA_NE)

## 1.2 Supprimer donnees NA > 0.15 ##

seuil <- nrow(Data) * 0.15
Data_NA <- colSums(is.na(Data)) 

# Pour regarder les NA en d?tail
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

# Pour regarder les var supprime en  d?tail
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

#### Etape 1.5 : Regroupement variables ####

# 1.5.1 Base PC

Data <- subset(Data, select = -c(Biosec_clust_PSE_5levels, T01_T_EXT_3, T01_T_EXT_2, T10_PS_EauDebi_1))

Data$T13_ENG_AlimPot_type <- factor(ifelse(is.na(Data$T13_ENG_AlimPot_type), NA, 
                                           ifelse(Data$T13_ENG_AlimPot_type %in% c(0, 1), "0/1", "2")))


Data$T10_PS_EauNbPopPo_1 <- factor(
  ifelse(
    is.na(Data$T10_PS_EauNbPopPo_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$T10_PS_EauNbPopPo_1 %in% c(0, 1),    # Si la valeur est 0 ou 1, la recoder en "0/1"
      "0/1",
      as.character(Data$T10_PS_EauNbPopPo_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)

Data$T10_PS_EauNbPopPo_3 <- factor(ifelse(is.na(Data$T10_PS_EauNbPopPo_3), NA, 
                                          ifelse(Data$T10_PS_EauNbPopPo_3 %in% c(0, 1), "0/1", "2")))



Data$T13_ENG_EauNbPo_1 <- factor(
  ifelse(
    is.na(Data$T13_ENG_EauNbPo_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$T13_ENG_EauNbPo_1 %in% c(0, 1),    
      "0/1",
      as.character(Data$T13_ENG_EauNbPo_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)

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


Data$X07x1_AN_CONST5_mean_1 <- factor(
  ifelse(
    is.na(Data$X07x1_AN_CONST5_mean_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$X07x1_AN_CONST5_mean_1 %in% c(0, 1),    
      "0/1",
      as.character(Data$X07x1_AN_CONST5_mean_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)

Data$Label <- factor(ifelse(is.na(Data$Label), NA, 
                            ifelse(Data$Label %in% c(2, 3), "2/3", "1")))

# 1.5.2 Base Naisseur Engraisseur

Data <- subset(Data, select = -c(X12x1_TRP_BAT_6_reg_rec, X12x1_DET_BAT_1_reg_rec, X03x2_OT_repro_1_rec, 
                                 X03x2_OT_repro_2_rec, X03x2_OT_repro_3_rec, X17x3_AGFINADO_1,  Biosec_clust_4levels,
                                 Biosec_clust_5levels, T10_PS_EauNbPopPo_3, X14x5_NVAC1A_PPS_1, X14x5_NVAC1A_PPS_2,
                                 T10_PS_EauDebi_1, T10_PS_AlimPoNourrLongpPo_1, Biosec_clust_PSE_5levels, clustNaissACP,
                                 T16_BS_PresNidOiFAF, T16_BS_Pres.EssuiMain.y, T02_MAT_CourExt, T02_MAT_TrouCloiEXT,
                                 T02_MAT_TrouToit, T03_MAT_NidOiseau, T06_VG_TrouCloiEXT, T06_VG_TrouToit, T15_QUAR_Insect,
                                 T15_QUAR_Appat, MPSiET))

Data$X12x1_BLRACBD_2_reg <- factor(ifelse(is.na(Data$X12x1_BLRACBD_2_reg), NA, 
                                          ifelse(Data$X12x1_BLRACBD_2_reg %in% c(0, 1), "0/1", "2")))

Data$X12x1_BLRACBD_3_reg <- factor(ifelse(is.na(Data$X12x1_BLRACBD_3_reg), NA, 
                                          ifelse(Data$X12x1_BLRACBD_3_reg %in% c(0, 1), "0/1", "2")))

Data$X12x1_RACavTRP_3_reg_rec <- factor(ifelse(is.na(Data$X12x1_RACavTRP_3_reg_rec), NA, 
                                               ifelse(Data$X12x1_RACavTRP_3_reg_rec %in% c(0, 2), "0/2", "1")))

Data$X06x1_gene_majo_2_id1_rec <- factor(ifelse(is.na(Data$X06x1_gene_majo_2_id1_rec), NA, 
                                                ifelse(Data$X06x1_gene_majo_2_id1_rec %in% c(1, 2), "1/2", "0")))

Data$X14x5_NVAC1A_TR_1 <- factor(ifelse(is.na(Data$X14x5_NVAC1A_TR_1), NA, 
                                        ifelse(Data$X14x5_NVAC1A_TR_1 %in% c(2, 3), "2/3", "0/1")))

Data$X14x5_NVAC1A_TR_3 <- factor(ifelse(is.na(Data$X14x5_NVAC1A_TR_3), NA, 
                                        ifelse(Data$X14x5_NVAC1A_TR_3 %in% c(1, 2), "1/2", "0")))

Data$X17x1_NTRFO_1 <- factor(ifelse(is.na(Data$X17x1_NTRFO_1), NA, 
                                    ifelse(Data$X17x1_NTRFO_1 %in% c(2, 3), "2/3", "0/1")))

Data$T07_VG_EauNbTr_NbT_3 <- factor(ifelse(is.na(Data$T07_VG_EauNbTr_NbT_3), NA, 
                                           ifelse(Data$T07_VG_EauNbTr_NbT_3 %in% c(0, 1), "0/1", "2")))

Data$X17x1_QALMB_HIV_1 <- factor(ifelse(is.na(Data$X17x1_QALMB_HIV_1), NA, 
                                        ifelse(Data$X17x1_QALMB_HIV_1 %in% c(1, 2), "1/2", "0")))

Data$X17x1_QALMB_HIV_2 <- factor(ifelse(is.na(Data$X17x1_QALMB_HIV_2), NA, 
                                        ifelse(Data$X17x1_QALMB_HIV_2 %in% c(2, 3), "2/3", "0/1")))

Data$X09x5_NbREP.JR_1xPorcelet <- factor(ifelse(is.na(Data$X09x5_NbREP.JR_1xPorcelet), NA, 
                                                ifelse(Data$X09x5_NbREP.JR_1xPorcelet %in% c(1, 2), "1/2", "0")))

Data$T10_PS_TpsPres_rec <- factor(ifelse(is.na(Data$T10_PS_TpsPres_rec), NA, 
                                         ifelse(Data$T10_PS_TpsPres_rec %in% c(0, 1), "0/1", "2")))

Data$T10_PS_EauNbPopPo_1 <- factor(
  ifelse(
    is.na(Data$T10_PS_EauNbPopPo_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$T10_PS_EauNbPopPo_1 %in% c(0, 1),    # Si la valeur est 0 ou 1, la recoder en "0/1"
      "0/1",
      as.character(Data$T10_PS_EauNbPopPo_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)

T13_ENG_EauNbPo_1

Data$T13_ENG_EauNbPo_1 <- factor(
  ifelse(
    is.na(Data$T13_ENG_EauNbPo_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$T13_ENG_EauNbPo_1 %in% c(3, 4),    
      "3/4",
      as.character(Data$T13_ENG_EauNbPo_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)

Data$T13_ENG_EauNbPo_2 <- factor(ifelse(is.na(Data$T13_ENG_EauNbPo_2), NA, 
                                        ifelse(Data$T13_ENG_EauNbPo_2 %in% c(1, 2), "1/2", "0")))

Data$T13_ENG_NbPoBd_1 <- factor(ifelse(is.na(Data$T13_ENG_NbPoBd_1), NA, 
                                       ifelse(Data$T13_ENG_NbPoBd_1 %in% c(0, 1), "0/1", "2")))

Data$T13_ENG_NbPoBd_1 <- factor(
  ifelse(
    is.na(Data$T13_ENG_NbPoBd_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$T13_ENG_NbPoBd_1 %in% c(3, 4, 5),    
      "3/4/5",
      as.character(Data$T13_ENG_NbPoBd_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)


Data$X11_FREQ_LAV_1 <- factor(
  ifelse(
    is.na(Data$X11_FREQ_LAV_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$X11_FREQ_LAV_1 %in% c(2, 3),    
      "2/3",
      as.character(Data$X11_FREQ_LAV_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)



Data$X07x1_AN_CONST4_mean_1 <- factor(
  ifelse(
    is.na(Data$X07x1_AN_CONST4_mean_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$X07x1_AN_CONST4_mean_1 %in% c(3, 4),    
      "3/4",
      as.character(Data$X07x1_AN_CONST4_mean_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)


Data$X07x1_AN_CONST5_mean_1 <- factor(
  ifelse(
    is.na(Data$X07x1_AN_CONST5_mean_1),             # Si la valeur est NA, la garder telle quelle
    NA,
    ifelse(
      Data$X07x1_AN_CONST5_mean_1 %in% c(3, 4),    
      "3/4",
      as.character(Data$X07x1_AN_CONST5_mean_1)    # Sinon, laisser les autres valeurs telles quelles
    )
  )
)

Data$Label <- factor(ifelse(is.na(Data$Label), NA, 
                            ifelse(Data$Label %in% c(2, 3), "2/3", "1")))

Data$T16_BS_PresNidOiHangard <- factor(ifelse(is.na(Data$T16_BS_PresNidOiHangard), NA, 
                                              ifelse(Data$T16_BS_PresNidOiHangard %in% c(1, 3), "1/3", "2")))

Data$T16_BS_PresNidOiCouloir <- factor(ifelse(is.na(Data$T16_BS_PresNidOiCouloir), NA, 
                                              ifelse(Data$T16_BS_PresNidOiCouloir %in% c(1, 3), "1/3", "2")))

Data$MAT_Bat <- factor(ifelse(is.na(Data$MAT_Bat), NA, 
                              ifelse(Data$MAT_Bat %in% c(1, 2), "1/2", "0")))

Data$Mode_Stock_Lit2 <- factor(ifelse(is.na(Data$Mode_Stock_Lit2), NA, 
                                      ifelse(Data$Mode_Stock_Lit2 %in% c(1, 2), "1/2", "0")))



#### Etape 2 : Etude lien var Y et var X ####

# 2 .1 #

seuil_sign <- 0.1

Data_fact <- Data %>%
  select_if(is.factor)
variable_cible <- Data_fact$y13_BEA_NE
autres_variables <- Data_fact[, -1]
variables_significatives_p <- list()

# Effectuez le test du chi-deux pour chaque variable 
for (i in seq_along(autres_variables)) {
  variable = autres_variables[[i]]
  chi_squared_result <- tryCatch({
    chisq.test(table(variable_cible, variable))
  }, warning = function(w) {
    # En cas d'avertissement, effectuer un test du chi-deux exact
    exact_test <- chisq.test(table(variable_cible, variable), simulate.p.value = TRUE)
    return(exact_test)
  })
  
  # Verifiez si la p-valeur est inferieure 0.1
  if (chi_squared_result$p.value < seuil_sign) {
    variables_significatives_p[[names(autres_variables)[i]]] <- chi_squared_result$p.value
  }
}

variables_sign_fact <- names(variables_significatives_p)

#### Etape 6 : Presentation Resultats ####

print("variables avec plus de 15% de NA")
print(var_a_suppr)

print("variables factorielles avec modalit? representant plus de 85% donnees")
print(var_a_suppr2)

print("variables factorielles avec plus de 3 modalit? et dont une modalit? repr?sente moins de 15% des donnees")
print(occurrences3)