#### Preparation ####

rm(list = ls())
set.seed(3011)

## CHARGEMENT DES PACKAGES ##

library(Hmisc)
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(vcd)
library(openxlsx)
library(readxl)
library(reshape2)

## CHARGEMENT DES DONNEES ##

# setwd("O:/Annee2/stats/Groupe22/Donnees") # Chemin VM
# setwd("C:/Users/Vito/Desktop/D?p?t Projet Statistique 2A/1.Donnees") # mon dossier (Vito)
setwd("C:/Users/nsdk1/Desktop/R/Projet_stat/Source") # Chemin perso Nathan

base_NE_BEA <- readRDS(file="base_NE_X_varY_BEA.RData")
base_PC_BEA <- readRDS(file="base_PC_X_varY_BEA.RData")
base_Repro_BEA <- readRDS(file="base_Repro_X_varY_BEA.RData")

## CHOIX DE LA BASE ## 

# Data <- base_NE_BEA
# Data_name <- "base_NE_BEA"

# Data <- base_PC_BEA
# Data_name <- "base_PC_BEA"

Data <- base_Repro_BEA
Data_name <- "base_Repro_BEA"

##### Etape 1 : Verification donnees #####

# str(Data, list.len =30)

# 1.1  Mettre les variables au bon format

Data[,2] <- as.factor(Data[,2])

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



# Et pour les numeriques dont la variance est nulle : 

colonnes_numeriques <- names(Data)[sapply(Data, is.numeric)]
var_num_a_suppr<-c()

for (col in colonnes_numeriques){
  if (var(Data[[col]], na.rm=TRUE) ==0) {
    var_num_a_suppr <- c(var_num_a_suppr,col)
  }}

Data <- Data %>% # Filtrage
  select(-all_of(var_num_a_suppr))


rm(var_num_a_suppr, colonnes_numeriques, col)


## 1.4 Potentiel regroupements ##

Data_fact <- Data %>% 
  select_if(is.factor)

seuil <- nrow(Data) * 0.15
occurrences2 <- sapply(Data_fact, function(x) table(x))
var_a_potent_regroup <- names(occurrences2)[sapply(occurrences2, function(x) any(x <= seuil))]
base_var_regroup <- Data[, (names(Data) %in% var_a_potent_regroup)] 
occurrences3 <- sapply(base_var_regroup, function(x) table(x))

rm(Data_fact, seuil, occurrences2, base_var_regroup, occurrences3)


## Etape 1.5 : Regroupement variables ##

print("Attention au choix de la base")

"██████╗░░█████╗░
██╔══██╗██╔══██╗
██████╔╝██║░░╚═╝
██╔═══╝░██║░░██╗
██║░░░░░╚█████╔╝
╚═╝░░░░░░╚════╝░"

#### Regroupement base PC ####

if (Data_name == "base_PC_BEA") {
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
}



"███╗░░██╗███████╗
████╗░██║██╔════╝
██╔██╗██║█████╗░░
██║╚████║██╔══╝░░
██║░╚███║███████╗
╚═╝░░╚══╝╚══════╝"

#### Regroupement base NE ####

if (Data_name == "base_NE_BEA") {
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
  
  
  
  Data$X14x5_NVAC1A_TR_1 <- factor(
    ifelse(
      is.na(Data$X14x5_NVAC1A_TR_1),             # Si la valeur est NA, la garder telle quelle
      NA,
      ifelse(
        Data$X14x5_NVAC1A_TR_1 %in% c(2, 3),    
        "2/3",
        as.character(Data$X14x5_NVAC1A_TR_1)    # Sinon, laisser les autres valeurs telles quelles
      )
    )
  )
  
  
  Data$X14x5_NVAC1A_TR_3 <- factor(ifelse(is.na(Data$X14x5_NVAC1A_TR_3), NA, 
                                          ifelse(Data$X14x5_NVAC1A_TR_3 %in% c(1, 2), "1/2", "0")))
  
  
  Data$X17x1_NTRFO_1 <- factor(
    ifelse(
      is.na(Data$X17x1_NTRFO_1),             # Si la valeur est NA, la garder telle quelle
      NA,
      ifelse(
        Data$X17x1_NTRFO_1 %in% c(2, 3),    
        "2/3",
        as.character(Data$X17x1_NTRFO_1)    # Sinon, laisser les autres valeurs telles quelles
      )
    )
  )
  
  Data$T07_VG_EauNbTr_NbT_3 <- factor(ifelse(is.na(Data$T07_VG_EauNbTr_NbT_3), NA, 
                                             ifelse(Data$T07_VG_EauNbTr_NbT_3 %in% c(0, 1), "0/1", "2")))
  
  Data$X17x1_QALMB_HIV_1 <- factor(ifelse(is.na(Data$X17x1_QALMB_HIV_1), NA, 
                                          ifelse(Data$X17x1_QALMB_HIV_1 %in% c(1, 2), "1/2", "0")))
  
  
  Data$X17x1_QALMB_HIV_2 <- factor(
    ifelse(
      is.na(Data$X17x1_QALMB_HIV_2),             # Si la valeur est NA, la garder telle quelle
      NA,
      ifelse(
        Data$X17x1_QALMB_HIV_2 %in% c(2, 3),    
        "2/3",
        as.character(Data$X17x1_QALMB_HIV_2)    # Sinon, laisser les autres valeurs telles quelles
      )
    )
  )
  
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
}




"███████╗████████╗░█████╗░██████╗░███████╗ ██████╗░
██╔════╝╚══██╔══╝██╔══██╗██╔══██╗██╔════╝  ╚════██╗
█████╗░░░░░██║░░░███████║██████╔╝█████╗░░  ░░███╔═╝
██╔══╝░░░░░██║░░░██╔══██║██╔═══╝░██╔══╝░░  ██╔══╝░░
███████╗░░░██║░░░██║░░██║██║░░░░░███████╗  ███████╗
╚══════╝░░░╚═╝░░░╚═╝░░╚═╝╚═╝░░░░░╚══════╝  ╚══════╝"

#### Etape 2 : Etude lien var Y et var X ####

# 2 .1 # Significativite variable fact (Nathan)

seuil_sign <- 0.1

Data_fact <- Data %>%
  select_if(is.factor)

variable_cible <- Data_fact[,1]
autres_variables <- Data_fact[, -1]
variables_significatives_p <- list()
p_val_fact_etape2<-data.frame(var= c(""),
                              p_val_etape2=c(""))


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
  
  nom_variable_i <- names(autres_variables)[i]
  
  p_val_fact_etape2<-rbind(p_val_fact_etape2,c(nom_variable_i,chi_squared_result$p.value))
  
  
  # Verifiez si la p-valeur est inferieure 0.1
  if (chi_squared_result$p.value < seuil_sign) {
    variables_significatives_p[[nom_variable_i]] <- chi_squared_result$p.value
  }
}

variables_sign_fact <- names(variables_significatives_p)

#p_value
# print(p_val_fact_etape2)


rm(Data_fact, chi_squared_result, i, variable, variable_cible, autres_variables, variables_significatives_p, nom_variable_i)


# 2.2 # Significativite variable num (Maxime)

colonnes_numeriques <- names(Data)[sapply(Data, is.numeric)]
test_rate <-c()

p_val_num_etape2 <- data.frame(var= c(""),p_val_etape2=c(""))

for (var in colonnes_numeriques) {
  
  #Test de normalité
  shapiro_test_result <- shapiro.test(Data[[var]])
  
  if (shapiro_test_result$p.value >= 0.05) {  #Perform ANOVA TEST
    
    anova_test_result<- aov(Data[[var]] ~ Data[[2]])
    p_value_anova <- summary(anova_test_result)[[1]][["Pr(>F)"]][1]
    
    p_val_num_etape2 <- rbind(p_val_num_etape2, c(var, p_value_anova ))
    
    if (p_value_anova >= seuil_sign) { #Pour NE les 4 variables "normal" passent le test
      test_rate <- unique(c(test_rate, var))  
    }}
  
  else{  # Perform Kruskal-Wallis test
    kruskal_test_result <- kruskal.test(Data[[var]] ~ Data[[2]])
    
    p_val_num_etape2 <- rbind(p_val_num_etape2, c(var, kruskal_test_result$p.value ))
    
    if (kruskal_test_result$p.value >= seuil_sign) {
      test_rate <- unique(c(test_rate, var))  
      
    }}
}

variables_sign_num <- setdiff(colonnes_numeriques, test_rate)

rm(kruskal_test_result, test_rate, shapiro_test_result, anova_test_result, p_value_anova, var, colonnes_numeriques)


# ### AFFICHER UN HISTOGRAMME
 # plot_data <- data.frame(X = Data[["T03_MAT_AgePo"]],  #On peut choisir la variable ici
 #                          Color = as.factor(Data[[2]]))
 # 
 # mu <- aggregate(X ~ Color, data = plot_data, mean)
 # #Création du graphique
 #  p <- ggplot(plot_data, aes(x = X, fill = Color)) +
 #    geom_histogram(color = "white", position = "stack", bins = 50) +
 #    labs(x = "A03_sdSeroTTg", y = "Fréquence") +
 #    geom_vline(data = mu, aes(xintercept = X, color = Color), alpha = 0.8, linetype = "dashed") +
 #    geom_text(data = mu, aes(x = X + 0.1, y = 30, label = paste("Moyenne:", round(X, 2))), color = "black", size = 3, vjust = -1) +
 #    theme_minimal()
 #  print(p)


## 2.3 ## Base avec variables significative
# 
Data <- Data %>%
  select(1:2, all_of(variables_sign_fact), all_of(variables_sign_num))

"███████╗████████╗░█████╗░██████╗░███████╗  ██████╗░
██╔════╝╚══██╔══╝██╔══██╗██╔══██╗██╔════╝  ╚════██╗
█████╗░░░░░██║░░░███████║██████╔╝█████╗░░  ░█████╔╝
██╔══╝░░░░░██║░░░██╔══██║██╔═══╝░██╔══╝░░  ░╚═══██╗
███████╗░░░██║░░░██║░░██║██║░░░░░███████╗  ██████╔╝
╚══════╝░░░╚═╝░░░╚═╝░░╚═╝╚═╝░░░░░╚══════╝  ╚═════╝░"

#### Etape 3 : Attribution des donnees manquantes ####

nombre_na_par_variable <- colSums(is.na(Data))

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

Data <- Data %>%
  mutate_at(-1, remplacer_na)

rm(nombre_na_par_variable)

#### Etape 4 : Etude univariée du lien entre la variable Y et les variables X après affectation de na ####

# 4.1 # Significativite variable fact 

seuil_sign <- 0.1

Data_fact <- Data %>%
  select_if(is.factor)

variable_cible <- Data_fact[,1]
autres_variables <- Data_fact[, -1]
variables_significatives_p <- list()
p_val_fact_etape4<-data.frame(var= c(""),
                              p_val_etape4=c(""))


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
  
  nom_variable_i <- names(autres_variables)[i]
  p_val_fact_etape4<-rbind(p_val_fact_etape4,c(nom_variable_i,chi_squared_result$p.value))
  
  
  # Verifiez si la p-valeur est inferieure 0.1
  if (chi_squared_result$p.value < seuil_sign) {
    variables_significatives_p[[nom_variable_i]] <- chi_squared_result$p.value
  }
}

variables_sign_fact2 <- names(variables_significatives_p)

variables_significatives_p <- as.data.frame(variables_significatives_p)
variables_significatives_p <- melt(variables_significatives_p) # Le package reshape2 et la fonction melt héros de la nation

#p_value

pval_fact_sign <- full_join(p_val_fact_etape2, p_val_fact_etape4)

rm(p_val_fact_etape2, p_val_fact_etape4)
rm(Data_fact, chi_squared_result, i, variable, variable_cible, autres_variables,nom_variable_i)


# 4.2 # Significativite variable num (Maxime)

colonnes_numeriques <- names(Data)[sapply(Data, is.numeric)]
test_rate <-c()

p_val_num_etape4 <- data.frame(var= c(""),p_val_etape4=c(""))

for (var in colonnes_numeriques) {
  
  #Test de normalité
  shapiro_test_result <- shapiro.test(Data[[var]])
  
  if (shapiro_test_result$p.value >= 0.05) {  #Perform ANOVA TEST
    
    anova_test_result<- aov(Data[[var]] ~ Data[[2]])
    p_value_anova <- summary(anova_test_result)[[1]][["Pr(>F)"]][1]
    
    p_val_num_etape4 <- rbind(p_val_num_etape4, c(var, p_value_anova ))
    
    if (p_value_anova >= seuil_sign) {
      test_rate <- unique(c(test_rate, var))  
    }}
  
  else{  # Perform Kruskal-Wallis test
    kruskal_test_result <- kruskal.test(Data[[var]] ~ Data[[2]])
    
    p_val_num_etape4 <- rbind(p_val_num_etape4, c(var, kruskal_test_result$p.value ))
    
    if (kruskal_test_result$p.value >= seuil_sign) {
      test_rate <- unique(c(test_rate, var))  
      
    }}
}

variables_sign_num2 <- setdiff(colonnes_numeriques, test_rate)

#p_value

pval_num_sign <- full_join(p_val_num_etape2, p_val_num_etape4)

rm(p_val_num_etape2)
rm(kruskal_test_result, test_rate, shapiro_test_result, anova_test_result, p_value_anova, var, colonnes_numeriques)

### AFFICHER UN HISTOGRAMME
# plot_data <- data.frame(X = Data[["A05_MdTGRIPPE"]],  #On peut choisir la variable ici
#                         Color = as.factor(Data[[2]]))
# 
# mu <- aggregate(X ~ Color, data = plot_data, mean)
# Création du graphique
# p <- ggplot(plot_data, aes(x = X, fill = Color)) +
#   geom_histogram(color = "white", position = "stack", bins = 50) +
#   labs(x = "A03_sdSeroTTg", y = "Fréquence") +   
#   geom_vline(data = mu, aes(xintercept = X, color = Color), alpha = 0.8, linetype = "dashed") +
#   geom_text(data = mu, aes(x = X + 0.1, y = 30, label = paste("Moyenne:", round(X, 2))), color = "black", size = 3, vjust = -1) +
#   theme_minimal()
# print(p)

# 4.3 Différence étape 2 / 4 

#4.3.1 Factorielle

diff_e2_e4_fact <- setdiff(variables_sign_fact, variables_sign_fact2)
diff_e2_e4_fact <- pval_fact_sign %>%
  filter(var %in% diff_e2_e4_fact)

#4.3.1 Numérique

diff_e2_e4_num<- setdiff(variables_sign_num, variables_sign_num2)
diff_e2_e4_num <- pval_fact_sign %>%
  filter(var %in% diff_e2_e4_fact)

rm(pval_fact_sign,pval_num_sign)

#4.4 Tableau de contingence 

var_diff_fact <- diff_e2_e4_fact$var

Variable_y <- names(Data)[2]

Data_fact <- Data %>% 
  select(Variable_y,all_of(var_diff_fact))

for (col in names(Data_fact)[-1]) {
  contingency_table <- table(Data_fact[[Variable_y]], Data_fact[[col]])
  print(paste("Tableau de contingence pour", Variable_y, "et", col))
  print(contingency_table)
}

## 4.5 ## Base avec variables significative

Data <- Data %>%
  select(1:2, all_of(variables_sign_fact2), all_of(variables_sign_num2))


rm(Variable_y,col, contingency_table,Data_fact)


#### Etape 5 : Etude des corrélations entre les variables X retenues à p<0.1 (?) 
#### 5.1 Numérique ####

Data_numeric <- Data %>%
  select_if(is.numeric)

#Matrice corr
correlation_matrix <- rcorr(as.matrix(Data_numeric))

# Extraire les p-values
p_values <- correlation_matrix$P

#Extraire couple variable, corrélation < 0.05
significant_variables <- which(p_values < 0.05, arr.ind = TRUE)
significant_variables <- data.frame(
  Variable1 = rownames(p_values)[significant_variables[, 2]],
  Variable2 = colnames(p_values)[significant_variables[, 1]],
  P_Value = p_values[significant_variables]
)

variables_num_corr <- significant_variables %>%
  distinct(Variable1, .keep_all = TRUE)

variables_num_corr <- variables_num_corr$Variable1

#p_value
Data_numeric<- Data %>% 
  select(all_of(variables_num_corr))

if (ncol(Data_numeric) == 0) {
  print("La base de données est vide. Aucun code ne sera exécuté.")
} else {
  correlation_matrix <- rcorr(as.matrix(Data_numeric))
 
  # Extraire les p-values
  p_values <- correlation_matrix$P
  
  # Appliquer la condition pour ne garder que les valeurs < 0.05
  p_values[p_values >= 0.05] <- ''
  p_values <- apply(p_values, c(1, 2), as.numeric)
  p_values <- round(p_values, digits = 10)
  # p_values[is.na(p_values)] <- ''
  p_values_num <- as.data.frame(p_values)
}

## Compte des p-values significatives (variables num?riques)
non_na_counts_num <- rowSums(!is.na(p_values_num)) 
count_occurrences_num <- data.frame(var = colnames(p_values_num), nb_var_correle = non_na_counts_num)
count_occurrences_num <- count_occurrences_num %>% 
   arrange(desc(nb_var_correle))
count_occurrences_num <- left_join(count_occurrences_num, p_val_num_etape4)

rm(p_values)
rm(Data_numeric, correlation_matrix)

#### 5.2 Factorielle ####

seuil_sign2 <- 0.05

Data_fact <- Data %>% 
  select_if(is.factor) %>% 
  select(-1)

variables_significatives <- list()

for (i in seq_along(names(Data_fact))) {
  variable_cible2 <- names(Data_fact)[i]
  for (j in seq_along(names(Data_fact)[-i])) {
    variable2 <- names(Data_fact)[-i][j]
    
    # Effectuez le test du chi-deux pour chaque paire de variables 
    chi_squared_result <- tryCatch({
      chisq.test(table(Data_fact[[variable_cible2]], Data_fact[[variable2]]))
    }, warning = function(w) {
      # En cas d'avertissement, effectuer un test du chi-deux exact
      exact_test <- chisq.test(table(Data_fact[[variable_cible2]], Data_fact[[variable2]]), simulate.p.value = TRUE)
      return(exact_test)
    })
    
    # Vérifiez si la p-valeur est inférieure à seuil_sign2
    if (chi_squared_result$p.value < seuil_sign2) {
      # Stocker les informations sur la variable cible2 et la variable2 ainsi que leur p-value associée
      variables_significatives[[length(variables_significatives) + 1]] <- list(variable_cible2 = variable_cible2,
                                                                               variable2 = variable2,
                                                                               p_value = chi_squared_result$p.value)
    }
  }
}

variables_significatives <- do.call(rbind, variables_significatives)
variables_significatives <- as.data.frame(variables_significatives)

variables_fact_corr <- variables_significatives %>%
  distinct(variable_cible2, .keep_all = TRUE)

variables_fact_corr <- variables_fact_corr$variable_cible2
variables_fact_corr <- as.character(variables_fact_corr)

rm(variable_cible2, variable2,i,j, Data_fact)
rm(colonnes_numeriques)

#p_value

Data_fact <- Data %>% 
  select(all_of(variables_fact_corr))

if (ncol(Data_fact) == 0) {
  print("La base de données est vide. Aucun code ne sera exécuté.")
} else { 
  # Initialiser une matrice pour stocker les p-values des tests du chi-deux
  p_values_contingency <- matrix(NA, nrow = ncol(Data_fact), ncol = ncol(Data_fact), dimnames = list(names(Data_fact), names(Data_fact)))
  
  # Calculer les p-values pour chaque paire de variables cat?gorielles
  for (i in 1:(ncol(Data_fact) - 1)) {
    for (j in (i + 1):ncol(Data_fact)) {
      # Effectuer le test du chi-deux pour chaque paire de variables 
      chi_squared_result <- tryCatch({
        chisq.test(table(Data_fact[[i]], Data_fact[[j]]))
      }, warning = function(w) {
        # En cas d'avertissement, effectuer un test du chi-deux exact
        exact_test <- chisq.test(table(Data_fact[[i]], Data_fact[[j]]), simulate.p.value = TRUE)
        return(exact_test)
      })
      
      # Stocker la p-value dans la matrice des p-values
      p_values_contingency[i, j] <- chi_squared_result$p.value
      p_values_contingency[j, i] <- chi_squared_result$p.value  # La matrice est sym?trique
    }
  }
  p_values_fact <- p_values_contingency
  p_values_fact[p_values_fact >= 0.05] <- ''
  p_values_fact <- apply(p_values_fact, c(1, 2), as.numeric)
  p_values_fact <- round(p_values_fact, digits = 10)
  # p_values_fact[is.na(p_values_fact)] <- ''
  p_values_fact <- as.data.frame(p_values_fact)
  }

## Compte des p-values significatives (variables num?riques)
non_na_counts_fact <- rowSums(!is.na(p_values_fact)) 
count_occurrences_fact<- data.frame(variable = colnames(p_values_fact), nb_var_correle = non_na_counts_fact)
count_occurrences_fact<- count_occurrences_fact %>% 
  arrange(desc(nb_var_correle))
count_occurrences_fact <- left_join(count_occurrences_fact, variables_significatives_p)

rm(Data_fact, p_values_contingency, i, j, chi_squared_result)
rm(significant_variables,variables_significatives)
rm(diff_e2_e4_fact, diff_e2_e4_num)

#### 6 Resultat / Export  #### 

# chemin_export <- "C:/Users/Vito/Desktop/D?p?t Projet Statistique 2A/1.Donnees" #Vito
chemin_export <- "C:/Users/nsdk1/Desktop/R/Projet_stat/Projet_stat_groupe_22" # Nathan

## DATA

file5 <- paste(chemin_export,"/",Data_name,".xlsx", sep = "")
write.xlsx(Data, file = file5, rowNames = TRUE)

## VARIABLES

file1 <- paste(chemin_export,"/p_values_fact_",Data_name,".xlsx", sep = "")
write.xlsx(p_values_fact, file = file1, rowNames = TRUE)

file2 <- paste(chemin_export,"/p_values_num_",Data_name,".xlsx", sep = "")
write.xlsx(p_values_num, file = file2, rowNames = TRUE)

file3 <- paste(chemin_export,"/count_occurrences_num_",Data_name,".xlsx", sep = "")
write.xlsx(count_occurrences_num, file = file3, rowNames = TRUE)

file4 <- paste(chemin_export,"/count_occurrences_fact_",Data_name,".xlsx", sep = "")
write.xlsx(count_occurrences_fact, file = file4, rowNames = TRUE)
