rm(list = ls())

## CHARGEMENT DES PACKAGES ##

library(readxl)
library(dplyr)


## CHARGEMENT DES DONNEES ##

Data <- read_excel("~/PROJET STAT 2A/R/base_NE_X_varY_BEA.xlsx")


#################################################################
### Toutes les données doivent être de type Factor ou Numeric ###
#################################################################





str(Data)


#Liste des variables_factor Repro :   
c("CODE_ELEVAGE",
"X01x1_UTH_class",
"X01x1_SAU_class",
"X02x1_NBANDE_class",
"X02x1_INTERVBD_class",
"X02x1_NBDPS_class",
"X03x1_UTH_PORC_class",
"Zone",
"T03_MAT_logement")


#Liste des variables_factor NE :   
variables_factor <- c("CODE_ELEVAGE",
  "X01x1_UTH_class",
  "X01x1_SAU_class",
  "X02x1_NBANDE_class",
  "X02x1_INTERVBD_class",
  "X02x1_NBDPS_class",
  "X03x1_UTH_PORC_class",
  "Zone",
  "T03_MAT_logement",
  "X02x1_NBDENG_class",
  "A03_PosSeroMyAs")

#Liste des variables_factor PC :   
 c("CODE_ELEVAGE",
  "type_elevage",
  "X01x1_UTH_class",
  "X01x1_SAU_class",
  "X02x1_NBANDE_class",
  "X02x1_INTERVBD_class",
  "X02x1_NBDPS_class",
  "X03x1_UTH_PORC_class",
  "X02x1_TYPE_ELEVAGE_NEW",
  "Zone",
  "X02x1_NBDENG_class",
  "A03_PosSeroMyAs")


# Convertissez les colonnes spécifiées en facteur
Data <- Data %>%
  mutate(across(all_of(variables_factor), as.factor))

# Convertissez toutes les colonnes sauf celles spécifiées en numérique
Data <- Data %>%
  mutate(across(-all_of(variables_factor), as.numeric))

str(Data)

#################################################################
### Suppression de données >15% données manquantes(NA)    ### 
#################################################################

Data_NA <- c(colSums(is.na(Data)) / nrow(Data))
names(Data_NA) <- colnames(Data)


print("NA >15%"  )
print(Data_NA[Data_NA > 0.15])


variable_a_supprimer <- c(names(Data_NA[Data_NA > 0.15]))


#################################################################
### Suppression de variables avec 1 modalité >=85% répondants
#################################################################

# (?) Faut-il prendre en compte les non-réponses en commpte ?
# -> Je les ai pris 


# Créez une liste vide avec le bon nombre d'éléments
Data_Mode <- vector("list", length = ncol(Data))
names(Data_Mode) <- colnames(Data)


# Boucle à travers chaque colonne
for (col in 1:ncol(Data)) {
  Data_Mode[[col]] <- table(Data[, col]) / nrow(Data)
}


print("Modalité >85%" )
for (var in names(Data_Mode)) {
  if (max(Data_Mode[[var]]) > 0.85) {
    print(var)
    variable_a_supprimer <- unique(c(variable_a_supprimer, var))
  }
}



#################################################################
### Vérification si existence de modalité avec <=15% de répondants
#################################################################

print("Modalité <15% en bimodal" )
for (var in names(Data_Mode)){
  if (min(Data_Mode[[var]]) < 0.15 && length(Data_Mode[[var]])<=2 ){
    variable_a_supprimer <- unique(c(variable_a_supprimer, var))
    print(var)
  }
}



detection <- c()
for (var in names(Data_Mode)){
  if (min(Data_Mode[[var]]) < 0.15 && length(Data_Mode[[var]])>2 ){
    detection <- unique(c(detection, var))
  }
}
print("Modalité <15% avec 2+ modalités" )
print(detection)

# [!] Reste à décider quelles variables supprimer, print(Data_Mode[[var]])


datamode

#################################################################
### Etape Finale
#################################################################

Data_PC <- Data[, -which(names(Data) %in% variable_a_supprimer)]

print(paste('Nombre de variables supprimées :',length(variable_a_supprimer)))

#################################################################
###   A suivre ... (exporter en R Data)
#################################################################

Data_PC


rm(list = c("Data_Mode","Data", "col", 
            "Data_NA", "var", "variable_a_supprimer",
            "variables_factor"))






