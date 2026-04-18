#Importation des librairies nécessaires
library(haven)
library(dplyr)
#--------------------------------------------------------
#Importations des bases brutes
#--------------------------------------------------------
section11a = read_dta("data/row/sect11a1_plantingw4.dta")
section11b1 = read_dta("data/row/sect11b1_plantingw4.dta")
sectiona = read_dta("data/row/secta_harvestw4.dta")

nrow(section11b1)
ncol(section11b1)
n_distinct(section11b1$hhid)
#---------------------------------------------------------
#Nouvelles bases
#---------------------------------------------------------

#Construction d'une base contenant toutes les informations sur les parcelles d'exploitation
etat = sectiona %>%
  select(hhid, state) %>%
  distinct(hhid, .keep_all = TRUE)
base_parcelle = section11a %>%
  left_join(section11b1, by = c("hhid", "plotid")) %>%
  left_join(etat, by = "hhid")

#Recodage des variables et insertion des nouvelles variables demandées
base_parcelle = base_parcelle %>%
  mutate(aire_declare = as.numeric(s11aq4aa),
         zone.x = as.numeric(zone.x),
         zone = as_factor(sector.x),
         aire_GPS = as.numeric(s11aq4c)*0.0001, #Conversion direct en hectares
         s11aq4b = as.numeric(s11aq4b),
         s11b1q4 = as.numeric(s11b1q4),
         tenure = factor(s11b1q4, levels = c(1,2,3,4,5,6,7), 
                         labels = c("propriété", "location", "prêt", "communautaire", "héritage", "metayage", "échange_temporaire")),
         
         aire_hectare = case_when(s11aq4b == 5 ~ aire_declare*0.4,
                                  s11aq4b == 6 ~ aire_declare,
                                  s11aq4b == 7 ~ aire_declare*0.0001,
                                  
                                  s11aq4b == 1 & zone.x == 1 ~ aire_declare*0.00012,
                                  s11aq4b == 1 & zone.x == 2 ~ aire_declare*0.00016,
                                  s11aq4b == 1 & zone.x == 3 ~ aire_declare*0.00011,
                                  s11aq4b == 1 & zone.x == 4 ~ aire_declare*0.00019,
                                  s11aq4b == 1 & zone.x == 5 ~ aire_declare*0.00021,
                                  s11aq4b == 1 & zone.x == 6 ~ aire_declare*0.00012,
                                  
                                  s11aq4b == 2 & zone.x == 1 ~ aire_declare*0.0027,
                                  s11aq4b == 2 & zone.x == 2 ~ aire_declare*0.004,
                                  s11aq4b == 2 & zone.x == 3 ~ aire_declare*0.00494,
                                  s11aq4b == 2 & zone.x == 4 ~ aire_declare*0.0023,
                                  s11aq4b == 2 & zone.x == 5 ~ aire_declare*0.0023,
                                  s11aq4b == 2 & zone.x == 6 ~ aire_declare*0.0001,
                                  
                                  s11aq4b == 3 & zone.x == 1 ~ aire_declare*0.00006,
                                  s11aq4b == 3 & zone.x == 2 ~ aire_declare*0.00016,
                                  s11aq4b == 3 & zone.x == 3 ~ aire_declare*0.00004,
                                  s11aq4b == 3 & zone.x == 4 ~ aire_declare*0.00004,
                                  s11aq4b == 3 & zone.x == 5 ~ aire_declare*0.00013,
                                  s11aq4b == 3 & zone.x == 6 ~ aire_declare*0.00041),
         etat = as_factor(state)
  )
#Base des superficies cultivées par ménage et nombre de parcelles par ménages
expl_menages = base_parcelle %>%
  group_by(hhid) %>%
  summarise(aire = sum(aire_hectare, na.rm = TRUE), parcelles = n(), .groups = "drop")

#-----------------------------------------------------------------------------
#Les valeurs manquantes et les valeurs aberrantes de la variable aire_hectare
#-----------------------------------------------------------------------------
#Les valeurs manquantes
manquantes = sum(is.na(base_parcelle$aire_hectare))
cat("Les valeurs manquantes après le calcul de superficie en hectares sont au nombres de : \n"); print(manquantes)

#Les valeurs aberrantes
aberante0 = sum((base_parcelle$aire_hectare < 0), na.rm = TRUE)
if (aberante0 >0){
  cat("les valeurs abberantes négatives sont au nombre de :"); print(aberante0)
}else{
  cat("Il n'y a pas de valeurs abberantes négatives")
}
aberante1 = sum((base_parcelle$aire_hectare > 500), na.rm = TRUE)
if (aberante1 >0){
  print("les valeurs abberantes supérieures à 500 hectares sont au nombre de : "); print(aberante1)
}else{
  print("Il n'y a pas de valeurs abberantes supérieures à 500 hectares")
}

#Enregistrement des bases
saveRDS(base_parcelle, "data/processed/base_parcelle.rds")
saveRDS(expl_menages, "data/processed/exploitation_menages.rds")