###############################################################
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4, 2018–2019)
# SCRIPT   : TP5 — Analyse des cultures pratiquées
# BASES    : secta1_harvestw4, secta_harvestw4,
#            sect11a1_plantingw4, sect11b1_plantingw4
###############################################################
rm(list = ls())

library(haven)
library(dplyr)
library(visdat)
library(labelled)

#Importer les variables
secta3i_harvestw4 <- read_dta("data/raw/secta3i_harvestw4.dta") 
secta3ii_harvestw4 <- read_dta("data/raw/secta3ii_harvestw4.dta") 
secta_harvestw4 <- read_dta("data/raw/secta_harvestw4.dta")
secta11c2_harvestw4 <- read_dta("data/raw/secta11c2_harvestw4.dta")
sect11a1_plantingw4 <- read_dta("data/raw/sect11a1_plantingw4.dta")
#Sélectionner les variables
#-------
## Section A : Variable géographiques & Poids
#-------
secta_harvestw4 <- secta_harvestw4 %>%
  select(
    hhid, # Identifiant
    state,  # Etat
    zone, 
    lga,  # Code LGA
    sector,  # Milieu de résidence
    ea,  # Zone de dénombrement
    cluster,  # Grappe
    strata, # Strate
    wt_wave4,  # Pondération
    interview_result  # Résultat de l'interview
  ) %>%
  rename(
    milieu_residence      = sector,
    poids_wave4     = wt_wave4
  )
#-------
## Section A3i : Cultures
#-------
secta3i_harvestw4 <- secta3i_harvestw4 %>%
  select(
    hhid, # Identifiant
    plotid,
    cropcode
  ) %>%
  rename(
    culture      = cropcode
  )

#-------
## Section A3ii : Cultures
#-------
secta3ii_harvestw4 <- secta3ii_harvestw4 %>%
  select(
    hhid,
    cropcode,
    sa3iiq1a,      # Quantité totale récoltée (unité locale)
    sa3iiq1_conv   # Facteur de conversion en kg
  ) %>%
  rename(
    qte_recoltee   = sa3iiq1a,
    facteur_conv   = sa3iiq1_conv
  ) %>%
  mutate(
    production_kg = qte_recoltee * facteur_conv
  )  %>% 
  rename(
    culture      = cropcode
  ) %>% 
  select(
    hhid,
    culture,
    production_kg #Production en kg
  )

#-------
## Section 11c2 : Engrais
#-------

secta11c2_harvestw4 <- secta11c2_harvestw4 %>%
  select(
    hhid,
    plotid,
    s11dq1a,   # Utilisation engrais chimique (1=Oui)
    s11c2q36_1, # Type d'engrais : NPK
    s11c2q36_2, # Type d'engrais : URÉE
    s11dq36    # Utilisation engrais organique (1=Oui)
  ) %>%
  rename(
    engrais_chimique = s11dq1a,
    engrais_npk      = s11c2q36_1,
    engrais_uree     = s11c2q36_2,
    engrais_organique = s11dq36
  )
  #-------
  ## Section 11A1 (Planting) : Informations sur la surface des parcelles
  #-------
sect11a1_plantingw4 <- sect11a1_plantingw4 %>%
  select(
    hhid, plotid, s11aq4aa, s11aq4b,
    s11aq4a, s11aq4b1, s11aq4c
  ) %>%
  rename(
    plot_area_declared       = s11aq4aa,   # superficie déclarée
    plot_area_unit           = s11aq4b,    # unité de mesure déclarée
    plot_area_gps        = s11aq4c     # superficie GPS en m²
  )
vis_miss(sect11a1_plantingw4)

#Valeurs manquantes
vis_miss(secta_harvestw4)
# Suppression des ménages sans pondération
secta_harvestw4 <- secta_harvestw4 %>% 
  filter(!is.na(poids_wave4))
vis_miss(secta_harvestw4)

vis_miss(secta3i_harvestw4)
#Fusions et création des bases finales

##Bases parcelles
processed_parcelles_tp5_w4 <- inner_join(
  x=secta_harvestw4,
  y=secta3i_harvestw4,
  by='hhid'
)


processed_parcelles_tp5_w4 <- left_join(
  x=processed_parcelles_tp5_w4,
  y=secta11c2_harvestw4,
  by=c("hhid","plotid")
)

processed_parcelles_tp5_w4 <- left_join(
  x=processed_parcelles_tp5_w4,
  y=sect11a1_plantingw4,
  by=c("hhid","plotid")
)

processed_parcelles_tp5_w4 <- processed_parcelles_tp5_w4 %>%
  mutate(
    engrais_npk  = labelled(
      ifelse(is.na(engrais_npk),  99, engrais_npk),
      c("Oui" = 1, "Non" = 0, "Non concerné" = 99)
    ),
    engrais_uree = labelled(
      ifelse(is.na(engrais_uree), 99, engrais_uree),
      c("Oui" = 1, "Non" = 0, "Non concerné" = 99)
    ),
    engrais_chimique =labelled(engrais_chimique,
      c("Oui" = 1, "Non" = 2)
    ),
    engrais_organique =labelled(engrais_organique,
                               c("Oui" = 1, "Non" = 2)
    )
  )

processed_parcelles_tp5_w4 <- processed_parcelles_tp5_w4 %>% 
  mutate(
    superficie_ha = case_when(
      plot_area_unit ==5 ~ plot_area_declared*0.4,
      plot_area_unit ==6 ~ plot_area_declared,
      plot_area_unit ==7 ~ plot_area_declared*0.0001,
      
      zone == 1 & plot_area_unit ==1 ~ plot_area_declared*0.00012,
      zone == 1 & plot_area_unit ==2 ~ plot_area_declared*0.0027, 
      zone == 1 & plot_area_unit ==2 ~ plot_area_declared*0.00006,
      
      zone == 2 & plot_area_unit ==1 ~ plot_area_declared*0.00016,
      zone == 2 & plot_area_unit ==2 ~ plot_area_declared*0.004, 
      zone == 2 & plot_area_unit ==2 ~ plot_area_declared*0.00016,
      
      zone == 3 & plot_area_unit ==1 ~ plot_area_declared*0.00011,
      zone == 3 & plot_area_unit ==2 ~ plot_area_declared*0.00494, 
      zone == 3 & plot_area_unit ==2 ~ plot_area_declared*0.00004,
      
      zone == 4 & plot_area_unit ==1 ~ plot_area_declared*0.00019,
      zone == 4 & plot_area_unit ==2 ~ plot_area_declared*0.0023, 
      zone == 4 & plot_area_unit ==2 ~ plot_area_declared*0.00004,
  
      zone == 5 & plot_area_unit ==1 ~ plot_area_declared*0.00021,
      zone == 5 & plot_area_unit ==2 ~ plot_area_declared*0.0023, 
      zone == 5 & plot_area_unit ==2 ~ plot_area_declared*0.00013,
  
      zone == 6 & plot_area_unit ==1 ~ plot_area_declared*0.00012,
      zone == 6 & plot_area_unit ==2 ~ plot_area_declared*0.00001, 
      zone == 6 & plot_area_unit ==2 ~ plot_area_declared*0.00041    
  
    )
  )  %>% 
  set_variable_labels(
    superficie_ha = "Superficie de la parcelle (hectares)"
  )

vis_miss(processed_parcelles_tp5_w4)

##Bases cultures
processed_cultures_tp5_w4 <- inner_join(
  x=secta_harvestw4,
  y=secta3ii_harvestw4,
  by="hhid") %>% 
  select(-c("interview_result"))

vis_miss(processed_cultures_tp5_w4)
#On considérera que 
processed_cultures_tp5_w4[is.na(processed_cultures_tp5_w4)] <- 0
# Création de la variable type de culture

processed_cultures_tp5_w4 <- processed_cultures_tp5_w4 %>%
  mutate(
    code_culture = as.numeric(substr(culture, 1, 4)),
    
    type_culture = case_when(
      
      # Céréales
      code_culture %in% c(1070, 1080, 1100, 1110, 2010, 2280) ~ "Céréale",
      #                    Sorgho Maïs  Mil   Riz   Acha  Blé
      
      # Légumineuses
      code_culture %in% c(1010, 1060, 2020, 2040, 2150, 2220) ~ "Légumineuse",
      #                    Niébé Arach. Bamb. Sésam. P.Ang. Soja
      
      # Tubercules
      code_culture %in% c(1020, 1040, 1121, 1122, 1123, 1124,
                          2030, 2160, 2170, 2180, 2181, 2190) ~ "Tubercule",
      #                    Manioc Cocoym IgBl IgJn IgEau Ig3F
      #                    Banane Ananas Plantain PdT PatDce Citrouille
      
      # Cultures de rente
      code_culture %in% c(1050, 2230, 2240, 2250,
                          3020, 3040, 3060, 3180, 3230) ~ "Culture de rente",
      #                    Coton CàSucre Thé Tabac
      #                    Cajou Cacao  Café PalmHuile Caoutch.
      
      TRUE ~ "Autres"
    )
  )
#Labellisation
labels_culture <- c(
  # Image 3
  "Niébé (haricot)"         = 1010,
  "Manioc"                  = 1020,
  "Cocoyam"                 = 1040,
  "Coton"                   = 1050,
  "Arachide"                = 1060,
  "Sorgho"                  = 1070,
  "Maïs"                    = 1080,
  "Melon / Egusi"           = 1090,
  "Mil"                     = 1100,
  "Riz"                     = 1110,
  "Igname blanche"          = 1121,
  "Igname jaune"            = 1122,
  "Igname d'eau"            = 1123,
  "Igname (trois feuilles)" = 1124,
  "Acha"                    = 2010,
  "Bambara groundnut"       = 2020,
  "Banane"                  = 2030,
  "Sésame"                  = 2040,
  "Carotte"                 = 2050,
  "Concombre"               = 2060,
  "Chou"                    = 2070,
  "Laitue"                  = 2071,
  
  # Image 2
  "Aubergine (garden egg)"  = 2080,
  "Ail"                     = 2090,
  "Gingembre"               = 2100,
  "Gombo"                   = 2120,
  "Oignon"                  = 2130,
  "Poivron doux (tatashe)"  = 2141,
  "Piment petit (rodo)"     = 2142,
  "Piment fort (shombo)"    = 3030,
  "Pois d'Angole"           = 2150,
  "Ananas"                  = 2160,
  "Plantain"                = 2170,
  "Pomme de terre"          = 2180,
  "Patate douce"            = 2181,
  "Citrouille"              = 2190,
  "Épinard / légumes verts" = 2194,
  "Soja"                    = 2220,
  "Canne à sucre"           = 2230,
  "Thé"                     = 2240,
  "Tabac"                   = 2250,
  "Tomate"                  = 2260,
  "Noix"                    = 2270,
  "Blé"                     = 2280,
  
  # Image 1
  "Zobo (hibiscus)"         = 2290,
  "Pomme"                   = 3010,
  "Noix de cajou"           = 3020,
  "Cacao"                   = 3040,
  "Noix de coco"            = 3050,
  "Café"                    = 3060,
  "Pamplemousse"            = 3080,
  "Goyave"                  = 3090,
  "Noix de cola"            = 3110,
  "Citron"                  = 3120,
  "Lime"                    = 3130,
  "Mandarine / tangerine"   = 3150,
  "Mangue"                  = 3160,
  "Orange"                  = 3170,
  "Palmier à huile"         = 3180,
  "Ogbono (oro seed)"       = 3190,
  "Haricot huileux"         = 3200,
  "Papaye"                  = 3210,
  "Poire"                   = 3220,
  "Avocat"                  = 3221,
  "Caoutchouc"              = 3230,
  "Autre (à préciser)"      = 9999
)
processed_parcelles_tp5_w4 <- processed_parcelles_tp5_w4 %>%
  mutate(
    culture_code = as.numeric(substr(culture, 1, 4)),
    culture = labelled(culture_code, labels_culture),
    milieu_residence = labelled(milieu_residence,
                                c(
                                  "Urbain" = 1,
                                  "Rural"  = 2
                                )),
    
    zone = labelled(zone,
                    c(
                      "Nord Central" = 1,
                      "Nord Est"     = 2,
                      "Nord Ouest"   = 3,
                      "Sud Est"      = 4,
                      "Sud Sud"      = 5,
                      "Sud Ouest"    = 6
                    )),
    
    state = labelled(state,
                     c(
                       "Abia"        = 1,  "Adamawa"   = 2,  "Akwa Ibom" = 3,
                       "Anambra"     = 4,  "Bauchi"    = 5,  "Bayelsa"   = 6,
                       "Benue"       = 7,  "Borno"     = 8,  "Cross River" = 9,
                       "Delta"       = 10, "Ebonyi"    = 11, "Edo"       = 12,
                       "Ekiti"       = 13, "Enugu"     = 14, "Gombe"     = 15,
                       "Imo"         = 16, "Jigawa"    = 17, "Kaduna"    = 18,
                       "Kano"        = 19, "Katsina"   = 20, "Kebbi"     = 21,
                       "Kogi"        = 22, "Kwara"     = 23, "Lagos"     = 24,
                       "Nasarawa"    = 25, "Niger"     = 26, "Ogun"      = 27,
                       "Ondo"        = 28, "Osun"      = 29, "Oyo"       = 30,
                       "Plateau"     = 31, "Rivers"    = 32, "Sokoto"    = 33,
                       "Taraba"      = 34, "Yobe"      = 35, "Zamfara"   = 36,
                       "FCT (Abuja)" = 37
                     ))
  )


processed_cultures_tp5_w4 <- processed_cultures_tp5_w4 %>%
  mutate(
    culture_code = as.numeric(substr(culture, 1, 4)),
    culture = labelled(culture_code, labels_culture),
    milieu_residence = labelled(milieu_residence,
                                c(
                                  "Urbain" = 1,
                                  "Rural"  = 2
                                )),
    
    zone = labelled(zone,
                    c(
                      "Nord Central" = 1,
                      "Nord Est"     = 2,
                      "Nord Ouest"   = 3,
                      "Sud Est"      = 4,
                      "Sud Sud"      = 5,
                      "Sud Ouest"    = 6
                    )),
    
    state = labelled(state,
                     c(
                       "Abia"        = 1,  "Adamawa"   = 2,  "Akwa Ibom" = 3,
                       "Anambra"     = 4,  "Bauchi"    = 5,  "Bayelsa"   = 6,
                       "Benue"       = 7,  "Borno"     = 8,  "Cross River" = 9,
                       "Delta"       = 10, "Ebonyi"    = 11, "Edo"       = 12,
                       "Ekiti"       = 13, "Enugu"     = 14, "Gombe"     = 15,
                       "Imo"         = 16, "Jigawa"    = 17, "Kaduna"    = 18,
                       "Kano"        = 19, "Katsina"   = 20, "Kebbi"     = 21,
                       "Kogi"        = 22, "Kwara"     = 23, "Lagos"     = 24,
                       "Nasarawa"    = 25, "Niger"     = 26, "Ogun"      = 27,
                       "Ondo"        = 28, "Osun"      = 29, "Oyo"       = 30,
                       "Plateau"     = 31, "Rivers"    = 32, "Sokoto"    = 33,
                       "Taraba"      = 34, "Yobe"      = 35, "Zamfara"   = 36,
                       "FCT (Abuja)" = 37
                     ))
  )



var_label(processed_parcelles_tp5_w4) <- list(
  
  # Identifiants
  hhid               = "Identifiant du ménage",
  plotid             = "Identifiant de la parcelle",
  
  # Géographie / structure enquête
  state              = "État",
  lga                = "Code LGA",
  milieu_residence   = "Milieu de résidence",
  ea                 = "Zone de dénombrement",
  cluster            = "Grappe",
  strata             = "Strate",
  
  # Poids / enquête
  poids_wave4        = "Pondération du ménage",
  interview_result   = "Résultat final de l'interview",

  engrais_chimique  = "Utilisation d'engrais chimique (1=Oui)",
  engrais_npk       = "Type d'engrais : NPK",
  engrais_uree      = "Type d'engrais : Urée",
  engrais_organique = "Utilisation d'engrais organique (1=Oui)",
    # Superficie
  plot_area_unit           = "Unité de mesure",
  plot_area_gps            = "Superficie GPS (m²)"

)

var_label(processed_cultures_tp5_w4) <- list(
  
  # Identifiants
  hhid               = "Identifiant du ménage",
  culture = "Culture",
  type_culture       = "Type de culture (catégorisé)",
  # Géographie / structure enquête
  state              = "État",
  lga                = "Code LGA",
  milieu_residence   = "Milieu de résidence",
  ea                 = "Zone de dénombrement",
  cluster            = "Grappe",
  strata             = "Strate",
  
  # Poids / enquête
  poids_wave4        = "Pondération du ménage",
  
  # Production
  production_kg = "Production (en kg)"
  
)
#Exportation
write_dta(processed_parcelles_tp5_w4,
          "data/processed/processed_parcelles_tp5_w4.dta")
write_dta(processed_cultures_tp5_w4,
          "data/processed/processed_cultures_tp5_w4.dta")