library("haven")
library(dplyr)
library(ggplot2)
library(gtsummary)
library(janitor)
library(labelled)
library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(naniar)
library(mice)


#Importation des bases
#guess_encoding("data/raw/prix_marches.csv")

choc_men <- read.csv("data/raw/chocs_menages.csv",
                     header = TRUE,
                     sep =',',
                     #dec =,
                     fileEncoding = "UTF-8"
                       )

ensan_cons <- read_excel("data/raw/ensan_consommation.xlsx")
                    

ensan_indiv <- read.csv("data/raw/ensan_individus.csv",
                     header = TRUE,
                     sep =',',
                     #dec =,
                     fileEncoding = "ASCII"
)


ensan_men <- read_dta("data/raw/ensan_menages.dta")


prix_marche <- read.csv("data/raw/prix_marches.csv",
                        header = TRUE,
                        sep =',',
                        #dec =,
                        fileEncoding = "UTF-8"
)

lim_tchad <- st_read("data/raw/tcd_admin1.shp") 


######Fusionner les modules en un jeu de données ménages enrichi comportant les caractéristiques du chef de ménage, 
#la consommation alimentaire agrégée et les chocs subis, par des jointures adaptées. Vérifier explicitement que 
#le nombre de lignes du jeu de données ménages reste inchangé après chaque jointure. 

nrow(ensan_men)
t1 <- n_distinct(ensan_men$hhid)
t1
#Tous les individus ont ils un ménage dans la base ensan_men ?
#Transformation en 


str(ensan_men$hhid)
str(ensan_indiv$hhid)
str(ensan_cons$hhid)

ensan_men <- ensan_men|>
  mutate(hhid1 = as.integer(hhid))



ensan_cons <- ensan_cons|>
  mutate(hhid1 = as.integer(hhid))


ensan_indiv<- ensan_indiv|>
  mutate(hhid1 =hhid)

#View(ensan_men)


#?as_integer

individus_orphelins <- anti_join(ensan_indiv, ensan_men, by = "hhid1")
nrow(individus_orphelins)#Tous les individus ont un menage dans base men

#Regardons les doublons 


#Est ce que tous les menages correspondent 

?get_dupes

#Il ya 3540 ménages au total
# NE pas oublier le plan de sondage 
#On fait des jointures successives avec la clé hhid 

#CORRECTION DES DOUBLONS SUR LA BASE IND
names(ensan_indiv)
nrow(get_dupes(ensan_indiv))
ensan_indiv1 <- distinct(ensan_indiv)



#dim(ensan_men)
#names(ensan_men)


#Il ya 30 individus répétés à supprimer de la base
#Comment supprimer des doublons sur R

#Supression des doublons
ensan_men3 <- distinct(ensan_men2)
nrow(ensan_men3)



#Correction de l'orthographe dans ensan_men3
?clean_names


#JEU DE DONNEE MENAGE
names(ensan_indiv1)
chefs_menage <- ensan_indiv1 |> 
  filter(rang == 1) |>   # Rang 1 = chef de ménage 
  select(hhid1, sexe_cm = sexe, age_cm = age, educ_cm = niveau_educ) 

ensan_men4 <- ensan_men |> 
  left_join(chefs_menage, by = "hhid1") 

nrow(ensan_men4)

ensan_men4$hhid <-NULL

ensan_men5 <- ensan_men4|> 
  left_join(ensan_cons, by = "hhid1") 

nrow(ensan_men5)

names(ensan_men5)



#3. Identifier et corriger les libellés de régions
#incohérents entre fichiers avant toute jointure spatiale. 

names(lim_tchad)
unique(lim_tchad1$adm1_name)

table(ensan_men5$region)


?clean_names
?str_

?stringr
ensan_men6 <- clean_names(ensan_men5)
lim_tchad1 <- clean_names(lim_tchad)



lim_tchad1 <- lim_tchad |>
  mutate(
    adm1_name = str_to_upper(adm1_name),                 
    adm1_name = str_replace_all(adm1_name, "’", "'"),     
    adm1_name = str_replace_all(adm1_name, "É", "E"),    
    adm1_name = str_replace_all(adm1_name, "È", "E"),
    adm1_name = str_replace_all(adm1_name, "À", "A"),
    adm1_name = str_replace_all(adm1_name, "Ï", "I"),
    adm1_name = str_squish(adm1_name)                     
  )

ensan_men6 <- ensan_men6 |>
  mutate(
    region = str_to_upper(region),
    region = str_replace_all(region, "’", "'"),     
    region = str_replace_all(region, "É", "E"),    
    region = str_replace_all(region, "È", "E"),
    region = str_replace_all(region, "À", "A"),
    region = str_replace_all(region, "Ï", "I"),
    region = str_squish(region))                
   

unique(lim_tchad1$adm1_name)


####4. Produire un diagnostic complet des valeurs 
#manquantes avec naniar (résumé, visualisation, cooccurrences), en 
#distinguant les valeurs manquantes standards des valeurs manquantes taguées Stata

# Valeurs manquantes spéciales Stata (.a, .b, etc.) 
names(ensan_men6)

#NA STANDARDS

ensan_men6 |> filter(is.na(poids_final))|> nrow()  # NA standard 
ensan_men6 |> filter(is.na(age_cm))|> nrow()  #74 NA STANDARDS
ensan_men6 |> filter(is.na(educ_cm))|> nrow()# 0 NA STANDARDS
ensan_men6 |> filter(is.na(sexe_cm))|> nrow() #0 NA STANDARDS
ensan_men6 |> filter(is.na(taille_menage))|> nrow() #150
ensan_men6 |> filter(is.na(cons_cereales))|> nrow()
ensan_men6 |> filter(is.na(cons_legumes))|> nrow()
ensan_men6 |> filter(is.na(cons_poisson))|> nrow()
ensan_men6 |> filter(is.na(cons_huile))|> nrow()
ensan_men6 |> filter(is.na(cons_sucre))|> nrow()
ensan_men6 |> filter(is.na(revenu))|> nrow() #369



#NA TAGGES

ensan_men6 |> filter(is_tagged_na(age_cm)) |> nrow()#0  # NA taggés 
ensan_men6 |> filter(is_tagged_na(taille_menage)) |> nrow()#0
ensan_men6 |> filter(is_tagged_na(educ_cm)) |> nrow()#0
ensan_men6 |> filter(is_tagged_na(revenu)) |> nrow()#0


# Explorer les valeurs manquantes 

#vis_miss(ensan_men6)              # Visualisation cartographique des NA 


names(ensan_men6)
vars_cles <- c("age_cm", 
                "taille_menage","revenu")

p_miss <- vis_miss(
  ensan_men6 %>% select(all_of(vars_cles)),
  warn_large_data = FALSE
) +
  labs(
    title    = "Valeurs manquantes sur toutes les variables ",
  ) +
  theme_minimal(base_size = 12)


p_miss
ggsave(
  filename = "outputs/figures/valeurs_manquantes_variables.png",
  plot     = p_miss,
  width    = 12,
  height   = 6,
  dpi      = 150
)
#saveRDS(p_miss, "outputs/figures/valeurs_manquantes_variables.rds")


# Matrice de co-occurrence des NA:quelles variables sont manquantes ensemble ? 
pmiss2 <- gg_miss_upset(ensan_men6, nsets = 8) 

pmiss2


 

# Imputation multiple (M = 5 jeux de données imputés) 
ensan_men_imp <- mice( 
  ensan_men6 |> select(revenu, age_cm,  taille_menage, milieu,educ_cm,
                       region,strate,poids_final,grappe,hhid1,cons_cereales,cons_legumes,
                       cons_poisson,cons_huile,cons_tubercules, cons_fruits,cons_oeufs,
                       cons_sucre,cons_legumineuses,cons_viande,cons_lait,cons_condiments), 
  m     = 5,          # Nombre de jeux imputés 
  maxit = 20,         # Itérations de convergence 
  method = "pmm",     # Predictive Mean Matching (recommandé pour variables continues) 
seed  = 42 
)

# Vérifier la convergence 
plot(ensan_men_imp)

fit_imputed <- with(ensan_men_imp, lm(log(revenu) ~ age_cm + educ_cm + taille_menage + 
                              milieu)) 
pool(fit_imputed) |> summary() 


# Extrayons le premier jeu imputé
ensan_men_impute <- complete(ensan_men_imp, 1)

sum(is.na(ensan_men_impute))

###6. Recoder les variables catégorielles nécessaires à l’analyse 
#(milieu, niveau d’éducation du chef de ménage, 
#quintiles de revenu par tête).

###RECODAGE

attributes(ensan_men_impute$milieu)
attributes(ensan_men_impute$educ_cm)
table(ensan_men_impute$educ_cm)

ensan_men_impute <- ensan_men_impute |> 
  mutate( 
    # Recodage avec case_when (plus flexible que if_else) 
    milieu_label = case_when( 
      milieu == 1 ~ "Urbain", 
      milieu == 2 ~ "Rural", 
      TRUE        ~ NA_character_ 
    ))


## QUINTILE DE REVENU PAR TËTE PAR MENAGE
ensan_men_impute <- ensan_men_impute |> 
  mutate(rev_per_tete = revenu/taille_menage)


ensan_men_impute <- ensan_men_impute |> 
  mutate(rev_per_tete = revenu/taille_menage)




# Attribution des quintiles

ensan_men_impute <- ensan_men_impute |>
  mutate(quintile = ntile(rev_per_tete, 5))

#ensan_men_impute


library(srvyr) 


#7. Déclarer le plan de sondage complet
#(grappes, strates, poids finaux) avec srvyr.

library(srvyr)

plan_sondage <- ensan_men_impute %>%
  as_survey_design(
    ids     = grappe,        # Identifiant des grappes (PSU)
    strata  = milieu,        # Variable de stratification
    weights = poids_final,   # Poids de sondage finaux
    nest    = TRUE
  )


##8. Construire le score de diversité alimentaire des ménages (HDDS) et la classification FAO associée. 

ensan_men_impute <- ensan_men_impute |> 
  mutate( 
    # Score HDDS:somme des 12 groupes alimentaires 
    hdds = rowSums(across(starts_with("cons_")), na.rm = TRUE), 
    
    # Classification FAO 
    securite_alim = case_when( 
      hdds <= 3  ~ "Insécurité alimentaire sévère", 
      hdds <= 6  ~ "Insécurité alimentaire modérée", 
      hdds <= 9  ~ "Sécurité alimentaire acceptable", 
      TRUE       ~ "Bonne diversité alimentaire" 
    ) |> factor(levels = c("Insécurité alimentaire sévère", 
                           "Insécurité alimentaire modérée", 
                           "Sécurité alimentaire acceptable", 
                           "Bonne diversité alimentaire"), ordered = TRUE) 
  )
#names(ensan_men_impute)


hhds_par_region <- ensan_men_impute %>%
  group_by(region) %>%
  summarise(
    hdds_moyen = mean(hdds, na.rm = TRUE),
    
    .groups = "drop"
  )

#CARTE CLOROPETHE
carte_data_adm1 <- lim_tchad1 %>%
  left_join(hhds_par_region, by = c("adm1_name" = "region") )
#names(lim_tchad1)

ggplot(carte_data_adm1) +
  geom_sf(aes(fill = hdds_moyen), color = "white", linewidth = 0.2) +
  
  scale_fill_viridis_c(name = "SCA hdds_moyen", option = "plasma", na.value = "grey85")+
  labs(title = "indice de diversité") +
  theme_minimal()
ggsave("outputs/figures/carte_hhds_etat.png", width=8, height=6)




  

#Calculer le coefficient de Gini du revenu par tête au 
#niveau national et par région, avec un intervalle de confiance 
#obtenu par bootstrap.


library(infer)

#install.packages("ineq")
library(ineq)

boot_gini <- ensan_men_impute %>%
  specify(response = rev_per_tete) %>%
  generate(reps = 2000, type = "bootstrap") %>%
  summarise(stat = ineq(rev_per_tete, type = "Gini"))


# Intervalle de confiance à 95% par percentile bootstrap 
get_ci(boot_gini, level = 0.95, type = "percentile") 




