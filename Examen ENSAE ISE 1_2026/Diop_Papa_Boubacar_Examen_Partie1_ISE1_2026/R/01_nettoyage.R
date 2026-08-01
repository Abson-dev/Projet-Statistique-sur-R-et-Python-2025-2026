rm(list=ls())
{
  library(tidyverse)
  library(haven)
  library(readxl)
  library(mice)
  library(sf)
  library(naniar)
}

# 1. Importation des données

menage <- read_dta("data/raw/ensan_menages.dta")
head(menage)
nrow(menage)

individus <- read.csv('data/raw/ensan_individus.csv')
head(individus)

consommation_groupe <- read_excel("data/raw/ensan_consommation.xlsx",sheet = "Consommation_groupes")
head(consommation_groupe)

depense_menage <- read_excel("data/raw/ensan_consommation.xlsx",sheet = "Depenses_menage")
head(depense_menage)

prix <- read.csv("data/raw/prix_marches.csv")
head(prix)

chocs <- read.csv("data/raw/chocs_menages.csv")
head(chocs)

limit <- st_read("data/raw/tcd_admin1.shp")
st_crs(limit)


#2. Fusion des jeux de données

# On doit d'abord filtrer la base des individus en ne selectionnant que le chef de ménage dont le numéro d'ordre est 1.
chef_menage <- individus |> filter(rang == 1)
View(chef_menage)
nrow(chef_menage)
length(unique(chef_menage$hhid))
length(chef_menage$hhid)
# Dans cette base, hhid, n'est pas unique. On supprime les doublons
chef_menage <- unique(chef_menage)
nrow(chef_menage)

# On remarque que dans la base des chefs de ménage, le hhid n'est pas du même type que dans la base ménage. On va le transformer dans la base menagepour pouvoir faire la jointure
menage <- menage |> mutate(hhid_2 = as.integer(hhid))

# On peut à présent joindre les caractéristiques du chef de ménage à la base ménage avec comme clé hhid
menage <- left_join(menage, chef_menage, by=join_by(hhid_2==hhid))
View(menage)
nrow(menage)

# On supprime la variable rang
menage <- menage |> select(-rang)


# Jointure avec la consommation alimentaire agrégée
menage <- left_join(menage, consommation_groupe, by=join_by(hhid==hhid))
View(menage)
nrow(menage)

# Jointure avec les chocs subis
menage <- left_join(menage, chocs, by=join_by(hhid_2==hhid))
View(menage)
nrow(menage)

# Création de la variable revenu par tête
menage <- menage |> mutate(revenu_pc = revenu/taille_menage)

#3. Identification et correction des libellés des régions
# Les noms des régions dans le fichier ménage ne sont pas tous en majuscule alors qu'ils le sont dans le fichier des données spatiales

menage <- menage |> mutate(region = toupper(region))
View(menage)

# Passons maintenant à la jointure spatiale
menage <- left_join(menage, limit, by=join_by(region==adm1_name))
View(menage)
nrow(menage)

#4. Diagnostic des valeurs manquantes
miss_summary(menage)
vis_miss(menage)

#5. Explication du mécanisme pour le revenu et l'âge
sum(is.na(menage$revenu))

# On procède par modélisation de la non-réponse
menage <- menage |> mutate(revenu_na = as.numeric(is.na(revenu)))
modele_logit <- glm(revenu_na ~ region+milieu+taille_menage+age+sexe, data=menage, family=binomial)
summary(modele_logit)

# On constate que le coefficient de la variable milieu est significatif. Donc ce sont des MAR. Vérifions par un boxplot
boxplot(revenu~milieu, data=menage)


sum(is.na(menage$age))

modele_logit <- glm(as.numeric(is.na(age)) ~ region+milieu+taille_menage+sexe+revenu, data=menage, family=binomial)
summary(modele_logit)

# On constate que les coefficients ne sont pas significatifs au seuil de 1%. On peut soupconner que le mécanisme est un MCAR

# Imputation par mice
imputed <- mice(menage,seed=123)
summary(imputed)

menage_imputed <- complete(imputed, 1)


#6. Recodage des variables nécessaires à l'analyse
menage_imputed <- menage_imputed |> mutate(milieu = if_else(milieu == 1,"Urbain", "Rural"))


# Suppression des colonnes inutiles
menage_imputed <- menage_imputed |> select(-c(hhid_2,adm1_name1, adm1_name2, adm1_name3, adm0_name1, adm0_name2, adm0_name3, valid_to, valid_on, lang, lang1, lang2, lang3, revenu_na,geometry))


names(menage_imputed)


# Enregistrement dans une base sous-format stata

write_dta(menage_imputed, "data/processed/data_processed.dta")


