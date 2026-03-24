# =============================================================================
# Nettoyage et transformation des données
# TP1 : Profil démographique des ménages nigérians
# GHS Panel Nigeria | Wave 4 | ENSAE ISE 1 — 2025-2026
# =============================================================================

library(haven)
library(dplyr)
library(sjlabelled)

# --- Chargement -------------------------------------------
sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")

#--- Diagnitique -------------------------------------------------
sum(is.na(sect1_w4$s1q4))
boxplot(sect1_w4$s1q4)
summary(sect1_w4$s1q4, na.rm = FALSE) # Max age = 130 ans
sum(is.na(sect1_w4$s1q2))
table(secta_w4$zone)
unique(secta_w4$zone)
unique(sect1_w4$s1q3)
get_label(secta_w4)

# --- Nettoyage sect1 ---------------------------------------------------------

sect1_clean <- sect1_w4 |>
  mutate(
    # Âge numérique, exclure valeurs aberrantes
    age = as.numeric(s1q4),
    
    # Sexe labelisé
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    
    # Lien de parenté regroupé en 4 catégories
    lien_parente = case_when(
      s1q3 == 1          ~ "Chef de ménage",
      s1q3 == 2          ~ "Conjoint(e)",
      s1q3 %in% c(3, 4, 5) ~ "Enfant",
      s1q3 %in% c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15)   ~ "Autre"
    ) |> factor(levels = c("Chef de ménage", "Conjoint(e)", "Enfant", "Autre")),
    
    # Groupes quinquennaux pour pyramide des âges
    groupe_age = cut(
      age,
      breaks = seq(0, 105, by = 5),
      right  = FALSE,
      labels = paste(seq(0, 104, by = 5), sep = "-")
    )
  ) |>
  # Supprimer les âges aberrants
  #filter(age | (age >= 0 & age <= 100))|>
  filter(is.na(age) | (age >= 0 & age <= 100))
  

sect1_clean <- sect1_clean |>
  select(hhid, indiv, age, sexe, lien_parente, groupe_age) 

cat("Lignes avant nettoyage :", nrow(sect1_w4), "\n")
cat("Lignes après nettoyage :", nrow(sect1_clean), "\n")



# --- Nettoyage secta (zone, état, LGA) ---------------------------------------
secta_clean <- secta_w4 |>
  select(hhid, zone, state, lga, wt_wave4, sector) |>
  mutate(
    zone = factor(
      zone,
      levels = c(1,2,3,4,5,6),
      labels = c("Centre-Nord","Nord-Est","Nord-Ouest",
                 "Sud-Est","Sud-Sud","Sud-Ouest")
    ),
    
    zone1 = factor(
      sector,
      levels = c(1,2),
      labels = c("Urban","Rural")
    ),
    
    
    poids_menage = as.numeric(wt_wave4)
  ) |>
  distinct(hhid, .keep_all = TRUE)




# --- Taille des ménages (nombre de membres par hhid) -------------------------
taille_menage <- sect1_clean |>
  group_by(hhid) |>
  summarise(taille_menage = n(), .groups = "drop")

summary(taille_menage$taille_menage, na.rm = FALSE) 
sum(is.na(taille_menage$taille_menage))

# --- Jointure finale : individus + zone + taille -----------------------------
data_tp1 <- sect1_clean |>
  left_join(secta_clean,   by = "hhid") |>
  left_join(taille_menage, by = "hhid")

data_tp1 <- data_tp1 |> mutate(poids_individu = poids_menage*taille_menage)

cat("Dimensions du fichier final :", dim(data_tp1), "\n")
glimpse(data_tp1)

# --- Sauvegarde --------------------------------------------------------------
saveRDS(data_tp1,      "data/processed/data_tp1_clean.rds")
saveRDS(taille_menage, "data/processed/taille_menage.rds")

