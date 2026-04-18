# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# SCRIPT   : scripts/05_tache24_carte.R
# TÂCHE 24 : Carte choroplèthe + top 5 / bottom 5 états
# BASE     : data/processed/superficie_menage.rds
#            data/raw/gadm41_NGA_1.shp
# SORTIES  : outputs/figures/t24_*.png | outputs/tables/t24_*.csv
# ================================================================

source("scripts/functions.R")

library(dplyr)
library(ggplot2)
library(sf)
library(scales)

superficie_menage <- readRDS("data/processed/superficie_menage.rds")


# ================================================================
# ÉTAPE 1 — MÉDIANE PONDÉRÉE PAR ÉTAT
# On utilise weight_wave4 pour calculer une médiane représentative
# de la population de chaque état (et non de l'échantillon brut)
# ================================================================

sup_par_etat <- superficie_menage %>%
  filter(!is.na(superficie_totale),
         !is.na(nom_etat),
         !is.na(poids_vague4)) %>%
  group_by(nom_etat) %>%
  summarise(
    n_menages        = n(),
    mediane_pond_ha  = mediane_ponderee(superficie_totale, poids_vague4),
    mediane_brute_ha = median(superficie_totale),
    moyenne_ha       = weighted.mean(superficie_totale, poids_vague4),
    .groups          = "drop"
  ) %>%
  mutate(across(c(mediane_pond_ha, mediane_brute_ha, moyenne_ha),
                ~round(., 3)))

cat("=== Statistiques par état ===\n")
print(sup_par_etat, n = 40)
sauvegarder_tableau(sup_par_etat, "t24_superficie_par_etat.csv")


# ================================================================
# ÉTAPE 2 — CHARGEMENT ET HARMONISATION DU SHAPEFILE GADM
# Le shapefile GADM (niveau 1) contient les 36 états + FCT.
# Les noms GADM diffèrent parfois des noms GHS → harmonisation.
# ================================================================

nga_etats <- st_read("data/raw/gadm41_NGA_1.shp", quiet = TRUE)

cat("\nColonnes du shapefile :\n")
print(names(nga_etats))

# Extraire les noms GADM pour inspection
noms_gadm <- nga_etats$NAME_1
cat("\nNoms d'états dans GADM :\n")
print(sort(noms_gadm))

cat("\nNoms d'états dans les données GHS :\n")
print(sort(unique(sup_par_etat$nom_etat)))


# ================================================================
# ÉTAPE 3 — TABLE DE CORRESPONDANCE NOM GHS → NOM GADM
# On corrige les divergences connues.
# ================================================================

correspondance_noms <- tribble(
  ~nom_etat_ghs,           ~nom_etat_gadm,
  "Abia",                  "Abia",
  "Adamawa",               "Adamawa",
  "Akwa Ibom",             "Akwa Ibom",
  "Anambra",               "Anambra",
  "Bauchi",                "Bauchi",
  "Bayelsa",               "Bayelsa",
  "Benue",                 "Benue",
  "Borno",                 "Borno",
  "Cross River",           "Cross River",
  "Delta",                 "Delta",
  "Ebonyi",                "Ebonyi",
  "Edo",                   "Edo",
  "Ekiti",                 "Ekiti",
  "Enugu",                 "Enugu",
  "FCT",                   "Federal Capital Territory",
  "Gombe",                 "Gombe",
  "Imo",                   "Imo",
  "Jigawa",                "Jigawa",
  "Kaduna",                "Kaduna",
  "Kano",                  "Kano",
  "Katsina",               "Katsina",
  "Kebbi",                 "Kebbi",
  "Kogi",                  "Kogi",
  "Kwara",                 "Kwara",
  "Lagos",                 "Lagos",
  "Nasarawa",              "Nassarawa",
  "Niger",                 "Niger",
  "Ogun",                  "Ogun",
  "Ondo",                  "Ondo",
  "Osun",                  "Osun",
  "Oyo",                   "Oyo",
  "Plateau",               "Plateau",
  "Rivers",                "Rivers",
  "Sokoto",                "Sokoto",
  "Taraba",                "Taraba",
  "Yobe",                  "Yobe",
  "Zamfara",               "Zamfara"
)

# Joindre la table de correspondance aux données GHS
sup_par_etat <- sup_par_etat %>%
  left_join(correspondance_noms,
            by = c("nom_etat" = "nom_etat_ghs"))

# Vérifier les états sans correspondance
sans_match <- sup_par_etat %>%
  filter(is.na(nom_etat_gadm)) %>%
  pull(nom_etat)
if (length(sans_match) > 0) {
  cat("\nATTENTION — États sans correspondance GADM :", sans_match, "\n")
} else {
  cat("\nToutes les correspondances sont établies.\n")
}

# Jointure spatiale : shapefile + données
nga_carte <- nga_etats %>%
  left_join(
    sup_par_etat %>% select(nom_etat_gadm, mediane_pond_ha, n_menages),
    by = c("NAME_1" = "nom_etat_gadm")
  )

cat(sprintf("États avec données : %d / %d\n",
            sum(!is.na(nga_carte$mediane_pond_ha)),
            nrow(nga_carte)))


# ================================================================
# ÉTAPE 4 — CARTE CHOROPLÈTHE (superficie médiane pondérée)
# Palette YlOrRd (jaune → rouge = faible → élevé)
# ================================================================

p_carte <- ggplot(nga_carte) +
  geom_sf(aes(fill = mediane_pond_ha),
          color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low      = "#FFF7BC",
    high     = "#B30000",
    na.value = "grey80",
    name     = "Médiane\n(ha)",
    labels   = label_number(accuracy = 0.01)
  ) +
  labs(
    title    = "Superficie médiane des exploitations par état",
    subtitle = "Médiane pondérée (weight_wave4) — GHS Nigeria W4",
    caption  = "Source : NBS Nigeria GHS Panel W4 (2018-2019) | Fond : GADM v4.1"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption  = element_text(hjust = 0.5, color = "grey55", size = 9),
    legend.position = "right"
  )

print(p_carte)
sauvegarder_figure("t24_carte_superficie_etats.png", largeur = 10, hauteur = 8)


# ================================================================
# ÉTAPE 5 — TABLEAU TOP 5 / BOTTOM 5
# ================================================================

classement <- sup_par_etat %>%
  arrange(desc(mediane_pond_ha)) %>%
  mutate(rang = row_number())

top5    <- head(classement, 5) %>% mutate(groupe = "Top 5 (plus grande superficie)")
bottom5 <- tail(classement, 5) %>% mutate(groupe = "Bottom 5 (plus petite superficie)")

tableau_top_bottom <- bind_rows(top5, bottom5) %>%
  select(rang, nom_etat, mediane_pond_ha, mediane_brute_ha, n_menages, groupe)

cat("\n=== Top 5 / Bottom 5 des états ===\n")
print(tableau_top_bottom, n = 10)
sauvegarder_tableau(tableau_top_bottom, "t24_top5_bottom5_etats.csv")


# ================================================================
# ÉTAPE 6 — GRAPHIQUE CLASSEMENT DES ÉTATS
# Barplot horizontal avec Top 5 en vert et Bottom 5 en rouge
# ================================================================

p_classement <- tableau_top_bottom %>%
  mutate(
    nom_etat = reorder(nom_etat, mediane_pond_ha),
    couleur  = ifelse(groupe == "Top 5 (plus grande superficie)",
                      "#238B45", "#CB181D")
  ) %>%
  ggplot(aes(x = nom_etat, y = mediane_pond_ha, fill = couleur)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f ha", mediane_pond_ha)),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(tableau_top_bottom$mediane_pond_ha) * 1.2)) +
  scale_fill_identity() +
  labs(
    title    = "Top 5 et Bottom 5 des états — superficie médiane",
    subtitle = "Vert = plus grande superficie | Rouge = plus petite",
    x        = NULL,
    y        = "Superficie médiane pondérée (ha)",
    caption  = "GHS Nigeria W4 — médiane pondérée (weight_wave4)"
  ) +
  theme_tp4()

print(p_classement)
sauvegarder_figure("t24_top5_bottom5.png", largeur = 10, hauteur = 6)


# ================================================================
# ÉTAPE 7 — INDICATEURS DE DISPARITÉ GÉOGRAPHIQUE
# ================================================================

cat("\n=== Disparités géographiques ===\n")
cat(sprintf("Maximum  : %s — %.3f ha\n",
            classement$nom_etat[1], classement$mediane_pond_ha[1]))
cat(sprintf("Minimum  : %s — %.3f ha\n",
            classement$nom_etat[nrow(classement)],
            classement$mediane_pond_ha[nrow(classement)]))
cat(sprintf("Ratio max/min : %.1f\n",
            classement$mediane_pond_ha[1] /
              classement$mediane_pond_ha[nrow(classement)]))
cat(sprintf("CV inter-états : %.1f%%\n",
            sd(classement$mediane_pond_ha) /
              mean(classement$mediane_pond_ha) * 100))

cat("\n✔ Tâche 24 terminée.\n")
