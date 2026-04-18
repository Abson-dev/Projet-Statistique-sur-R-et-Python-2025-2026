# ============================================================
#  02_nettoyage.R — Tâche 19
#  Nettoyage et construction des variables
# ============================================================

source("R/01_import.R")

# On travaille sur une copie des données brutes
parcelles <- parcelles_brut
tenure    <- tenure_brut

# ── 1. Fusionner les poids depuis le fichier de couverture ───
# Les poids ne sont pas dans le fichier parcelles

poids <- couverture %>%
  select(hhid, wt_wave4, wt_longpanel, strata, cluster = ea)

parcelles <- parcelles %>%
  left_join(poids, by = "hhid")

# 115 ménages sans poids — hors panel de sondage

# ── 2. Facteurs de conversion zone-spécifiques ───────────────
# Source : World Bank GHS Nigeria Documentation
# Nécessaire car les superficies sont déclarées en unités
# locales variables selon la région

facteurs <- tribble(
  ~zone, ~heaps,   ~ridges,  ~stands,
  1,     0.00012,  0.00270,  0.00006,
  2,     0.00016,  0.00400,  0.00016,
  3,     0.00011,  0.00494,  0.00004,
  4,     0.00019,  0.00230,  0.00004,
  5,     0.00021,  0.00230,  0.00013,
  6,     0.00012,  0.00001,  0.00041
)

parcelles <- parcelles %>%
  mutate(zone = as.numeric(zone)) %>%
  left_join(facteurs, by = "zone")

# ── 3. Conversion des superficies en hectares ────────────────
# s11aq4b : code de l'unité de mesure déclarée
# s11aq4aa : valeur brute de la superficie

parcelles <- parcelles %>%
  mutate(
    superficie_ha = case_when(
      s11aq4b == 1 ~ as.numeric(s11aq4aa) * heaps,    # Heaps
      s11aq4b == 2 ~ as.numeric(s11aq4aa) * ridges,   # Ridges
      s11aq4b == 3 ~ as.numeric(s11aq4aa) * stands,   # Stands
      s11aq4b == 5 ~ as.numeric(s11aq4aa) * 0.404686, # Acres
      s11aq4b == 6 ~ as.numeric(s11aq4aa) * 1.0,      # Hectares
      s11aq4b == 7 ~ as.numeric(s11aq4aa) / 10000,    # Mètres carrés
      TRUE         ~ NA_real_                           # Code inconnu
    )
  )

# Résultats contrôle qualité :
# 4 valeurs manquantes (codes unité non reconnus)
# 0 valeurs négatives
# 5 valeurs aberrantes > 500 ha

# ── 4. Agrégation par ménage ─────────────────────────────────

menages <- parcelles %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    wt_longpanel         = first(wt_longpanel),
    strata               = first(strata),
    cluster              = first(cluster),
    .groups = "drop"
  )
# ============================================================
#  02_nettoyage.R — Tâche 19
#  Nettoyage et construction des variables
# ============================================================

source("R/01_import.R")

# On travaille sur une copie des données brutes
parcelles <- parcelles_brut
tenure    <- tenure_brut

# ── 1. Fusionner les poids depuis le fichier de couverture ───
# Les poids ne sont pas dans le fichier parcelles

poids <- couverture %>%
  select(hhid, wt_wave4, wt_longpanel, strata, cluster = ea)

parcelles <- parcelles %>%
  left_join(poids, by = "hhid")

# 115 ménages sans poids — hors panel de sondage

# ── 2. Facteurs de conversion zone-spécifiques ───────────────
# Source : World Bank GHS Nigeria Documentation
# Nécessaire car les superficies sont déclarées en unités
# locales variables selon la région

facteurs <- tribble(
  ~zone, ~heaps,   ~ridges,  ~stands,
  1,     0.00012,  0.00270,  0.00006,
  2,     0.00016,  0.00400,  0.00016,
  3,     0.00011,  0.00494,  0.00004,
  4,     0.00019,  0.00230,  0.00004,
  5,     0.00021,  0.00230,  0.00013,
  6,     0.00012,  0.00001,  0.00041
)

parcelles <- parcelles %>%
  mutate(zone = as.numeric(zone)) %>%
  left_join(facteurs, by = "zone")

# ── 3. Conversion des superficies en hectares ────────────────
# s11aq4b : code de l'unité de mesure déclarée
# s11aq4aa : valeur brute de la superficie

parcelles <- parcelles %>%
  mutate(
    superficie_ha = case_when(
      s11aq4b == 1 ~ as.numeric(s11aq4aa) * heaps,    # Heaps
      s11aq4b == 2 ~ as.numeric(s11aq4aa) * ridges,   # Ridges
      s11aq4b == 3 ~ as.numeric(s11aq4aa) * stands,   # Stands
      s11aq4b == 5 ~ as.numeric(s11aq4aa) * 0.404686, # Acres
      s11aq4b == 6 ~ as.numeric(s11aq4aa) * 1.0,      # Hectares
      s11aq4b == 7 ~ as.numeric(s11aq4aa) / 10000,    # Mètres carrés
      TRUE         ~ NA_real_                           # Code inconnu
    )
  )

# Résultats contrôle qualité :
# 4 valeurs manquantes (codes unité non reconnus)
# 0 valeurs négatives
# 5 valeurs aberrantes > 500 ha

# ── 4. Agrégation par ménage ─────────────────────────────────

menages <- parcelles %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    wt_longpanel         = first(wt_longpanel),
    strata               = first(strata),
    cluster              = first(cluster),
    .groups = "drop"
  )

# 3 913 ménages | médiane 0.988 ha | moyenne 1.89 ha

# ── 5. Sauvegarde ────────────────────────────────────────────

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

saveRDS(parcelles, "data/processed/parcelles.rds")
saveRDS(menages,   "data/processed/menages.rds")
# 3 913 ménages | médiane 0.988 ha | moyenne 1.89 ha

# ── 5. Sauvegarde ────────────────────────────────────────────

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

saveRDS(parcelles, "data/processed/parcelles.rds")
saveRDS(menages,   "data/processed/menages.rds")