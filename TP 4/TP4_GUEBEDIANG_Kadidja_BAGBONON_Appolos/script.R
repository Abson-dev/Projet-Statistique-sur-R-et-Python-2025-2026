

# 0. CHARGEMENT DES PACKAGES

#install.packages("dplyr")
#library(dplyr)
#install.packages("rlang")
install.packages("remotes")
remotes::install_github("ropensci/rnaturalearthhires")
packages_requis <- c(
  "haven", "survey", "srvyr", "ggplot2", "scales",
  "gtsummary", "patchwork", "ggrepel", "naniar", "forcats",
  "sf", "rnaturalearth", "rnaturalearthdata", "viridis", "gt")

for (pkg in packages_requis) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
library(dplyr)
# Dossier de sortie
dir.create("output", showWarnings = FALSE)

#sessionInfo()


# 1. IMPORTATION DES DONNÉES


url_base <- "https://raw.githubusercontent.com/KadidjaGUEBEDIANG/GHS/main/"

fichiers <- c(
  "sect11a1_plantingw4.dta",
  "secta1_harvestw4.dta",
  "secta_plantingw4.dta",
  "sect11a_plantingw4.dta",
  "sect11b1_plantingw4.dta",
  "secta_harvestw4.dta"
)

noms <- c(
  "sect11a1_plantingw4", "secta1_harvestw4", "secta_plantingw4",
  "sect11a_plantingw4", "sect11b1_plantingw4", "secta_harvestw4"
)

for (i in seq_along(fichiers)) {
  temp <- tempfile(fileext = ".dta")
  download.file(paste0(url_base, fichiers[i]), temp, mode = "wb")
  assign(noms[i], read_dta(temp))
}

cat("✓ Données importées\n")


# 2. NETTOYAGE & PRÉPARATION (pré-Q19)


# --- Identifier les ménages à interview incomplète ---
menages_interview_incomplet <- secta_plantingw4 %>%
  filter(interview_result != 1) %>%
  inner_join(sect11a_plantingw4, by = "hhid")

hhid_communs <- intersect(sect11a1_plantingw4$hhid, menages_interview_incomplet$hhid)

# --- Bases nettoyées (sans les 3 hhid problématiques) ---
sect11a1_plantingw4_propre <- sect11a1_plantingw4 %>%
  filter(!hhid %in% hhid_communs)

sect11a_plantingw4_propre <- sect11a_plantingw4 %>%
  filter(!hhid %in% hhid_communs)

# --- Facteurs de conversion unités locales → hectares (par zone géopolitique) ---
# zone : 1=NC, 2=NE, 3=NW, 4=SE, 5=SS, 6=SW
sect11a1_plantingw4_propre <- sect11a1_plantingw4_propre %>%
  mutate(
    facteur = case_when(
      # HEAPS (1)
      s11aq4b == 1 & zone == 1 ~ 0.00012,
      s11aq4b == 1 & zone == 2 ~ 0.00016,
      s11aq4b == 1 & zone == 3 ~ 0.00011,
      s11aq4b == 1 & zone == 4 ~ 0.00019,
      s11aq4b == 1 & zone == 5 ~ 0.00021,
      s11aq4b == 1 & zone == 6 ~ 0.00012,
      # RIDGES (2)
      s11aq4b == 2 & zone == 1 ~ 0.0027,
      s11aq4b == 2 & zone == 2 ~ 0.004,
      s11aq4b == 2 & zone == 3 ~ 0.00494,
      s11aq4b == 2 & zone == 4 ~ 0.0023,
      s11aq4b == 2 & zone == 5 ~ 0.0023,
      s11aq4b == 2 & zone == 6 ~ 0.00001,
      # STANDS (3)
      s11aq4b == 3 & zone == 1 ~ 0.00006,
      s11aq4b == 3 & zone == 2 ~ 0.00016,
      s11aq4b == 3 & zone == 3 ~ 0.00004,
      s11aq4b == 3 & zone == 4 ~ 0.00004,
      s11aq4b == 3 & zone == 5 ~ 0.00013,
      s11aq4b == 3 & zone == 6 ~ 0.00041,
      TRUE ~ NA_real_
    ),
    superficie = case_when(
      s11aq4a == 1                      ~ s11aq4c / 10000,       # GPS m² → ha
      s11aq4a == 2 & s11aq4b == 6       ~ s11aq4aa,              # ha
      s11aq4a == 2 & s11aq4b == 5       ~ s11aq4aa * 0.4,        # acres
      s11aq4a == 2 & s11aq4b == 7       ~ s11aq4aa / 10000,      # m²
      s11aq4a == 2 & s11aq4b %in% c(1,2,3) ~ s11aq4aa * facteur, # locales
      TRUE ~ NA_real_
    )
  )

# --- Jointure pour construire la base principale ---
secta1_harvestw4_sup <- secta1_harvestw4 %>%
  left_join(
    sect11a1_plantingw4_propre %>% select(hhid, plotid, superficie),
    by = c("hhid", "plotid")
  ) %>%
  left_join(
    sect11b1_plantingw4 %>% select(hhid, plotid, s11b1q4),
    by = c("hhid", "plotid")
  ) %>%
  left_join(
    secta_harvestw4 %>% select(hhid, wt_wave4),
    by = "hhid"
  ) %>%
  left_join(
    sect11a1_plantingw4 %>% select(hhid, plotid, s11aq4aa, s11aq4b),
    by = c("hhid", "plotid")
  )

# --- Superficie estimée (déclarée) en ha ---
secta1_harvestw4_sup <- secta1_harvestw4_sup %>%
  mutate(
    facteur = case_when(
      s11aq4b == 1 & zone == 1 ~ 0.00012, s11aq4b == 1 & zone == 2 ~ 0.00016,
      s11aq4b == 1 & zone == 3 ~ 0.00011, s11aq4b == 1 & zone == 4 ~ 0.00019,
      s11aq4b == 1 & zone == 5 ~ 0.00021, s11aq4b == 1 & zone == 6 ~ 0.00012,
      s11aq4b == 2 & zone == 1 ~ 0.0027,  s11aq4b == 2 & zone == 2 ~ 0.004,
      s11aq4b == 2 & zone == 3 ~ 0.00494, s11aq4b == 2 & zone == 4 ~ 0.0023,
      s11aq4b == 2 & zone == 5 ~ 0.0023,  s11aq4b == 2 & zone == 6 ~ 0.00001,
      s11aq4b == 3 & zone == 1 ~ 0.00006, s11aq4b == 3 & zone == 2 ~ 0.00016,
      s11aq4b == 3 & zone == 3 ~ 0.00004, s11aq4b == 3 & zone == 4 ~ 0.00004,
      s11aq4b == 3 & zone == 5 ~ 0.00013, s11aq4b == 3 & zone == 6 ~ 0.00041,
      TRUE ~ NA_real_
    ),
    superficie_estimee = case_when(
      s11aq4b == 6                  ~ s11aq4aa,
      s11aq4b == 5                  ~ s11aq4aa * 0.4,
      s11aq4b == 7                  ~ s11aq4aa / 10000,
      s11aq4b %in% c(1,2,3)        ~ s11aq4aa * facteur,
      TRUE ~ NA_real_
    )
  )

# --- Superficie totale par ménage (NA si au moins un plot manquant) ---
secta1_harvestw4_sup <- secta1_harvestw4_sup %>%
  group_by(hhid) %>%
  mutate(
    superficie_par_menage = ifelse(
      any(is.na(superficie)), NA_real_, sum(superficie)
    ),
    nb_parcelles = n_distinct(plotid)
  ) %>%
  ungroup()

# --- Strate et filtre poids manquants ---
secta1_harvestw4_sup <- secta1_harvestw4_sup %>%
  mutate(strata = paste(zone, sector, sep = "_")) %>%
  filter(!is.na(wt_wave4))

cat("✓ Base principale construite :",
    n_distinct(secta1_harvestw4_sup$hhid), "ménages\n")


# 3. DESIGN D'ENQUÊTE (pondération)
#    Note : on utilise srvyr pour avoir dplyr-friendly survey


# Design parcelle (1 ligne = 1 parcelle)
design_parcelle <- svydesign(
  ids     = ~1,
  weights = ~wt_wave4,
  data    = secta1_harvestw4_sup %>% filter(!is.na(superficie)),
  nest    = TRUE
)

# Design ménage (1 ligne = 1 ménage)
data_menage <- secta1_harvestw4_sup %>%
  distinct(hhid, .keep_all = TRUE) %>%
  filter(!is.na(superficie_par_menage), superficie_par_menage > 0, !is.na(wt_wave4))

design_menage <- svydesign(
  ids     = ~1,
  weights = ~wt_wave4,
  data    = data_menage,
  nest    = TRUE
)

# Versions srvyr
srvyr_parcelle <- as_survey_design(design_parcelle)
srvyr_menage   <- as_survey_design(design_menage)

cat("✓ Designs d'enquête créés\n")

# Palette de couleurs zones géopolitiques
couleurs_zone <- c(
  " North Central" = "#2E86AB",
  " North East"    = "#A23B72",
  " North West"    = "#F18F01",
  " South East"    = "#C73E1D",
  " South South"   = "#3B1F2B",
  " South West"    = "#44BBA4"
)

# Thème publication
theme_ghs <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle    = element_text(color = "grey40", size = 11, hjust = 0),
    plot.caption     = element_text(color = "grey55", size = 9, hjust = 1),
    axis.title       = element_text(face = "bold", size = 11),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
theme_set(theme_ghs)

caption_note <- paste0(
  "Source : GHS-Panel Wave 4, Nigeria 2018/2019 (NBS/Banque Mondiale)\n",
  "Estimations pondérées — représentatives à l'échelle nationale"
)



# QUESTION 19 — PRÉPARATION : VALEURS MANQUANTES & ABERRANTES


cat("\n===== QUESTION 19 : VALEURS MANQUANTES & ABERRANTES =====\n")

# --- 19A. Exploration initiale ---
cat("\n--- Structure générale ---\n")
glimpse(secta1_harvestw4)
cat("\nNombre de ménages (harvestw4) :", n_distinct(secta1_harvestw4$hhid), "\n")
cat("Nombre de parcelles (harvestw4) :", nrow(secta1_harvestw4), "\n")

# --- 19B. Doublons sur hhid + plotid ---
doublons_q19 <- secta1_harvestw4_sup %>%
  group_by(hhid, plotid) %>%
  filter(n() > 1) %>%
  ungroup()
cat("\nNombre de lignes en doublon (hhid × plotid) :", nrow(doublons_q19), "\n")

# --- 19C. Carte des valeurs manquantes (naniar) ---
# Sur un sous-ensemble des variables-clés pour la lisibilité
vars_cles <- secta1_harvestw4_sup %>%
  select(hhid, plotid, superficie, superficie_estimee,
         superficie_par_menage, s11b1q4, wt_wave4, zone, state, sector)

p_vis_miss <- vis_miss(vars_cles, warn_large_data = FALSE) +
  labs(
    title   = "Carte des valeurs manquantes — variables agricoles clés",
    caption = caption_note
  )

ggsave("output/fig0_vis_miss.png",
       p_vis_miss, width = 12, height = 6, dpi = 300, bg = "white")
cat("✓ Figure 0 sauvegardée : output/fig0_vis_miss.png\n")

# Résumé textuel des NA
cat("\nProportion de valeurs manquantes par variable-clé :\n")
vars_cles %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "pct_NA") %>%
  arrange(desc(pct_NA)) %>%
  print()

# --- 19D. Valeurs aberrantes de superficie ---
cat("\n--- Valeurs aberrantes : superficie ---\n")

# Superficie négative ou nulle
nb_neg <- sum(secta1_harvestw4_sup$superficie <= 0, na.rm = TRUE)
# Superficie > 500 ha (seuil agronomique Nigeria)
nb_sup500 <- sum(secta1_harvestw4_sup$superficie > 500, na.rm = TRUE)
# Top 5 valeurs extrêmes
top5_superf <- secta1_harvestw4_sup %>%
  filter(!is.na(superficie)) %>%
  arrange(desc(superficie)) %>%
  select(hhid, plotid, superficie, zone, state) %>%
  head(5)

cat("  Superficie ≤ 0 ha :", nb_neg, "parcelles\n")
cat("  Superficie > 500 ha :", nb_sup500, "parcelles\n")
cat("  Top 5 valeurs :\n")
print(top5_superf)

# Statistiques de base (pondérées) sur superficie par parcelle
cat("\n--- Statistiques descriptives initiales (pondérées) ---\n")
svymean(~superficie, design_parcelle, na.rm = TRUE)
svyquantile(~superficie, design_parcelle,
            quantiles = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
            na.rm = TRUE)



# QUESTION 20 — ANALYSE UNIVARIÉE DE LA SUPERFICIE


cat("\n===== QUESTION 20 : ANALYSE UNIVARIÉE DE LA SUPERFICIE =====\n")

# ─── 20A. Statistiques descriptives pondérées par décile ───

# Déciles (parcelle)
deciles_parcelle <- svyquantile(
  ~superficie, design_parcelle,
  quantiles = seq(0, 1, 0.1), na.rm = TRUE
)
cat("\nDéciles superficie/parcelle (pondérés) :\n")
print(deciles_parcelle)

# Déciles (ménage)
deciles_menage <- svyquantile(
  ~superficie_par_menage, design_menage,
  quantiles = seq(0, 1, 0.1), na.rm = TRUE
)
cat("\nDéciles superficie/ménage (pondérés) :\n")
print(deciles_menage)

# Tableau gtsummary PONDÉRÉ avec tbl_svysummary 


srvyr_stats <- as_survey_design(
  svydesign(
    ids     = ~1,
    weights = ~wt_wave4,
    data    = secta1_harvestw4_sup %>%
      filter(!is.na(superficie), !is.na(wt_wave4)) %>%
      mutate(
        zone_label   = as_factor(zone),
        sector_label = as_factor(sector)
      ),
    nest = TRUE
  )
)

tbl_superficie <- tbl_svysummary(
  data     = srvyr_stats,
  include  = c(superficie, superficie_par_menage, zone_label),
  by       = sector_label,
  label    = list(
    superficie            ~ "Superficie par parcelle (ha)",
    superficie_par_menage ~ "Superficie par ménage (ha)",
    zone_label            ~ "Zone géopolitique"
  ),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})\n[médiane: {median}; IQR: {p25}–{p75}]",
    all_categorical() ~ "{n_unweighted} obs. → {p}% (pondéré)"
  ),
  digits  = all_continuous() ~ 3,
  missing = "ifany",
  missing_text = "Manquant"
) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption(
    "**Tableau 1.** Statistiques descriptives de la superficie agricole par milieu — Nigeria 2018/2019 (estimations pondérées)"
  )

tbl_superficie %>%
  as_gt() %>%
  gt::gtsave("output/tableau1_stats_superficie.html")
cat("✓ Tableau 1 sauvegardé : output/tableau1_stats_superficie.html\n")

# Note pédagogique sur les pondérations
cat("\n--- NOTE SUR LES PONDÉRATIONS ---\n")
cat("  Effectif brut (ménages dans l'échantillon) :",
    n_distinct(secta1_harvestw4_sup$hhid), "\n")
cat("  Effectif pondéré estimé (ménages nigérians représentés) :",
    round(sum(data_menage$wt_wave4)), "\n")
cat("  → Le Nigeria compte ~40 millions de ménages.\n")
cat("    tbl_svysummary() affiche les effectifs pondérés, pas les effectifs bruts.\n")
cat("    tbl_summary() (sans 'svy') affiche l'échantillon brut : c'était l'erreur.\n")

# ─── 20B. Histogrammes (échelle log) ───

# Histogramme superficie/parcelle
data_hist <- secta1_harvestw4_sup %>%
  filter(!is.na(superficie), superficie > 0, !is.na(wt_wave4))

p_hist_parcelle <- ggplot(data_hist, aes(x = superficie, weight = wt_wave4)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins  = 50, fill = "#2E86AB", color = "white", alpha = 0.85
  ) +
  geom_density(
    aes(weight = wt_wave4 / sum(wt_wave4, na.rm = TRUE)),
    color = "#C73E1D", linewidth = 1, linetype = "dashed"
  ) +
  scale_x_log10(
    labels = label_number(accuracy = 0.01),
    breaks = c(0.01, 0.1, 0.5, 1, 2, 5, 10, 50, 100)
  ) +
  labs(
    title    = "Distribution de la superficie par parcelle (échelle logarithmique)",
    subtitle = "Estimations pondérées — représentatives à l'échelle nationale",
    x        = "Superficie (hectares, log₁₀)", y = "Densité", caption = caption_note
  )

ggsave("output/fig1a_histogramme_parcelle.png",
       p_hist_parcelle, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Figure 1a sauvegardée\n")

# Histogramme superficie/ménage
data_hist_menage <- secta1_harvestw4_sup %>%
  distinct(hhid, .keep_all = TRUE) %>%
  filter(!is.na(superficie_par_menage), superficie_par_menage > 0, !is.na(wt_wave4))

p_hist_menage <- ggplot(data_hist_menage,
                        aes(x = superficie_par_menage, weight = wt_wave4)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins  = 50, fill = "#44BBA4", color = "white", alpha = 0.85
  ) +
  geom_density(
    aes(weight = wt_wave4 / sum(wt_wave4, na.rm = TRUE)),
    color = "#A23B72", linewidth = 1, linetype = "dashed"
  ) +
  scale_x_log10(
    labels = label_number(accuracy = 0.01),
    breaks = c(0.01, 0.1, 0.5, 1, 2, 5, 10, 50, 100)
  ) +
  labs(
    title    = "Distribution de la superficie totale par ménage (échelle logarithmique)",
    subtitle = "Estimations pondérées — représentatives à l'échelle nationale",
    x        = "Superficie totale (hectares, log₁₀)", y = "Densité", caption = caption_note
  )

ggsave("output/fig1b_histogramme_menage.png",
       p_hist_menage, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Figure 1b sauvegardée\n")

# ─── 20C. Boxplot par zone géopolitique (quantiles pondérés calculés manuellement) ───

quantiles_ponderes_zone <- secta1_harvestw4_sup %>%
  filter(!is.na(superficie), superficie > 0, !is.na(wt_wave4)) %>%
  mutate(zone_label = as_factor(zone)) %>%
  group_by(zone_label) %>%
  summarise(
    q10 = {
      x <- superficie; w <- wt_wave4
      ord <- order(x); cw <- cumsum(w[ord]); tw <- sum(w)
      x[ord][which(cw >= tw * 0.10)[1]]
    },
    q25 = {
      x <- superficie; w <- wt_wave4
      ord <- order(x); cw <- cumsum(w[ord]); tw <- sum(w)
      x[ord][which(cw >= tw * 0.25)[1]]
    },
    mediane = {
      x <- superficie; w <- wt_wave4
      ord <- order(x); cw <- cumsum(w[ord]); tw <- sum(w)
      x[ord][which(cw >= tw * 0.50)[1]]
    },
    q75 = {
      x <- superficie; w <- wt_wave4
      ord <- order(x); cw <- cumsum(w[ord]); tw <- sum(w)
      x[ord][which(cw >= tw * 0.75)[1]]
    },
    q90 = {
      x <- superficie; w <- wt_wave4
      ord <- order(x); cw <- cumsum(w[ord]); tw <- sum(w)
      x[ord][which(cw >= tw * 0.90)[1]]
    },
    .groups = "drop"
  )

p_boxplot <- ggplot(quantiles_ponderes_zone,
                    aes(y = reorder(zone_label, mediane), fill = zone_label)) +
  geom_crossbar(
    aes(x = mediane, xmin = q25, xmax = q75),
    width = 0.5, alpha = 0.85
  ) +
  geom_errorbarh(
    aes(xmin = q10, xmax = q90),
    height = 0.25, linewidth = 0.7
  ) +
  geom_point(aes(x = mediane), size = 3, shape = 21, fill = "white") +
  scale_x_log10(
    labels = label_number(accuracy = 0.01),
    breaks = c(0.01, 0.1, 0.5, 1, 2, 5, 10, 50)
  ) +
  scale_fill_manual(values = couleurs_zone, guide = "none") +
  labs(
    title    = "Distribution pondérée de la superficie par zone géopolitique",
    subtitle = "Boîtes = Q25–Q75 | Moustaches = D1–D9 | Point = médiane pondérée",
    x        = "Superficie (hectares, log₁₀)", y = NULL, caption = caption_note
  )

ggsave("output/fig2_boxplot_zone.png",
       p_boxplot, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Figure 2 sauvegardée\n")

# ─── 20D. Scatter : superficie déclarée vs GPS ───

data_scatter <- secta1_harvestw4_sup %>%
  filter(
    !is.na(superficie_estimee), superficie_estimee > 0,
    !is.na(superficie),          superficie > 0,
    !is.na(wt_wave4)
  ) %>%
  filter(
    superficie_estimee < quantile(superficie_estimee, 0.99, na.rm = TRUE),
    superficie         < quantile(superficie,         0.99, na.rm = TRUE)
  )

rho <- cor(data_scatter$superficie_estimee, data_scatter$superficie,
           method = "spearman", use = "complete.obs")

p_scatter <- ggplot(data_scatter,
                    aes(x = superficie_estimee, y = superficie,
                        size = wt_wave4, alpha = wt_wave4)) +
  geom_point(color = "#2E86AB", shape = 16) +
  geom_abline(intercept = 0, slope = 1,
              color = "#C73E1D", linewidth = 1.2, linetype = "dashed") +
  geom_smooth(method = "loess", color = "#F18F01",
              linewidth = 1, se = TRUE, alpha = 0.15) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_size_continuous(range = c(0.3, 2), guide = "none") +
  scale_alpha_continuous(range = c(0.2, 0.7), guide = "none") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
           label = paste0("ρ de Spearman = ", round(rho, 3)),
           size = 4.5, fontface = "bold", color = "#2E86AB") +
  labs(
    title    = "Superficie déclarée vs. superficie GPS par parcelle",
    subtitle = "Ligne rouge = équivalence parfaite | Courbe orange = tendance LOESS",
    x        = "Superficie déclarée (ha, log₁₀)",
    y        = "Superficie GPS (ha, log₁₀)",
    caption  = caption_note
  )

ggsave("output/fig3_scatter_gps_vs_declare.png",
       p_scatter, width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ Figure 3 sauvegardée — ρ Spearman :", round(rho, 4), "\n")



# QUESTION 21 — RÉGIME DE TENURE FONCIÈRE

cat("\n===== QUESTION 21 : RÉGIME DE TENURE FONCIÈRE =====\n")

data_tenure <- secta1_harvestw4_sup %>%
  filter(!is.na(s11b1q4), !is.na(wt_wave4)) %>%
  mutate(
    tenure = case_when(
      s11b1q4 == 1 ~ "Achat direct",
      s11b1q4 == 2 ~ "Location (cash/nature)",
      s11b1q4 == 3 ~ "Usage gratuit",
      s11b1q4 == 4 ~ "Distribution communautaire",
      s11b1q4 == 5 ~ "Héritage familial",
      s11b1q4 == 6 ~ "Métayage",
      s11b1q4 == 7 ~ "Échange temporaire",
      TRUE         ~ "Autre"
    ),
    zone_label   = as_factor(zone),
    sector_label = as_factor(sector)
  )

design_tenure <- svydesign(
  ids = ~1, weights = ~wt_wave4, data = data_tenure, nest = TRUE
)

# Proportions pondérées
freq_tenure <- svytable(~tenure, design_tenure)
prop_tenure <- prop.table(freq_tenure) * 100
df_tenure <- data.frame(
  tenure     = names(prop_tenure),
  proportion = as.numeric(prop_tenure),
  effectif   = as.numeric(freq_tenure)
) %>% arrange(desc(proportion))

cat("Répartition pondérée des régimes de tenure :\n")
print(df_tenure)

# Tableau gtsummary pondéré (tbl_svysummary)
srvyr_tenure <- as_survey_design(design_tenure)
tbl_tenure <- tbl_svysummary(
  data     = srvyr_tenure,
  include  = c(tenure, sector_label),
  by       = sector_label,
  label    = list(tenure ~ "Régime de tenure foncière"),
  statistic = all_categorical() ~ "{n_unweighted} obs. → {p}% (pondéré)",
  missing   = "no"
) %>%
  add_overall() %>%
  add_p(test = all_categorical() ~ "svychisq") %>%
  bold_labels() %>%
  bold_p(t = 0.05) %>%
  modify_header(label ~ "**Régime de tenure**") %>%
  modify_caption(
    "**Tableau 2.** Répartition pondérée des régimes de tenure foncière par milieu — Nigeria 2018/2019"
  )

tbl_tenure %>%
  as_gt() %>%
  gt::gtsave("output/tableau2_tenure.html")
cat("✓ Tableau 2 sauvegardé\n")

# Barplot horizontal global
p_tenure <- ggplot(df_tenure,
                   aes(x = reorder(tenure, proportion),
                       y = proportion,
                       fill = reorder(tenure, proportion))) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.1, size = 3.8, fontface = "bold") +
  scale_fill_manual(
    values = colorRampPalette(
      c("#2E86AB","#44BBA4","#F18F01","#C73E1D","#A23B72","#3B1F2B","#8B5CF6")
    )(nrow(df_tenure)),
    guide = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(
    title    = "Régimes de tenure foncière des parcelles agricoles",
    subtitle = "Proportions pondérées — représentatives à l'échelle nationale",
    x = NULL, y = "Proportion (%)", caption = caption_note
  )

ggsave("output/fig4_barplot_tenure.png",
       p_tenure, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Figure 4 sauvegardée\n")

# Chi-deux pondéré : tenure × milieu
cat("\n--- Test Chi-deux pondéré : tenure × milieu de résidence ---\n")
chi2_milieu <- svychisq(~tenure + sector_label, design_tenure, statistic = "Chisq")
print(chi2_milieu)

# Chi-deux pondéré : tenure × zone géopolitique
cat("\n--- Test Chi-deux pondéré : tenure × zone géopolitique ---\n")
chi2_zone <- svychisq(~tenure + zone_label, design_tenure, statistic = "Chisq")
print(chi2_zone)

# Barplot tenure par milieu (rural vs urbain)
df_tenure_milieu <- data_tenure %>%
  count(tenure, sector_label, wt = wt_wave4, name = "poids") %>%
  group_by(sector_label) %>%
  mutate(proportion = poids / sum(poids) * 100) %>%
  ungroup()

p_tenure_milieu <- ggplot(df_tenure_milieu,
                          aes(x = reorder(tenure, proportion),
                              y = proportion,
                              fill = sector_label)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  scale_fill_manual(
    values = c("1. Urban" = "#2E86AB", "2. Rural" = "#44BBA4"),
    name = "Milieu"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  coord_flip() +
  labs(
    title    = "Régimes de tenure foncière selon le milieu de résidence",
    subtitle = "Estimations pondérées | Chi-deux pondéré appliqué (svychisq)",
    x = NULL, y = "Proportion (%)", caption = caption_note
  )

ggsave("output/fig5_tenure_par_milieu.png",
       p_tenure_milieu, width = 11, height = 6, dpi = 300, bg = "white")
cat("✓ Figure 5 sauvegardée\n")


# QUESTION 23 — SUPERFICIE TOTALE DU MÉNAGE vs NOMBRE DE PARCELLES


cat("\n===== QUESTION 23 : SUPERFICIE × NOMBRE DE PARCELLES =====\n")

data_q23 <- secta1_harvestw4_sup %>%
  distinct(hhid, .keep_all = TRUE) %>%
  filter(
    !is.na(superficie_par_menage), superficie_par_menage > 0,
    !is.na(nb_parcelles),
    !is.na(wt_wave4)
  ) %>%
  mutate(zone_label = as_factor(zone))

# Corrélation de Spearman avec IC (bootstrap pondéré)
rho_q23 <- cor(data_q23$superficie_par_menage, data_q23$nb_parcelles,
               method = "spearman", use = "complete.obs")

set.seed(42)
n_boot <- 1000
boot_rho <- replicate(n_boot, {
  idx <- sample(nrow(data_q23), replace = TRUE,
                prob = data_q23$wt_wave4 / sum(data_q23$wt_wave4))
  cor(data_q23$superficie_par_menage[idx],
      data_q23$nb_parcelles[idx],
      method = "spearman", use = "complete.obs")
})
ic_low  <- quantile(boot_rho, 0.025, na.rm = TRUE)
ic_high <- quantile(boot_rho, 0.975, na.rm = TRUE)

cat(sprintf("ρ Spearman (superficie × nb parcelles) : %.4f [IC95%% : %.4f – %.4f]\n",
            rho_q23, ic_low, ic_high))

# Scatter + LOESS
p_q23 <- ggplot(data_q23,
                aes(x = nb_parcelles, y = superficie_par_menage,
                    size = wt_wave4, color = zone_label)) +
  geom_point(alpha = 0.45, shape = 16) +
  geom_smooth(aes(weight = wt_wave4, group = 1),
              method = "loess", color = "#C73E1D",
              linewidth = 1.3, se = TRUE, fill = "#C73E1D", alpha = 0.15) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1:13) +
  scale_color_manual(values = couleurs_zone, name = "Zone") +
  scale_size_continuous(range = c(0.5, 3), guide = "none") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
           label = sprintf("ρ = %.3f [IC95%% : %.3f – %.3f]",
                           rho_q23, ic_low, ic_high),
           size = 4.2, fontface = "bold", color = "#2E86AB") +
  labs(
    title    = "Superficie totale du ménage selon le nombre de parcelles cultivées",
    subtitle = "Courbe rouge = tendance LOESS pondérée | Taille des points ∝ poids",
    x        = "Nombre de parcelles",
    y        = "Superficie totale (ha, log₁₀)",
    caption  = caption_note
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

ggsave("output/fig6_scatter_superficie_nparcelles.png",
       p_q23, width = 11, height = 7, dpi = 300, bg = "white")
cat("✓ Figure 6 sauvegardée\n")

# Tableau superficies médianes par nombre de parcelles (srvyr)
design_q23 <- svydesign(ids = ~1, weights = ~wt_wave4, data = data_q23, nest = TRUE)
srvyr_q23  <- as_survey_design(design_q23)

tbl_q23 <- srvyr_q23 %>%
  group_by(nb_parcelles) %>%
  summarise(
    mediane_pond = survey_median(superficie_par_menage, na.rm = TRUE, vartype = "ci"),
    n_pond       = survey_total(vartype = NULL)
  ) %>%
  rename(
    "Nb parcelles"          = nb_parcelles,
    "Superficie médiane (ha)" = mediane_pond,
    "IC bas 95%"            = mediane_pond_low,
    "IC haut 95%"           = mediane_pond_upp,
    "Effectif pondéré"      = n_pond
  ) %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Superficie médiane par ménage selon le nombre de parcelles",
    subtitle = "GHS-Panel Wave 4 — Estimations pondérées"
  ) %>%
  gt::fmt_number(columns = 2:4, decimals = 3) %>%
  gt::fmt_number(columns = 5, decimals = 0) %>%
  gt::tab_source_note("Source : NBS/Banque Mondiale — GHS-Panel Wave 4 (2018/2019)")

gt::gtsave(tbl_q23, "output/tableau3_superficie_nparcelles.html")
cat("✓ Tableau 3 sauvegardé\n")


# QUESTION 24 — CARTE CHOROPLÈTHE : SUPERFICIE MÉDIANE PAR ÉTAT

cat("\n===== QUESTION 24 : CARTE CHOROPLÈTHE — SUPERFICIE MÉDIANE PAR ÉTAT =====\n")

# ─── 24A. Calcul de la médiane pondérée par État ───

data_q24 <- secta1_harvestw4_sup %>%
  distinct(hhid, .keep_all = TRUE) %>%
  filter(
    !is.na(superficie_par_menage), superficie_par_menage > 0,
    !is.na(wt_wave4)
  ) %>%
  mutate(state_label = as_factor(state))

mediane_etat <- data_q24 %>%
  group_by(state_label) %>%
  summarise(
    mediane_pond = {
      x <- superficie_par_menage; w <- wt_wave4
      ord <- order(x)
      x_ord <- x[ord]; w_ord <- w[ord]
      cw <- cumsum(w_ord)
      x_ord[which(cw >= sum(w_ord) / 2)[1]]
    },
    n_menages    = n(),
    n_pond       = sum(wt_wave4),
    .groups = "drop"
  ) %>%
  arrange(desc(mediane_pond))

cat("Médiane pondérée de superficie par État :\n")
print(mediane_etat, n = 40)

# ─── 24B. Récupération du shapefile du Nigeria ───

nigeria_sf <- tryCatch({
  ne_states(country = "nigeria", returnclass = "sf")
}, error = function(e) {
  # Fallback : GeoJSON GADM via GitHub
  url_gadm <- "https://raw.githubusercontent.com/longhotsummer/sa-parliament-map/master/json/nigeria-states.json"
  tryCatch(
    sf::read_sf(url_gadm),
    error = function(e2) NULL
  )
})

if (!is.null(nigeria_sf)) {
  cat("✓ Shapefile du Nigeria chargé :",
      nrow(nigeria_sf), "États/entités\n")
  cat("  Colonnes disponibles :", paste(names(nigeria_sf), collapse = ", "), "\n")
} else {
  cat("⚠ Impossible de charger le shapefile — carte non générée.\n")
  cat("  Vérifiez votre connexion ou installez : install.packages('rnaturalearthdata')\n")
}

# ─── 24C. Harmonisation des noms d'États ───


if (!is.null(nigeria_sf)) {
  
  # Identifier la colonne des noms d'États dans le shapefile
  col_nom_sf <- names(nigeria_sf)[
    names(nigeria_sf) %in% c("name", "NAME_1", "state_name", "admin1Name")
  ][1]
  if (is.na(col_nom_sf)) col_nom_sf <- names(nigeria_sf)[2]
  
  cat("  Colonne de noms dans le shapefile :", col_nom_sf, "\n")
  cat("  Noms États shapefile :", paste(sort(nigeria_sf[[col_nom_sf]]), collapse = "; "), "\n")
  cat("  Noms États GHS :", paste(sort(levels(data_q24$state_label)), collapse = "; "), "\n")
  
  # Correspondances manuelles 
        correspondance <- tibble(
          nom_ghs = levels(data_q24$state_label)
        ) %>%
          mutate(
            nom_sf = case_when(
              grepl("Abia", nom_ghs, ignore.case = TRUE) ~ "Abia",
              grepl("Adamawa", nom_ghs, ignore.case = TRUE) ~ "Adamawa",
              grepl("Akwa Ibom", nom_ghs, ignore.case = TRUE) ~ "Akwa Ibom",
              grepl("Anambra", nom_ghs, ignore.case = TRUE) ~ "Anambra",
              grepl("Bauchi", nom_ghs, ignore.case = TRUE) ~ "Bauchi",
              grepl("Bayelsa", nom_ghs, ignore.case = TRUE) ~ "Bayelsa",
              grepl("Benue", nom_ghs, ignore.case = TRUE) ~ "Benue",
              grepl("Borno", nom_ghs, ignore.case = TRUE) ~ "Borno",
              grepl("Cross River", nom_ghs, ignore.case = TRUE) ~ "Cross River",
              grepl("Delta", nom_ghs, ignore.case = TRUE) ~ "Delta",
              grepl("Ebonyi", nom_ghs, ignore.case = TRUE) ~ "Ebonyi",
              grepl("Edo", nom_ghs, ignore.case = TRUE) ~ "Edo",
              grepl("Ekiti", nom_ghs, ignore.case = TRUE) ~ "Ekiti",
              grepl("Enugu", nom_ghs, ignore.case = TRUE) ~ "Enugu",
              grepl("Gombe", nom_ghs, ignore.case = TRUE) ~ "Gombe",
              grepl("Imo", nom_ghs, ignore.case = TRUE) ~ "Imo",
              grepl("Jigawa", nom_ghs, ignore.case = TRUE) ~ "Jigawa",
              grepl("Kaduna", nom_ghs, ignore.case = TRUE) ~ "Kaduna",
              grepl("Kano", nom_ghs, ignore.case = TRUE) ~ "Kano",
              grepl("Katsina", nom_ghs, ignore.case = TRUE) ~ "Katsina",
              grepl("Kebbi", nom_ghs, ignore.case = TRUE) ~ "Kebbi",
              grepl("Kogi", nom_ghs, ignore.case = TRUE) ~ "Kogi",
              grepl("Kwara", nom_ghs, ignore.case = TRUE) ~ "Kwara",
              grepl("Lagos", nom_ghs, ignore.case = TRUE) ~ "Lagos",
              grepl("Nasarawa", nom_ghs, ignore.case = TRUE) ~ "Nassarawa",
              grepl("Niger", nom_ghs, ignore.case = TRUE) ~ "Niger",
              grepl("Ogun", nom_ghs, ignore.case = TRUE) ~ "Ogun",
              grepl("Ondo", nom_ghs, ignore.case = TRUE) ~ "Ondo",
              grepl("Osun", nom_ghs, ignore.case = TRUE) ~ "Osun",
              grepl("Oyo", nom_ghs, ignore.case = TRUE) ~ "Oyo",
              grepl("Plateau", nom_ghs, ignore.case = TRUE) ~ "Plateau",
              grepl("Rivers", nom_ghs, ignore.case = TRUE) ~ "Rivers",
              grepl("Sokoto", nom_ghs, ignore.case = TRUE) ~ "Sokoto",
              grepl("Taraba", nom_ghs, ignore.case = TRUE) ~ "Taraba",
              grepl("Yobe", nom_ghs, ignore.case = TRUE) ~ "Yobe",
              grepl("Zamfara", nom_ghs, ignore.case = TRUE) ~ "Zamfara",
              grepl("FCT", nom_ghs, ignore.case = TRUE) ~ "Federal Capital Territory",
              TRUE ~ NA_character_
            )
          )

  mediane_etat <- mediane_etat %>%
    mutate(
      state_clean = gsub("^\\d+\\.\\s*", "", as.character(state_label))
    )
  
  
  # Jointure shapefile + données GHS
  nigeria_map <- nigeria_sf %>%
    rename(nom_sf = !!col_nom_sf) %>%
    left_join(
      mediane_etat %>%
        mutate(state_label_chr = as.character(state_label)) %>%
        left_join(correspondance, by = c("state_label_chr" = "nom_ghs")) %>%
        select(nom_sf, mediane_pond, n_menages, n_pond),
      by = "nom_sf"
    ) %>%
    # Quintiles pour la légende discrète
    mutate(
      quintile = cut(
        mediane_pond,
        breaks = quantile(mediane_pond, probs = seq(0, 1, 0.2), na.rm = TRUE),
        labels = c("Q1 (plus petites)", "Q2", "Q3", "Q4", "Q5 (plus grandes)"),
        include.lowest = TRUE
      )
    )
  
  # ─── 24D. Carte choroplèthe principale ───
  p_carte <- ggplot(nigeria_map) +
    geom_sf(aes(fill = mediane_pond),
            color = "white", linewidth = 0.4) +
    scale_fill_gradientn(
      colors  = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84",
                  "#FC8D59", "#EF6548", "#D7301F", "#990000"),
      name    = "Superficie\nmédiane (ha)",
      labels  = label_number(accuracy = 0.1),
      na.value = "grey80",
      guide   = guide_colorbar(
        barwidth  = 15, barheight = 0.7,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(
      title    = "Superficie médiane des exploitations agricoles par État nigérian",
      subtitle = "Wave 4 (2018/2019) — Médiane pondérée par ménage agricole",
      caption  = paste0(caption_note,
                        "\nGris = États sans données ou correspondance non trouvée")
    ) +
    theme_void(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle   = element_text(color = "grey30", size = 11, hjust = 0.5, margin = margin(b = 8)),
      plot.caption    = element_text(color = "grey50", size = 8, hjust = 0.5),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold", size = 10),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(10, 10, 10, 10)
    )
  
  ggsave("output/fig7_carte_superficie_etat.png",
         p_carte, width = 12, height = 14, dpi = 300, bg = "white")
  cat("✓ Figure 7 — Carte choroplèthe sauvegardée\n")
  
  # ─── 24E. Carte avec étiquettes des États ( annotée) ───
  nigeria_centroids <- nigeria_map %>%
    mutate(centroid = sf::st_centroid(geometry)) %>%
    mutate(
      lon = sf::st_coordinates(centroid)[, 1],
      lat = sf::st_coordinates(centroid)[, 2]
    )
  
  p_carte_label <- p_carte +
    geom_text(
      data = nigeria_centroids %>% filter(!is.na(mediane_pond)),
      aes(x = lon, y = lat,
          label = paste0(nom_sf, "\n", round(mediane_pond, 1), " ha")),
      size = 2.2, fontface = "bold", color = "white",
      check_overlap = TRUE
    )
  
  ggsave("output/fig7b_carte_superficie_etat_labels.png",
         p_carte_label, width = 14, height = 16, dpi = 300, bg = "white")
  cat("✓ Figure 7b — Carte annotée sauvegardée\n")
  
  # ─── 24F. Dot plot  (complément à la carte) ───
  # Classement des États par superficie médiane : lecture directe des valeurs
  
  p_dotplot <- ggplot(
    mediane_etat,
    aes(x = mediane_pond,
        y = reorder(state_label, mediane_pond),
        color = mediane_pond)
  ) +
    geom_segment(aes(xend = 0, yend = reorder(state_label, mediane_pond)),
                 linewidth = 0.5, color = "grey80") +
    geom_point(size = 3.5) +
    scale_color_gradientn(
      colors = c("#FDD49E", "#FC8D59", "#D7301F", "#990000"),
      name = "Superficie (ha)",
      guide = "none"
    ) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    labs(
      title    = "Superficie médiane des exploitations par État nigérian",
      subtitle = "Dot plot de Cleveland — Médiane pondérée | Wave 4 (2018/2019)",
      x        = "Superficie médiane (ha)",
      y        = NULL,
      caption  = caption_note
    ) +
    theme(axis.text.y = element_text(size = 8))
  
  ggsave("output/fig8_dotplot_superficie_etat.png",
         p_dotplot, width = 10, height = 14, dpi = 300, bg = "white")
  cat("✓ Figure 8 — Dot plot Cleveland sauvegardé\n")
  
} else {
  # Fallback : heatmap État × zone si le shapefile n'est pas disponible
  cat("⚠ Fallback : génération de la heatmap État × zone géopolitique\n")
  
  data_heatmap <- data_q24 %>%
    mutate(zone_label = as_factor(zone)) %>%
    group_by(state_label, zone_label) %>%
    summarise(
      mediane_pond = {
        x <- superficie_par_menage; w <- wt_wave4
        ord <- order(x); cw <- cumsum(w[ord])
        x[ord][which(cw >= sum(w) / 2)[1]]
      },
      n = n(), .groups = "drop"
    )
  
  p_heatmap <- ggplot(data_heatmap,
                      aes(x = zone_label,
                          y = reorder(state_label, mediane_pond),
                          fill = mediane_pond)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(mediane_pond, 2)), size = 2.8, fontface = "bold",
              color = ifelse(data_heatmap$mediane_pond > median(data_heatmap$mediane_pond, na.rm = TRUE),
                             "white", "black")) +
    scale_fill_gradientn(
      colors = c("#EFF3FF","#BDD7E7","#6BAED6","#2171B5","#08306B"),
      name = "Superficie\nmédiane (ha)",
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title   = "Superficie médiane des exploitations par État et zone géopolitique",
      x       = "Zone géopolitique", y = "État", caption = caption_note
    ) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1, size = 9),
      axis.text.y = element_text(size = 8)
    )
  
  ggsave("output/fig7_heatmap_superficie_etat.png",
         p_heatmap, width = 13, height = 14, dpi = 300, bg = "white")
  cat("✓ Figure 7 — Heatmap (fallback) sauvegardée\n")
}

# Tableau récapitulatif par État
tbl_heatmap <- mediane_etat %>%
  rename(
    "État"                    = state_label,
    "Superficie médiane (ha)" = mediane_pond,
    "Nb ménages (éch.)"       = n_menages,
    "Nb ménages (pondéré)"    = n_pond
  ) %>%
  arrange(desc(`Superficie médiane (ha)`)) %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Superficie médiane des exploitations agricoles par État",
    subtitle = "GHS-Panel Wave 4, Nigeria 2018/2019 — Estimations pondérées"
  ) %>%
  gt::fmt_number(columns = "Superficie médiane (ha)", decimals = 3) %>%
  gt::fmt_number(columns = "Nb ménages (pondéré)", decimals = 0) %>%
  gt::data_color(
    columns = "Superficie médiane (ha)",
    palette = c("#FDD49E", "#990000")
  ) %>%
  gt::tab_source_note("Source : NBS/Banque Mondiale — GHS-Panel Wave 4 (2018/2019)") %>%
  gt::opt_stylize(style = 3)

gt::gtsave(tbl_heatmap, "output/tableau4_superficie_etat.html")
cat(" Tableau 4 sauvegardé\n")

