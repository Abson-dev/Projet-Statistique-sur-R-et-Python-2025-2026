# ============================================================
# analyses_tp5.R — Q25 et Q26
# Nigeria GHS Panel W4 | TP5 | ENSAE ISE1 2025-2026
# ============================================================
source("R/00_setup.R")

# ============================================================
# BLOC 1 — IMPORT ET NETTOYAGE
# ============================================================



# ── secta3i : field crops ─────────────────────────────────────
s3i <- read_dta(paste0(path_raw, "secta3i_harvestw4.dta"))
cat("secta3i :", nrow(s3i), "lignes\n")
s3i %>% count(cropcode, sort = TRUE) %>% print(n = 10)

# ── secta3ii : tree/permanent crops ──────────────────────────
s3ii <- read_dta(paste0(path_raw, "secta3ii_harvestw4.dta"))
cat("secta3ii :", nrow(s3ii), "lignes\n")
s3ii %>% count(cropcode, sort = TRUE) %>% print(n = 10)

# ── Variables communes entre les deux ─────────────────────────
intersect(names(s3i), names(s3ii))


# Convertir les labels avant bind_rows pour éviter le conflit
cultures_harvest <- bind_rows(
  s3i  %>% 
    dplyr::select(hhid, ea, sector, zone, state, cropcode) %>%
    mutate(cropcode = as.numeric(cropcode)),   # ← supprimer les labels Stata
  s3ii %>% 
    dplyr::select(hhid, ea, sector, zone, state, cropcode) %>%
    mutate(cropcode = as.numeric(cropcode))    # ← supprimer les labels Stata
) %>%
  dplyr::filter(!is.na(cropcode), cropcode != 9999) %>%
  mutate(
    crop_label = case_when(         # ← on recrée les labels manuellement
      cropcode == 1010 ~ "BEANS/COWPEA",
      cropcode == 1020 ~ "CASSAVA",
      cropcode == 1040 ~ "COCOYAM",
      cropcode == 1060 ~ "GROUND NUT/PEANUTS",
      cropcode == 1070 ~ "GUINEA CORN (SORGHUM)",
      cropcode == 1080 ~ "MAIZE",
      cropcode == 1090 ~ "MELON/EGUSI",
      cropcode == 1100 ~ "MILLET/MAIWA",
      cropcode == 1110 ~ "RICE",
      cropcode == 1121 ~ "YAM, WHITE",
      cropcode == 1122 ~ "YAM, YELLOW",
      cropcode == 1123 ~ "WATER YAM",
      cropcode == 1124 ~ "YAM, THREE LEAVED",
      cropcode == 2040 ~ "BEENI-SEED/SESAME",
      cropcode == 2120 ~ "OKRO",
      cropcode == 2170 ~ "PLANTAIN",
      cropcode == 2190 ~ "PUMPKIN",
      cropcode == 2194 ~ "GREEN VEGETABLE",
      cropcode == 2220 ~ "SOYA BEANS",
      cropcode == 2260 ~ "TOMATO",
      cropcode == 2142 ~ "PEPPER, SMALL (RODO)",
      cropcode == 3040 ~ "COCOA",
      cropcode == 3110 ~ "KOLANUT",
      cropcode == 3180 ~ "OIL PALM TREE",
      cropcode == 3020 ~ "CASHEW",
      cropcode == 2030 ~ "BANANA",
      cropcode == 2060 ~ "CUCUMBER",
      TRUE             ~ paste0("OTHER_", cropcode)
    ),
    type_culture = case_when(
      crop_label %in% c("MAIZE", "MILLET/MAIWA",
                        "GUINEA CORN (SORGHUM)", "RICE")      ~ "Céréale",
      crop_label %in% c("CASSAVA", "YAM, WHITE", "WATER YAM",
                        "COCOYAM", "YAM, THREE LEAVED",
                        "SWEET POTATO")                        ~ "Tubercule",
      crop_label %in% c("BEANS/COWPEA", "GROUND NUT/PEANUTS",
                        "SOYA BEANS")                          ~ "Légumineuse",
      crop_label %in% c("OIL PALM TREE", "COCOA",
                        "PLANTAIN", "KOLANUT")                 ~ "Culture de rente",
      TRUE                                                     ~ "Autre"
    )
  )




# ── Vérifications ─────────────────────────────────────────────
cat("Total lignes empilées    :", nrow(cultures_harvest), "\n")
cat("Ménages distincts        :", n_distinct(cultures_harvest$hhid), "\n")
cat("Cultures distinctes      :", n_distinct(cultures_harvest$cropcode), "\n")

# Aperçu des cultures
cultures_harvest %>%
  count(crop_label, sort = TRUE) %>%
  print(n = 15)

# ── Joindre les poids ─────────────────────────────────────────
cultures_harvest <- cultures_harvest %>%
  left_join(poids, by = "hhid") %>%
  dplyr::filter(!is.na(wt_wave4))

cat("Après jointure poids :", nrow(cultures_harvest), "\n")

# ── Niveau ménage ─────────────────────────────────────────────
cultures_hh_h <- cultures_harvest %>%
  group_by(hhid, sector, wt_wave4, ea) %>%
  summarise(nb_cultures = n_distinct(cropcode), .groups = "drop")

# ── Indicatrice par ménage × culture ──────────────────────────
toutes_cultures_h <- cultures_harvest %>%
  distinct(cropcode, crop_label, type_culture)

cultures_indic_h <- cultures_hh_h %>%
  dplyr::select(hhid, sector, wt_wave4, ea) %>%
  cross_join(toutes_cultures_h) %>%
  left_join(
    cultures_harvest %>%
      distinct(hhid, cropcode) %>%
      mutate(cultive = 1),
    by = c("hhid", "cropcode")
  ) %>%
  mutate(cultive = replace_na(cultive, 0))

# ── Design ────────────────────────────────────────────────────
design_indic_h <- cultures_indic_h %>%
  as_survey_design(
    ids     = ea,
    strata  = sector,
    weights = wt_wave4,
    nest    = TRUE
  )

# ── Top 15 ────────────────────────────────────────────────────
freq_h <- design_indic_h %>%
  group_by(crop_label, type_culture) %>%
  summarise(
    pct = survey_mean(cultive, vartype = NULL) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct)) %>%
  slice_head(n = 15) %>%
  mutate(crop_label = fct_reorder(crop_label, pct))

print(freq_h)

# Corriger les catégories
freq_h <- freq_h %>%
  mutate(
    type_culture = case_when(
      crop_label %in% c("OKRO", "PUMPKIN", "TOMATO")  ~ "Légume",
      crop_label == "BEENI-SEED/SESAME"                ~ "Légumineuse",
      TRUE                                              ~ type_culture
    ),
    crop_label = fct_reorder(crop_label, pct)
  )

# ── Graphique final ───────────────────────────────────────────
couleurs_type <- c(
  "Céréale"          = "#E76F51",
  "Tubercule"        = "#2A9D8F",
  "Légumineuse"      = "#E9C46A",
  "Culture de rente" = "#264653",
  "Légume"           = "#8338EC",
  "Autre"            = "#A8DADC"
)

p_q25 <- ggplot(freq_h,
                aes(x = crop_label, y = pct, fill = type_culture)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    hjust = -0.1,
    size  = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(values = couleurs_type) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Top 15 des cultures pratiquées au Nigeria (Vague 4, 2018-2019)",
    subtitle = "% de ménages ayant récolté chaque culture — estimations pondérées",
    x        = NULL,
    y        = "% de ménages",
    fill     = "Type de culture",
    caption  = "Source : Nigeria GHS Panel W4 | secta3i + secta3ii harvestw4"
  )

print(p_q25)
ggsave(paste0(path_fig, "Q25_top15_cultures.png"),
       plot = p_q25, width = 10, height = 7, dpi = 300)
cat("✅ Q25 terminée\n")
# ============================================================
# Q26 — Diversification culturale
# Distribution + comparaison rural/urbain
# ============================================================
#install.packages('coin')
library(coin)

design_hh <- cultures_hh %>%
  as_survey_design(
    ids     = ea,
    strata  = sector,
    weights = wt_wave4,
    nest    = TRUE
  )

# ── Stats descriptives pondérées ──────────────────────────────
stats_glob <- design_hh %>%
  summarise(
    moy    = survey_mean(nb_cultures,   vartype = "ci"),
    median = survey_median(nb_cultures, vartype = "ci"),
    q1     = survey_quantile(nb_cultures, 0.25, vartype = NULL),
    q3     = survey_quantile(nb_cultures, 0.75, vartype = NULL)
  )
cat("\n── Stats globales ──\n")
print(stats_glob)

stats_zone <- design_hh %>%
  group_by(sector_label) %>%
  summarise(
    moy    = survey_mean(nb_cultures,   vartype = "ci"),
    median = survey_median(nb_cultures, vartype = "ci")
  )
cat("\n── Stats par zone ──\n")
print(stats_zone)


# ── Test de Wilcoxon pondéré ──────────────────────────────────
wilcox_pond <- svyranktest(
  nb_cultures ~ sector_label,
  design = design_hh
)
cat("\n── Test de Wilcoxon pondéré ──\n")
print(wilcox_pond)

# Taille d'effet (données brutes)
r_effet <- cultures_hh %>%
  wilcox_effsize(nb_cultures ~ sector_label)
cat("\n── Taille d'effet ──\n")
print(r_effet)

# ── Histogramme ───────────────────────────────────────────────
p_histo <- ggplot(cultures_hh, aes(x = nb_cultures)) +
  geom_histogram(
    binwidth = 1,
    fill     = "#2A9D8F",
    color    = "white"
  ) +
  scale_x_continuous(breaks = 1:max(cultures_hh$nb_cultures)) +
  labs(
    title   = "Distribution du nombre de cultures par ménage (W4)",
    x       = "Nombre de cultures différentes",
    y       = "Nombre de ménages",
    caption = "Source : Nigeria GHS Panel W4 | sect11f_plantingw4"
  )

# ── Violin plot rural vs urbain ───────────────────────────────
p_violin <- ggplot(
  cultures_hh,
  aes(x = sector_label, y = nb_cultures, fill = sector_label)
) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  annotate(
    "text",
    x     = 1.5,
    y     = max(cultures_hh$nb_cultures) * 0.95,
    label = paste0(
      "Wilcoxon p = ", format(wilcox_pond$p.value, digits = 3),
      "\nr = ",        round(r_effet$effsize, 3)
    ),
    size = 4
  ) +
  scale_fill_manual(values = c("Urban" = "#E76F51", "Rural" = "#2A9D8F")) +
  labs(
    title   = "Diversification culturale : Rural vs Urbain (W4)",
    x       = NULL,
    y       = "Nombre de cultures par ménage",
    caption = "Source : Nigeria GHS Panel W4 | sect11f_plantingw4"
  ) +
  theme(legend.position = "none")

# ── Assembler ─────────────────────────────────────────────────
p_q26 <- p_histo / p_violin +
  plot_annotation(
    title = "Q26 — Diversification culturale des ménages nigérians (W4)"
  )

print(p_q26)
ggsave(
  paste0(path_fig, "Q26_diversification.png"),
  plot   = p_q26,
  width  = 10,
  height = 12,
  dpi    = 300
)
cat("✅ Q26 terminée\n")

# Vérification du maximum et de la distribution
summary(cultures_hh$nb_cultures)

# Voir les ménages qui ont énormément de cultures (le "top 10")
cultures_hh %>% 
  arrange(desc(nb_cultures)) %>% 
  head(10)

# --- 1.1 Chargement des données ---
check_engrais <- read_dta(paste0(path_raw, "secta11c2_harvestw4.dta"))


eng_lab <- tibble(
  variable = names(check_engrais),
  label    = sapply(check_engrais, function(x) attr(x, "label"))
)
print (eng_lab, n= Inf)


# --- 1.2 Nettoyage et Agrégation au niveau Ménage ---
# On passe de l'unité "parcelle" à l'unité "ménage"
adoption_hh <- check_engrais %>%
  select(hhid, sector, ea, state, s11c2q36_1, s11c2q36_2, s11dq36) %>%
  mutate(
    # Labels propres
    zone_label = as_factor(sector) %>% stringr::str_remove("^\\d+\\.\\s*"),
    state_label = as_factor(state) %>% stringr::str_remove("^\\d+\\.\\s*"),
    
    # Recodage binaire (1/0) : les NAs deviennent 0 (justifié par skip pattern)
    NPK     = if_else(s11c2q36_1 == 1, 1, 0, missing = 0),
    Urea    = if_else(s11c2q36_2 == 1, 1, 0, missing = 0),
    Organic = if_else(s11dq36 == 1, 1, 0, missing = 0),
    
    # Variable globale pour le test du Chi-deux
    utilise_engrais = if_else(NPK == 1 | Urea == 1 | Organic == 1, 1, 0)
  ) %>%
  group_by(hhid, zone_label, state_label, sector, ea, state) %>%
  summarise(across(c(NPK, Urea, Organic, utilise_engrais), max), .groups = "drop") %>%
  left_join(poids, by = "hhid") %>%
  filter(!is.na(wt_wave4))

# --- 1.3 Formatage "Long" pour les graphiques par type ---
final_long <- adoption_hh %>%
  pivot_longer(
    cols     = c(NPK, Urea, Organic), 
    names_to = "type_engrais", 
    values_to = "utilise"
  )

# Réglage pour les unités isolées
options(survey.lonely.psu = "adjust")

# Design national (format Wide pour les tests et gtsummary)
design_wide <- adoption_hh %>%
  as_survey_design(ids = ea, strata = sector, weights = wt_wave4, nest = TRUE)

# Design format Long (pour les stats descriptives par type d'engrais)
design_long <- final_long %>%
  as_survey_design(ids = ea, strata = sector, weights = wt_wave4, nest = TRUE)

# Test global : Est-ce que le secteur (Rural/Urbain) influence l'usage d'engrais ?
test_chi2 <- svychisq(~utilise_engrais + zone_label, design = design_wide)
print(test_chi2)

# Tableau des 37 États avec taux d'utilisation



library(gtsummary)
# --- LE CODE FINAL DÉFINITIF ---
tableau_final_q27 <- design_long %>%
  tbl_svysummary(
    by = zone_label,
    include = c(state_label, type_engrais, utilise),
    # On force 'utilise' en continu pour avoir la moyenne (le taux)
    type = list(utilise ~ "continuous"),
    label = list(
      state_label ~ "État",
      type_engrais ~ "Type d'engrais",
      utilise ~ "Taux d'utilisation"
    ),
    statistic = list(utilise ~ "{mean}")
  ) %>%
  # Correction pour gtsummary 2.0 : on formate toutes les stats continues d'un coup
  modify_fmt_fun(all_continuous() ~ label_style_percent(accuracy = 0.1)) %>%
  add_p(utilise ~ "t.test") %>%
  bold_labels() %>%
  # On change le label de l'en-tête (la colonne de gauche)
  modify_header(label = "**Indicateurs**")

# Affichage
tableau_final_q27

# 1. Identifier les 5 États les plus importants (en poids agricole)
top5_list <- adoption_hh %>%
  group_by(state_label) %>%
  summarise(poids_total = sum(wt_wave4)) %>%
  top_n(5, poids_total) %>%
  pull(state_label)

# 2. Préparer les données du graphique (moyennes pondérées)
plot_stats <- design_long %>%
  filter(state_label %in% top5_list) %>%
  group_by(state_label, zone_label, type_engrais) %>%
  summarise(taux = survey_mean(utilise, vartype = "ci"), .groups = "drop")

# 3. Le Barplot final
p_q27 <- ggplot(plot_stats, aes(x = type_engrais, y = taux, fill = zone_label)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = taux_low, ymax = taux_upp), 
                position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~state_label, ncol = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Utilisation des engrais dans les 5 principaux États",
       fill = "Secteur", y = "Taux d'adoption", x = "")

print(p_q27)
ggsave(
  paste0(path_fig, "Q27_engrais.png"),
  plot   = p_q27,
  width  = 10,
  height = 12,
  dpi    = 300
)


# --- 1. TABLEAU GLOBAL ---
tab_global <- design_wide %>%
  tbl_svysummary(
    by        = zone_label,
    include   = utilise_engrais,
    type      = list(utilise_engrais ~ "continuous"),
    statistic = list(utilise_engrais ~ "{mean}"),
    label     = list(utilise_engrais ~ "Utilise au moins un engrais (Global)")
  ) %>%
  modify_fmt_fun(all_continuous() ~ label_style_percent(accuracy = 0.1)) %>%
  add_p(test = list(utilise_engrais ~ "svy.t.test"))

# --- 2. TABLEAU PAR TYPE (design_wide, N correct, une ligne par type) ---
tab_type <- design_wide %>%
  tbl_svysummary(
    by        = zone_label,
    include   = c(NPK, Urea, Organic),          # ✅ Variables binaires directes
    type      = list(c(NPK, Urea, Organic) ~ "continuous"),
    statistic = list(everything() ~ "{mean}"),
    label     = list(
      NPK     ~ "NPK",
      Urea    ~ "Urée",
      Organic ~ "Organique"
    )
  ) %>%
  modify_fmt_fun(all_continuous() ~ label_style_percent(accuracy = 0.1)) %>%
  add_p(test = list(everything() ~ "svy.t.test"))

# --- 3. FUSION ---
# 1. Création de votre beau tableau
tableau_final_parfait <- tbl_stack(
  list(tab_global, tab_type),
  group_header = c("Usage global", "Détail par type d'engrais")
) %>%
  modify_header(label = "**Indicateurs d'utilisation**") %>%
  bold_labels()

# 2. TRANSFORMATION CRUCIALE (pour éviter l'erreur systemfonts dans le Rmd)
# On le convertit en simple tableau de données (tibble)
tableau_final_clean <- tableau_final_parfait %>%
  as_tibble()

# 3. Sauvegarder la version "NETTOYÉE"
if(!dir.exists("outputs")) dir.create("outputs")
saveRDS(tableau_final_clean, file = "outputs/tableau_q27_clean.rds")

# Affichage pour vérification dans la console
print(tableau_final_clean)


# Tableau détaillé par État - Version Robuste
tableau_etats_q27 <- design_long %>%
  tbl_svysummary(
    by = zone_label,
    include = c(state_label, utilise),
    type = list(utilise ~ "continuous"),
    label = list(
      state_label ~ "État du Nigeria",
      utilise ~ "Taux d'utilisation moyen"
    ),
    statistic = list(utilise ~ "{mean}")
  ) %>%
  modify_fmt_fun(all_continuous() ~ label_style_percent(accuracy = 0.1)) %>%
  # On utilise {n} qui est le standard gtsummary pour le nombre de ménages
  modify_header(
    label = "**Localisation géographique**",
    stat_1 = "**Rural**\n(Ménages pondérés)",
    stat_2 = "**Urbain**\n(Ménages pondérés)"
  ) %>%
  bold_labels()

tableau_etats_q27
#install.packages ("flextable")





# 1. Identifier les 5 États ayant le plus grand poids agricole (Représentativité nationale)
# On calcule la somme des poids par État pour identifier les pôles agricoles majeurs
top5_states <- adoption_hh %>%
  group_by(state_label) %>%
  summarise(poids_total = sum(wt_wave4, na.rm = TRUE)) %>%
  slice_max(poids_total, n = 5) %>%
  pull(state_label)

# 2. Préparation des estimations pondérées (Inférence statistique)
# Utilisation de survey_mean sur l'objet design pour obtenir les taux nationaux par État
plot_data_states <- design_long %>%
  filter(state_label %in% top5_states) %>%
  group_by(state_label, type_engrais) %>%
  summarise(
    # Calcul de la moyenne pondérée et de l'intervalle de confiance à 95%
    taux = survey_mean(utilise, vartype = "ci", na.rm = TRUE),
    .groups = "drop"
  )

# 3. Création du graphique comparatif multi-états
p_states_final <- ggplot(plot_data_states, aes(x = type_engrais, y = taux, fill = type_engrais)) +
  # Barres de moyennes pondérées
  geom_col(alpha = 0.85, show.legend = FALSE, width = 0.7) +
  # Intervalles de confiance (indispensables avec les pondérations)
  geom_errorbar(aes(ymin = taux_low, ymax = taux_upp), 
                width = 0.2, color = "grey30", size = 0.6) +
  # Facettage : un panneau par État
  facet_wrap(~state_label, ncol = 3) +
  # Formatage des axes et échelles
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("NPK" = "#2c3e50", "Urea" = "#3498db", "Organic" = "#27ae60")) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  labs(
    title = "Profil de fertilisation dans les 5 principaux États agricoles",
    subtitle = "Estimations nationales pondérées (%) | IC à 95%",
    x = "Type d'intrant",
    y = "Taux d'adoption (Estimation population)",
    caption = "Source : Calculs de l'auteur d'après GHS-LSMS Wave 4 (Pondérations appliquées)"
  ) +
  theme(
    strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 14)
  )

# Affichage du graphique final
print(p_states_final)

# --- SAUVEGARDE POUR INTÉGRATION WORD ---
ggsave(
  paste0(path_fig, "Q27_etats_comparatif_pondere.png"),
  plot   = p_states_final,
  width  = 9,
  height = 6,
  dpi    = 300
)



















# Vérifier le label
attr(check_engrais$s11dq36, "label")
attr(check_engrais$s11c2q36_1, "label")
attr(check_engrais$s11c2q36_2, "label")


#étudions ces bases
source("R/00_setup.R")

# ── Charger les fichiers ──────────────────────────────────────
prod_raw <- read_dta(paste0(path_raw, "secta3i_harvestw4.dta"))
area_raw <- read_dta(paste0(path_raw, "sect11a1_plantingw4.dta"))
poids<- read_dta(paste0(path_raw, "secta_plantingw4.dta")) %>%
  dplyr::select(hhid, ea, sector,state, wt_wave4) %>%
  dplyr::filter(!is.na(wt_wave4))




prod_lab <- tibble(
  variable = names(prod_raw),
  label    = sapply(prod_raw, function(x) attr(x, "label"))
)
print (prod_lab, n= Inf)


area_lab <- tibble(
  var = names(area_raw),
  label = sapply(area_raw, function(x) attr(x, "label")))
print(area_lab, n= Inf)


crop_labels <- prod_raw %>%
  select(cropcode) %>%
  mutate(label = as_factor(cropcode)) %>%
  distinct() %>%
  arrange(cropcode)

print(crop_labels, n = Inf)



print("--- UNITÉS DE SURFACE (s11aq4b) ---")
area_raw %>%
  select(s11aq4b) %>%
  mutate(label_unite = as_factor(s11aq4b)) %>%
  distinct() %>%
  print()

# 2. Vérification rapide des valeurs GPS
print("--- APERÇU DES VALEURS GPS (s11aq4c) ---")
summary(area_raw$s11aq4c)

# 3. Vérification du facteur de conversion de production
print("--- APERÇU DU FACTEUR DE CONVERSION PROD (sa3iq6_conv) ---")
summary(prod_raw$sa3iq6_conv)


# 1. On identifie les parcelles de Maïs/Millet
parcelles_interet <- prod_raw %>%
  filter(cropcode %in% c(1080, 1100)) %>%
  select(hhid, plotid, cropcode)

# 2. On regarde l'état du GPS uniquement pour ces parcelles
diag_gps_restreint <- parcelles_interet %>%
  left_join(area_raw, by = c("hhid", "plotid")) %>%
  group_by(cropcode) %>%
  summarise(
    Total_Parcelles = n(),
    Nb_GPS_OK = sum(!is.na(s11aq4c)),
    Nb_GPS_Manquant = sum(is.na(s11aq4c)),
    Pct_Couverture_GPS = (Nb_GPS_OK / Total_Parcelles) * 100
  ) %>%
  mutate(Nom_Culture = as_factor(cropcode))

print(diag_gps_restreint)


# Comparaison GPS vs Unités Locales (Heaps, Ridges)
verif_unites_locales <- area_raw %>%
  filter(!is.na(s11aq4c) & s11aq4b %in% c(1, 2, 3)) %>% # 1:Heaps, 2:Ridges, 3:Stands
  mutate(
    surf_gps = s11aq4c / 10000,
    # On calcule le coefficient implicite : combien de m2 par unité déclarée ?
    coeff_reel = s11aq4c / s11aq4aa
  )

# Statistique descriptive des coefficients réels par unité
verif_unites_locales %>%
  group_by(as_factor(s11aq4b)) %>%
  summarise(
    n = n(),
    coeff_median_m2 = median(coeff_reel, na.rm = TRUE),
    ecart_type = sd(coeff_reel, na.rm = TRUE)
  )


# --- calcul final du rendement Q28 ---
data_q28_final <- prod_raw %>%
  # 1. Filtre cultures
  filter(cropcode %in% c(1080, 1100)) %>%
  # 2. Calcul production en kg
  mutate(prod_kg = sa3iq6i * sa3iq6_conv) %>%
  # 3. Jointure avec surface GPS uniquement
  inner_join(
    area_raw %>% 
      filter(!is.na(s11aq4c)) %>% 
      mutate(surf_ha = s11aq4c / 10000) %>%
      select(hhid, plotid, surf_ha),
    by = c("hhid", "plotid")
  ) %>%
  # 4. Calcul du rendement
  mutate(rendement = prod_kg / surf_ha) %>%
  # 5. Nettoyage des outliers (IQR x 3)
  filter(rendement <= (quantile(rendement, 0.75, na.rm=T) + 3 * IQR(rendement, na.rm=T))) %>%
  # 6. Ajout des noms d'États (via le fichier poids ou labels)
  left_join(poids %>% select(hhid, state), by = "hhid") %>%
  mutate(state_label = as_factor(state))

#install.packages("questionr")
library(questionr) # Pour les statistiques pondérées





# ==============================================================================
# ANALYSE DES RENDEMENTS CÉRÉALIERS - CULTURE PRINCIPALE (MAÏS OU MILLET)
# ==============================================================================

# Ce script respecte les consignes suivantes :
# 1. Sélection de la culture DOMINANTE par ménage (Maïs ou Millet)
# 2. Calcul du rendement (kg/ha) basé sur cette culture principale
# 3. Utilisation exclusive de la surface mesurée par GPS
# 4. Nettoyage des valeurs aberrantes par la méthode IQR x 3 pondérée
# 5. Comparaison par État via Boxplots classés par médiane


library(Hmisc)    # Pour les statistiques pondérées


# --- 1. CHARGEMENT DES DONNÉES ---
poids <- read_dta(paste0(path_raw, "secta_plantingw4.dta")) %>%
  select(hhid, wt_wave4) %>%
  filter(!is.na(wt_wave4))
colnames(poids)
area_clean <- read_dta(paste0(path_raw, "sect11a1_plantingw4.dta")) %>%
  filter(!is.na(s11aq4c) & s11aq4c > 0) %>%
  mutate(surf_ha = s11aq4c / 10000) %>%
  select(hhid, plotid, surf_ha)

prod_raw <- read_dta(paste0(path_raw, "secta3i_harvestw4.dta")) %>%
  filter(cropcode %in% c(1080, 1100)) %>%
  mutate(prod_kg = sa3iq6i * sa3iq6_conv) %>%
  filter(!is.na(prod_kg) & prod_kg > 0)

# --- 2. IDENTIFICATION DE LA CULTURE PRINCIPALE PAR MÉNAGE ---
# On choisit la culture qui a la plus grande production totale pour le ménage
prod_principale <- prod_raw %>%
  group_by(hhid, cropcode) %>%
  summarise(prod_totale_hh = sum(prod_kg, na.rm = TRUE), .groups = "drop") %>%
  group_by(hhid) %>%
  # On ne garde que la ligne avec la production max (la culture dominante)
  slice_max(prod_totale_hh, n = 1, with_ties = FALSE) %>%
  select(hhid, crop_dominante = cropcode)

# --- 3. FUSION ET CALCUL DU RENDEMENT ---
# On filtre la base de production pour ne garder que la culture dominante
data_yield <- prod_raw %>%
  inner_join(prod_principale, by = c("hhid", "cropcode" = "crop_dominante")) %>%
  inner_join(area_clean, by = c("hhid", "plotid")) %>%
  inner_join(poids, by = "hhid") %>%
  mutate(rendement = prod_kg / surf_ha)


intersect(names(poids), names(prod_raw))
# --- 4. NETTOYAGE DES OUTLIERS (IQR x 3 PONDÉRÉ) ---
q_weights <- data_yield$wt_wave4
Q1_w <- wtd.quantile(data_yield$rendement, weights = q_weights, probs = 0.25, na.rm = TRUE)
Q3_w <- wtd.quantile(data_yield$rendement, weights = q_weights, probs = 0.75, na.rm = TRUE)
IQR_w <- Q3_w - Q1_w
seuil_max <- Q3_w + 3 * IQR_w

data_q28_final <- data_yield %>%
  filter(rendement <= seuil_max) %>%
  mutate(state_label = as_factor(state))

# --- 5. STATISTIQUES ET VISUALISATION ---
stats_globales <- data_q28_final %>%
  summarise(
    Nb_Menages_Analyses = n_distinct(hhid),
    Rendement_Median    = Hmisc::wtd.quantile(rendement, weights = wt_wave4, probs = 0.5),
    Rendement_Moyen     = Hmisc::wtd.mean(rendement, weights = wt_wave4)
  )

print("--- STATISTIQUES PONDÉRÉES (CULTURE PRINCIPALE UNIQUEMENT) ---")
print(stats_globales)

# Sélection Top 15 États
top_etats <- data_q28_final %>%
  count(state_label) %>%
  slice_max(n, n = 15) %>%
  pull(state_label)

data_graph <- data_q28_final %>%
  filter(state_label %in% top_etats)

y_limit_95 <- wtd.quantile(data_q28_final$rendement, weights = data_q28_final$wt_wave4, probs = 0.95)

ggplot(data_graph, aes(x = reorder(state_label, rendement, median), y = rendement)) +
  geom_boxplot(fill = "#5D9CEC", color = "#2F3C4F", alpha = 0.7, outlier.shape = NA) +
  coord_flip(ylim = c(0, y_limit_95)) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Rendement de la culture principale par État (Maïs ou Millet)",
    subtitle = "Sélection de la culture dominante par ménage | Pondérations appliquées",
    x = "État",
    y = "Rendement (kg/ha)",
    caption = "Source : GHS-Panel Wave 4 | Analyse restreinte à la culture dominante du ménage"
  )





# ==============================================================================
# GÉNÉRATION DES STATISTIQUES POUR LA RÉDACTION DU RAPPORT (Q28)
# ==============================================================================

library(tidyverse)
library(Hmisc)


# On calcule les stats par culture principale (Maïs vs Millet)
stats_detaillees <- data_q28_final %>%
  mutate(crop_label = as_factor(cropcode)) %>%
  group_by(crop_label) %>%
  summarise(
    N_obs = n(),
    # Tendance centrale
    Mediane = Hmisc::wtd.quantile(rendement, weights = wt_wave4, probs = 0.5),
    Moyenne = Hmisc::wtd.mean(rendement, weights = wt_wave4),
    
    # Dispersion
    Ecart_Type = sqrt(Hmisc::wtd.var(rendement, weights = wt_wave4)),
    CV = (Ecart_Type / Moyenne) * 100,
    
    # Inégalités (Productivité)
    P10 = Hmisc::wtd.quantile(rendement, weights = wt_wave4, probs = 0.1),
    P90 = Hmisc::wtd.quantile(rendement, weights = wt_wave4, probs = 0.9),
    Ratio_P90_P10 = P90 / P10,
    .groups = "drop"
  )

# --- STATS PAR ZONE (RURAL vs URBAIN) ---
stats_zone <- data_q28_final %>%
  group_by(zone) %>%
  summarise(
    Mediane = Hmisc::wtd.quantile(rendement, weights = wt_wave4, probs = 0.5),
    Moyenne = Hmisc::wtd.mean(rendement, weights = wt_wave4),
    .groups = "drop"
  )

# Affichage des résultats pour copie dans le rapport
print("--- TABLEAU 1 : PERFORMANCE PAR CULTURE (PONDÉRÉ) ---")
print(stats_detaillees)

print("--- TABLEAU 2 : PERFORMANCE PAR ZONE (PONDÉRÉ) ---")
print(stats_zone)

# --- PHRASE TYPE POUR LE RAPPORT ---
# "Le rendement médian du Maïs (X kg/ha) est supérieur de Y% à celui du Millet (Z kg/ha). 
# Cependant, l'agriculture céréalière est marquée par de fortes inégalités, 
# les 10% les plus productifs (P90) produisant [Ratio] fois plus par hectare 
# que les 10% les moins performants (P10)."







# ==============================================================================
# QUESTION 29 : ANALYSE DE L'IMPACT DE L'ENGRAIS CHIMIQUE
# ==============================================================================
# ==============================================================================
# Q29 : ANALYSE STATISTIQUE ET ÉCONOMÉTRIQUE DE L'IMPACT DES INTRANTS
# ==============================================================================

library(tidyverse)
library(survey)
library(srvyr)
library(broom)
poids <- read_dta(paste0(path_raw, "secta_plantingw4.dta")) %>%
  select(hhid, wt_wave4) %>%
  filter(!is.na(wt_wave4))
#colnames (data_q28_final)
data_reg <- data_q28_final %>%
  # Conversion systématique en character pour éviter les erreurs de type
  mutate(
    hhid = as.character(hhid),
    plotid = as.character(plotid)
  ) %>%
  left_join(
    poids %>% 
      mutate(hhid = as.character(hhid)) %>% 
      select(hhid), 
    by = "hhid"
  )
data_reg <- data_reg%>%
  # On récupère l'info engrais de check_engrais (s11dq1a)
  left_join(
    check_engrais %>% 
      mutate(
        hhid = as.character(hhid), 
        plotid = as.character(plotid)
      ) %>%
      select(hhid, plotid, s11dq1a),
    by = c("hhid", "plotid")
  ) %>%
  mutate(
    # On s'assure d'avoir exactement deux niveaux et on gère les NA
    utilise_engrais = case_when(
      s11dq1a == 1 ~ "Yes",
      s11dq1a == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    zone_label = as_factor(zone)
  ) %>%
  # ÉTAPE CRUCIALE : on retire les NA pour que le test ait exactement 2 niveaux
  filter(!is.na(utilise_engrais), !is.na(surf_ha), !is.na(rendement)) %>%
  # On force le type facteur pour éviter les erreurs de niveaux
  mutate(utilise_engrais = as.factor(utilise_engrais))

# --- 2. TEST DE WILCOXON (CORRIGÉ) ---
# On vérifie si la variable a bien 2 niveaux avant de lancer le test
if(length(unique(data_reg$utilise_engrais)) == 2) {
  test_wilcox <- wilcox.test(rendement ~ utilise_engrais, data = data_reg)
  print(test_wilcox)
} else {
  print("Erreur : La variable utilise_engrais n'a pas exactement 2 niveaux (Yes/No).")
  print(table(data_reg$utilise_engrais))
}

# --- 3. PLAN DE SONDAGE (SURVEY DESIGN) ---
# Pour la régression pondérée
survey_design_final <- svydesign(
  ids = ~1, 
  strata = ~state, 
  weights = ~wt_wave4, 
  data = data_reg
)


# --- 4. MODÈLE ÉCONOMÉTRIQUE (svyglm) ---
# On teste l'engrais tout en contrôlant par la taille de la parcelle et la zone
modele_complet <- svyglm(
  rendement ~ utilise_engrais + surf_ha + zone_label, 
  design = survey_design_final
)

# --- 5. EXPORTATION DES RÉSULTATS ---
stats_export <- broom::tidy(modele_complet, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "utilise_engraisYes" ~ "Impact Engrais (Oui vs Non)",
      term == "surf_ha" ~ "Effet Taille Parcelle (par ha)",
      TRUE ~ term
    )
  )

print(stats_export)

# --- 6. SYNTHÈSE POUR LE RAPPORT ---
gain_engrais <- stats_export$estimate[stats_export$term == "Impact Engrais (Oui vs Non)"]
p_val_engrais <- stats_export$p.value[stats_export$term == "Impact Engrais (Oui vs Non)"]

cat("\n--- SYNTHÈSE POUR LE RAPPORT ---\n")
cat(paste0("L'utilisation d'engrais augmente le rendement moyen de ", round(gain_engrais, 1), " kg/ha.\n"))
cat(paste0("Ce résultat est hautement significatif (p = ", format.pval(p_val_engrais), ").\n"))


ggplot(stats_export, aes(x = estimate, y = reorder(term, estimate))) +
  # Ligne de référence à zéro
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  # Barres d'erreur (Intervalles de confiance à 95%)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#2C3E50") +
  # Points des coefficients
  geom_point(size = 4, aes(color = (estimate > 0))) +
  # Esthétique
  scale_color_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C")) +
  labs(
    title = "Déterminants du Rendement Céréalier au Nigeria",
    subtitle = "Coefficients issus de la régression pondérée (svyglm)",
    x = "Impact sur le rendement (en kg/ha)",
    y = "",
    caption = "Intervalles de confiance à 95% | Source : GHS-LSMS Wave 4"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )
