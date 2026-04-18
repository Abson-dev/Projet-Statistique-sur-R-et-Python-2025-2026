# ==============================================================================
# TP5 - Script 02 : Analyses statistiques et visualisations
# Analyse 5 : Cultures, intrants et rendements agricoles
# Données : Nigeria GHS Panel W4 (2018-2019), LSMS-ISA
# Auteurs  : Nkwa T. Leslye & Ouattara Ousmane, ENSAE ISE1 2025-2026
# Superviseur : M. Aboubacar HEMA, IFPRI
# ==============================================================================

cat("=== Démarrage de 02_analyses.R ===\n")


# ---------------------------------------------------------------------------- #
# 1. CHARGEMENT DES DONNÉES PRÉPARÉES
# ---------------------------------------------------------------------------- #
df_main     <- readRDS(here("data", "processed", "df_main.rds"))
df_menage   <- readRDS(here("data", "processed", "df_menage.rds"))
df_intrants <- readRDS(here("data", "processed", "df_intrants.rds"))
df_sup      <- readRDS(here("data", "processed", "df_sup.rds"))
crop_labels <- readRDS(here("data", "processed", "crop_labels.rds"))
poids       <- readRDS(here("data", "processed", "poids.rds"))

# Supprimer les labels Stata pour éviter les erreurs de type haven_labelled
df_intrants <- labelled::unlabelled(df_intrants)
poids       <- labelled::unlabelled(poids)

# Référence source pour les légendes
source_ghs <- "Source : GHS Panel W4 (2018-2019), NBS Nigeria / World Bank LSMS-ISA."

# ---------------------------------------------------------------------------- #
# 2. PALETTE DE COULEURS ET THÈME GRAPHIQUE (couleurs agronomiques FAO)
# ---------------------------------------------------------------------------- #
col_cereale   <- "#2E7D32"   # Vert foncé
col_tubercule <- "#F57F17"   # Ocre
col_legum     <- "#1565C0"   # Bleu
col_rente     <- "#6A1B9A"   # Violet
col_legume    <- "#00838F"   # Teal
col_fruit     <- "#C62828"   # Rouge
col_autre     <- "#546E7A"   # Gris ardoise
col_rural     <- "#4CAF50"
col_urbain    <- "#455A64"
col_npk       <- "#1B5E20"
col_uree      <- "#F9A825"
col_orga      <- "#795548"
col_pest      <- "#B71C1C"

palette_type <- c(
  "Céréale"          = col_cereale,
  "Tubercule"        = col_tubercule,
  "Légumineuse"      = col_legum,
  "Culture de rente" = col_rente,
  "Légume"           = col_legume,
  "Fruit"            = col_fruit,
  "Autre"            = col_autre
)

theme_tp5 <- theme_minimal(base_size = 10) +
  theme(
    plot.title      = element_text(size = 11, face = "bold", hjust = 0),
    plot.subtitle   = element_text(size = 9,  color = "grey40"),
    plot.caption    = element_text(size = 7,  color = "grey50", hjust = 1),
    axis.title      = element_text(size = 9),
    legend.position = "bottom",
    legend.title    = element_text(size = 8),
    legend.text     = element_text(size = 8)
  )
theme_set(theme_tp5)

# ---------------------------------------------------------------------------- #
# FONCTION UTILITAIRE : formatage flextable style LSMS
# ---------------------------------------------------------------------------- #
style_lsms <- function(ft) {
  vert_h <- "#1B5E20"; vert_c <- "#E8F5E9"; blanc <- "#FFFFFF"
  nr     <- nrow(ft$body$dataset)
  ft |>
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::fontsize(size = 9,   part = "body")   |>
    flextable::fontsize(size = 9.5, part = "header") |>
    flextable::bg(bg = vert_h, part = "header")      |>
    flextable::color(color = blanc, part = "header") |>
    flextable::bold(part = "header")                 |>
    flextable::align(align = "center", part = "header") |>
    flextable::bg(i = seq(1, nr, 2), bg = blanc,  part = "body") |>
    flextable::bg(i = seq(2, nr, 2), bg = vert_c, part = "body") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::border_outer(
      part   = "all",
      border = officer::fp_border(color = vert_h, width = 1.5)) |>
    flextable::border_inner_h(
      part   = "body",
      border = officer::fp_border(color = "#AAAAAA", width = 0.4)) |>
    flextable::padding(padding.left = 6, padding.right = 6,
                       padding.top  = 3, padding.bottom = 3, part = "all")
}

# ============================================================
# TÂCHE 1 : 15 CULTURES LES PLUS FRÉQUENTES (W4)
# Barplot horizontal ordonné par fréquence, coloré par type
# ============================================================
cat("\n--- Tâche 1 : Top 15 cultures W4 ---\n")

top15_cultures <- df_main |>
  filter(harvested == 1, !is.na(crop_name)) |>
  count(crop_name, crop_type, wt = wt_wave4, name = "n_pond") |>
  arrange(desc(n_pond)) |>
  slice_head(n = 15) |>
  mutate(
    prop      = n_pond / sum(n_pond),
    pct_lbl   = paste0(round(prop * 100, 1), "%"),
    crop_name = fct_reorder(crop_name, n_pond)
  )

fig01 <- ggplot(top15_cultures,
                aes(x = crop_name, y = prop, fill = crop_type)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = pct_lbl), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(values = palette_type, name = "Type de culture") +
  labs(
    title    = "Les 15 cultures les plus pratiquées au Nigeria (W4, 2018-2019)",
    subtitle = "Part pondérée (wt_wave4) — ménages ayant récolté",
    x        = NULL,
    y        = "Part pondérée (%)",
    caption  = source_ghs
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(here("outputs", "figures", "fig01_top15_cultures.png"),
       fig01, width = 9, height = 6, dpi = 300)
cat("fig01 exportée\n")

# Tableau Top 15
tab_top15 <- top15_cultures |>
  arrange(desc(prop)) |>
  mutate(Rang = row_number(), `Part (%)` = round(prop * 100, 1)) |>
  select(Rang, Culture = crop_name, Type = crop_type, `Part (%)`) |>
  flextable() |>
  set_caption("Top 15 cultures pratiquées (W4, pondéré)") |>
  style_lsms()

saveRDS(tab_top15, here("data", "processed", "tab_top15.rds"))
cat("Tableau top15 sauvegardé\n")

# ============================================================
# TÂCHE 2 : DIVERSIFICATION CULTURALE PAR MÉNAGE
# Histogramme + Violin plot + Test de Wilcoxon Rural/Urbain
# ============================================================
cat("\n--- Tâche 2 : Diversification culturale ---\n")

# Statistiques descriptives globales
stats_divers <- df_menage |>
  summarise(
    N   = n(),
    Moy = mean(n_cultures),
    Med = median(n_cultures),
    SD  = sd(n_cultures),
    Min = min(n_cultures),
    Max = max(n_cultures),
    Q1  = quantile(n_cultures, 0.25),
    Q3  = quantile(n_cultures, 0.75)
  )
cat("Diversification globale :\n"); print(stats_divers)

# Statistiques par milieu
stats_divers_milieu <- df_menage |>
  filter(!is.na(milieu)) |>
  group_by(milieu) |>
  summarise(
    N   = n(),
    Moy = round(mean(n_cultures), 2),
    Med = median(n_cultures),
    SD  = round(sd(n_cultures), 2),
    .groups = "drop"
  )
cat("Diversification par milieu :\n"); print(stats_divers_milieu)

saveRDS(stats_divers,        here("data", "processed", "stats_divers.rds"))
saveRDS(stats_divers_milieu, here("data", "processed", "stats_divers_milieu.rds"))

# Test de Wilcoxon non-paramétrique (distributions asymétriques)
# wilcox_divers <- df_menage |>
#   filter(!is.na(milieu)) |>
#   wilcox_test(n_cultures ~ milieu, paired = FALSE) |>
#   add_significance()

# Taille d'effet r = |Z| / sqrt(N)
wilcox_divers <- df_menage |>
  dplyr::filter(!is.na(milieu)) |>
  rstatix::wilcox_test(n_cultures ~ milieu, paired = FALSE) |>
  rstatix::add_significance()

effect_divers <- df_menage |>
  filter(!is.na(milieu)) |>
  wilcox_effsize(n_cultures ~ milieu)

cat(sprintf("Wilcoxon Rural vs Urbain : W = %.0f, p = %.4f, r = %.3f\n",
            wilcox_divers$statistic,
            wilcox_divers$p,
            effect_divers$effsize))

saveRDS(list(wilcox = wilcox_divers, effect = effect_divers),
        here("data", "processed", "wilcox_divers.rds"))

# Figure 2a : Histogramme
fig02a <- ggplot(df_menage, aes(x = n_cultures)) +
  geom_histogram(binwidth = 1, fill = col_cereale, color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(df_menage$n_cultures),
             linetype = "dashed", color = col_rente, linewidth = 0.9) +
  annotate("text",
           x     = median(df_menage$n_cultures) + 0.4,
           y     = Inf, vjust = 2,
           label = paste0("Médiane = ", median(df_menage$n_cultures)),
           color = col_rente, size = 3.2, fontface = "italic") +
  scale_x_continuous(breaks = 1:max(df_menage$n_cultures)) +
  labs(
    title    = "Distribution du nombre de cultures par ménage",
    subtitle = "GHS Panel W4 — cultures effectivement récoltées",
    x        = "Nombre de cultures différentes",
    y        = "Nombre de ménages",
    caption  = source_ghs
  )

# Figure 2b : Violin plot par milieu
fig02b <- ggplot(df_menage |> filter(!is.na(milieu)),
                 aes(x = milieu, y = n_cultures, fill = milieu)) +
  geom_violin(alpha = 0.75, trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.5,
               alpha = 0.6, fill = "white") +
  scale_fill_manual(values = c("Urbain" = col_urbain, "Rural" = col_rural)) +
  stat_summary(fun = median, geom = "point",
               shape = 23, size = 3, fill = "white") +
  annotate("text",
           x = 1.5, y = max(df_menage$n_cultures) * 0.95,
           label = paste0("p Wilcoxon = ",
                          ifelse(wilcox_divers$p < 0.001, "< 0,001",
                                 round(wilcox_divers$p, 3))),
           size = 3.5, color = "grey30") +
  labs(
    title    = "Nombre de cultures par milieu de résidence",
    subtitle = "Violin + boxplot — médiane en losange blanc",
    x        = NULL,
    y        = "Nombre de cultures",
    caption  = source_ghs
  ) +
  theme(legend.position = "none")

fig02 <- fig02a | fig02b
ggsave(here("outputs", "figures", "fig02_diversification.png"),
       fig02, width = 12, height = 5, dpi = 300)
cat("fig02 exportée\n")

# ============================================================
# TÂCHE 3 : UTILISATION DES ENGRAIS
# Taux par type et zone + Test chi-deux
# ============================================================
cat("\n--- Tâche 3 : Analyse des engrais ---\n")

# df_intrants contient déjà zone, state, sector
# On joint uniquement wt_wave4, cluster, strata depuis poids
# pour éviter les doublons de colonnes (.x / .y)
df_int_zone <- df_intrants |>
  left_join(poids |> dplyr::select(hhid, wt_wave4, cluster, strata),
            by = "hhid") |>
  mutate(
    milieu = factor(as.integer(sector),
                    levels = c(1, 2),
                    labels = c("Urbain", "Rural")),
    zone_f = factor(as.integer(zone),
                    levels = 1:6,
                    labels = c("North Central", "North East", "North West",
                               "South East",    "South South", "South West"))
  )

# Taux d'utilisation pondérés par zone
taux_engrais_zone <- df_int_zone |>
  filter(!is.na(zone_f), !is.na(wt_wave4)) |>
  group_by(zone_f) |>
  summarise(
    N          = n(),
    taux_npk   = weighted.mean(npk_bin,       wt_wave4, na.rm = TRUE),
    taux_uree  = weighted.mean(uree_bin,       wt_wave4, na.rm = TRUE),
    taux_orga  = weighted.mean(organique_bin,  wt_wave4, na.rm = TRUE),
    taux_inorg = weighted.mean(inorg_bin,      wt_wave4, na.rm = TRUE),
    taux_pest  = weighted.mean(pesticide_bin,  wt_wave4, na.rm = TRUE),
    .groups = "drop"
  )

cat("Taux d'utilisation des intrants par zone :\n")
print(taux_engrais_zone)
saveRDS(taux_engrais_zone, here("data", "processed", "taux_engrais_zone.rds"))

# Taux nationaux (pondérés)
taux_global <- df_int_zone |>
  filter(!is.na(wt_wave4)) |>
  summarise(
    npk   = weighted.mean(npk_bin,      wt_wave4, na.rm = TRUE),
    uree  = weighted.mean(uree_bin,     wt_wave4, na.rm = TRUE),
    orga  = weighted.mean(organique_bin,wt_wave4, na.rm = TRUE),
    inorg = weighted.mean(inorg_bin,    wt_wave4, na.rm = TRUE),
    pest  = weighted.mean(pesticide_bin,wt_wave4, na.rm = TRUE)
  )
cat("Taux globaux :\n"); print(taux_global)
saveRDS(taux_global, here("data", "processed", "taux_global.rds"))

# Barplot groupé : taux par type et zone
taux_long <- taux_engrais_zone |>
  pivot_longer(cols      = starts_with("taux_"),
               names_to  = "type_engrais",
               values_to = "taux") |>
  mutate(
    type_engrais = recode(type_engrais,
      "taux_npk"   = "NPK",
      "taux_uree"  = "Urée",
      "taux_orga"  = "Organique",
      "taux_inorg" = "Chimique (total)",
      "taux_pest"  = "Pesticide"
    )
  ) |>
  filter(type_engrais %in% c("NPK", "Urée", "Organique", "Pesticide"))

fig03 <- ggplot(taux_long,
                aes(x = zone_f, y = taux, fill = type_engrais)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.7, alpha = 0.9) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(
    values = c("NPK" = col_npk, "Urée" = col_uree,
               "Organique" = col_orga, "Pesticide" = col_pest),
    name = "Type d'intrant"
  ) +
  labs(
    title    = "Taux d'utilisation des intrants agricoles par zone géopolitique",
    subtitle = "Proportions pondérées (wt_wave4) — GHS Panel W4",
    x        = NULL,
    y        = "Taux d'utilisation (%)",
    caption  = source_ghs
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))

ggsave(here("outputs", "figures", "fig03_taux_engrais.png"),
       fig03, width = 10, height = 5.5, dpi = 300)
cat("fig03 exportée\n")

# Test chi-deux : zone × utilisation engrais inorganique
tab_chi2     <- table(df_int_zone$zone_f, df_int_zone$inorg_bin)
chi2_engrais <- chisq.test(tab_chi2)
v_cramer_engrais <- sqrt(chi2_engrais$statistic /
                         (sum(tab_chi2) * (min(dim(tab_chi2)) - 1)))

cat(sprintf("Chi-deux zone x engrais : X2 = %.1f, df = %d, p < 0.001\n",
            chi2_engrais$statistic, chi2_engrais$parameter))
cat(sprintf("V de Cramer : %.3f\n", v_cramer_engrais))

saveRDS(list(chi2 = chi2_engrais, v_cramer = v_cramer_engrais),
        here("data", "processed", "chi2_engrais.rds"))

# Tableau intrants par milieu
taux_milieu <- df_int_zone |>
  filter(!is.na(milieu), !is.na(wt_wave4)) |>
  group_by(milieu) |>
  summarise(
    N                  = n(),
    `NPK (%)`          = round(100 * weighted.mean(npk_bin,       wt_wave4, na.rm = TRUE), 1),
    `Urée (%)`         = round(100 * weighted.mean(uree_bin,      wt_wave4, na.rm = TRUE), 1),
    `Organique (%)`    = round(100 * weighted.mean(organique_bin, wt_wave4, na.rm = TRUE), 1),
    `Pesticide (%)`    = round(100 * weighted.mean(pesticide_bin, wt_wave4, na.rm = TRUE), 1),
    `Inorganique (%)` = round(100 * weighted.mean(inorg_bin,     wt_wave4, na.rm = TRUE), 1),
    .groups = "drop"
  )

tab_milieu_ft <- taux_milieu |>
  flextable() |>
  set_caption("Taux d'utilisation des intrants par milieu (%, pondéré wt_wave4)") |>
  style_lsms()

saveRDS(tab_milieu_ft, here("data", "processed", "tab_milieu_intrants.rds"))
cat("Tableau intrants par milieu sauvegardé\n")

# ============================================================
# TÂCHE 4 : RENDEMENTS MAÏS (1080) ET MIL (1100)
# Statistiques + Boxplots par État
# ============================================================
cat("\n--- Tâche 4 : Rendements maïs et mil ---\n")

df_rdt <- df_main |>
  filter(
    cropcode %in% c(1080, 1100),
    harvested == 1,
    !is.na(rendement_ha_clean),
    !is.na(sup_ha)
  ) |>
  mutate(
    culture = if_else(cropcode == 1080, "Maïs", "Mil"),
    culture = factor(culture)
  )

cat(sprintf("Obs maïs (après nettoyage) : %d\n", sum(df_rdt$culture == "Maïs")))
cat(sprintf("Obs mil  (après nettoyage) : %d\n", sum(df_rdt$culture == "Mil")))

# Statistiques descriptives des rendements
stats_rdt <- df_rdt |>
  group_by(culture) |>
  summarise(
    N   = n(),
    Moy = round(mean(rendement_ha_clean),            0),
    Med = round(median(rendement_ha_clean),           0),
    SD  = round(sd(rendement_ha_clean),               0),
    Q1  = round(quantile(rendement_ha_clean, 0.25),   0),
    Q3  = round(quantile(rendement_ha_clean, 0.75),   0),
    .groups = "drop"
  )
cat("Statistiques rendements :\n"); print(stats_rdt)
saveRDS(stats_rdt, here("data", "processed", "stats_rdt.rds"))

# États avec N >= 15
etats_ok <- df_rdt |>
  count(state_name, culture) |>
  filter(n >= 15) |>
  pull(state_name) |>
  unique()

fig04 <- df_rdt |>
  filter(state_name %in% etats_ok) |>
  mutate(state_name = fct_reorder(state_name,
                                  rendement_ha_clean, .fun = median)) |>
  ggplot(aes(x = state_name, y = rendement_ha_clean, fill = culture)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.5, width = 0.6) +
  scale_y_continuous(labels = label_comma(big.mark = " "),
                     limits = c(0, NA)) +
  scale_fill_manual(values = c("Maïs" = col_cereale, "Mil" = col_tubercule),
                    name = "Culture") +
  labs(
    title    = "Distribution des rendements à l'hectare — Maïs et Mil par État",
    subtitle = "Outliers écartés (IQR × 3) — États avec N ≥ 15",
    x        = NULL,
    y        = "Rendement (kg/ha)",
    caption  = source_ghs
  ) +
  coord_flip() +
  facet_wrap(~ culture, scales = "free_x")

ggsave(here("outputs", "figures", "fig04_rendements_etat.png"),
       fig04, width = 12, height = 8, dpi = 300)
cat("fig04 exportée\n")

# Tableau statistiques rendements
tab_rdt_ft <- stats_rdt |>
  rename(Culture = culture, `N obs.` = N,
         `Moyenne (kg/ha)` = Moy, `Médiane (kg/ha)` = Med,
         `Écart-type` = SD) |>
  flextable() |>
  set_caption("Statistiques descriptives des rendements maïs et mil (kg/ha, W4)") |>
  style_lsms()

saveRDS(tab_rdt_ft, here("data", "processed", "tab_rdt.rds"))
cat("Tableau rendements sauvegardé\n")

# ============================================================
# TÂCHE 5 : ENGRAIS CHIMIQUE × RENDEMENT
# Boxplots groupés + Test de Wilcoxon + Taille d'effet
# ============================================================
cat("\n--- Tâche 5 : Engrais chimique vs Rendement ---\n")

# chimique_bin est déjà dans df_rdt (hérité de df_main)
df_rdt_engrais <- df_rdt |>
  filter(!is.na(chimique_bin)) |>
  mutate(
    engrais_f = factor(chimique_bin,
                       levels = c(0, 1),
                       labels = c("Sans engrais chimique",
                                  "Avec engrais chimique"))
  )

cat(sprintf("Obs avec info engrais : %d\n", nrow(df_rdt_engrais)))
cat("Répartition :\n")
print(table(df_rdt_engrais$engrais_f, df_rdt_engrais$culture))

# Tests de Wilcoxon

wilcox_rdt_mais <- df_rdt_engrais |>
  dplyr::filter(culture == "Maïs") |>
  rstatix::wilcox_test(rendement_ha_clean ~ engrais_f) |>
  rstatix::add_significance()

wilcox_rdt_mil <- df_rdt_engrais |>
  dplyr::filter(culture == "Mil") |>
  rstatix::wilcox_test(rendement_ha_clean ~ engrais_f) |>
  rstatix::add_significance()

# Tailles d'effet
effect_mais <- df_rdt_engrais |>
  filter(culture == "Maïs") |>
  wilcox_effsize(rendement_ha_clean ~ engrais_f)

effect_mil <- df_rdt_engrais |>
  filter(culture == "Mil") |>
  wilcox_effsize(rendement_ha_clean ~ engrais_f)

cat(sprintf("Wilcoxon Maïs (engrais) : p = %.4f, r = %.3f (%s)\n",
            wilcox_rdt_mais$p, effect_mais$effsize, effect_mais$magnitude))
cat(sprintf("Wilcoxon Mil  (engrais) : p = %.4f, r = %.3f (%s)\n",
            wilcox_rdt_mil$p,  effect_mil$effsize,  effect_mil$magnitude))

saveRDS(list(
  wilcox_mais = wilcox_rdt_mais, effect_mais = effect_mais,
  wilcox_mil  = wilcox_rdt_mil,  effect_mil  = effect_mil
), here("data", "processed", "wilcox_rdt_engrais.rds"))

fig05 <- ggplot(df_rdt_engrais,
                aes(x = engrais_f, y = rendement_ha_clean, fill = engrais_f)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5, width = 0.55) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "white") +
  scale_fill_manual(
    values = c("Sans engrais chimique" = "#B0BEC5",
               "Avec engrais chimique" = col_npk)
  ) +
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  facet_wrap(~ culture) +
  labs(
    title    = "Rendement selon l'utilisation d'engrais chimique (NPK/Urée)",
    subtitle = "Boxplot — losange blanc = moyenne — Test de Wilcoxon",
    x        = NULL,
    y        = "Rendement (kg/ha)",
    caption  = source_ghs
  ) +
  theme(legend.position  = "none",
        axis.text.x      = element_text(size = 8))

ggsave(here("outputs", "figures", "fig05_rdt_engrais.png"),
       fig05, width = 9, height = 5, dpi = 300)
cat("fig05 exportée\n")

# ============================================================
# FIGURE SYNTHÈSE COMBINÉE
# ============================================================
cat("\n--- Figure synthèse combinée ---\n")

fig_synthese <- (fig01) / (fig03) +
  plot_annotation(
    title   = "TP5 — Cultures, intrants et rendements (GHS W4, Nigeria 2018-2019)",
    caption = source_ghs,
    theme   = theme(
      plot.title   = element_text(size = 13, face = "bold"),
      plot.caption = element_text(size = 7, color = "grey50")
    )
  )

ggsave(here("outputs", "figures", "fig_synthese.png"),
       fig_synthese, width = 12, height = 14, dpi = 200)
cat("fig_synthese exportée\n")

cat("\n=== 02_analyses.R terminé avec succès ===\n")
cat("Figures dans : outputs/figures/\n")
cat("Tableaux dans : data/processed/\n")
