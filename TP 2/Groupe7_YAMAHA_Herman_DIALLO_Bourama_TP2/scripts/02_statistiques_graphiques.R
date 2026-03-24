# =============================================================================
# Script 02 : Statistiques descriptives et représentations graphiques
# Thème 2 – Éducation et alphabétisation des membres des ménages
# Enquête GHS Nigeria 2018 (Wave 4) – avec pondérations transversales wt_wave4
# Réalisé par : Herman YAMAHA, Bourama DIALLO
#
# NOTE MÉTHODOLOGIQUE :
#   Toutes les statistiques sont calculées avec les pondérations wt_wave4.
#   - Graphiques  : aes(weight = wt_wave4) dans ggplot2
#   - Proportions : survey_mean() via srvyr
#   - Tests       : svydesign() via survey
#   - Tableau     : tbl_svysummary() via gtsummary → export Excel (openxlsx)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(rstatix)
library(ggpubr)
library(gtsummary)
library(viridis)
library(patchwork)
library(scales)
library(survey)
library(srvyr)
library(openxlsx)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Chargement des objets (exécuter 01 si absents)
if (!file.exists("data/processed/df_adultes.rds")) {
  cat("Objets absents — exécution de 01_import_nettoyage.R...\n")
  source("scripts/01_import_nettoyage.R")
}

df_adultes <- readRDS("data/processed/df_adultes.rds")
df_educ    <- readRDS("data/processed/df_educ.rds")
df_scol    <- readRDS("data/processed/df_scol.rds")

# --------------------------------------------------------------------------
# Plans de sondage
# --------------------------------------------------------------------------
design_adultes <- df_adultes %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

design_scol <- df_scol %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

# --------------------------------------------------------------------------
# Palette et thème communs
# --------------------------------------------------------------------------
couleurs_niveaux <- c(
  "Aucun"            = "#B2182B",
  "Primaire"         = "#E66101",
  "Junior Secondary" = "#FDB863",
  "Senior Secondary" = "#5E3C99",
  "Tertiaire"        = "#1B7837"
)

theme_projet <- theme_light(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", colour = "#2C3E50", size = 13),
    plot.subtitle     = element_text(colour = "#5D6D7E", size = 10),
    plot.caption      = element_text(colour = "grey45", size = 8.5, hjust = 1),
    panel.grid.minor  = element_blank()
  )

# ============================================================
# TÂCHE 8 : Répartition du niveau d'instruction
# ============================================================
cat("\n--- Tâche 8 : Répartition globale du niveau d'instruction ---\n")

# Proportions pondérées
tab_freq_pond <- design_adultes %>%
  group_by(niveau_educ) %>%
  summarise(prop = survey_mean(vartype = "ci")) %>%
  mutate(pct  = prop * 100,
         etiq = paste0(round(pct, 1), "%"))

cat("Proportions pondérées :\n"); print(as.data.frame(tab_freq_pond))

g8 <- ggplot(tab_freq_pond,
             aes(x = pct, y = fct_rev(niveau_educ), fill = niveau_educ)) +
  geom_col(width = 0.55, color = "grey20", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = prop_low * 100, xmax = prop_upp * 100),
                 height = 0.25, color = "grey30", linewidth = 0.6) +
  geom_text(aes(x = prop_upp * 100, label = etiq),
            hjust = -0.25, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = couleurs_niveaux, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Répartition du niveau d'instruction des adultes (18 ans et +)",
       subtitle = "Données GHS – Wave 4, Nigeria (2018) | Barres d'erreur : IC 95%",
       x = "Pourcentage (%)", y = NULL,
       caption = "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4 | Traitement : Groupe 7") +
  theme_projet +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig01_barplot_niveau_instruction.png", g8,
       width = 8, height = 5, dpi = 150)
cat("  \u2713 fig01 exportée\n")

# ============================================================
# TÂCHE 9 : Instruction selon le genre + Chi2 + V de Cramer
# ============================================================
cat("\n--- Tâche 9 : Instruction selon le genre ---\n")

# Proportions pondérées par sexe et niveau
tab_genre_pond <- design_adultes %>%
  filter(!is.na(sexe_label)) %>%
  group_by(sexe_label, niveau_educ) %>%
  summarise(n_pond = survey_total(vartype = NULL), .groups = "drop") %>%
  group_by(sexe_label) %>%
  mutate(pct = n_pond / sum(n_pond) * 100) %>%
  ungroup()

g9 <- ggplot(tab_genre_pond,
             aes(x = sexe_label, y = pct / 100, fill = niveau_educ)) +
  geom_col(color = "grey30", width = 0.45) +
  geom_text(aes(label = ifelse(pct >= 3, paste0(round(pct, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.1, color = "white", fontface = "bold") +
  scale_fill_manual(values = couleurs_niveaux, name = "Niveau d'instruction") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title    = "Profil éducatif selon le genre (adultes 18+)",
       subtitle = "Représentation en barres empilées à 100% – GHS-W4, 2018",
       x = NULL, y = "Part relative",
       caption = paste0(
         "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4 | Traitement : Groupe 7\n",
         "Note : Valeurs < 3% non affichées dans les barres.\n",
         paste0(
           tab_genre_pond %>%
             filter(pct < 3) %>%
             mutate(ligne = paste0(sexe_label, " – ", niveau_educ,
                                   " : ", round(pct, 1), "%")) %>%
             pull(ligne) %>%
             paste(collapse = " | ")
         )
       )) +
  theme_projet +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7.5, color = "grey40",
                                    hjust = 0, lineheight = 1.3))

ggsave("outputs/fig02_barres100_genre.png", g9,
       width = 9, height = 5, dpi = 150)
cat("  \u2713 fig02 exportée\n")

# Test du Chi-deux (sur données non pondérées — test pondéré via svychisq)
svy_chi2 <- svychisq(~ sexe_label + niveau_educ,
                     design = svydesign(ids = ~hhid, weights = ~wt_wave4,
                                        data = df_adultes %>% filter(!is.na(wt_wave4),
                                                                     !is.na(sexe_label))))
cat("\nTest du Chi-deux pondéré (svychisq) :\n"); print(svy_chi2)

# V de Cramer (sur effectifs non pondérés pour la formule standard)
table_croisee <- table(df_adultes$sexe_label, df_adultes$niveau_educ)
res_chi2_brut <- chisq.test(table_croisee)
n_total  <- sum(table_croisee)
dim_min  <- min(nrow(table_croisee) - 1, ncol(table_croisee) - 1)
v_cramer <- sqrt(res_chi2_brut$statistic / (n_total * dim_min))
cat(sprintf("V de Cramer : %.4f\n", v_cramer))

saveRDS(list(table = table_croisee, chi2_pond = svy_chi2,
             chi2_brut = res_chi2_brut, v_cramer = v_cramer),
        "data/processed/resultats_tache9.rds")

# ============================================================
# TÂCHE 10 : Niveau d'instruction par tranche d'âge
# ============================================================
cat("\n--- Tâche 10 : Instruction par tranche d'âge ---\n")

df_age <- df_adultes %>%
  filter(!is.na(groupe_age)) %>%
  mutate(niveau_num = as.numeric(niveau_educ))

# Kruskal-Wallis (non paramétrique, pas de version pondérée standard)
test_kw   <- kruskal.test(niveau_num ~ groupe_age, data = df_age)
test_dunn <- df_age %>%
  dunn_test(niveau_num ~ groupe_age, p.adjust.method = "bonferroni")

cat("\nTest de Kruskal-Wallis :\n"); print(test_kw)
cat("\nComparaisons de Dunn :\n"); print(test_dunn)

# Médianes pondérées par groupe d'âge pour annotation
design_age <- df_age %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

med_pond <- design_age %>%
  group_by(groupe_age) %>%
  summarise(med = survey_quantile(niveau_num, quantiles = 0.5,
                                  vartype = NULL)) %>%
  as.data.frame()

g10 <- ggplot(df_age,
              aes(x = groupe_age, y = niveau_num, fill = groupe_age)) +
  geom_boxplot(aes(weight = wt_wave4), alpha = 0.70,
               outlier.alpha = 0.10, width = 0.55) +
  geom_jitter(alpha = 0.035, width = 0.12, size = 0.4, color = "grey50") +
  scale_y_continuous(breaks = 1:5, labels = levels(df_adultes$niveau_educ)) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title    = "Niveau d'instruction par tranche d'âge (adultes 18+)",
       subtitle = paste0("Kruskal-Wallis : H = ", round(test_kw$statistic, 2),
                         ", p < 0,001"),
       x = "Tranche d'âge", y = "Niveau d'instruction",
       caption = "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4 | Traitement : Groupe 7") +
  theme_projet +
  theme(panel.grid.major.x = element_blank())

ggsave("outputs/fig03_boxplot_age.png", g10,
       width = 9, height = 6, dpi = 150)
cat("  \u2713 fig03 exportée\n")

saveRDS(list(kruskal = test_kw, dunn = test_dunn),
        "data/processed/resultats_tache10.rds")

# ============================================================
# TÂCHE 11 : Taux de scolarisation 6-17 ans – urbain vs rural
# ============================================================
cat("\n--- Tâche 11 : Scolarisation des 6-17 ans selon le milieu ---\n")

# Taux pondérés par milieu
taux_scol_pond <- design_scol %>%
  group_by(zone_scol) %>%
  summarise(
    prop     = survey_mean(scolarise, na.rm = TRUE, vartype = "ci"),
    .groups  = "drop"
  ) %>%
  mutate(pct       = prop * 100,
         borne_inf = prop_low * 100,
         borne_sup = prop_upp * 100)

cat("\nTaux de scolarisation pondérés avec IC 95% :\n")
print(as.data.frame(taux_scol_pond))

# Chi-deux pondéré
svy_chi2_scol <- svychisq(
  ~ zone_scol + scolarise,
  design = svydesign(ids = ~hhid, weights = ~wt_wave4,
                     data = df_scol %>% filter(!is.na(wt_wave4)))
)
cat("\nChi-deux pondéré scolarisation :\n"); print(svy_chi2_scol)

g11 <- ggplot(taux_scol_pond,
              aes(x = zone_scol, y = pct, fill = zone_scol,
                  ymin = borne_inf, ymax = borne_sup)) +
  geom_col(width = 0.45, alpha = 0.90) +
  geom_errorbar(width = 0.12, color = "grey25", linewidth = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -1.9, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Urbain" = "#2166AC", "Rural" = "#B2182B"),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 108),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Taux de scolarisation des 6-17 ans selon le milieu de résidence",
       subtitle = paste0("Chi-deux = ", round(svy_chi2_scol$statistic, 1),
                         " | Barres d'erreur : IC 95%"),
       x = NULL, y = "Taux de scolarisation (%)",
       caption = "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4 | Traitement : Groupe 7") +
  theme_projet +
  theme(panel.grid.major.x = element_blank())

ggsave("outputs/fig04_scolarisation_milieu.png", g11,
       width = 7, height = 5, dpi = 150)
cat("  \u2713 fig04 exportée\n")

saveRDS(list(chi2_pond = svy_chi2_scol, taux = taux_scol_pond),
        "data/processed/resultats_tache11.rds")

# ============================================================
# TÂCHE 12 : Cartes de chaleur par État (proportions pondérées)
# ============================================================
cat("\n--- Tâche 12 : Cartes de chaleur – non-instruction par État ---\n")

# Proportions pondérées par État
design_hm <- df_adultes %>%
  filter(!is.na(wt_wave4), !is.na(state_name)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

df_heatmap <- design_hm %>%
  group_by(state_name) %>%
  summarise(
    n_pond    = survey_total(vartype = NULL),
    pct_aucun = survey_mean(niveau_educ == "Aucun", na.rm = TRUE,
                             vartype = NULL) * 100
  ) %>%
  as.data.frame() %>%
  arrange(desc(pct_aucun))

cat("\n10 États avec le plus fort taux de non-instruction :\n")
print(head(df_heatmap, 10))

g12a <- ggplot(df_heatmap,
               aes(x = 1,
                   y = fct_reorder(state_name, pct_aucun),
                   fill = pct_aucun)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct_aucun, 1), "%")),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "% sans instruction",
                       labels = function(x) paste0(x, "%")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title    = "Proportion d'adultes (18+) sans instruction par État",
       subtitle = "Intensité colorimétrique — GHS Wave 4, 2018",
       x = NULL, y = NULL,
       caption = "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4 | Traitement : Groupe 7") +
  theme_light(base_size = 10) +
  theme(plot.title   = element_text(face = "bold", size = 12, colour = "#2C3E50"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid   = element_blank())

ggsave("outputs/fig05_heatmap_non_instruction_etat.png", g12a,
       width = 8, height = 11, dpi = 150)
cat("  \u2713 fig05 exportée\n")

# Ventilation État × Niveau : tableau pondéré 

df_etat_niv <- design_hm %>%
  group_by(state_name, niveau_educ) %>%
  summarise(n_pond = survey_total(vartype = NULL), .groups = "drop") %>%
  as.data.frame() %>%
  group_by(state_name) %>%
  mutate(
    pct_brut = n_pond / sum(n_pond) * 100,
    pct      = round(pct_brut, 1),

  ) %>%
  ungroup() %>%
  select(state_name, niveau_educ, n_pond, pct)

# Mise en forme large (une colonne par niveau)
df_etat_niv_large <- df_etat_niv %>%
  select(state_name, niveau_educ, pct) %>%
  tidyr::pivot_wider(names_from  = niveau_educ,
                     values_from = pct,
                     values_fill = 0) %>%
  left_join(
    df_heatmap %>% select(state_name, pct_aucun),
    by = "state_name"
  ) %>%
  arrange(desc(pct_aucun)) %>%
  select(state_name, Aucun, Primaire,
         `Junior Secondary`, `Senior Secondary`, Tertiaire) %>%
  rename(État = state_name)

saveRDS(list(df_heatmap = df_heatmap, df_etat_niv_large = df_etat_niv_large),
        "data/processed/df_heatmap.rds")

# ============================================================
# TÂCHE 13 : Export Excel — 2 feuilles
#   Feuille 1 : Profil éducatif par milieu (effectifs pondérés)
#   Feuille 2 : Ventilation du niveau d'instruction par État
# ============================================================
cat("\n--- Export Excel (2 feuilles) ---\n")

# ---------- Feuille 1 : Profil éducatif pondéré par milieu ----------
# On calcule manuellement les effectifs et proportions pondérés
# pour éviter tout affichage d'effectifs de l'échantillon.

design_tab <- df_adultes %>%
  filter(!is.na(wt_wave4), !is.na(zone_label)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

# Fonction helper : stats pondérées pour une variable catégorielle
stats_pond_cat <- function(design, var, by_var) {
  design %>%
    group_by(across(all_of(c(by_var, var)))) %>%
    summarise(
      eff_pond  = survey_total(vartype = NULL),
      prop_pond = survey_mean(vartype = "ci"),
      .groups   = "drop"
    ) %>%
    as.data.frame() %>%
    mutate(
      pct      = round(prop_pond * 100, 1),
      ic_inf   = round(prop_pond_low * 100, 1),
      ic_sup   = round(prop_pond_upp * 100, 1),
      cellule  = paste0(format(round(eff_pond), big.mark = " "),
                        "  (", pct, "% [", ic_inf, "-", ic_sup, "])"),
      milieu   = !!sym(by_var),
      modalite = as.character(!!sym(var))
    ) %>%
    select(milieu, modalite, eff_pond, pct, ic_inf, ic_sup, cellule)
}

variables_tab <- list(
  "Niveau d'instruction" = "niveau_educ",
  "Sexe"                 = "sexe_label",
  "Tranche d'âge"        = "groupe_age"
)

# Construction du tableau Feuille 1
lignes_f1 <- list()
for (libelle in names(variables_tab)) {
  var <- variables_tab[[libelle]]

  # Ensemble
  ens <- design_tab %>%
    group_by(across(all_of(var))) %>%
    summarise(eff_pond  = survey_total(vartype = NULL),
              prop_pond = survey_mean(vartype = "ci"), .groups = "drop") %>%
    as.data.frame() %>%
    mutate(pct    = round(prop_pond * 100, 1),
           ic_inf = round(prop_pond_low * 100, 1),
           ic_sup = round(prop_pond_upp * 100, 1),
           Ensemble = paste0(format(round(eff_pond), big.mark = " "),
                             "  (", pct, "% [", ic_inf, "-", ic_sup, "])"),
           modalite = as.character(!!sym(var))) %>%
    select(modalite, Ensemble)

  # Urbain
  urb <- stats_pond_cat(design_tab %>% filter(zone_label == "Urbain"), var, "zone_label") %>%
    select(modalite, Urbain = cellule)

  # Rural
  rur <- stats_pond_cat(design_tab %>% filter(zone_label == "Rural"), var, "zone_label") %>%
    select(modalite, Rural = cellule)

  # Ligne entête variable
  entete <- data.frame(
    Variable = libelle, Modalité = NA_character_,
    Ensemble = NA_character_, Urbain = NA_character_,
    Rural = NA_character_, `p-valeur` = NA_character_,
    check.names = FALSE
  )

  # p-valeur pondérée (svychisq)
  svy_p <- tryCatch({
    res <- svychisq(
      as.formula(paste0("~ zone_label + ", var)),
      design = svydesign(ids = ~hhid, weights = ~wt_wave4,
                         data = df_adultes %>% filter(!is.na(wt_wave4),
                                                      !is.na(zone_label)))
    )
    ifelse(res$p.value < 0.001, "< 0,001",
           format(round(res$p.value, 3), nsmall = 3))
  }, error = function(e) "—")

  df_var <- ens %>%
    left_join(urb, by = "modalite") %>%
    left_join(rur, by = "modalite") %>%
    mutate(Variable  = "",
           `p-valeur` = c(svy_p, rep(NA_character_, n() - 1))) %>%
    rename(Modalité = modalite) %>%
    select(Variable, Modalité, Ensemble, Urbain, Rural, `p-valeur`)

  lignes_f1 <- c(lignes_f1, list(entete, df_var))
}

df_feuille1 <- bind_rows(lignes_f1)

# ---------- Construction du classeur Excel ----------
wb <- createWorkbook()

# Styles communs
style_titre   <- createStyle(fontName = "Arial", fontSize = 13,
                              textDecoration = "bold", fontColour = "#1A5276")
style_source  <- createStyle(fontName = "Arial", fontSize = 9,
                              fontColour = "grey50", textDecoration = "italic")
style_header  <- createStyle(fontName = "Arial", fontSize = 11,
                              textDecoration = "bold", halign = "center",
                              fgFill = "#1A5276", fontColour = "white",
                              border = "TopBottomLeftRight",
                              borderColour = "white")
style_entete_var <- createStyle(fontName = "Arial", fontSize = 10,
                                 textDecoration = "bold",
                                 fgFill = "#D5E8F0",
                                 border = "TopBottomLeftRight",
                                 borderColour = "#CCCCCC")
style_cellule <- createStyle(fontName = "Arial", fontSize = 10,
                              halign = "center",
                              border = "TopBottomLeftRight",
                              borderColour = "#CCCCCC", wrapText = TRUE)
style_alt     <- createStyle(fgFill = "#EAF2F8")
style_note    <- createStyle(fontName = "Arial", fontSize = 9,
                              fontColour = "grey50", textDecoration = "italic")

# ===== FEUILLE 1 : Profil éducatif =====
addWorksheet(wb, "Profil_Educatif")

writeData(wb, "Profil_Educatif",
          "Profil éducatif par milieu de résidence",
          startRow = 1, startCol = 1)
writeData(wb, "Profil_Educatif",
          "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4",
          startRow = 2, startCol = 1)
writeData(wb, "Profil_Educatif", df_feuille1,
          startRow = 4, startCol = 1, headerStyle = style_header)

nr1 <- nrow(df_feuille1); nc1 <- ncol(df_feuille1)

# Styles du corps
addStyle(wb, "Profil_Educatif", style_cellule,
         rows = 4:(nr1 + 4), cols = 1:nc1, gridExpand = TRUE, stack = TRUE)

# Lignes d'en-tête de variable (Variable non vide)
for (i in seq_len(nr1)) {
  if (!is.na(df_feuille1$Variable[i]) && df_feuille1$Variable[i] != "") {
    addStyle(wb, "Profil_Educatif", style_entete_var,
             rows = i + 4, cols = 1:nc1, gridExpand = TRUE, stack = TRUE)
  }
}
# Lignes alternées pour les modalités
lignes_mod <- which(df_feuille1$Variable == "" | is.na(df_feuille1$Variable))
for (i in seq_along(lignes_mod)) {
  if (i %% 2 == 0)
    addStyle(wb, "Profil_Educatif", style_alt,
             rows = lignes_mod[i] + 4, cols = 1:nc1, gridExpand = TRUE,
             stack = TRUE)
}

addStyle(wb, "Profil_Educatif", style_titre,   rows = 1, cols = 1)
addStyle(wb, "Profil_Educatif", style_source,  rows = 2, cols = 1)
setColWidths(wb, "Profil_Educatif",
             cols = 1:nc1, widths = c(26, 22, 28, 28, 28, 12))

writeData(wb, "Profil_Educatif",
          paste0("Note : Effectifs pondérés représentatifs de la population.",
                 " Format : N (% [IC95% inf – sup])."),
          startRow = nr1 + 6, startCol = 1)
addStyle(wb, "Profil_Educatif", style_note, rows = nr1 + 6, cols = 1)

# ===== FEUILLE 2 : Ventilation par État =====
addWorksheet(wb, "Niveaux_par_Etat")

writeData(wb, "Niveaux_par_Etat",
          "Ventilation du niveau d'instruction par État — adultes 18 ans et plus",
          startRow = 1, startCol = 1)
writeData(wb, "Niveaux_par_Etat",
          "Source : GHS-W4, Nigeria 2018 | Pondération : wt_wave4 | Proportions pondérées (%)",
          startRow = 2, startCol = 1)
writeData(wb, "Niveaux_par_Etat", df_etat_niv_large,
          startRow = 4, startCol = 1, headerStyle = style_header)

nr2 <- nrow(df_etat_niv_large); nc2 <- ncol(df_etat_niv_large)

addStyle(wb, "Niveaux_par_Etat", style_cellule,
         rows = 4:(nr2 + 4), cols = 1:nc2, gridExpand = TRUE, stack = TRUE)
for (i in seq(2, nr2, by = 2))
  addStyle(wb, "Niveaux_par_Etat", style_alt,
           rows = i + 4, cols = 1:nc2, gridExpand = TRUE, stack = TRUE)

# Coloration conditionnelle : colonne Aucun → dégradé rouge
pcts_aucun <- df_etat_niv_large$Aucun
q33 <- quantile(pcts_aucun, 0.33, na.rm = TRUE)
q66 <- quantile(pcts_aucun, 0.66, na.rm = TRUE)
for (i in seq_len(nr2)) {
  v <- pcts_aucun[i]
  if (!is.na(v)) {
    bg <- if (v >= q66) "#F4CCCC" else if (v >= q33) "#FCE8DC" else "#EAF4EA"
    addStyle(wb, "Niveaux_par_Etat",
             createStyle(fgFill = bg, fontName = "Arial", fontSize = 10,
                         halign = "center", border = "TopBottomLeftRight",
                         borderColour = "#CCCCCC"),
             rows = i + 4, cols = 2, stack = FALSE)
  }
}

addStyle(wb, "Niveaux_par_Etat", style_titre,  rows = 1, cols = 1)
addStyle(wb, "Niveaux_par_Etat", style_source, rows = 2, cols = 1)
setColWidths(wb, "Niveaux_par_Etat",
             cols = 1:nc2, widths = c(18, rep(16, nc2 - 1)))

writeData(wb, "Niveaux_par_Etat",
          paste0("Note : Proportions pondérées (%) calculées avec wt_wave4. ",
                 "Chaque ligne somme à 100%. ",
                 "États classés du plus touché au moins touché."),
          startRow = nr2 + 6, startCol = 1)
addStyle(wb, "Niveaux_par_Etat", style_note, rows = nr2 + 6, cols = 1)

saveWorkbook(wb, "outputs/tableau_gtsummary.xlsx", overwrite = TRUE)
cat("  \u2713 tableau_gtsummary.xlsx exporté (2 feuilles)\n")
cat("      Feuille 1 : Profil éducatif par milieu (effectifs pondérés)\n")
cat("      Feuille 2 : Ventilation par État (proportions pondérées)\n")

cat("\n====== Ensemble des analyses réalisées ======\n")
cat("Figures PNG dans outputs/ :\n")
for (f in list.files("outputs/", pattern = "\\.png$")) cat(" -", f, "\n")
cat("Tableau Excel dans outputs/ :\n")
for (f in list.files("outputs/", pattern = "\\.xlsx$")) cat(" -", f, "\n")
cat("\nNote : La ventilation par État (fig06 supprimée) est disponible\n")
cat("       dans la feuille 'Niveaux_par_Etat' du fichier Excel.\n")
