##############################################################################
#  TP2 — ÉDUCATION ET ALPHABÉTISATION DES MÉNAGES NIGÉRIANS
#  Nigeria GHS Panel — Vague 4 (2018) | ENSAE ISE 1 | 2025-2026
#  Auteurs : Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba
##############################################################################
#
#  BASES UTILISÉES :
#    sect2_harvestw4.dta  → éducation (s2aq9, s2aq5, s2aq6, s2aq13a)
#    sect1_harvestw4.dta  → sexe, âge, secteur (jointure)
#    secta_harvestw4.dta  → poids de sondage wt_wave4 (niveau ménage)
#
#  CORRECTIONS v2 :
#    - map_educ vectorisée via Vectorize() (évite list vs character avec mapply)
#    - educ_dist : extraction niveau robuste (str_remove + str_squish)
#    - Chemin SCRIPT_DIR robuste même en knit/Rscript
#    - scale_y heatmap dynamique selon nb d'États
#    - limits y T10 fixé à c(NA, 5.6) pour ne jamais couper les annotations
##############################################################################

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr, viridis, survey, purrr)
options(survey.lonely.psu = "adjust")

# ── CHEMIN ROBUSTE (RStudio, Rscript, knitr) ─────────────────────────────────
SCRIPT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f    <- args[grep("--file=", args)]
    if (length(f)) dirname(normalizePath(sub("--file=", "", f)))
    else getwd()
  }
)
TP_DIR   <- dirname(SCRIPT_DIR)
DATA_DIR <- file.path(TP_DIR, "data", "NGA_2018_GHSP-W4_v03_M_Stata12")
OUT      <- file.path(TP_DIR, "outputs")
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

f_sect2 <- file.path(DATA_DIR, "sect2_harvestw4.dta")
f_sect1 <- file.path(DATA_DIR, "sect1_harvestw4.dta")
f_secta <- file.path(DATA_DIR, "secta_harvestw4.dta")
if (!all(file.exists(c(f_sect2, f_sect1, f_secta))))
  stop("Fichiers introuvables. Verifier data/NGA_2018_GHSP-W4_v03_M_Stata12/")
message("Fichiers trouves\n")

# ── PALETTE & THEME ───────────────────────────────────────────────────────────
COLS_EDUC <- c("Aucun"       = "#E74C3C",
               "Primaire"    = "#F39C12",
               "Junior Sec." = "#F1C40F",
               "Senior Sec." = "#27AE60",
               "Tertiaire"   = "#2980B9")
BLUE_ENSAE <- "#003E74"
COL_R <- "#27AE60"; COL_U <- "#E67E22"; COL_M <- "#1B6CA8"; COL_F <- "#C0392B"

THEME <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 8)),
        plot.caption  = element_text(size = 8, color = "grey55", hjust = 0),
        axis.title    = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey92"),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text       = element_text(face = "bold"))

# ── CLASSIFICATION NIVEAU ÉDUCATION (VECTORISÉE) ─────────────────────────────
# CORRECTION : Vectorize() garantit un character vector (pas une list)
.map_educ_scalar <- function(ever_attended, s2aq9_val) {
  ever_str  <- as.character(ever_attended)
  s2aq9_str <- as.character(s2aq9_val)
  if (grepl("^2", ever_str)) return("Aucun")
  code <- suppressWarnings(as.integer(str_extract(s2aq9_str, "^\\d+")))
  if (is.na(code)) return(NA_character_)
  if (code %in% c(0, 1:3, 51, 52))                    return("Aucun")
  if (code %in% c(11:16, 61))                          return("Primaire")
  if (code %in% 21:23)                                 return("Junior Sec.")
  if (code %in% c(24:28, 31:33, 321, 322))             return("Senior Sec.")
  if (code %in% c(34, 35, 41:43, 411, 412, 421:424))   return("Tertiaire")
  return(NA_character_)
}
map_educ <- Vectorize(.map_educ_scalar, USE.NAMES = FALSE)

# ── CHARGEMENT & NETTOYAGE ────────────────────────────────────────────────────
message("Chargement des donnees...")

dfsec_poids <- read_dta(f_secta) %>%
  select(hhid, wt_wave4) %>% distinct(hhid, .keep_all = TRUE)

df_s1 <- read_dta(f_sect1) %>%
  mutate(s1q4a_num = as.integer(str_extract(as.character(s1q4a), "^\\d+"))) %>%
  filter(s1q4a_num != 2 | is.na(s1q4a_num)) %>%
  mutate(sex       = as.integer(str_extract(as.character(s1q2), "^\\d+")),
         sex_label = factor(case_when(sex == 1 ~ "Homme", sex == 2 ~ "Femme")),
         age       = as.numeric(s1q4),
         age       = if_else(age > 110 | age < 0, NA_real_, age),
         sec_num   = as.integer(str_extract(as.character(sector), "^\\d+")),
         sec_label = factor(case_when(sec_num == 1 ~ "Urbain", sec_num == 2 ~ "Rural"),
                            levels = c("Rural", "Urbain")),
         state_clean = str_squish(str_remove(as.character(state), "^\\d+\\.?\\s*")))

df_educ <- read_dta(f_sect2) %>%
  left_join(df_s1 %>% select(hhid, indiv, sex_label, age, sec_label, sec_num, state_clean),
            by = c("hhid", "indiv")) %>%
  left_join(dfsec_poids, by = "hhid") %>%
  mutate(
    educ_level    = map_educ(s2aq6, s2aq9),
    educ_ord      = factor(educ_level, levels = names(COLS_EDUC), ordered = TRUE),
    educ_num      = as.integer(educ_ord),
    alphab        = case_when(str_detect(as.character(s2aq5), "^1") ~ "Alphabetise",
                              str_detect(as.character(s2aq5), "^2") ~ "Non alphab.",
                              TRUE ~ NA_character_),
    scol_actuelle = case_when(str_detect(as.character(s2aq13a), "^1") ~ 1L,
                              str_detect(as.character(s2aq13a), "^2") ~ 0L,
                              TRUE ~ NA_integer_)
  )

adults   <- df_educ %>% filter(!is.na(age), age >= 18, !is.na(educ_level), !is.na(wt_wave4))
N_adults <- nrow(adults)
message(sprintf("Adultes 18+ avec education et poids : %d\n", N_adults))

svy_educ <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4, data = adults)

# ── TACHE 7 — VALEURS MANQUANTES ─────────────────────────────────────────────
message("=== Tache 7 : Valeurs manquantes ===")
p_miss <- gg_miss_var(
  df_educ %>% select(`Educ. (s2aq9)`   = educ_level,
                     `Alphab. (s2aq5)` = alphab,
                     `Scol. (s2aq13a)` = scol_actuelle,
                     Age               = age,
                     Sexe              = sex_label,
                     Poids_wt_wave4    = wt_wave4),
  show_pct = TRUE) +
  labs(title    = "Valeurs manquantes — Variables cles TP2 (2018)",
       subtitle = "sect2_harvestw4 | Membres presents | Poids wt_wave4 inclus",
       caption  = "Source : GHS Panel W4") + THEME
ggsave(file.path(OUT, "T7_missing.png"), p_miss, w = 8, h = 5, dpi = 200, bg = "white")
message("  -> T7_missing.png")

# ── TACHE 8 — DISTRIBUTION NIVEAU ÉDUCATION (PONDÉRÉE) ───────────────────────
message("\n=== Tache 8 : Distribution niveau education (ponderee) ===")
educ_prop <- svymean(~factor(educ_level, levels = names(COLS_EDUC)), svy_educ, na.rm = TRUE)
educ_ci   <- confint(educ_prop, level = 0.95)

# CORRECTION : extraction robuste du libellé de niveau
raw_names   <- names(coef(educ_prop))
clean_names <- str_remove(raw_names, ".*\\)")   # retire tout jusqu'au dernier ")"
clean_names <- str_squish(clean_names)

educ_dist <- data.frame(
  educ_level = clean_names,
  pct        = as.numeric(coef(educ_prop)) * 100,
  pct_lo     = educ_ci[, 1] * 100,
  pct_hi     = educ_ci[, 2] * 100,
  stringsAsFactors = FALSE
) %>%
  left_join(adults %>% count(educ_level, name = "n"), by = "educ_level") %>%
  mutate(educ_f = factor(educ_level, levels = names(COLS_EDUC)),
         educ_f = fct_reorder(educ_f, pct))

cat("\n--- Distribution education ponderee ---\n")
print(educ_dist %>% select(educ_level, n, pct, pct_lo, pct_hi))

p_educ_dist <- ggplot(educ_dist, aes(x = pct, y = educ_f, fill = educ_f)) +
  geom_col(alpha = 0.87, width = 0.68) +
  geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi),
                 height = 0.25, linewidth = 0.9, color = "grey25") +
  geom_text(aes(label = sprintf("%.1f%%   (n=%s)", pct, comma(n))),
            hjust = -0.05, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = COLS_EDUC, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .26))) +
  labs(title    = "Niveau d'education des adultes (18+) — W4 (2018) · Pondere",
       subtitle = sprintf("N=%s adultes | Poids wt_wave4 | IC 95%% linearisation Taylor",
                          comma(N_adults)),
       x = "Proportion ponderee (%)", y = "Niveau d'education",
       caption = "Source : GHS Panel W4 | s2aq9 + s2aq6 | Ponderation wt_wave4") + THEME
ggsave(file.path(OUT, "T8_educ_distribution.png"), p_educ_dist, w = 10, h = 6, dpi = 200, bg = "white")
message("  -> T8_educ_distribution.png")

# ── TACHE 9 — EDUCATION × SEXE + CHI² PONDÉRÉ ────────────────────────────────
message("\n=== Tache 9 : Education x Sexe (pondere) ===")
adults_sex <- adults %>%
  filter(!is.na(sex_label)) %>%
  mutate(educ_f = factor(educ_level, levels = names(COLS_EDUC)))
svy_sex  <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4, data = adults_sex)
chi2_svy <- svychisq(~educ_f + sex_label, svy_sex, statistic = "Chisq")

educ_sex <- adults_sex %>%
  count(educ_level, sex_label) %>%
  group_by(sex_label) %>% mutate(pct = n / sum(n) * 100) %>% ungroup() %>%
  mutate(educ_level = factor(educ_level, levels = names(COLS_EDUC)))

cont   <- table(adults_sex$educ_level, adults_sex$sex_label)
chi2_b <- chisq.test(cont)
V_cr   <- sqrt(chi2_b$statistic / (sum(cont) * (min(nrow(cont), ncol(cont)) - 1)))
message(sprintf("Chi2 survey=%.2f p=%.2e | V Cramer=%.3f", chi2_svy$statistic, chi2_svy$p.value, V_cr))

p_educ_sex <- ggplot(educ_sex, aes(x = sex_label, y = pct, fill = educ_level)) +
  geom_col(position = "fill", alpha = 0.87, width = 0.6) +
  geom_text(aes(label = if_else(pct > 3.5, sprintf("%.0f%%", pct), "")),
            position = position_fill(vjust = 0.5),
            size = 3.4, color = "white", fontface = "bold") +
  scale_fill_manual(values = COLS_EDUC, name = "Niveau") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  annotate("label", x = 1.5, y = 1.07,
           label = sprintf("Chi2 survey=%.1f · p<0.001\nV Cramer=%.3f", chi2_svy$statistic, V_cr),
           size = 3.2, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Niveau d'education par sexe (adultes 18+) — W4 (2018) · Pondere",
       subtitle = "Barres 100% empilees | Chi2 survey + V de Cramer | Poids wt_wave4",
       x = "Sexe", y = "Proportion",
       caption = sprintf("Chi2 survey=%.1f · V Cramer=%.3f\nSource : GHS Panel W4 | wt_wave4",
                         chi2_svy$statistic, V_cr)) +
  THEME + theme(legend.position = "right")
ggsave(file.path(OUT, "T9_educ_par_sexe.png"), p_educ_sex, w = 9, h = 7, dpi = 200, bg = "white")
message("  -> T9_educ_par_sexe.png")

# ── TACHE 10 — AGE × ÉDUCATION (KW + DUNN) ───────────────────────────────────
message("\n=== Tache 10 : Age x education ===")
adults <- adults %>%
  mutate(age_grp = cut(age, breaks = c(18, 30, 45, 60, Inf),
                       labels = c("18-30", "31-45", "46-60", "60+"),
                       right = FALSE, include.lowest = TRUE))

kw     <- kruskal.test(educ_num ~ age_grp, data = adults %>% filter(!is.na(age_grp)))
dunn_r <- dunn_test(adults %>% filter(!is.na(age_grp)), educ_num ~ age_grp,
                    p.adjust.method = "bonferroni")
message(sprintf("Kruskal-Wallis : H=%.2f, df=%d, p=%.2e", kw$statistic, kw$parameter, kw$p.value))
print(dunn_r)

wmeans_age <- adults %>%
  filter(!is.na(educ_level), !is.na(age_grp)) %>%
  group_by(age_grp) %>%
  summarise(wm = weighted.mean(educ_num, wt_wave4, na.rm = TRUE), .groups = "drop")

p_age_educ <- ggplot(adults %>% filter(!is.na(educ_level), !is.na(age_grp)),
                     aes(x = age_grp, y = educ_num, fill = age_grp)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.15, width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +
  geom_point(data = wmeans_age, aes(x = age_grp, y = wm),
             shape = 8, size = 5, color = "#E67E22", inherit.aes = FALSE) +
  stat_summary(fun = mean, geom = "text",
               aes(label = sprintf("%.2f", after_stat(y))), vjust = -1.1, size = 2.9) +
  scale_fill_viridis_d(option = "plasma", direction = -1, guide = "none") +
  # CORRECTION : limits(NA, 5.6) pour laisser de la place à l'annotation
  scale_y_continuous(breaks = 1:5,
                     labels = c("Aucun", "Primaire", "Jr Sec.", "Sr Sec.", "Tertiaire"),
                     limits = c(NA, 5.6)) +
  annotate("label", x = 3.5, y = 5.4,
           label = sprintf("Kruskal-Wallis\nH=%.1f · p<0.001", kw$statistic),
           size = 3.2, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Niveau d'education par groupe d'age (adultes 18+) — W4 · Pondere",
       subtitle = "Diamant = Moy brute | Etoile = Moy ponderee (wt_wave4) | KW + Dunn Bonferroni",
       x = "Groupe d'age", y = "Niveau d'education",
       caption = sprintf("H=%.2f · p=%.2e | Source : GHS Panel W4", kw$statistic, kw$p.value)) + THEME
ggsave(file.path(OUT, "T10_age_education.png"), p_age_educ, w = 10, h = 7, dpi = 200, bg = "white")
message("  -> T10_age_education.png")

# ── TACHE 11 — SCOLARISATION 6-17 ANS (PONDÉRÉE) ────────────────────────────
message("\n=== Tache 11 : Scolarisation 6-17 ans (ponderee) ===")
children <- df_educ %>%
  filter(!is.na(age), between(age, 6, 17), !is.na(sec_label),
         !is.na(scol_actuelle), !is.na(wt_wave4))
cat(sprintf("Enfants 6-17 avec info et poids : %d\n", nrow(children)))

children <- children %>% mutate(scol_f = factor(scol_actuelle))
svy_ch   <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4, data = children)
chi_sc   <- svychisq(~scol_f + sec_label, svy_ch, statistic = "Chisq")
message(sprintf("Chi2 survey scolarisation x secteur : %.2f, p=%.4f",
                chi_sc$statistic, chi_sc$p.value))

enroll_df <- children %>%
  group_by(sec_label) %>%
  summarise(N      = n(),
            n_scol = sum(scol_actuelle),
            pct_w  = weighted.mean(scol_actuelle, wt_wave4, na.rm = TRUE) * 100,
            .groups = "drop") %>%
  mutate(pct    = n_scol / N * 100,
         pct_lo = map2_dbl(n_scol, N, ~binom.test(.x, .y)$conf.int[1] * 100),
         pct_hi = map2_dbl(n_scol, N, ~binom.test(.x, .y)$conf.int[2] * 100))
print(enroll_df)

p_enroll <- ggplot(enroll_df, aes(x = sec_label, y = pct_w, fill = sec_label)) +
  geom_col(alpha = 0.87, width = 0.52) +
  geom_errorbar(aes(ymin = pct_lo, ymax = pct_hi),
                width = 0.15, linewidth = 1, color = "grey20") +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s/%s)", pct_w, comma(n_scol), comma(N))),
            vjust = -0.35, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("Rural" = COL_R, "Urbain" = COL_U), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 108)) +
  annotate("label", x = 1.5, y = 96,
           label = sprintf("Chi2 survey=%.2f\np=%.4f", chi_sc$statistic, chi_sc$p.value),
           size = 3.3, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Scolarisation 2018/2019 des 6-17 ans — W4 · Ponderee",
       subtitle = "Variable s2aq13a | Taux pondere (wt_wave4) | IC 95% Clopper-Pearson",
       x = "Milieu", y = "Taux de scolarisation pondere (%)",
       caption = sprintf("N=%s enfants 6-17 | Chi2 survey=%.2f · p=%.4f\nSource : GHS Panel W4 | s2aq13a | wt_wave4",
                         comma(nrow(children)), chi_sc$statistic, chi_sc$p.value)) + THEME
ggsave(file.path(OUT, "T11_scolarisation.png"), p_enroll, w = 9, h = 7, dpi = 200, bg = "white")
message("  -> T11_scolarisation.png")

# ── TACHE 12 — HEATMAP ÉTAT × SANS INSTRUCTION (PONDÉRÉE) ────────────────────
message("\n=== Tache 12 : Heatmap analphabetisme par Etat (pondere) ===")

state_educ <- adults %>%
  filter(!is.na(state_clean), state_clean != "", !is.na(wt_wave4)) %>%
  mutate(state_clean = str_squish(str_remove(state_clean, "^[0-9]+\\.?\\s*"))) %>%
  filter(nchar(state_clean) > 1) %>%
  group_by(state_clean) %>%
  summarise(N         = n(),
            pct_aucun = weighted.mean(educ_level == "Aucun", wt_wave4, na.rm = TRUE) * 100,
            .groups   = "drop") %>%
  filter(N >= 15) %>%
  mutate(
    etat_f    = fct_reorder(state_clean, pct_aucun),
    txt_color = if_else(pct_aucun > 45, "white", "grey15")
  )

cat(sprintf("\nEtats inclus : %d | Plage : %.1f%% - %.1f%%\n",
            nrow(state_educ), min(state_educ$pct_aucun), max(state_educ$pct_aucun)))
print(state_educ %>% arrange(pct_aucun) %>% select(state_clean, N, pct_aucun))

# Hauteur adaptée au nombre d'États
n_etats    <- nrow(state_educ)
hauteur_hm <- max(10, min(20, n_etats * 0.40))

p_hm <- ggplot(state_educ, aes(x = "% adultes sans instruction", y = etat_f, fill = pct_aucun)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct_aucun), color = txt_color),
            size = 3.1, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027", "#67001f"),
    values = scales::rescale(c(0, 15, 25, 40, 55, 70, 85)),
    name   = "% sans\ninstruction",
    labels = function(x) paste0(x, "%"),
    guide  = guide_colorbar(barwidth = 0.9, barheight = 14, title.position = "top")
  ) +
  labs(
    title    = "Taux d'adultes (18+) sans instruction par Etat — W4 (2018) · Pondere",
    subtitle = sprintf("%d Etats classes du moins affecte (bas) au plus affecte (haut) | wt_wave4", n_etats),
    x = NULL, y = NULL,
    caption  = "Source : GHS Panel W4 | sect1 + sect2_harvestw4 | Ponderation wt_wave4"
  ) +
  THEME +
  theme(
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.text.y     = element_text(size = 9.5, color = "grey15"),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(OUT, "T12_heatmap_etats.png"), p_hm,
       w = 10, h = hauteur_hm, dpi = 200, bg = "white")
message("  -> T12_heatmap_etats.png")

# ── TABLEAU GTSUMMARY PONDÉRÉ ─────────────────────────────────────────────────
message("\n=== Tableau gtsummary pondere ===")
svy_tbl <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4,
                     data = adults %>%
                       filter(!is.na(sex_label), !is.na(sec_label)) %>%
                       mutate(`Niveau education` = factor(educ_level, levels = names(COLS_EDUC)),
                              Sexe   = sex_label,
                              Milieu = sec_label,
                              Age    = age))
print(
  tbl_svysummary(svy_tbl,
    include   = c(Milieu, Age, `Niveau education`),
    by        = Sexe,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n_unweighted} ({p}%)")
  ) %>%
    add_p(test = list(`Niveau education` ~ "svy.chisq.test",
                      Age               ~ "svy.wilcox.test")) %>%
    add_overall() %>% bold_labels() %>%
    modify_caption("**Tableau 2. Education (ponderee) par sexe — Adultes 18+, W4 (2018)**") %>%
    modify_footnote(everything() ~ "Analyses ponderees wt_wave4 | Chi2 survey · Wilcoxon survey")
)

# ── FIGURE DE SYNTHÈSE ────────────────────────────────────────────────────────
message("\n=== Figure de synthese TP2 ===")
fig_tp2 <- (p_educ_dist | p_educ_sex) /
           (p_age_educ  | p_enroll)   /
            p_hm +
  plot_layout(heights = c(1.2, 1.2, 2.2)) +
  plot_annotation(
    title    = "TP2 — Education & Alphabetisation · Nigeria GHS Panel W4 (2018) · Pondere",
    subtitle = "Auteurs : Cheikh M. M. Ndiaye & Awa Ba | ENSAE ISE 1 | 2025-2026 | wt_wave4",
    caption  = paste0("Analyses ponderees (wt_wave4) | Correction s2aq6='No' -> Aucun | ",
                      "Scolarisation : s2aq13a (2018/2019)\n",
                      "Tests : Chi2 survey · V Cramer · Kruskal-Wallis · Dunn Bonferroni"),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      plot.caption  = element_text(size = 8,  color = "grey50")
    )
  )
ggsave(file.path(OUT, "TP2_Figure_Synthese.png"), fig_tp2,
       w = 20, h = 26, dpi = 200, bg = "white")
message(sprintf("Figure de synthese -> %s", file.path(OUT, "TP2_Figure_Synthese.png")))
message("\nTP2 TERMINE — analyses ponderees (wt_wave4)")
