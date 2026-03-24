##############################################################################
#  TP1 — PROFIL DÉMOGRAPHIQUE DES MÉNAGES NIGÉRIANS
#  Nigeria GHS Panel — Vague 4 (2018) | ENSAE ISE 1 | 2025-2026
#  Auteurs : Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba
##############################################################################
#
#  BASES UTILISÉES :
#    sect1_harvestw4.dta  → individus : sexe, âge, lien de parenté
#    secta_harvestw4.dta  → ménages   : zone, état, LGA + poids (wt_wave4)
#
#  NOTE MÉTHODOLOGIQUE — PONDÉRATION :
#    Le GHS W4 est une enquête en grappes stratifiées. Le fichier secta contient
#    le poids de sondage wt_wave4 (niveau ménage). Ce poids est joint à sect1
#    via hhid pour toutes les analyses individuelles.
#    → On multiplie DIRECTEMENT par le poids (jamais par 1-poids).
#    → Intégration via le package {survey} (svydesign + svymean, tbl_svysummary).
#    → La pondération s'applique : statistiques descriptives, proportions,
#      tableau gtsummary, pyramide des âges, répartition par zone.
#    → Pas de pondération pour : tests Wilcoxon bruts (robustes),
#      graphiques boxplot (structure de distribution).
#
#  NOTE FILTRE :
#    s1q4a = code 2 → membres absents (3 780 obs.) → exclus de toutes analyses
#
#  STRUCTURE :
#    TP1/
#    ├── scripts/TP1_Demographie.R      ← ce fichier
#    ├── data/NGA_2018_GHSP-W4_v03_M_Stata12/
#    └── outputs/                       ← toutes les sorties ici
##############################################################################

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr, survey, purrr)

# ── 1. CHEMINS AUTOMATIQUES ───────────────────────────────────────────────────
SCRIPT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f    <- args[grep("--file=", args)]
    if (length(f)) dirname(normalizePath(sub("--file=", "", f)))
    else getwd()
  }
)
TP_DIR     <- dirname(SCRIPT_DIR)
DATA_DIR   <- file.path(TP_DIR, "data", "NGA_2018_GHSP-W4_v03_M_Stata12")
OUT        <- file.path(TP_DIR, "outputs")
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

f_sect1 <- file.path(DATA_DIR, "sect1_harvestw4.dta")
f_secta <- file.path(DATA_DIR, "secta_harvestw4.dta")

if (!file.exists(f_sect1) || !file.exists(f_secta)) {
  stop("Fichiers introuvables. Extraire NGA_2018_GHSP-W4_v03_M_Stata12 dans TP1/data/")
}
message("Fichiers trouves\n")

# ── 2. PALETTE & THEME ────────────────────────────────────────────────────────
COL_M  <- "#1B6CA8"
COL_F  <- "#C0392B"
COL_R  <- "#27AE60"
COL_U  <- "#E67E22"

THEME <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 10, color = "grey40", margin = margin(b = 8)),
    plot.caption     = element_text(size = 8,  color = "grey55", hjust = 0),
    axis.title       = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    strip.text       = element_text(face = "bold")
  )

# ── 3. CHARGEMENT & NETTOYAGE ─────────────────────────────────────────────────
message("Chargement des donnees...")

# Poids depuis secta (niveau menage) → wt_wave4
dfsec_poids <- read_dta(f_secta) %>%
  select(hhid, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

df_raw <- read_dta(f_sect1) %>%
  mutate(s1q4a_num = as.integer(str_extract(as.character(s1q4a), "^\\d+")))

n_avant <- nrow(df_raw)
df <- df_raw %>% filter(s1q4a_num != 2 | is.na(s1q4a_num))
n_apres <- nrow(df)
n_exclus <- n_avant - n_apres
message(sprintf("  Filtre s1q4a : %d exclus | %d retenus", n_exclus, n_apres))

df <- df %>%
  mutate(
    sex       = as.integer(str_extract(as.character(s1q2), "^\\d+")),
    sex_label = factor(case_when(sex == 1 ~ "Homme", sex == 2 ~ "Femme"),
                       levels = c("Homme", "Femme")),
    age       = as.numeric(s1q4),
    age       = if_else(age > 110 | age < 0, NA_real_, age),
    rel_code  = as.integer(str_extract(as.character(s1q3), "^\\d+")),
    sec_num   = as.integer(str_extract(as.character(sector), "^\\d+")),
    sec_label = factor(case_when(sec_num == 1 ~ "Urbain", sec_num == 2 ~ "Rural"),
                       levels = c("Rural", "Urbain")),
    zone_clean  = str_remove(as.character(zone), "^\\d+\\.\\s*"),
    state_clean = str_remove(as.character(state), "^\\d+\\.\\s*")
  )

# Taille menage (membres presents)
hh_size_df <- df %>% count(hhid, name = "hh_size")
df <- df %>% left_join(hh_size_df, by = "hhid")

# JOINTURE DES POIDS (wt_wave4 depuis secta, via hhid)
df <- df %>% left_join(dfsec_poids, by = "hhid")
n_sans_poids <- sum(is.na(df$wt_wave4))
message(sprintf("  wt_wave4 joints : %d sans poids (%.1f%%)",
                n_sans_poids, n_sans_poids / nrow(df) * 100))

# Design survey individus
svy_design <- svydesign(
  ids     = ~hhid,
  strata  = ~sec_num,
  weights = ~wt_wave4,
  data    = df %>% filter(!is.na(wt_wave4)),
  nest    = TRUE
)

hh_df <- df %>% distinct(hhid, .keep_all = TRUE) %>%
  filter(!is.na(sec_label), !is.na(hh_size), !is.na(wt_wave4))

message(sprintf("W4 : %d individus | %d menages\n", nrow(df), n_distinct(df$hhid)))

# ── 4. TACHE 1 — VALEURS MANQUANTES ─────────────────────────────────────────
message("=== Tache 1 : Valeurs manquantes ===")

p_miss <- gg_miss_var(
  df %>% select(Sexe = sex, Age = age, Parente = rel_code,
                Milieu = sec_label, `Taille.men` = hh_size, Poids_wt_wave4 = wt_wave4),
  show_pct = TRUE) +
  labs(title    = "Valeurs manquantes — Variables cles W4 (2018)",
       subtitle = "sect1_harvestw4 | Membres presents (s1q4a=YES) | Poids wt_wave4 inclus",
       caption  = "Source : GHS Panel W4") + THEME
ggsave(file.path(OUT, "T1_missing.png"), p_miss, w = 8, h = 5, dpi = 200, bg = "white")
print(p_miss)
message("  -> T1_missing.png sauvegarde")

# ── 5. TACHE 2 — AGE (PONDERE) ───────────────────────────────────────────────
message("\n=== Tache 2 : Age (pondere) ===")

age_v <- df %>% filter(!is.na(age)) %>% pull(age)

# Stats ponderees via survey
svy_age    <- subset(svy_design, !is.na(age))
age_moy_w  <- as.numeric(coef(svymean(~age, svy_age)))
age_med_w  <- as.numeric(coef(svyquantile(~age, svy_age, quantiles = 0.5, ci = FALSE)))
age_q1_w   <- as.numeric(coef(svyquantile(~age, svy_age, quantiles = 0.25, ci = FALSE)))
age_q3_w   <- as.numeric(coef(svyquantile(~age, svy_age, quantiles = 0.75, ci = FALSE)))
age_sd_w   <- as.numeric(sqrt(coef(svyvar(~age, svy_age))))
age_cv_w   <- age_sd_w / age_moy_w * 100
age_asym_w <- 3 * (age_moy_w - age_med_w) / age_sd_w

cat("\n--- Stats age ponderees ---\n")
cat(sprintf("Moy pond.=%.2f | Med pond.=%.1f | SD pond.=%.2f | CV=%.1f%% | Asym=%.3f\n",
            age_moy_w, age_med_w, age_sd_w, age_cv_w, age_asym_w))

set.seed(42)
sw <- shapiro.test(sample(age_v, min(5000, length(age_v))))
message(sprintf("Shapiro-Wilk : W=%.4f, p=%.2e -> %s", sw$statistic, sw$p.value,
                ifelse(sw$p.value < 0.05, "NON-NORMALE", "NORMALE")))

p_hist <- ggplot(df %>% filter(!is.na(age), !is.na(wt_wave4)),
                 aes(x = age, weight = wt_wave4)) +
  geom_histogram(binwidth = 5, fill = COL_M, color = "white", alpha = 0.85) +
  geom_vline(xintercept = age_med_w, color = COL_F, linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = age_med_w + 6, y = Inf, vjust = 1.6,
           label = sprintf("Med. pond. = %.0f ans", age_med_w),
           color = COL_F, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title    = "Distribution des ages (ponderes) — W4 (2018)",
       subtitle = sprintf("N=%s | Moy pond.=%.1f | Med pond.=%.0f | CV=%.1f%%",
                          comma(length(age_v)), age_moy_w, age_med_w, age_cv_w),
       x = "Age (annees)", y = "Effectif pondere",
       caption = "Source : GHS Panel W4 | Poids wt_wave4") + THEME

p_box <- ggplot(df %>% filter(!is.na(age)), aes(x = "W4 (2018)", y = age)) +
  geom_boxplot(fill = COL_M, color = "#154360", alpha = 0.82, width = 0.45,
               outlier.color = COL_F, outlier.alpha = 0.2, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "#F39C12") +
  annotate("text", x = 1, y = 105,
           label = sprintf("Moy pond.=%.1f\nMed pond.=%.0f\nSD=%.1f\nCV=%.1f%%",
                           age_moy_w, age_med_w, age_sd_w, age_cv_w),
           size = 3.3, hjust = 0.5, color = "grey20") +
  labs(title = "Boxplot — Age W4", subtitle = "Mediane | Valeurs extremes",
       x = "", y = "Age (annees)") + THEME +
  theme(axis.text.x = element_text(face = "bold"))

p_age <- (p_hist | p_box) +
  plot_annotation(
    title   = "Tache 2 — Analyse univariee de l'age (pondere wt_wave4) · W4 (2018)",
    caption = sprintf("Shapiro-Wilk W=%.4f, p=%.2e -> Distribution NON-NORMALE",
                      sw$statistic, sw$p.value),
    theme   = theme(plot.title = element_text(face = "bold", size = 13))
  )
ggsave(file.path(OUT, "T2_age_univarie.png"), p_age, w = 13, h = 6, dpi = 200, bg = "white")
print(p_age)
message("  -> T2_age_univarie.png sauvegarde")

# ── 6. TACHE 3 — PYRAMIDE DES AGES (PONDEREE) ────────────────────────────────
message("\n=== Tache 3 : Pyramide ponderee ===")

pyr <- df %>%
  filter(!is.na(age), !is.na(sex_label), between(age, 0, 100), !is.na(wt_wave4)) %>%
  mutate(age_grp = cut(age, breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 95, 5), "-", seq(4, 99, 5)),
                       right = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(age_grp)) %>%
  group_by(age_grp, sex_label) %>%
  summarise(n_pond = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  mutate(n_plot  = if_else(sex_label == "Homme", -n_pond, n_pond),
         age_grp = fct_rev(age_grp))

tot_h <- sum(pyr$n_pond[pyr$sex_label == "Homme"])
tot_f <- sum(pyr$n_pond[pyr$sex_label == "Femme"])
max_bar <- max(abs(pyr$n_plot))
breaks_x <- seq(-ceiling(max_bar / 2e6) * 2e6, ceiling(max_bar / 2e6) * 2e6, 2e6)

p_pyr <- ggplot(pyr, aes(x = n_plot, y = age_grp, fill = sex_label)) +
  geom_col(alpha = 0.85, width = 0.88) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
  scale_x_continuous(
    labels = function(x) paste0(abs(x) / 1e6, "M"),
    breaks = breaks_x) +
  scale_fill_manual(values = c("Homme" = COL_M, "Femme" = COL_F), name = "Sexe") +
  annotate("text", x = -max_bar * 0.75, y = nlevels(pyr$age_grp) + 0.5,
           label = sprintf("Hommes\n%.1fM", tot_h / 1e6),
           color = COL_M, fontface = "bold", size = 3.5, hjust = 0.5) +
  annotate("text", x =  max_bar * 0.75, y = nlevels(pyr$age_grp) + 0.5,
           label = sprintf("Femmes\n%.1fM", tot_f / 1e6),
           color = COL_F, fontface = "bold", size = 3.5, hjust = 0.5) +
  labs(title    = "Pyramide des ages — Nigeria GHS Panel W4 (2018) — Effectifs ponderes",
       subtitle = sprintf("Population estimee : %.1fM individus | Poids wt_wave4 | Groupes quinquennaux",
                          (tot_h + tot_f) / 1e6),
       x = "Effectif pondere (millions)", y = "Groupe d'age (ans)",
       caption  = sprintf("Rapport de masculinite : %.1f H/100 F\nSource : GHS Panel W4 | wt_wave4",
                          tot_h / tot_f * 100)) +
  THEME + theme(legend.position = "top", panel.grid.major.y = element_blank())
ggsave(file.path(OUT, "T3_pyramide_ages.png"), p_pyr, w = 10, h = 13, dpi = 200, bg = "white")
print(p_pyr)
message("  -> T3_pyramide_ages.png sauvegarde")

# ── 7. TACHE 4 — LIEN DE PARENTE (PONDERE) ───────────────────────────────────
message("\n=== Tache 4 : Parente (ponderee) ===")

rel_lab <- c("1"="Chef de menage","2"="Conjoint(e)","3"="Enfant propre",
             "4"="Beau-fils/fille","5"="Enfant adopte","6"="Petit-enfant",
             "7"="Frere/Soeur","8"="Neveu/Niece","9"="Beau-fr./Belle-soeur",
             "10"="Parent","11"="Beau-parent","12"="Gendre/Bru","14"="Autre")

df_rel <- df %>%
  filter(!is.na(rel_code), !is.na(wt_wave4)) %>%
  mutate(rel_label = coalesce(rel_lab[as.character(rel_code)], paste0("Code ", rel_code)))

# Proportions ponderees
svy_rel <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4,
                     data = df_rel, nest = TRUE)
rel_prop <- svymean(~factor(rel_label), svy_rel, na.rm = TRUE)
rel_ci   <- confint(rel_prop, level = 0.95)

rel_df <- data.frame(
  rel_label = gsub("^factor\\(rel_label\\)", "", rownames(rel_ci)),
  pct    = as.numeric(coef(rel_prop)) * 100,
  pct_lo = rel_ci[, 1] * 100,
  pct_hi = rel_ci[, 2] * 100
) %>%
  arrange(desc(pct)) %>% head(8) %>%
  left_join(df_rel %>% count(rel_label, name = "n"), by = "rel_label") %>%
  mutate(rel_label = fct_reorder(rel_label, pct),
         pct_lo    = pmax(pct_lo, 0))

cat("\n--- Proportions ponderees parente ---\n")
print(rel_df %>% select(rel_label, n, pct, pct_lo, pct_hi))

p_rel <- ggplot(rel_df, aes(x = pct, y = rel_label, fill = pct)) +
  geom_col(alpha = 0.87, width = 0.65) +
  geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi),
                 height = 0.28, linewidth = 0.9, color = "grey25") +
  geom_text(aes(label = sprintf("%.1f%%  (n=%s)", pct, comma(n))),
            hjust = -0.08, size = 3.3, fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .22))) +
  labs(title    = "Lien de parente avec le chef — W4 (2018) · Pondere (wt_wave4)",
       subtitle = "Proportions ponderees + IC 95% (linearisation de Taylor) | Top 8",
       x = "Proportion ponderee (%)", y = NULL,
       caption  = "Source : GHS Panel W4 | Poids wt_wave4") + THEME
ggsave(file.path(OUT, "T4_lien_parente.png"), p_rel, w = 10, h = 6, dpi = 200, bg = "white")
print(p_rel)
message("  -> T4_lien_parente.png sauvegarde")

# ── 8. TACHE 5 — TAILLE MENAGE RURAL vs URBAIN ───────────────────────────────
message("\n=== Tache 5 : Taille menage ===")

cat("\n--- Stats taille menage ---\n")
print(hh_df %>% group_by(sec_label) %>%
  summarise(N = n(),
            Mediane     = median(hh_size),
            Moy_brute   = round(mean(hh_size), 2),
            Moy_pond    = round(weighted.mean(hh_size, wt_wave4, na.rm = TRUE), 2),
            SD          = round(sd(hh_size), 2),
            Q1 = quantile(hh_size, .25),
            Q3 = quantile(hh_size, .75), .groups = "drop"))

wx    <- wilcox.test(hh_size ~ sec_label, data = hh_df, exact = FALSE)
n_r   <- sum(hh_df$sec_label == "Rural")
n_u   <- sum(hh_df$sec_label == "Urbain")
r_eff <- as.numeric(wx$statistic) / (n_r * n_u)
eff_lab <- case_when(r_eff < 0.1 ~ "negligeable", r_eff < 0.3 ~ "faible",
                     r_eff < 0.5 ~ "modere", TRUE ~ "fort")
moy_r_w <- weighted.mean(hh_df$hh_size[hh_df$sec_label == "Rural"],
                         hh_df$wt_wave4[hh_df$sec_label == "Rural"], na.rm = TRUE)
moy_u_w <- weighted.mean(hh_df$hh_size[hh_df$sec_label == "Urbain"],
                         hh_df$wt_wave4[hh_df$sec_label == "Urbain"], na.rm = TRUE)
message(sprintf("Wilcoxon : W=%.0f | p=%.4e | r=%.3f (%s)", wx$statistic, wx$p.value, r_eff, eff_lab))

p_hh <- ggplot(hh_df, aes(x = sec_label, y = hh_size, fill = sec_label)) +
  geom_boxplot(alpha = 0.82, width = 0.45,
               outlier.color = "grey60", outlier.alpha = 0.3, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +
  stat_summary(aes(weight = wt_wave4), fun = weighted.mean,
               geom = "point", shape = 8, size = 5, color = "#E67E22") +
  scale_fill_manual(values = c("Rural" = COL_R, "Urbain" = COL_U), guide = "none") +
  annotate("label", x = 1.5, y = max(hh_df$hh_size) * 0.90,
           label = sprintf("Wilcoxon\nW = %s\np = %.4e\nr = %.3f (%s)",
                           comma(wx$statistic), wx$p.value, r_eff, eff_lab),
           size = 3.1, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Taille des menages : Rural vs Urbain — W4 (2018) · Ponderee",
       subtitle = "Ligne = Mediane | Diamant = Moy. brute | Etoile = Moy. pond. (wt_wave4)",
       x = "Milieu", y = "Membres presents",
       caption  = sprintf("Moy. pond. — Rural : %.1f | Urbain : %.1f\nSource : GHS Panel W4",
                          moy_r_w, moy_u_w)) + THEME
ggsave(file.path(OUT, "T5_taille_menage.png"), p_hh, w = 8, h = 7, dpi = 200, bg = "white")
print(p_hh)
message("  -> T5_taille_menage.png sauvegarde")

# ── 9. ZONES GEOPOLITIQUES (PONDEREES) ───────────────────────────────────────
zone_d <- df %>%
  filter(!is.na(zone_clean), !is.na(wt_wave4)) %>%
  group_by(zone_clean) %>%
  summarise(n = n(), n_pond = sum(wt_wave4), .groups = "drop") %>%
  mutate(pct = n_pond / sum(n_pond) * 100,
         zone_clean = fct_reorder(zone_clean, n_pond))

p_zone <- ggplot(zone_d, aes(x = pct, y = zone_clean, fill = pct)) +
  geom_col(alpha = 0.87, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%  (%s)", pct, comma(n))),
            hjust = -0.05, size = 3.4, fontface = "bold") +
  scale_fill_gradient(low = "#D6EAF8", high = "#1A5276", guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  labs(title    = "Distribution par zone geopolitique — W4 (2018) · Ponderee",
       subtitle = "Proportions ponderees (wt_wave4) | 6 zones geopolitiques",
       x = "Proportion ponderee (%)", y = NULL,
       caption  = "Source : GHS Panel W4 | wt_wave4") + THEME
ggsave(file.path(OUT, "T5b_zones.png"), p_zone, w = 9, h = 5, dpi = 200, bg = "white")
message("  -> T5b_zones.png sauvegarde")

# ── 10. TACHE 6 — TABLEAU GTSUMMARY PONDERE ──────────────────────────────────
message("\n=== Tache 6 : Tableau gtsummary pondere ===")

svy_tbl <- svydesign(
  ids     = ~hhid,
  strata  = ~sec_num,
  weights = ~wt_wave4,
  data    = df %>%
    filter(!is.na(sec_label), !is.na(wt_wave4)) %>%
    mutate(Sexe          = sex_label,
           `Age (ans)`   = age,
           `Taille men`  = hh_size,
           Milieu        = sec_label),
  nest    = TRUE
)

tbl <- tbl_svysummary(
  svy_tbl,
  include   = c(Sexe, `Age (ans)`, `Taille men`),
  by        = Milieu,
  statistic = list(all_continuous()  ~ "{mean} ({sd}) | med. {median} [{p25}-{p75}]",
                   all_categorical() ~ "{n_unweighted} ({p}%)"),
  digits    = list(all_continuous() ~ 1)
) %>%
  add_p(test = list(all_continuous()  ~ "svy.wilcox.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_overall() %>% bold_labels() %>%
  modify_caption("**Tableau 1. Caracteristiques demographiques par milieu (pondere) — W4 (2018)**") %>%
  modify_footnote(everything() ~ "Analyses ponderees wt_wave4 | Wilcoxon survey (continu) | Chi2 survey (categoriel)")
print(tbl)

# ── 11. FIGURE DE SYNTHESE ────────────────────────────────────────────────────
message("\n=== Figure de synthese ===")

sex_pond <- df %>% filter(!is.na(sex_label), !is.na(wt_wave4)) %>%
  group_by(sex_label) %>%
  summarise(n_pond = sum(wt_wave4), n = n(), .groups = "drop") %>%
  mutate(pct = n_pond / sum(n_pond) * 100)

p_sex <- ggplot(sex_pond, aes(x = sex_label, y = n_pond / 1e6, fill = sex_label)) +
  geom_col(alpha = 0.87, width = 0.55) +
  geom_text(aes(label = sprintf("%s\n%.1f%%", comma(n), pct)),
            vjust = -0.3, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("Homme" = COL_M, "Femme" = COL_F), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "M"),
                     expand = expansion(mult = c(0, .15))) +
  labs(title = "Repartition par sexe (ponderee)", x = NULL, y = "Eff. pond. (M)") + THEME

fig_final <- (p_pyr | (p_sex / p_hh)) /
  (p_hist | p_rel | p_zone) +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title    = "TP1 — Profil Demographique des Menages Nigerians · GHS Panel W4 (2018) · Pondere",
    subtitle = "Auteurs : Cheikh M. M. Ndiaye & Awa Ba | ENSAE ISE 1 | 2025-2026 | wt_wave4",
    caption  = paste0("Filtre : s1q4a != NO | N=", comma(n_apres), " individus · ",
                      comma(n_distinct(df$hhid)), " menages | Tests : Shapiro-Wilk · Wilcoxon"),
    theme    = theme(plot.title    = element_text(face = "bold", size = 15),
                     plot.subtitle = element_text(size = 10, color = "grey35"),
                     plot.caption  = element_text(size = 8,  color = "grey50"))
  )
ggsave(file.path(OUT, "TP1_Figure_Synthese.png"), fig_final,
       w = 20, h = 18, dpi = 200, bg = "white")
message(sprintf("Figure de synthese -> %s", file.path(OUT, "TP1_Figure_Synthese.png")))
message("\nTP1 TERMINE — analyses ponderees (wt_wave4)")
