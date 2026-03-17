##############################################################################
#  TP1 — PROFIL DÉMOGRAPHIQUE DES MÉNAGES NIGÉRIANS
#  Nigeria GHS Panel — Vague 4 (2018) | ENSAE ISE 1 | 2025-2026
#  Auteurs : Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba
##############################################################################
#
#  BASES UTILISÉES :
#    sect1_harvestw4.dta  → individus : sexe, âge, lien de parenté
#    secta_harvestw4.dta  → ménages   : zone, état, LGA
#
#  NOTE MÉTHODOLOGIQUE IMPORTANTE :
#    GHS W4 est une enquête PANEL. Le fichier sect1 contient aussi les
#    membres qui ont QUITTÉ le ménage entre vagues (s1q4a = "NO" = 3780 obs).
#    Ces individus n'ont pas de données démographiques renseignées.
#    FILTRE OBLIGATOIRE : garder uniquement s1q4a != "NO"
#    Sans filtre : 30 337 obs → Avec filtre : 26 557 membres présents
#
#  STRUCTURE DU DOSSIER :
#    TP1/
#    ├── scripts/TP1_Demographie.R      ← ce fichier
#    ├── data/
#    │   └── NGA_2018_GHSP-W4_v03_M_Stata12/  ← dossier W4 extrait ici
#    └── outputs/                              ← graphiques sauvegardés ici
#
#  LANCEMENT (dans RStudio) :
#    → Ouvrir ce script et cliquer "Source"
#    → Les chemins sont automatiques
##############################################################################

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr)

# ── 1. CHEMINS AUTOMATIQUES ───────────────────────────────────────────────────
SCRIPT_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
TP_DIR     <- dirname(SCRIPT_DIR)
DATA_DIR   <- file.path(TP_DIR, "data", "NGA_2018_GHSP-W4_v03_M_Stata12")
OUT        <- file.path(TP_DIR, "outputs")
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

f_sect1 <- file.path(DATA_DIR, "sect1_harvestw4.dta")
f_secta <- file.path(DATA_DIR, "secta_harvestw4.dta")

if (!file.exists(f_sect1) || !file.exists(f_secta)) {
  stop("Fichiers introuvables. Extraire NGA_2018_GHSP-W4_v03_M_Stata12.zip dans TP1/data/")
}
message("✓ Fichiers trouvés\n")

# ── 2. PALETTE & THÈME ────────────────────────────────────────────────────────
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
message("Chargement des données...")

df_raw <- read_dta(f_sect1)

# ── FILTRE CRITIQUE : membres présents seulement ──────────────────────────────
# s1q4a = "Is [NAME] still a member of this household?"
# → "NO" (code 2) = membres partis depuis la vague précédente
# → Sans données démographiques, à exclure de toutes les analyses
df_raw <- df_raw %>%
  mutate(s1q4a_num = as.integer(str_extract(as.character(s1q4a), "^\\d+")))

n_avant <- nrow(df_raw)
df <- df_raw %>% filter(s1q4a_num != 2 | is.na(s1q4a_num))
n_apres <- nrow(df)
message(sprintf("  Filtre s1q4a : %d exclus (membres absents) | %d retenus",
                n_avant - n_apres, n_apres))

# Variables
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

# Taille de ménage (sur membres présents uniquement)
hh_size_df <- df %>% count(hhid, name = "hh_size")
df <- df %>% left_join(hh_size_df, by = "hhid")

dfsec <- read_dta(f_secta) %>%
  mutate(sec_num   = as.integer(str_extract(as.character(sector), "^\\d+")),
         sec_label = factor(case_when(sec_num == 1 ~ "Urbain", sec_num == 2 ~ "Rural"),
                            levels = c("Rural", "Urbain"))) %>%
  distinct(hhid, .keep_all = TRUE)

message(sprintf("✓ W4 : %d individus présents | %d ménages\n",
                nrow(df), n_distinct(df$hhid)))

# ── 4. TÂCHE 1 — EXPLORATION & VALEURS MANQUANTES ─────────────────────────────
message("=== Tâche 1 : Exploration ===")
glimpse(df %>% select(hhid, indiv, sex_label, age, rel_code, sec_label, hh_size))
message(sprintf("Doublons hhid+indiv : %d",
                sum(duplicated(paste(df$hhid, df$indiv)))))

p_miss <- gg_miss_var(
  df %>% select(Sexe = sex, `Âge` = age, Parenté = rel_code,
                Milieu = sec_label, `Taille mén.` = hh_size),
  show_pct = TRUE) +
  labs(title    = "Valeurs manquantes — Variables clés W4 (2018)",
       subtitle = "sect1_harvestw4 | Membres présents uniquement (s1q4a=YES)",
       caption  = "Source : GHS Panel W4") + THEME
ggsave(file.path(OUT, "T1_missing.png"), p_miss, w = 8, h = 5, dpi = 200, bg = "white")
print(p_miss)

# ── 5. TÂCHE 2 — ANALYSE UNIVARIÉE DE L'ÂGE ──────────────────────────────────
message("\n=== Tâche 2 : Analyse univariée de l'âge ===")
age_v <- df %>% filter(!is.na(age)) %>% pull(age)

stats_age <- tibble(
  N             = length(age_v),
  Moyenne       = round(mean(age_v), 2),
  Médiane       = median(age_v),
  SD            = round(sd(age_v), 2),
  Q1            = quantile(age_v, .25),
  Q3            = quantile(age_v, .75),
  Min           = min(age_v),
  Max           = max(age_v),
  CV_pct        = round(sd(age_v) / mean(age_v) * 100, 1),
  Asym_Pearson  = round(3 * (mean(age_v) - median(age_v)) / sd(age_v), 3)
)
cat("\n--- Statistiques descriptives âge ---\n")
print(stats_age)

set.seed(42)
sw <- shapiro.test(sample(age_v, min(5000, length(age_v))))
message(sprintf("Shapiro-Wilk : W = %.4f, p = %.2e → %s",
                sw$statistic, sw$p.value,
                ifelse(sw$p.value < 0.05, "NON-NORMALE → tests non-paramétriques", "NORMALE")))

p_hist <- ggplot(df %>% filter(!is.na(age)), aes(x = age)) +
  geom_histogram(binwidth = 5, fill = COL_M, color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(age_v), color = COL_F, linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = median(age_v) + 4, y = Inf, vjust = 1.6,
           label = sprintf("Médiane = %d ans", as.integer(median(age_v))),
           color = COL_F, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = comma) +
  labs(title    = "Distribution des âges — W4 (2018)",
       subtitle = sprintf("N = %s | Binwidth = 5 ans | Moy = %.1f | CV = %.1f%%",
                          comma(length(age_v)), mean(age_v), stats_age$CV_pct),
       x = "Âge (années)", y = "Effectif",
       caption = "Source : GHS Panel W4 | s1q4 — membres présents") + THEME

p_box <- ggplot(df %>% filter(!is.na(age)), aes(x = factor("W4 (2018)"), y = age)) +
  geom_boxplot(fill = COL_M, color = "#154360", alpha = 0.82, width = 0.45,
               outlier.color = COL_F, outlier.alpha = 0.2, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "#F39C12") +
  annotate("text", x = 1, y = 105,
           label = sprintf("♦ Moy = %.1f\nMéd = %d\nSD = %.1f\nCV = %.1f%%",
                           mean(age_v), as.integer(median(age_v)), sd(age_v),
                           stats_age$CV_pct),
           size = 3.3, hjust = 0.5, color = "grey20") +
  labs(title = "Boxplot — Âge W4",
       subtitle = "♦ Moyenne | Ligne = Médiane",
       x = "", y = "Âge (années)") + THEME +
  theme(axis.text.x = element_text(face = "bold"))

p_age <- (p_hist | p_box) +
  plot_annotation(
    title   = "Tâche 2 — Analyse univariée de l'âge · W4 (2018)",
    caption = sprintf("Shapiro-Wilk W = %.4f, p = %.2e → Distribution NON-NORMALE",
                      sw$statistic, sw$p.value),
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )
ggsave(file.path(OUT, "T2_age_univarie.png"), p_age, w = 13, h = 6, dpi = 200, bg = "white")
print(p_age)

# ── 6. TÂCHE 3 — PYRAMIDE DES ÂGES ───────────────────────────────────────────
message("\n=== Tâche 3 : Pyramide des âges ===")
pyr <- df %>%
  filter(!is.na(age), !is.na(sex_label), between(age, 0, 100)) %>%
  mutate(age_grp = cut(age, breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 95, 5), "–", seq(4, 99, 5)),
                       right = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(age_grp)) %>%
  count(age_grp, sex_label) %>%
  mutate(n_plot = if_else(sex_label == "Homme", -n, n),
         age_grp = fct_rev(age_grp))

tot_h <- sum(pyr$n[pyr$sex_label == "Homme"])
tot_f <- sum(pyr$n[pyr$sex_label == "Femme"])

p_pyr <- ggplot(pyr, aes(x = n_plot, y = age_grp, fill = sex_label)) +
  geom_col(alpha = 0.85, width = 0.88) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
  scale_x_continuous(labels = function(x) comma(abs(x)),
                     breaks = seq(-2500, 2500, 500), limits = c(-2700, 2700)) +
  scale_fill_manual(values = c("Homme" = COL_M, "Femme" = COL_F), name = "Sexe") +
  annotate("text", x = -2300, y = nlevels(pyr$age_grp) + 0.5,
           label = sprintf("Hommes\n%s", comma(tot_h)),
           color = COL_M, fontface = "bold", size = 3.5, hjust = 0.5) +
  annotate("text", x =  2300, y = nlevels(pyr$age_grp) + 0.5,
           label = sprintf("Femmes\n%s", comma(tot_f)),
           color = COL_F, fontface = "bold", size = 3.5, hjust = 0.5) +
  labs(title    = "Pyramide des âges — Nigeria GHS Panel · W4 (2018)",
       subtitle = sprintf("N = %s membres présents | Groupes quinquennaux",
                          comma(tot_h + tot_f)),
       x = "Effectif", y = "Groupe d'âge (ans)",
       caption  = paste0("Structure pyramidale = population jeune (base 0-14 large)\n",
                         "Source : GHS Panel W4 | s1q4 + s1q2 — membres présents")) +
  THEME + theme(legend.position = "top", panel.grid.major.y = element_blank())
ggsave(file.path(OUT, "T3_pyramide_ages.png"), p_pyr, w = 10, h = 13, dpi = 200, bg = "white")
print(p_pyr)

# ── 7. TÂCHE 4 — LIEN DE PARENTÉ + IC 95% ────────────────────────────────────
message("\n=== Tâche 4 : Lien de parenté ===")
rel_lab <- c("1"="Chef de ménage","2"="Conjoint(e)","3"="Enfant propre",
             "4"="Beau-fils/fille","5"="Enfant adopté","6"="Petit-enfant",
             "7"="Frère/Sœur","8"="Neveu/Nièce","9"="Beau-fr./Bel.-sœur",
             "10"="Parent","11"="Beau-parent","12"="Gendre/Bru","14"="Autre")
N_rel <- sum(!is.na(df$rel_code))

rel_df <- df %>%
  filter(!is.na(rel_code)) %>%
  mutate(rel_label = coalesce(rel_lab[as.character(rel_code)],
                              paste0("Code ", rel_code))) %>%
  count(rel_label) %>% arrange(desc(n)) %>% head(8) %>%
  mutate(
    pct    = n / N_rel * 100,
    pct_lo = purrr::map2_dbl(n, N_rel, ~ binom.test(.x, .y)$conf.int[1] * 100),
    pct_hi = purrr::map2_dbl(n, N_rel, ~ binom.test(.x, .y)$conf.int[2] * 100),
    rel_label = fct_reorder(rel_label, pct)
  )
cat("\n--- Proportions liens de parenté ---\n")
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
  labs(title    = "Lien de parenté avec le chef de ménage — W4 (2018)",
       subtitle = "Proportions + IC à 95% (test binomial exact de Clopper-Pearson) | Top 8",
       x = "Proportion (%)", y = NULL,
       caption  = "Source : GHS Panel W4 | s1q3 — sect1_harvestw4") + THEME
ggsave(file.path(OUT, "T4_lien_parente.png"), p_rel, w = 10, h = 6, dpi = 200, bg = "white")
print(p_rel)

# ── 8. TÂCHE 5 — TAILLE MÉNAGE RURAL vs URBAIN ───────────────────────────────
message("\n=== Tâche 5 : Taille ménage + Wilcoxon ===")
hh_df <- df %>% distinct(hhid, .keep_all = TRUE) %>%
  filter(!is.na(sec_label), !is.na(hh_size))

cat("\n--- Statistiques taille ménage ---\n")
print(hh_df %>% group_by(sec_label) %>%
  summarise(N=n(), Méd=median(hh_size), Moy=round(mean(hh_size),2),
            SD=round(sd(hh_size),2), Q1=quantile(hh_size,.25),
            Q3=quantile(hh_size,.75), Max=max(hh_size), .groups="drop"))

wx   <- wilcox.test(hh_size ~ sec_label, data = hh_df, exact = FALSE)
n_r  <- sum(hh_df$sec_label == "Rural")
n_u  <- sum(hh_df$sec_label == "Urbain")
r_eff <- as.numeric(wx$statistic) / (n_r * n_u)
eff_lab <- case_when(r_eff < 0.1 ~ "négligeable", r_eff < 0.3 ~ "faible",
                     r_eff < 0.5 ~ "modéré", TRUE ~ "fort")
message(sprintf("Wilcoxon : W = %.0f | p = %.4e | r = %.3f (%s)",
                wx$statistic, wx$p.value, r_eff, eff_lab))

p_hh <- ggplot(hh_df, aes(x = sec_label, y = hh_size, fill = sec_label)) +
  geom_boxplot(alpha = 0.82, width = 0.45,
               outlier.color = "grey60", outlier.alpha = 0.3, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +
  stat_summary(fun = mean, geom = "text",
               aes(label = sprintf("♦ %.1f", after_stat(y))),
               vjust = -1.1, size = 3.2) +
  scale_fill_manual(values = c("Rural" = COL_R, "Urbain" = COL_U), guide = "none") +
  annotate("label", x = 1.5, y = max(hh_df$hh_size) * 0.90,
           label = sprintf("Test de Wilcoxon\nW = %s\np = %.4e\nr = %.3f (%s)",
                           comma(wx$statistic), wx$p.value, r_eff, eff_lab),
           size = 3.1, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Taille des ménages : Rural vs Urbain — W4 (2018)",
       subtitle = "♦ Moyenne | Ligne = Médiane | Membres présents uniquement",
       x = "Milieu", y = "Nombre de membres présents",
       caption  = sprintf("Rural : méd=%d, moy=%.1f | Urbain : méd=%d, moy=%.1f\n%s",
                          as.integer(median(hh_df$hh_size[hh_df$sec_label=="Rural"])),
                          mean(hh_df$hh_size[hh_df$sec_label=="Rural"]),
                          as.integer(median(hh_df$hh_size[hh_df$sec_label=="Urbain"])),
                          mean(hh_df$hh_size[hh_df$sec_label=="Urbain"]),
                          "Source : GHS Panel W4 | sect1_harvestw4")) + THEME
ggsave(file.path(OUT, "T5_taille_menage.png"), p_hh, w = 8, h = 7, dpi = 200, bg = "white")
print(p_hh)

# ── 9. ZONES GÉOPOLITIQUES ────────────────────────────────────────────────────
zone_d <- df %>% filter(!is.na(zone_clean)) %>%
  count(zone_clean) %>%
  mutate(pct = n / sum(n) * 100,
         zone_clean = fct_reorder(zone_clean, n))

p_zone <- ggplot(zone_d, aes(x = pct, y = zone_clean, fill = pct)) +
  geom_col(alpha = 0.87, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%  (%s)", pct, comma(n))),
            hjust = -0.05, size = 3.4, fontface = "bold") +
  scale_fill_gradient(low = "#D6EAF8", high = "#1A5276", guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  labs(title    = "Distribution par zone géopolitique — W4 (2018)",
       subtitle = "6 zones géopolitiques du Nigéria",
       x = "Proportion (%)", y = NULL,
       caption  = "Source : GHS Panel W4 | sect1_harvestw4") + THEME
ggsave(file.path(OUT, "T5b_zones.png"), p_zone, w = 9, h = 5, dpi = 200, bg = "white")

# ── 10. TÂCHE 6 — TABLEAU GTSUMMARY ──────────────────────────────────────────
message("\n=== Tâche 6 : Tableau gtsummary ===")
tbl <- tbl_summary(
  df %>% filter(!is.na(sec_label)) %>%
    mutate(Sexe = sex_label, `Âge (ans)` = age, `Taille ménage` = hh_size) %>%
    select(Milieu = sec_label, Sexe, `Âge (ans)`, `Taille ménage`),
  by = Milieu,
  statistic = list(all_continuous()  ~ "{mean} ({sd}) | méd. {median} [{p25}–{p75}]",
                   all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_continuous() ~ 1)
) %>%
  add_p(test = list(all_continuous()  ~ "wilcox.test",
                    all_categorical() ~ "chisq.test")) %>%
  add_overall() %>% add_n() %>% bold_labels() %>%
  modify_caption("**Tableau 1. Caractéristiques démographiques par milieu — W4 (2018)**") %>%
  modify_footnote(everything() ~ "Wilcoxon (continu) · Chi² (catégoriel) | Membres présents uniquement")
print(tbl)

# ── 11. FIGURE DE SYNTHÈSE ────────────────────────────────────────────────────
message("\n=== Figure de synthèse ===")
p_sex <- ggplot(df %>% filter(!is.na(sex_label)),
                aes(x = sex_label, fill = sex_label)) +
  geom_bar(alpha = 0.87, width = 0.55) +
  geom_text(stat = "count", aes(label = sprintf("%s\n%.1f%%", comma(..count..),
             ..count.. / nrow(df %>% filter(!is.na(sex_label))) * 100)),
            vjust = -0.3, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("Homme" = COL_M, "Femme" = COL_F), guide = "none") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .12))) +
  labs(title = "Répartition par sexe", x = NULL, y = "Effectif") + THEME

fig_final <- (p_pyr | (p_sex / p_hh)) /
  (p_hist | p_rel | p_zone) +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title    = "TP1 — Profil Démographique des Ménages Nigérians · GHS Panel W4 (2018)",
    subtitle = "Auteurs : Cheikh M. M. Ndiaye & Awa Ba | ENSAE ISE 1 | 2025-2026",
    caption  = paste0("Filtre appliqué : membres présents (s1q4a ≠ NO) | N = 26 557 individus · 4 980 ménages\n",
                      "Tests : Shapiro-Wilk · Wilcoxon-Mann-Whitney · Binomial exact (Clopper-Pearson)"),
    theme = theme(plot.title    = element_text(face = "bold", size = 15),
                  plot.subtitle = element_text(size = 10, color = "grey35"),
                  plot.caption  = element_text(size = 8,  color = "grey50"))
  )
ggsave(file.path(OUT, "TP1_Figure_Synthese.png"), fig_final,
       w = 20, h = 18, dpi = 200, bg = "white")
message(sprintf("✓ Figure de synthèse → %s", file.path(OUT, "TP1_Figure_Synthese.png")))
message("\n✅ TP1 TERMINÉ")
