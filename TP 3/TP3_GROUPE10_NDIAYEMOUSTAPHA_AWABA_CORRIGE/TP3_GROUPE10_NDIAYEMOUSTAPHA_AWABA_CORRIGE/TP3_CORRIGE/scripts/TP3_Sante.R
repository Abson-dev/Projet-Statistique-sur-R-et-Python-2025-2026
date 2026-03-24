##############################################################################
#  TP3 — ACCÈS AUX SOINS ET DÉPENSES DE SANTÉ
#  Nigeria GHS Panel — Vague 4 (2018) | ENSAE ISE 1 | 2025-2026
#  Auteurs : Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba
##############################################################################
#
#  BASES UTILISÉES :
#    sect4a_harvestw4.dta  → santé individuelle
#    sect1_harvestw4.dta   → sexe, âge, secteur
#    totcons_final.dta     → consommation + poids wt_wave4
#
#  CORRECTIONS v2 :
#    - SCRIPT_DIR robuste (RStudio + Rscript + knitr)
#    - factor comparison via as.character() pour sec_label
#    - svyby rename robuste (grep sur nom colonne)
#    - protection division par zéro pour r_e
#    - annotate T18 positionné sur l'échelle log
#    - limits T17 ajustées à c(0,34) pour éviter coupure label
##############################################################################

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr, survey, purrr, viridis)
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

f_health <- file.path(DATA_DIR, "sect4a_harvestw4.dta")
f_sect1  <- file.path(DATA_DIR, "sect1_harvestw4.dta")
f_cons   <- file.path(DATA_DIR, "totcons_final.dta")
if (!all(file.exists(c(f_health, f_sect1, f_cons))))
  stop("Fichiers introuvables. Verifier data/NGA_2018_GHSP-W4_v03_M_Stata12/")
message("Fichiers trouves\n")

COL_M <- "#1B6CA8"; COL_F <- "#C0392B"; COL_R <- "#27AE60"; COL_U <- "#E67E22"
COLS_ILL <- c("Infectieuse"="#E74C3C","Respiratoire"="#3498DB",
              "Traumatique"="#E67E22","Chronique"="#8E44AD","Autre"="#95A5A6")
BLUE_ENSAE <- "#003E74"

THEME <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face="bold", size=14, margin=margin(b=6)),
        plot.subtitle = element_text(size=10, color="grey40", margin=margin(b=8)),
        plot.caption  = element_text(size=8, color="grey55", hjust=0),
        axis.title    = element_text(size=10, face="bold"),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(color="grey92"),
        plot.background   = element_rect(fill="white", color=NA),
        panel.background  = element_rect(fill="white", color=NA),
        strip.text        = element_text(face="bold"))

# ── CHARGEMENT ────────────────────────────────────────────────────────────────
message("Chargement des donnees...")

df_s1 <- read_dta(f_sect1) %>%
  mutate(s1q4a_num = as.integer(str_extract(as.character(s1q4a), "^\\d+"))) %>%
  filter(s1q4a_num != 2 | is.na(s1q4a_num)) %>%
  mutate(sex       = as.integer(str_extract(as.character(s1q2), "^\\d+")),
         sex_label = factor(case_when(sex==1~"Homme", sex==2~"Femme")),
         age       = as.numeric(s1q4),
         age       = if_else(age > 110 | age < 0, NA_real_, age),
         sec_num   = as.integer(str_extract(as.character(sector), "^\\d+")),
         sec_label = factor(case_when(sec_num==1~"Urbain", sec_num==2~"Rural"),
                            levels = c("Rural","Urbain")))

# totcons contient wt_wave4
df_cons <- read_dta(f_cons) %>%
  select(hhid, totcons_pc, wt_wave4) %>%
  mutate(quintile    = ntile(totcons_pc, 5),
         quint_label = paste0("Q", quintile))

df_h_raw <- read_dta(f_health)
df_h <- df_h_raw %>%
  left_join(df_s1 %>% select(hhid, indiv, sex_label, age, sec_label, sec_num),
            by = c("hhid","indiv")) %>%
  left_join(df_cons, by = "hhid") %>%
  mutate(
    ill_s4aq3   = str_detect(as.character(s4aq3), "^1"),
    cons_reason = as.integer(str_extract(as.character(s4aq2a), "^\\d+")),
    malade      = case_when(
      ill_s4aq3 ~ TRUE,
      !is.na(cons_reason) & cons_reason %in% c(1L, 2L) ~ TRUE,
      TRUE ~ FALSE),
    consulted     = str_detect(as.character(s4aq1), "^1"),
    dep_consult   = coalesce(suppressWarnings(as.numeric(s4aq9)), 0),
    dep_transport = replace_na(as.numeric(str_extract(as.character(s4aq10), "^\\d+\\.?\\d*")), 0),
    dep_meds      = replace_na(as.numeric(str_extract(as.character(s4aq14), "^\\d+\\.?\\d*")), 0),
    dep_hosp      = replace_na(as.numeric(str_extract(as.character(s4aq17), "^\\d+\\.?\\d*")), 0),
    dep_total     = dep_consult + dep_transport + dep_meds + dep_hosp,
    age_grp       = cut(age, breaks = c(0, 5, 15, 30, 45, 60, Inf),
                        labels = c("0-4","5-14","15-29","30-44","45-59","60+"),
                        right = FALSE, include.lowest = TRUE)
  )

n_sans_poids <- sum(is.na(df_h$wt_wave4))
message(sprintf("sect4a : %d individus | %d avec poids wt_wave4",
                nrow(df_h), nrow(df_h) - n_sans_poids))

# Seuil outliers depenses (IQR x3)
dep_pos_all <- df_h %>% filter(dep_total > 0) %>% pull(dep_total)
q3_d <- quantile(dep_pos_all, .75)
q1_d <- quantile(dep_pos_all, .25)
seuil_out <- q3_d + 3 * (q3_d - q1_d)

# Design survey
svy_h <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4,
                   data = df_h %>% filter(!is.na(wt_wave4)))

# ── TACHE 13 — MORBIDITE (PONDEREE) ──────────────────────────────────────────
message("\n=== Tache 13 : Morbidite (ponderee) ===")

morb_w_sv   <- svymean(~malade, svy_h, na.rm = TRUE)
morb_tot_w  <- as.numeric(coef(morb_w_sv)["maladeTRUE"]) * 100
message(sprintf("Taux de morbidite pondere : %.1f%%", morb_tot_w))

# Par sexe — IC survey
morb_sex_svy <- svyby(~malade, ~sex_label, svy_h, svymean, na.rm = TRUE, vartype = "ci")
# CORRECTION : rename robuste (cherche la colonne contenant "TRUE")
col_pct_s  <- grep("TRUE",  names(morb_sex_svy), value = TRUE)[1]
col_lo_s   <- grep("ci_l",  names(morb_sex_svy), value = TRUE)[1]
col_hi_s   <- grep("ci_u",  names(morb_sex_svy), value = TRUE)[1]
morb_sex <- morb_sex_svy %>%
  rename(pct = all_of(col_pct_s), pct_lo = all_of(col_lo_s), pct_hi = all_of(col_hi_s)) %>%
  mutate(across(c(pct, pct_lo, pct_hi), ~ . * 100)) %>%
  left_join(df_h %>% filter(!is.na(sex_label)) %>%
              group_by(sex_label) %>%
              summarise(N = n(), n_ill = sum(malade), .groups = "drop"),
            by = "sex_label")

# Par groupe d'age
morb_age_svy <- svyby(~malade, ~age_grp,
                      svy_h %>% subset(!is.na(age_grp)),
                      svymean, na.rm = TRUE, vartype = "ci")
col_pct_a <- grep("TRUE", names(morb_age_svy), value = TRUE)[1]
col_lo_a  <- grep("ci_l", names(morb_age_svy), value = TRUE)[1]
col_hi_a  <- grep("ci_u", names(morb_age_svy), value = TRUE)[1]
morb_age <- morb_age_svy %>%
  rename(pct = all_of(col_pct_a), pct_lo = all_of(col_lo_a), pct_hi = all_of(col_hi_a)) %>%
  mutate(across(c(pct, pct_lo, pct_hi), ~ . * 100)) %>%
  left_join(df_h %>% filter(!is.na(age_grp)) %>%
              group_by(age_grp) %>%
              summarise(N = n(), n_ill = sum(malade), .groups = "drop"),
            by = "age_grp")

p_morb_sex <- ggplot(morb_sex, aes(x = sex_label, y = pct, fill = sex_label)) +
  geom_col(alpha = 0.87, width = 0.52) +
  geom_errorbar(aes(ymin = pct_lo, ymax = pct_hi),
                width = 0.14, linewidth = 1, color = "grey20") +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", pct, comma(n_ill))),
            vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("Homme"=COL_M, "Femme"=COL_F), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 35)) +
  labs(title    = "Taux de morbidite par sexe — W4 (2018) · Pondere",
       subtitle = "IC 95% survey (linearisation Taylor) | wt_wave4",
       x = "Sexe", y = "Taux de morbidite pondere (%)",
       caption  = "Source : GHS Panel W4 | s4aq3 + s4aq2a | wt_wave4") + THEME

p_morb_age <- ggplot(morb_age, aes(x = age_grp, y = pct, fill = pct)) +
  geom_col(alpha = 0.87) +
  geom_errorbar(aes(ymin = pct_lo, ymax = pct_hi),
                width = 0.2, linewidth = 1, color = "grey20") +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.8, size = 3.3, fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 65)) +
  labs(title    = "Morbidite par groupe d'age — W4 · Pondere",
       subtitle = "IC 95% survey | Nette augmentation apres 45 ans",
       x = "Groupe d'age", y = "Taux pondere (%)",
       caption  = "Source : GHS Panel W4 | wt_wave4") + THEME

p_morb_comb <- (p_morb_sex | p_morb_age) +
  plot_annotation(title = sprintf(
    "Tache 13 — Morbidite · W4 (2018) · Taux global pondere = %.1f%%", morb_tot_w))
ggsave(file.path(OUT, "T13_morbidite.png"), p_morb_comb, w=14, h=6, dpi=200, bg="white")
print(p_morb_comb)
message("  -> T13_morbidite.png")

# ── TACHE 14 — TYPES DE MALADIES ─────────────────────────────────────────────
message("\n=== Tache 14 : Types de maladies ===")
ill_cat_fn <- function(v) {
  code <- suppressWarnings(as.integer(str_extract(v, "^\\d+")))
  case_when(
    code %in% c(1:8)           ~ "Infectieuse",
    code %in% c(9,10,15,16,17) ~ "Respiratoire",
    code == 11                 ~ "Traumatique",
    code %in% 20:27            ~ "Chronique",
    !is.na(code)               ~ "Autre",
    TRUE                       ~ NA_character_)
}
ill_df <- df_h %>%
  filter(!is.na(s4aq3b_1)) %>%
  mutate(ill_name = str_squish(str_remove(as.character(s4aq3b_1), "^\\d+\\.?\\s*")),
         ill_cat  = ill_cat_fn(as.character(s4aq3b_1))) %>%
  filter(ill_name != "", ill_name != "NA") %>%
  count(ill_name, ill_cat) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  mutate(ill_name = fct_reorder(ill_name, n))

p_ill <- ggplot(ill_df, aes(x = n, y = ill_name, fill = ill_cat)) +
  geom_col(alpha = 0.87, width = 0.72) +
  geom_text(aes(label = sprintf("%s  (%.1f%%)", comma(n), n/sum(n)*100)),
            hjust = -0.05, size = 3.4, fontface = "bold") +
  scale_fill_manual(values = COLS_ILL, name = "Categorie", na.value = "#95A5A6") +
  scale_x_continuous(expand = expansion(mult = c(0, .20)), labels = comma) +
  labs(title    = "Top 10 des maladies/blessures declarees — W4 (2018)",
       subtitle = "s4aq3b_1 | Paludisme = 1ere cause | Colore par categorie",
       x = "Nombre de cas", y = NULL,
       caption  = "Source : GHS Panel W4 | sect4a_harvestw4") +
  THEME + theme(legend.position = "right")
ggsave(file.path(OUT, "T14_types_maladies.png"), p_ill, w=11, h=7, dpi=200, bg="white")
print(p_ill)
message("  -> T14_types_maladies.png")

# ── TACHE 15 — RECOURS AUX SOINS (PONDERE) ───────────────────────────────────
message("\n=== Tache 15 : Recours aux soins (pondere) ===")
prov_map <- c("0"="Aucun soignant","1"="Guerisseur trad.","2"="Medecin",
              "3"="Dentiste","4"="Infirmier(e)","5"="Assist. medical",
              "6"="Sage-femme","7"="Pharmacist","8"="Chemist/Pharmacien",
              "9"="Accoucheuse","10"="Benevole sante","11"="Autre","15"="Agent CHEW")

prov_df <- df_h %>%
  filter(!is.na(s4aq6a), !is.na(wt_wave4)) %>%
  mutate(prov_code_s = str_extract(as.character(s4aq6a), "^\\d+"),
         prov_n      = coalesce(prov_map[prov_code_s], paste0("Code ", prov_code_s))) %>%
  group_by(prov_n) %>%
  summarise(n = n(), n_pond = sum(wt_wave4), .groups = "drop") %>%
  mutate(pct    = n_pond / sum(n_pond) * 100,
         prov_n = fct_reorder(prov_n, n_pond))

p_prov <- ggplot(prov_df, aes(x = pct, y = prov_n, fill = fct_rev(prov_n))) +
  geom_col(alpha = 0.87, width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%  (%s)", pct, comma(n))),
            hjust = -0.05, size = 3.4, fontface = "bold") +
  scale_fill_viridis_d(option = "turbo", direction = -1, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand  = expansion(mult = c(0, .28))) +
  labs(title    = "Recours aux soins par type de prestataire — W4 (2018) · Pondere",
       subtitle = "Proportions ponderees (wt_wave4) | s4aq6a",
       x = "Proportion ponderee (%)", y = NULL,
       caption  = "Source : GHS Panel W4 | sect4a_harvestw4 | wt_wave4") + THEME
ggsave(file.path(OUT, "T15_recours_soins.png"), p_prov, w=11, h=7, dpi=200, bg="white")
print(p_prov)
message("  -> T15_recours_soins.png")

# ── TACHE 16 — DEPENSES SANTE (PONDEREES) ────────────────────────────────────
message("\n=== Tache 16 : Depenses sante (ponderees) ===")
dep_pos   <- df_h %>% filter(dep_total > 0, !is.na(wt_wave4))
dep_moy_w <- weighted.mean(dep_pos$dep_total, dep_pos$wt_wave4)
dep_med_br <- median(dep_pos$dep_total)
cat(sprintf("Depense moy. ponderee : %s NGN | Mediane brute : %s NGN\n",
            comma(round(dep_moy_w)), comma(dep_med_br)))

p_dep_hist <- ggplot(df_h %>% filter(dep_total > 0, dep_total < seuil_out),
                     aes(x = dep_total, weight = wt_wave4)) +
  geom_histogram(bins = 55, fill = COL_M, color = "white", alpha = 0.85) +
  scale_x_log10(labels = label_dollar(prefix = "NGN ", big.mark = " "),
                breaks  = c(10, 50, 200, 500, 2000, 5000, 20000)) +
  geom_vline(xintercept = dep_med_br, color = COL_F, linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = dep_med_br * 2.2, y = Inf, vjust = 1.6,
           label    = sprintf("Med. brute = %s NGN", comma(dep_med_br)),
           color = COL_F, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title    = "Distribution ponderee des depenses de sante — W4 (2018)",
       subtitle = sprintf("s4aq9+s4aq10+s4aq14+s4aq17 | wt_wave4 | Echelle log | Moy. pond.=%s NGN",
                          comma(round(dep_moy_w))),
       x = "Depenses totales (NGN, log scale)", y = "Effectif pondere",
       caption  = sprintf("N=%s individus avec dep>0 | Source : GHS Panel W4 | wt_wave4",
                          comma(nrow(dep_pos)))) + THEME
ggsave(file.path(OUT, "T16_depenses_sante.png"), p_dep_hist, w=10, h=6, dpi=200, bg="white")
print(p_dep_hist)
message("  -> T16_depenses_sante.png")

# ── TACHE 17 — RECOURS × QUINTILE (PONDERE) ──────────────────────────────────
message("\n=== Tache 17 : Recours x quintile (pondere) ===")
df_hq <- df_h %>%
  filter(!is.na(quintile), !is.na(consulted), !is.na(wt_wave4)) %>%
  mutate(consulted_f = factor(as.integer(consulted)))
svy_q  <- svydesign(ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4, data = df_hq)
test_q <- svychisq(~consulted_f + quint_label, svy_q, statistic = "Chisq")
ct     <- table(as.integer(df_hq$consulted), df_hq$quint_label)
V_q_b  <- sqrt(chisq.test(ct)$statistic / (sum(ct) * (min(nrow(ct), ncol(ct)) - 1)))

# CORRECTION : rename robuste sur svyby
q_svy_raw <- svyby(~consulted, ~quint_label, svy_q, svymean, na.rm = TRUE)
col_cons   <- grep("consulted", names(q_svy_raw), value = TRUE)
col_cons   <- col_cons[!grepl("se\\.|SE", col_cons)][1]
q_consult  <- q_svy_raw %>%
  rename(pct = all_of(col_cons)) %>%
  mutate(pct = pct * 100) %>%
  left_join(df_hq %>% group_by(quint_label) %>%
              summarise(N = n(), n_cons = sum(consulted), .groups = "drop"),
            by = "quint_label")
cat(sprintf("Chi2 survey=%.2f · p=%.2e · V Cramer=%.3f\n",
            test_q$statistic, test_q$p.value, V_q_b))

p_quint <- ggplot(q_consult, aes(x = quint_label, y = pct, fill = quint_label)) +
  geom_col(alpha = 0.87, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.5, size = 3.8, fontface = "bold") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 34)) +
  annotate("label", x = 3, y = 29,
           label = sprintf("Chi2 survey=%.1f\np<0.001\nV Cramer=%.3f",
                           test_q$statistic, V_q_b),
           size = 3.3, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Taux de consultation x quintile — W4 (2018) · Pondere",
       subtitle = "Q1=Tres pauvres → Q5=Riches | Gradient = inegalites d'acces | wt_wave4",
       x = "Quintile de consommation", y = "Taux de consultation pondere (%)",
       caption  = "Source : GHS Panel W4 | sect4a x totcons_final | wt_wave4") + THEME
ggsave(file.path(OUT, "T17_recours_quintile.png"), p_quint, w=10, h=7, dpi=200, bg="white")
print(p_quint)
message("  -> T17_recours_quintile.png")

# ── TACHE 18 — DEPENSES RURAL vs URBAIN (PONDEREE) ───────────────────────────
message("\n=== Tache 18 : Depenses Rural vs Urbain (ponderee) ===")
dep_sec <- df_h %>%
  filter(dep_total > 0, dep_total < seuil_out, !is.na(sec_label), !is.na(wt_wave4))
wx_e <- wilcox.test(dep_total ~ sec_label, data = dep_sec, exact = FALSE)

# CORRECTION : as.character() pour factor comparison
n1  <- sum(as.character(dep_sec$sec_label) == "Rural")
n2  <- sum(as.character(dep_sec$sec_label) == "Urbain")
# CORRECTION : protection division par zero
r_e <- if (n1 > 0 && n2 > 0) as.numeric(wx_e$statistic) / (n1 * n2) else NA_real_

med_sec <- dep_sec %>%
  group_by(sec_label) %>%
  summarise(med   = median(dep_total),
            moy   = round(mean(dep_total)),
            moy_w = round(weighted.mean(dep_total, wt_wave4, na.rm = TRUE)),
            .groups = "drop")
cat(sprintf("Wilcoxon W=%.0f · p=%.4f · r=%.3f\n", wx_e$statistic, wx_e$p.value, r_e))
print(med_sec)

# CORRECTION : annotation positionnée sur l'échelle log (exp(mean(log(range))))
y_annot <- exp(mean(log(range(dep_sec$dep_total, na.rm = TRUE)))) * 5

p_viol <- ggplot(dep_sec, aes(x = sec_label, y = dep_total, fill = sec_label)) +
  geom_violin(alpha = 0.70, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.11, fill = "white", color = "grey20",
               outlier.shape = NA, linewidth = 0.9) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 5, fill = "white") +
  stat_summary(aes(weight = wt_wave4), fun = weighted.mean,
               geom = "point", shape = 8, size = 5, color = "#E67E22") +
  geom_text(data = med_sec,
            aes(x = sec_label, y = med, label = sprintf("Med\n%s NGN", comma(med))),
            vjust = -1.3, size = 3.4, fontface = "bold") +
  scale_fill_manual(values = c("Rural"=COL_R, "Urbain"=COL_U), guide = "none") +
  scale_y_log10(labels = label_dollar(prefix = "NGN ", big.mark = " "),
                breaks  = c(10, 50, 200, 500, 2000, 5000, 20000)) +
  annotate("label", x = 1.5, y = y_annot,
           label = sprintf("Wilcoxon\nW=%s\np=%.4f\nr=%.3f",
                           comma(wx_e$statistic), wx_e$p.value, r_e),
           size = 3.3, hjust = 0.5, fill = "white", label.size = 0.3) +
  labs(title    = "Depenses sante : Rural vs Urbain — W4 (2018) · Ponderee",
       subtitle = "Violin + boxplot | Mediane + Moy. pond. (etoile) | Echelle log",
       x = "Milieu", y = "Depenses totales (NGN, log scale)",
       caption  = sprintf("Wilcoxon W=%s · p=%.4f · r=%.3f\nSource : GHS Panel W4 | wt_wave4",
                          comma(wx_e$statistic), wx_e$p.value, r_e)) + THEME
ggsave(file.path(OUT, "T18_depenses_rural_urbain.png"), p_viol, w=9, h=8, dpi=200, bg="white")
print(p_viol)
message("  -> T18_depenses_rural_urbain.png")

# ── TABLEAU GTSUMMARY PONDERE ─────────────────────────────────────────────────
message("\n=== Tableau gtsummary pondere ===")
svy_tbl <- svydesign(
  ids = ~hhid, strata = ~sec_num, weights = ~wt_wave4,
  data = df_h %>%
    filter(!is.na(sec_label), !is.na(wt_wave4)) %>%
    mutate(`Malade`            = factor(as.integer(malade), 0:1, c("Non","Oui")),
           Consultation        = factor(as.integer(consulted), 0:1, c("Non","Oui")),
           `Dep. sante (NGN)`  = if_else(dep_total > 0, dep_total, NA_real_),
           Milieu = sec_label, Sexe = sex_label, Age = age))
print(tbl_svysummary(svy_tbl,
  include   = c(Sexe, Age, Malade, Consultation, `Dep. sante (NGN)`),
  by        = Milieu,
  statistic = list(all_continuous()  ~ "{median} [{p25}-{p75}]",
                   all_categorical() ~ "{n_unweighted} ({p}%)")
) %>%
  add_p(test = list(all_categorical() ~ "svy.chisq.test",
                    all_continuous()  ~ "svy.wilcox.test")) %>%
  add_overall() %>% bold_labels() %>%
  modify_caption("**Tableau 3. Indicateurs sante ponderes par milieu — W4 (2018)**") %>%
  modify_footnote(everything() ~ "Analyses ponderees wt_wave4 | Dep. = s4aq9+s4aq10+s4aq14+s4aq17"))

# ── FIGURE DE SYNTHESE ────────────────────────────────────────────────────────
message("\n=== Figure de synthese TP3 ===")
fig_tp3 <- (p_morb_sex | p_morb_age) /
           (p_ill       | p_prov)     /
           (p_viol      | p_quint)    +
  plot_annotation(
    title    = "TP3 — Acces aux Soins & Depenses de Sante · GHS Panel W4 (2018) · Pondere",
    subtitle = "Auteurs : Cheikh M. M. Ndiaye & Awa Ba | ENSAE ISE 1 | 2025-2026 | wt_wave4",
    caption  = paste0("Bases : sect4a_harvestw4 + sect1 + totcons_final | wt_wave4\n",
                      "Morbidite ponderee : s4aq3 + s4aq2a (1=maladie,2=blessure) | ",
                      "Depenses : s4aq9+s4aq10+s4aq14+s4aq17"),
    theme = theme(plot.title    = element_text(face="bold", size=15),
                  plot.subtitle = element_text(size=10, color="grey35"),
                  plot.caption  = element_text(size=8,  color="grey50"))
  )
ggsave(file.path(OUT, "TP3_Figure_Synthese.png"), fig_tp3, w=20, h=22, dpi=200, bg="white")
message(sprintf("Figure de synthese -> %s", file.path(OUT, "TP3_Figure_Synthese.png")))
message("\nTP3 TERMINE — analyses ponderees (wt_wave4)")
