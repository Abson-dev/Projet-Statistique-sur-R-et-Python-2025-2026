# ============================================================== #
#   TP2 — ÉDUCATION ET ALPHABÉTISATION DES MEMBRES               #
#   Nigeria GHS Panel — Wave 4 (2018)                            #
# ============================================================== #

# ── 0. PACKAGES ────────────────────────────────────────────────
packages <- c("haven", "dplyr", "ggplot2", "forcats", "rstatix",
              "ggpubr", "gtsummary", "viridis", "patchwork",
              "scales", "tidyr", "stringr", "moments", "gt")

installed  <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0)
  install.packages(to_install, dependencies = TRUE,
                   repos = "https://cran.rstudio.com/")
invisible(lapply(packages, library, character.only = TRUE))

# ── Dossiers output ───────────────────────────────────────────
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# ── Palette et thème ──────────────────────────────────────────
pal_educ <- c(
  "Aucun"            = "#E17055",
  "Primaire"         = "#FDCB6E",
  "Junior Secondary" = "#00CEC9",
  "Senior Secondary" = "#0984E3",
  "Tertiaire"        = "#6C5CE7"
)

theme_moderne <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16,
                                    color = "#2D3436",
                                    margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 12, color = "#636E72",
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "#B2BEC3",
                                    hjust = 1),
    axis.title       = element_text(face = "bold", size = 11,
                                    color = "#2D3436"),
    axis.text        = element_text(size = 10, color = "#636E72"),
    panel.grid.major = element_line(color = "#F0F0F0",
                                    linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    strip.text       = element_text(face = "bold", size = 11),
    plot.margin      = margin(15, 15, 10, 15)
  )
theme_set(theme_moderne)

# ══════════════════════════════════════════════════════════════ #
#  TÂCHE 7 — CHARGEMENT ET JOINTURE                             #
# ══════════════════════════════════════════════════════════════ #

sect2 <- read_dta("data/raw/sect2_harvestw4.dta")
sect1 <- read_dta("data/raw/sect1_harvestw4.dta")

sect1_reduit <- sect1 %>%
  select(
    hhid, indiv,
    sexe    = s1q2,
    age     = s1q4,
    zone_ru = sector,
    etat    = state
  )

data_educ <- sect2 %>%
  left_join(sect1_reduit, by = c("hhid", "indiv"))

# Valeurs manquantes — export
miss_educ <- data_educ %>%
  select(s2aq5, s2aq6, s2aq9, s2aq10, s2aq13a) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "Variable",
               values_to = "N_manquants") %>%
  mutate(Pct = round(N_manquants / nrow(data_educ) * 100, 2))

write.csv(miss_educ,
          "output/tables/T7_valeurs_manquantes.csv",
          row.names = FALSE)

# ══════════════════════════════════════════════════════════════ #
#  TÂCHE 8 — NIVEAU D'ÉDUCATION                                 #
# ══════════════════════════════════════════════════════════════ #

data_educ <- data_educ %>%
  mutate(
    niveau_educ = case_when(
      s2aq9 == 0                        ~ "Aucun",
      s2aq9 %in% c(1, 2, 3)             ~ "Aucun",
      s2aq9 %in% c(51, 52, 61)          ~ "Aucun",
      s2aq9 %in% c(11,12,13,14,15,16)   ~ "Primaire",
      s2aq9 %in% c(21, 22, 23)          ~ "Junior Secondary",
      s2aq9 %in% c(24,25,26,27,28)      ~ "Senior Secondary",
      s2aq9 == 321                       ~ "Senior Secondary",
      s2aq9 %in% c(31,33,34,35)         ~ "Tertiaire",
      s2aq9 %in% c(41, 43)              ~ "Tertiaire",
      s2aq9 %in% c(322, 411, 412)       ~ "Tertiaire",
      s2aq9 %in% c(421,422,423,424)     ~ "Tertiaire",
      TRUE                               ~ NA_character_
    ),
    niveau_educ = factor(niveau_educ,
                         levels  = c("Aucun", "Primaire", "Junior Secondary",
                                     "Senior Secondary", "Tertiaire"),
                         ordered = TRUE),
    educ_num = as.numeric(niveau_educ)
  )

# Fréquences — export
freq_educ <- data_educ %>%
  filter(!is.na(niveau_educ)) %>%
  count(niveau_educ, name = "n") %>%
  mutate(
    prop      = n / sum(n),
    pct_label = paste0(round(prop * 100, 1), "%\n(n=",
                       format(n, big.mark = " "), ")")
  )

write.csv(select(freq_educ, niveau_educ, n, prop),
          "output/tables/T8_freq_niveau_education.csv",
          row.names = FALSE)

# Barplot horizontal
p_educ_freq <- ggplot(freq_educ,
                      aes(x = fct_rev(niveau_educ), y = prop * 100,
                          fill = niveau_educ)) +
  geom_col(width = 0.72, alpha = 0.93, show.legend = FALSE) +
  geom_text(aes(label = pct_label),
            hjust = -0.08, size = 3.8,
            fontface = "bold", color = "#2D3436") +
  scale_fill_manual(values = pal_educ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(freq_educ$prop * 100) * 1.40),
    expand = c(0, 0)
  ) +
  coord_flip() +
  labs(
    title    = "Distribution du niveau d'éducation atteint",
    subtitle = paste0("Nigeria GHS Panel — Wave 4 (2018) | N = ",
                      format(sum(freq_educ$n), big.mark = " ")),
    x = NULL, y = "Proportion (%)",
    caption = "Source : World Bank LSMS-ISA | sect2_harvestw4"
  ) +
  theme(panel.grid.major.y = element_blank())

ggsave("output/figures/T8_niveau_education.png",
       p_educ_freq, width = 11, height = 6, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════ #
#  TÂCHE 9 — ÉDUCATION PAR SEXE (ADULTES 18+)                  #
# ══════════════════════════════════════════════════════════════ #

adultes <- data_educ %>%
  filter(!is.na(age), age >= 18,
         !is.na(niveau_educ), !is.na(sexe)) %>%
  mutate(
    sexe_label = factor(sexe,
                        levels = c(1, 2),
                        labels = c("Hommes", "Femmes"))
  )

# Tableau de contingence
tab_sexe_educ <- table(adultes$sexe_label, adultes$niveau_educ)
tab_sexe_educ <- tab_sexe_educ[, colSums(tab_sexe_educ) > 0,
                               drop = FALSE]

write.csv(as.data.frame.matrix(addmargins(tab_sexe_educ)),
          "output/tables/T9_contingence_sexe_education.csv")

# Test chi-deux / Fisher
chi2_test  <- suppressWarnings(chisq.test(tab_sexe_educ))
min_expect <- min(chi2_test$expected)

if (min_expect < 5) {
  fisher_t  <- fisher.test(tab_sexe_educ,
                           simulate.p.value = TRUE, B = 10000)
  p_val_t9  <- fisher_t$p.value
  test_nom9 <- "Fisher exact"
} else {
  p_val_t9  <- chi2_test$p.value
  test_nom9 <- "Chi-deux"
}

# V de Cramér
n_chi    <- sum(tab_sexe_educ)
chi2_val <- suppressWarnings(chisq.test(tab_sexe_educ)$statistic)
v_cramer <- ifelse(
  !is.nan(chi2_val) && !is.na(chi2_val) && chi2_val > 0,
  round(sqrt(chi2_val / (n_chi * (min(dim(tab_sexe_educ)) - 1))), 4),
  NA_real_
)

# Export résultats test
write.csv(data.frame(
  Test        = test_nom9,
  Statistique = round(chi2_val, 3),
  p_value     = round(p_val_t9, 6),
  V_Cramer    = v_cramer
),
"output/tables/T9_test_sexe_education.csv",
row.names = FALSE)

# Barplot 100% empilé
data_100 <- adultes %>%
  count(sexe_label, niveau_educ) %>%
  group_by(sexe_label) %>%
  mutate(prop  = n / sum(n),
         label = ifelse(prop > 0.03,
                        paste0(round(prop * 100, 0), "%"), "")) %>%
  ungroup()

annot_t9 <- paste0(
  test_nom9, " : p = ", format(p_val_t9, digits = 3),
  if (!is.na(v_cramer)) paste0(" | V Cramér = ", v_cramer) else "")

p_sexe_educ <- ggplot(data_100,
                      aes(x = sexe_label, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", width = 0.60, alpha = 0.93) +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            size = 3.8, fontface = "bold", color = "white") +
  scale_fill_manual(values = pal_educ, name = "Niveau d'éducation") +
  scale_y_continuous(labels = percent_format()) +
  annotate("text", x = 1.5, y = 1.06,
           label = annot_t9, size = 3.5,
           color = "#636E72", fontface = "italic", hjust = 0.5) +
  labs(
    title    = "Niveau d'éducation par sexe — Adultes 18+",
    subtitle = paste0("Barres 100% empilées | N = ",
                      format(nrow(adultes), big.mark = " "),
                      " | Nigeria GHS Panel Wave 4 (2018)"),
    x = NULL, y = "Proportion (%)",
    caption = "Source : World Bank LSMS-ISA"
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("output/figures/T9_education_sexe.png",
       p_sexe_educ, width = 10, height = 7, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════ #
#  TÂCHE 10 — ÂGE × NIVEAU D'ÉDUCATION                        #
# ══════════════════════════════════════════════════════════════ #

adultes_age <- adultes %>%
  filter(!is.na(age)) %>%
  mutate(
    groupe_age = cut(age,
                     breaks = c(18, 30, 45, 60, Inf),
                     labels = c("18-30 ans", "31-45 ans", "46-60 ans", "60+ ans"),
                     right  = FALSE)
  ) %>%
  filter(!is.na(groupe_age))

# Stats descriptives par groupe — export
stats_age_educ <- adultes_age %>%
  group_by(groupe_age) %>%
  summarise(
    N   = n(),
    Moy = round(mean(educ_num, na.rm = TRUE), 2),
    Méd = round(median(educ_num, na.rm = TRUE), 2),
    ET  = round(sd(educ_num, na.rm = TRUE), 2),
    .groups = "drop"
  )

write.csv(stats_age_educ,
          "output/tables/T10_stats_age_education.csv",
          row.names = FALSE)

# Kruskal-Wallis
eff_groupes <- adultes_age %>% count(groupe_age) %>% filter(n >= 2)
adultes_kw  <- adultes_age %>%
  filter(groupe_age %in% eff_groupes$groupe_age) %>%
  droplevels()

kw_test <- kruskal.test(educ_num ~ groupe_age, data = adultes_kw)

# Post-hoc Dunn si significatif
if (kw_test$p.value < 0.05 &&
    length(unique(adultes_kw$groupe_age)) >= 3) {
  dunn_res <- dunn_test(adultes_kw, educ_num ~ groupe_age,
                        p.adjust.method = "bonferroni")
  write.csv(select(dunn_res, group1, group2,
                   statistic, p.adj, p.adj.signif),
            "output/tables/T10_posthoc_dunn.csv",
            row.names = FALSE)
}

write.csv(data.frame(
  H       = round(kw_test$statistic, 3),
  ddl     = kw_test$parameter,
  p_value = round(kw_test$p.value, 6)
),
"output/tables/T10_kruskal_wallis.csv",
row.names = FALSE)

# Boxplot
couleurs_age <- c("18-30 ans" = "#4ECDC4", "31-45 ans" = "#45B7D1",
                  "46-60 ans" = "#FF8C42", "60+ ans"   = "#FF6B6B")

p_age_educ <- ggplot(adultes_age,
                     aes(x = groupe_age, y = educ_num, fill = groupe_age)) +
  geom_boxplot(width = 0.55, outlier.shape = 21,
               outlier.size = 2, outlier.alpha = 0.5,
               linewidth = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 4, color = "white", fill = "#2D3436") +
  scale_fill_manual(values = couleurs_age, guide = "none") +
  scale_y_continuous(
    breaks = 1:5,
    labels = c("Aucun","Primaire","Junior\nSec.",
               "Senior\nSec.","Tertiaire"),
    limits = c(0.5, 5.8)
  ) +
  annotate("text", x = 2, y = 5.6,
           label = paste0("Kruskal-Wallis H = ",
                          round(kw_test$statistic, 2),
                          " | p = ",
                          format(kw_test$p.value, digits = 3)),
           size = 3.8, color = "#636E72", fontface = "italic") +
  labs(
    title    = "Niveau d'éducation selon le groupe d'âge",
    subtitle = "Adultes 18+ | ◆ = Moyenne | Nigeria GHS Panel W4",
    x = "Groupe d'âge", y = "Niveau d'éducation",
    caption = "Source : World Bank LSMS-ISA"
  )

ggsave("output/figures/T10_age_education.png",
       p_age_educ, width = 11, height = 7, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════ #
#  TÂCHE 11 — SCOLARISATION 6-17 ANS : RURAL vs URBAIN         #
# ══════════════════════════════════════════════════════════════ #

enfants <- data_educ %>%
  filter(!is.na(age), age >= 6, age <= 17,
         !is.na(s2aq13a), !is.na(zone_ru)) %>%
  mutate(
    scolarise  = factor(s2aq13a,
                        levels = c(1, 2), labels = c("Scolarisé", "Non scolarisé")),
    zone_label = factor(zone_ru,
                        levels = c(1, 2), labels = c("Rural", "Urbain"))
  ) %>%
  filter(!is.na(scolarise), !is.na(zone_label))

# Tableau de contingence
tab_scol <- table(enfants$zone_label, enfants$scolarise)
write.csv(as.data.frame.matrix(addmargins(tab_scol)),
          "output/tables/T11_contingence_scolarisation_zone.csv")

# Test
chi2_scol <- suppressWarnings(chisq.test(tab_scol))
min_exp_s <- min(chi2_scol$expected)

if (min_exp_s < 5) {
  fish_s    <- fisher.test(tab_scol)
  p_scol    <- fish_s$p.value
  test_scol <- "Fisher exact"
} else {
  p_scol    <- chi2_scol$p.value
  test_scol <- "Chi-deux"
}

# Taux + IC 95%
taux_scol <- enfants %>%
  group_by(zone_label) %>%
  summarise(
    n_total = n(),
    n_scol  = sum(scolarise == "Scolarisé"),
    taux    = n_scol / n_total,
    ic_inf  = binom.test(n_scol, n_total)$conf.int[1],
    ic_sup  = binom.test(n_scol, n_total)$conf.int[2],
    .groups = "drop"
  )

write.csv(taux_scol,
          "output/tables/T11_taux_scolarisation_zone.csv",
          row.names = FALSE)

write.csv(data.frame(
  Test        = test_scol,
  Statistique = round(chi2_scol$statistic, 3),
  p_value     = round(p_scol, 6)
),
"output/tables/T11_test_scolarisation.csv",
row.names = FALSE)

# Barplot
p_scol <- ggplot(taux_scol,
                 aes(x = zone_label, y = taux * 100, fill = zone_label)) +
  geom_col(width = 0.55, alpha = 0.92, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ic_inf * 100, ymax = ic_sup * 100),
    width = 0.22, linewidth = 1.1, color = "#2D3436"
  ) +
  geom_text(
    aes(label = paste0(round(taux * 100, 1), "%")),
    vjust = -2.4, fontface = "bold", size = 5, color = "#2D3436"
  ) +
  scale_fill_manual(values = c("Rural" = "#96CEB4",
                               "Urbain" = "#45B7D1")) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 110), expand = c(0, 0)
  ) +
  annotate("text", x = 1.5, y = 103,
           label = paste0(test_scol, " : p = ",
                          format(p_scol, digits = 3)),
           size = 4, color = "#636E72", fontface = "italic") +
  labs(
    title    = "Taux de scolarisation des enfants 6-17 ans",
    subtitle = "Rural vs Urbain | Barres d'erreur = IC 95%\nNigeria GHS Panel Wave 4 (2018)",
    x = "Zone de résidence", y = "Taux de scolarisation (%)",
    caption = "Source : World Bank LSMS-ISA | sect2_harvestw4"
  )

ggsave("output/figures/T11_scolarisation_zone.png",
       p_scol, width = 9, height = 6, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════ #
#  TÂCHE 12 — PART D'ADULTES SANS INSTRUCTION PAR ÉTAT         #
# ══════════════════════════════════════════════════════════════ #

etats_nigeria <- c(
  "Abia","Adamawa","Akwa Ibom","Anambra","Bauchi",
  "Bayelsa","Benue","Borno","Cross River","Delta",
  "Ebonyi","Edo","Ekiti","Enugu","Gombe","Imo",
  "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kogi",
  "Kwara","Lagos","Nasarawa","Niger","Ogun","Ondo",
  "Osun","Oyo","Plateau","Rivers","Sokoto","Taraba",
  "Yobe","Zamfara","FCT"
)
state_names <- setNames(etats_nigeria, as.character(1:37))

adultes_etat <- adultes %>%
  filter(!is.na(etat), !is.na(niveau_educ)) %>%
  mutate(
    etat_nom = state_names[as.character(as.integer(etat))]
  ) %>%
  filter(!is.na(etat_nom))

heatmap_data <- adultes_etat %>%
  group_by(etat_nom) %>%
  summarise(
    n_total   = n(),
    n_aucun   = sum(niveau_educ == "Aucun"),
    pct_aucun = n_aucun / n_total * 100,
    .groups   = "drop"
  ) %>%
  filter(n_total >= 3) %>%
  arrange(desc(pct_aucun))

write.csv(heatmap_data,
          "output/tables/T12_sans_instruction_par_etat.csv",
          row.names = FALSE)

# Dot plot Cleveland
p_heatmap <- ggplot(heatmap_data,
                    aes(x    = pct_aucun,
                        y    = fct_reorder(etat_nom, pct_aucun),
                        fill = pct_aucun,
                        size = n_total)) +
  geom_point(shape = 21, alpha = 0.88) +
  geom_segment(
    aes(x = 0, xend = pct_aucun,
        y = fct_reorder(etat_nom, pct_aucun),
        yend = fct_reorder(etat_nom, pct_aucun)),
    color = "#E0E0E0", linewidth = 0.5
  ) +
  geom_text(aes(label = paste0(round(pct_aucun, 0), "%")),
            hjust = -0.5, size = 3.2,
            fontface = "bold", color = "#2D3436") +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name = "% sans\ninstruction") +
  scale_size_continuous(range = c(3, 10), name = "N adultes") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(heatmap_data$pct_aucun) * 1.25),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Part d'adultes sans instruction par État",
    subtitle = paste0("Nigeria GHS Panel Wave 4 (2018) | ",
                      nrow(heatmap_data), " États représentés\n",
                      "Taille du point = Nombre d'adultes enquêtés"),
    x = "% sans instruction", y = NULL,
    caption = "Source : World Bank LSMS-ISA | sect2_harvestw4"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#F0F0F0",
                                      linewidth = 0.5),
    legend.key.width   = unit(1.0, "cm")
  )

ggsave("output/figures/T12_sans_instruction_par_etat.png",
       p_heatmap,
       width  = 13,
       height = max(7, nrow(heatmap_data) * 0.45),
       dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════ #
#  TABLEAU GTSUMMARY                                            #
# ══════════════════════════════════════════════════════════════ #

gt_data2 <- adultes %>%
  mutate(
    zone_label  = factor(zone_ru,
                         levels = c(1, 2), labels = c("Rural", "Urbain")),
    alphabetise = factor(s2aq5,
                         levels = c(1, 2), labels = c("Oui", "Non")),
    groupe_age_f = cut(age,
                       breaks = c(18, 30, 45, 60, Inf),
                       labels = c("18-30","31-45","46-60","60+"),
                       right  = FALSE)
  ) %>%
  select(zone_label, sexe_label, niveau_educ,
         age, alphabetise, groupe_age_f)

tableau_gt2 <- gt_data2 %>%
  tbl_summary(
    by    = zone_label,
    label = list(
      sexe_label   ~ "Sexe",
      niveau_educ  ~ "Niveau d'éducation",
      age          ~ "Âge (années)",
      alphabetise  ~ "Alphabétisé(e)",
      groupe_age_f ~ "Groupe d'âge"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption(
    "**Tableau 2.** Profil éducatif par zone — Nigeria GHS Panel Wave 4 (2018)"
  )

tableau_gt2 %>%
  as_gt() %>%
  gt::gtsave("output/tables/T_gtsummary_profil_educatif.html")

# ══════════════════════════════════════════════════════════════ #
#  FIGURE DE SYNTHÈSE                                           #
# ══════════════════════════════════════════════════════════════ #

p_synthese <- (p_educ_freq | p_sexe_educ) /
  (p_age_educ  | p_scol)      +
  plot_annotation(
    title    = "TP2 — Éducation et Alphabétisation",
    subtitle = "Nigeria GHS Panel — Wave 4 (2018)",
    caption  = "Source : World Bank LSMS-ISA | ENSAE ISE1 2025-2026",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 17,
                                   color = "#2D3436", hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "#636E72",
                                   hjust = 0.5)
    )
  )

ggsave("output/figures/T_synthese_complete.png",
       p_synthese, width = 18, height = 14, dpi = 300, bg = "white")