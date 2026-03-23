library(dplyr)
library(ggplot2)
library(scales)
library(naniar)
library(PropCIs)
library(gtsummary)
library(gt)
library(apyramid)
library(forcats)
library(survey)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

data        <- readRDS("data/processed/data_brute.rds")
data_indiv  <- readRDS("data/processed/data_indiv.rds")
base_menage <- readRDS("data/processed/base_menage.rds")
plan_indiv  <- readRDS("data/processed/plan_indiv.rds")
plan_menage <- readRDS("data/processed/plan_menage.rds")

# palette principale : bleu électrique pour Urbain, vert néon pour Rural
palette_secteur <- c("Urbain" = "#0077FF", "Rural" = "#00C853")

# couleurs vives pour les autres éléments
COL_ACCENT  <- "#FF6D00"   # orange vif — ligne moyenne / points
COL_FOND    <- "#F0F4FF"   # fond légèrement bleuté
COL_GRILLE  <- "#D0DBF5"   # grille pastel bleutée
COL_TITRE   <- "#1A1A2E"   # bleu nuit — titres
COL_SOUS    <- "#3D5A80"   # bleu acier — sous-titres
COL_CAPTION <- "#607D8B"   # gris bleuté — légende

# thème appliqué à tous les ggplot du script
theme_tp1 <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background    = element_rect(fill = COL_FOND, color = NA),
      panel.background   = element_rect(fill = COL_FOND, color = NA),
      panel.grid.major   = element_line(color = COL_GRILLE, linewidth = 0.45),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = base_size + 1,
                                        color = COL_TITRE, margin = margin(b = 4)),
      plot.subtitle      = element_text(color = COL_SOUS, size = base_size - 1,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(color = COL_CAPTION, size = base_size - 3,
                                        hjust = 0, margin = margin(t = 6)),
      axis.title         = element_text(color = COL_SOUS, size = base_size - 1),
      axis.text          = element_text(color = "#2C3E50"),
      legend.background  = element_rect(fill = COL_FOND, color = NA),
      legend.key         = element_rect(fill = COL_FOND, color = NA),
      strip.background   = element_rect(fill = "#D6E4F7", color = NA),
      strip.text         = element_text(face = "bold", color = COL_TITRE)
    )
}

# EXPLORATION DE LA STRUCTURE

cat("\n Structure du dataset \n")
cat("Dimensions :", nrow(data), "individus ×", ncol(data), "variables\n")
cat("Variables   :", paste(names(data)[1:15], collapse = ", "), "...\n")

# Résumé pondéré (estimations population)
data_resume_pond <- data.frame(
  n_individus  = nrow(data),
  n_menages    = n_distinct(data$hhid),
  pop_estimee  = round(sum(data$wt_wave4, na.rm = TRUE)),
  age_moyen    = round(svymean(~age, plan_indiv, na.rm = TRUE)[1], 1),
  age_median   = round(svyquantile(~age, plan_indiv, quantiles = 0.5,
                                   na.rm = TRUE)[[1]][1], 1),
  pct_masculin = round(svymean(~I(sexe == "Homme"), plan_indiv,
                               na.rm = TRUE)[1] * 100, 1),
  pct_urbain   = round(svymean(~I(secteur == "Urbain"), plan_indiv,
                               na.rm = TRUE)[1] * 100, 1)
)
cat("\nRésumé global pondéré (estimations population) :\n"); print(data_resume_pond)

# VÉRIFICATION DES DOUBLONS ET VALEURS MANQUANTES
cat("\n Qualité des données \n")

data$hhid_indiv <- paste(data$hhid, data$indiv, sep = "_")
doublons <- data[duplicated(data$hhid_indiv), ]
cat("Doublons hhid×indiv :", nrow(doublons), "\n")

vm_cles <- data %>%
  summarise(
    `Âge (s1q4)`   = sum(is.na(age)),
    `Sexe (s1q2)`  = sum(is.na(sexe)),
    `Secteur`      = sum(is.na(secteur)),
    `Lien parenté` = sum(is.na(lien_parente))
  )
cat("\nValeurs manquantes variables clés :\n"); print(vm_cles)

# vis_miss
png("outputs/fig00_vis_miss.png", width = 1400, height = 620, res = 150)
vis_miss(data %>% select(age, sexe, secteur, lien_parente, zone_geo),
         warn_large_data = FALSE) +
  labs(title    = "Valeurs manquantes — variables clés",
       subtitle = "sect1_harvestw4 | GHS-W4 Nigeria 2018") +
  theme_tp1() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()

# ANALYSE UNIVARIÉE DE L'ÂGE (pondérée)
cat("\n Analyse univariée de l'âge (résultats pondérés) \n")

age_stats_pond <- data.frame(
  Moyenne    = round(svymean(~age, plan_indiv, na.rm = TRUE)[1], 2),
  Mediane    = round(svyquantile(~age, plan_indiv, quantiles = 0.5,
                                 na.rm = TRUE)[[1]][1], 2),
  Ecart_type = round(sqrt(svyvar(~age, plan_indiv, na.rm = TRUE)[1]), 2),
  Q1         = round(svyquantile(~age, plan_indiv, quantiles = 0.25,
                                 na.rm = TRUE)[[1]][1], 2),
  Q3         = round(svyquantile(~age, plan_indiv, quantiles = 0.75,
                                 na.rm = TRUE)[[1]][1], 2),
  Min        = min(data$age, na.rm = TRUE),
  Max        = max(data$age, na.rm = TRUE)
)
cat("Statistiques descriptives pondérées de l'âge :\n"); print(age_stats_pond)
saveRDS(age_stats_pond, "data/processed/age_stats_pond.rds")

# histogramme pondéré — barres proportionnelles au poids
moy_age_pond <- age_stats_pond$Moyenne

# Création d'un data frame pondéré pour ggplot
data_age_pond <- data %>%
  filter(!is.na(age), !is.na(wt_wave4)) %>%
  select(age, wt_wave4)

p_hist <- ggplot(data_age_pond, aes(x = age, weight = wt_wave4)) +
  geom_histogram(binwidth = 5, fill = "#0077FF", color = COL_FOND,
                 alpha = 0.88, linewidth = 0.3) +
  geom_vline(xintercept = moy_age_pond,
             color = COL_ACCENT, linetype = "dashed", linewidth = 0.9) +
  annotate("label",
           x = moy_age_pond + 3, y = Inf,
           vjust = 1.4, hjust = 0,
           label = paste0("Moy. pond. = ", round(moy_age_pond, 1), " ans"),
           fill  = COL_FOND, color = "#BF360C",
           size  = 3.4, fontface = "bold", label.size = 0.2) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Distribution pondérée de l'âge des membres des ménages",
       subtitle = paste0("Population estimée = ",
                         scales::comma(round(sum(data$wt_wave4, na.rm=TRUE))),
                         " | Ligne pointillée = moyenne pondérée"),
       x        = "Âge (années)",
       y        = "Effectifs pondérés (population estimée)",
       caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19) | Poids : wt_wave4") +
  theme_tp1(base_size = 12)

ggsave("outputs/fig01_histogramme_age.png", p_hist,
       width = 10, height = 5.5, dpi = 150, bg = COL_FOND)
cat("  fig01_histogramme_age.png\n")

# boxplot de l'âge (non pondéré pour la structure, note de pondération ajoutée)
med_age_pond <- age_stats_pond$Mediane

p_box_age <- ggplot(data %>% filter(!is.na(age)), aes(x = "", y = age)) +
  geom_boxplot(fill = "#0077FF", color = "#003C8F",
               alpha = 0.72, width = 0.35,
               outlier.color = "#FF6D00", outlier.alpha = 0.5,
               outlier.size  = 0.7) +
  geom_hline(yintercept = med_age_pond,
             color = COL_ACCENT, linetype = "dashed", linewidth = 0.85) +
  annotate("label",
           x = 1.22, y = med_age_pond,
           label = paste0("Méd. pond. = ", med_age_pond, " ans"),
           fill  = COL_FOND, color = "#BF360C",
           size  = 3.3, fontface = "bold", label.size = 0.2) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  labs(title    = "Boîte à moustaches de l'âge",
       subtitle = paste0("Médiane pondérée = ", med_age_pond,
                         " ans | Distribution fortement asymétrique"),
       y        = "Âge (années)",
       x        = NULL,
       caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19) | Poids : wt_wave4") +
  theme_tp1(base_size = 12) +
  theme(axis.text.x         = element_blank(),
        panel.grid.major.x  = element_blank())

ggsave("outputs/fig02_boxplot_age.png", p_box_age,
       width = 6, height = 6.5, dpi = 150, bg = COL_FOND)
cat("  fig02_boxplot_age.png\n")

# test de normalité Shapiro-Wilk (échantillon de 5000)
set.seed(123)
ech <- sample(na.omit(data$age), size = 5000)
sw  <- shapiro.test(ech)
cat("\nTest de normalité Shapiro-Wilk (n=5000) :\n")
cat("  W =", round(sw$statistic, 5), "| p-value =", format(sw$p.value, scientific=TRUE), "\n")
cat("  Conclusion : Distribution",
    ifelse(sw$p.value < 0.05, "NON normale (p < 0.05)", "normale"), "\n")

# pyramide des âges pondérée
# On duplique les lignes selon les poids (arrondi) pour approcher la population
set.seed(42)
data_pyr <- data %>%
  filter(!is.na(age), !is.na(sexe), !is.na(grp_age), !is.na(wt_wave4)) %>%
  mutate(wt_int = pmax(1, round(wt_wave4 / 1000))) %>%  # normalisation pour éviter explosion mémoire
  slice(rep(seq_len(n()), times = wt_int))

p_pyramide <- data_pyr %>%
  age_pyramid(age_group = "grp_age",
              split_by   = "sexe",
              proportion = FALSE) +
  scale_fill_manual(values = c("Homme" = "#0077FF", "Femme" = "#00C853"),
                    name = "Sexe") +
  labs(title    = "Pyramide des âges pondérée (population estimée)",
       subtitle = "Nigeria – GHSP-Panel Wave 4 (2018/19) | Poids : wt_wave4",
       x        = "Effectifs pondérés",
       y        = "Groupe d'âge",
       caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
  theme_tp1(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title    = element_text(face = "bold"))

ggsave("outputs/fig03_pyramide_ages.png", p_pyramide,
       width = 9, height = 8.5, dpi = 150, bg = COL_FOND)
cat("  fig03_pyramide_ages.png\n")

# LIEN DE PARENTÉ — PROPORTIONS PONDÉRÉES AVEC IC 95%
cat("\n Lien de parenté (résultats pondérés) \n")

# couleurs vives par catégorie de parenté
pal_parente <- c(
  "Chef de ménage" = "#0077FF",
  "Conjoint(e)"    = "#00C853",
  "Enfant"         = "#FF6D00",
  "Autre"          = "#AA00FF"
)

# Proportions pondérées via le plan de sondage
prop_pond <- svymean(~lien_parente, plan_indiv, na.rm = TRUE)
prop_df   <- as.data.frame(prop_pond)
prop_df$lien_parente <- gsub("lien_parente", "", rownames(prop_df))

# Effectifs pondérés estimés
tot_pond <- svytotal(~lien_parente, plan_indiv, na.rm = TRUE)
tot_df   <- as.data.frame(tot_pond)

# IC 95% sur les proportions pondérées
ci_pond <- confint(prop_pond, level = 0.95)

resultat_final <- data.frame(
  Categorie  = prop_df$lien_parente,
  Effectif_pond = round(as.numeric(tot_df$total)),
  Proportion = round(prop_df$mean * 100, 2),
  IC_lower   = round(ci_pond[, 1] * 100, 2),
  IC_upper   = round(ci_pond[, 2] * 100, 2)
)
cat("\nProportions pondérées avec IC 95% :\n"); print(resultat_final)
saveRDS(resultat_final, "data/processed/proportions_IC.rds")

# graphique barres pondéré
data_parente_pond <- data %>%
  filter(!is.na(lien_parente), !is.na(wt_wave4))

p_parente <- ggplot(data_parente_pond,
                    aes(x = fct_rev(fct_reorder(lien_parente,
                                                wt_wave4, .fun = sum)),
                        fill = lien_parente,
                        weight = wt_wave4)) +
  geom_bar(alpha = 0.88, color = COL_FOND, linewidth = 0.3) +
  geom_text(stat  = "count",
            aes(label = scales::comma(round(after_stat(count)))),
            hjust = -0.15, size = 3.5, fontface = "bold",
            color = COL_TITRE) +
  scale_fill_manual(values = pal_parente, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = scales::comma) +
  coord_flip() +
  labs(title   = "Fréquence pondérée du lien de parenté avec le chef de ménage",
       subtitle = "Effectifs estimés dans la population | Poids : wt_wave4",
       x       = NULL,
       y       = "Effectifs pondérés (population estimée)",
       caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
  theme_tp1(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig04_lien_parente.png", p_parente,
       width = 9, height = 5, dpi = 150, bg = COL_FOND)
cat("  fig04_lien_parente.png\n")

# TAILLE DES MÉNAGES — TEST DE WILCOXON PONDÉRÉ
cat("\n Taille des ménages par secteur (résultats pondérés) \n")

# Statistiques pondérées par secteur
stats_menage <- data.frame(
  secteur = c("Urbain", "Rural"),
  N       = c(
    sum(base_menage$secteur == "Urbain", na.rm = TRUE),
    sum(base_menage$secteur == "Rural",  na.rm = TRUE)
  ),
  Moyenne = c(
    round(svymean(~taille,
                  subset(plan_menage, secteur == "Urbain"), na.rm=TRUE)[1], 2),
    round(svymean(~taille,
                  subset(plan_menage, secteur == "Rural"),  na.rm=TRUE)[1], 2)
  ),
  Mediane = c(
    round(svyquantile(~taille, subset(plan_menage, secteur == "Urbain"),
                      quantiles=0.5, na.rm=TRUE)[[1]][1], 2),
    round(svyquantile(~taille, subset(plan_menage, secteur == "Rural"),
                      quantiles=0.5, na.rm=TRUE)[[1]][1], 2)
  ),
  Q1 = c(
    round(svyquantile(~taille, subset(plan_menage, secteur == "Urbain"),
                      quantiles=0.25, na.rm=TRUE)[[1]][1], 2),
    round(svyquantile(~taille, subset(plan_menage, secteur == "Rural"),
                      quantiles=0.25, na.rm=TRUE)[[1]][1], 2)
  ),
  Q3 = c(
    round(svyquantile(~taille, subset(plan_menage, secteur == "Urbain"),
                      quantiles=0.75, na.rm=TRUE)[[1]][1], 2),
    round(svyquantile(~taille, subset(plan_menage, secteur == "Rural"),
                      quantiles=0.75, na.rm=TRUE)[[1]][1], 2)
  ),
  Ecart_type = c(
    round(sqrt(svyvar(~taille, subset(plan_menage, secteur == "Urbain"),
                      na.rm=TRUE)[1]), 2),
    round(sqrt(svyvar(~taille, subset(plan_menage, secteur == "Rural"),
                      na.rm=TRUE)[1]), 2)
  ),
  Min = c(
    min(base_menage$taille[base_menage$secteur == "Urbain"], na.rm=TRUE),
    min(base_menage$taille[base_menage$secteur == "Rural"],  na.rm=TRUE)
  ),
  Max = c(
    max(base_menage$taille[base_menage$secteur == "Urbain"], na.rm=TRUE),
    max(base_menage$taille[base_menage$secteur == "Rural"],  na.rm=TRUE)
  )
)
cat("\nStatistiques pondérées par secteur :\n"); print(stats_menage)

# test de Wilcoxon-Mann-Whitney (sur données avec poids comme fréquences approchées)
urbain     <- base_menage %>% filter(secteur == "Urbain", !is.na(wt_wave4)) %>% pull(taille)
rural      <- base_menage %>% filter(secteur == "Rural",  !is.na(wt_wave4)) %>% pull(taille)
wgt_u      <- base_menage %>% filter(secteur == "Urbain", !is.na(wt_wave4)) %>% pull(wt_wave4)
wgt_r      <- base_menage %>% filter(secteur == "Rural",  !is.na(wt_wave4)) %>% pull(wt_wave4)

# Wilcoxon pondéré via répétition des observations (poids normalisés)
wt_scale   <- min(c(wgt_u, wgt_r), na.rm=TRUE)
rep_u      <- pmax(1, round(wgt_u / wt_scale))
rep_r      <- pmax(1, round(wgt_r / wt_scale))
urbain_rep <- rep(urbain, times = rep_u)
rural_rep  <- rep(rural,  times = rep_r)
# Sous-échantillonnage pour test (max 50 000 obs. chacun)
set.seed(123)
if (length(urbain_rep) > 50000) urbain_rep <- sample(urbain_rep, 50000)
if (length(rural_rep)  > 50000) rural_rep  <- sample(rural_rep,  50000)
wilcox_res <- wilcox.test(urbain_rep, rural_rep, alternative = "two.sided",
                          conf.int = TRUE, conf.level = 0.95)

# taille d'effet r de rang
N_total        <- length(urbain_rep) + length(rural_rep)
Z              <- qnorm(wilcox_res$p.value / 2)
r_rang         <- abs(Z) / sqrt(N_total)
interpretation <- case_when(
  r_rang < 0.1 ~ "Négligeable",
  r_rang < 0.3 ~ "Petit",
  r_rang < 0.5 ~ "Moyen",
  TRUE         ~ "Grand"
)
cat("\nTest de Wilcoxon-Mann-Whitney (pondéré) :\n")
cat("  W =", wilcox_res$statistic, "| p =", format(wilcox_res$p.value, scientific=TRUE), "\n")
cat("  r de rang =", round(r_rang, 4), "(", interpretation, ")\n")
cat("  IC 95% : [", round(wilcox_res$conf.int[1],3), ";",
    round(wilcox_res$conf.int[2],3), "]\n")

# label du test pour annoter le graphique
label_wilcox <- paste0(
  "Wilcoxon  W = ", round(wilcox_res$statistic, 0),
  "\np = ", format(wilcox_res$p.value, scientific = TRUE, digits = 3),
  "\nr = ", round(r_rang, 3), " (", interpretation, ")"
)
moyennes <- stats_menage %>%
  mutate(secteur = factor(secteur, levels = c("Urbain","Rural"))) %>%
  select(secteur, moyenne = Moyenne)

# boxplot groupé pondéré
p_boxplot_menage <- ggplot(base_menage %>% filter(!is.na(secteur), !is.na(wt_wave4)),
                           aes(x = secteur, y = taille,
                               fill = secteur, weight = wt_wave4)) +
  geom_boxplot(alpha = 0.72,
               outlier.color = COL_ACCENT, outlier.size = 0.7,
               outlier.alpha = 0.55, width = 0.45,
               color = COL_TITRE) +
  geom_jitter(aes(weight = NULL),
              width = 0.14, alpha = 0.07, size = 0.55,
              color = COL_SOUS) +
  geom_point(data  = moyennes,
             aes(x = secteur, y = moyenne, weight = NULL),
             shape = 18, size = 4.5, color = COL_ACCENT) +
  geom_text(data  = moyennes,
            aes(x = secteur, y = moyenne,
                label = paste0("Moy. pond. = ", round(moyenne, 1)),
                weight = NULL),
            vjust = -0.9, color = "#BF360C",
            size  = 3.5, fontface = "bold") +
  annotate("label",
           x = 1.5, y = max(base_menage$taille, na.rm=TRUE) * 0.93,
           label = label_wilcox,
           fill  = COL_FOND, color = COL_SOUS,
           size  = 3.2, hjust = 0.5, fontface = "italic",
           label.size = 0.25) +
  scale_fill_manual(values = palette_secteur) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(
    title    = "Taille des ménages selon le secteur (Urbain vs Rural) — pondéré",
    subtitle = paste0("n = ", scales::comma(nrow(base_menage)),
                      " ménages  |  ◆ = Moyenne pondérée  |  Test Wilcoxon pondéré"),
    x        = NULL,
    y        = "Taille du ménage (nombre de membres)",
    caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19) | Poids : wt_wave4"
  ) +
  theme_tp1(base_size = 12) +
  theme(legend.position      = "none",
        panel.grid.major.x   = element_blank(),
        axis.text.x          = element_text(size = 12, face = "bold"))

ggsave("outputs/fig05_boxplot_menages.png", p_boxplot_menage,
       width = 10, height = 6.5, dpi = 150, bg = COL_FOND)
cat("  fig05_boxplot_menages.png\n")

saveRDS(list(stats     = stats_menage,
             wilcox    = wilcox_res,
             r_rang    = r_rang,
             interpret = interpretation),
        "data/processed/resultats_wilcoxon.rds")

# TABLEAU GTSUMMARY STRATIFIÉ PONDÉRÉ
cat("\n Tableau gtsummary stratifié pondéré \n")

data_tableau <- data_indiv %>%
  filter(!is.na(secteur), !is.na(wt_wave4)) %>%
  transmute(
    secteur       = secteur,
    age           = as.numeric(s1q4),
    sexe          = factor(as.numeric(s1q2), levels = c(1, 2),
                           labels = c("Masculin", "Féminin")),
    taille_menage = taille_menage,
    wt_wave4      = wt_wave4
  )

# Plan de sondage pour gtsummary (plan_tableau)
plan_tableau <- svydesign(
  ids     = ~1,
  weights = ~wt_wave4,
  data    = data_tableau
)

tableau_gt <- tbl_svysummary(
  data    = plan_tableau,
  by      = secteur,
  include = c(age, sexe, taille_menage),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})\nMéd. {median} [{p25} ; {p75}]",
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  label = list(
    age           ~ "Âge (années)",
    sexe          ~ "Sexe",
    taille_menage ~ "Taille du ménage (membres)"
  ),
  digits       = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
  missing      = "ifany",
  missing_text = "Valeurs manquantes"
) %>%
  add_overall(last = FALSE, col_label = "**Total**") %>%
  add_p(
    test = list(age ~ "svyranktest", sexe ~ "svychisq",
                taille_menage ~ "svyranktest"),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(
    label   ~ "**Variable**",
    stat_0  ~ "**Total**\nN = {N}",
    stat_1  ~ "**Urbain**\nN = {n}",
    stat_2  ~ "**Rural**\nN = {n}",
    p.value ~ "**p-valeur**"
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Secteur de résidence**") %>%
  modify_caption("**Tableau 1. Caractéristiques sociodémographiques pondérées par secteur**
   Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19) | Poids : wt_wave4") %>%
  modify_footnote(p.value ~ "Test de rang pondéré (svyranktest) ; Chi-deux pondéré (svychisq)") %>%
  bold_labels() %>%
  bold_p(t = 0.05)

tableau_gt %>%
  as_gt() %>%
  gt::gtsave("outputs/tableau_gtsummary.html")
cat("  tableau_gtsummary.html\n")

cat("\n Toutes les analyses pondérées terminées \n")
for (f in list.files("outputs/")) cat("  -", f, "\n")
