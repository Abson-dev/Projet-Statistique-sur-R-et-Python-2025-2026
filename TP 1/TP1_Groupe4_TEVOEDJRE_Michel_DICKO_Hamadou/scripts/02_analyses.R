library(dplyr)
library(ggplot2)
library(scales)
library(naniar)
library(PropCIs)
library(gtsummary)
library(gt)
library(apyramid)
library(forcats)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

data <- readRDS("data/processed/data_brute.rds")
data_indiv  <- readRDS("data/processed/data_indiv.rds")
base_menage <- readRDS("data/processed/base_menage.rds")

# palette principale : terracotta pour Urbain, vert kaki pour Rural
palette_secteur <- c("Urbain" = "#C0572B", "Rural" = "#5B7A52")

# couleurs supplémentaires utilisées dans plusieurs graphiques
COL_ACCENT <- "#E8A045"   
COL_FOND <- "#FAF7F2"   
COL_GRILLE <- "#E8E2D9"   

# thème appliqué à tous les ggplot du script
theme_tp1 <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background    = element_rect(fill = COL_FOND, color = NA),
      panel.background   = element_rect(fill = COL_FOND, color = NA),
      panel.grid.major   = element_line(color = COL_GRILLE, linewidth = 0.45),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = base_size + 1,
                                        color = "#2E2319", margin = margin(b = 4)),
      plot.subtitle      = element_text(color = "#6B5C4A", size = base_size - 1,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(color = "#9C8B7A", size = base_size - 3,
                                        hjust = 0, margin = margin(t = 6)),
      axis.title         = element_text(color = "#4A3C2F", size = base_size - 1),
      axis.text          = element_text(color = "#5C4D3E"),
      legend.background  = element_rect(fill = COL_FOND, color = NA),
      legend.key         = element_rect(fill = COL_FOND, color = NA),
      strip.background   = element_rect(fill = "#EDE6DC", color = NA),
      strip.text         = element_text(face = "bold", color = "#2E2319")
    )
}

# EXPLORATION DE LA STRUCTURE

cat("\n Structure du dataset \n")
cat("Dimensions :", nrow(data), "individus ×", ncol(data), "variables\n")
cat("Variables   :", paste(names(data)[1:15], collapse = ", "), "...\n")

data_resume <- data %>%
  summarise(
    n_individus  = n(),
    n_menages    = n_distinct(hhid),
    age_moyen    = round(mean(age, na.rm = TRUE), 1),
    age_median   = median(age, na.rm = TRUE),
    pct_masculin = round(mean(sexe == "Homme", na.rm = TRUE) * 100, 1),
    pct_urbain   = round(mean(secteur == "Urbain", na.rm = TRUE) * 100, 1)
  )
cat("\nRésumé global :\n"); print(data_resume)

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

# ANALYSE UNIVARIÉE DE L'ÂGE
cat("\n Analyse univariée de l'âge \n")

age_stats <- data %>%
  filter(!is.na(age)) %>%
  summarise(
    Moyenne    = round(mean(age), 2),
    Mediane    = median(age),
    Ecart_type = round(sd(age), 2),
    Min        = min(age),
    Max        = max(age),
    Q1         = quantile(age, .25),
    Q3         = quantile(age, .75)
  )
cat("Statistiques descriptives de l'âge :\n"); print(age_stats)

# histogramme
moy_age <- mean(data$age, na.rm = TRUE)

p_hist <- ggplot(data %>% filter(!is.na(age)), aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#C0572B", color = COL_FOND,
                 alpha = 0.88, linewidth = 0.3) +
  geom_vline(xintercept = moy_age,
             color = COL_ACCENT, linetype = "dashed", linewidth = 0.9) +
  annotate("label",
           x = moy_age + 3, y = Inf,
           vjust = 1.4, hjust = 0,
           label = paste0("Moy. = ", round(moy_age, 1), " ans"),
           fill  = COL_FOND, color = "#8B5E2A",
           size  = 3.4, fontface = "bold", label.size = 0.2) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Distribution de l'âge des membres des ménages",
       subtitle = paste0("N = ", scales::comma(sum(!is.na(data$age))),
                         " individus  |  Ligne pointillée = moyenne"),
       x        = "Âge (années)",
       y        = "Effectifs",
       caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
  theme_tp1(base_size = 12)

ggsave("outputs/fig01_histogramme_age.png", p_hist,
       width = 10, height = 5.5, dpi = 150, bg = COL_FOND)
cat("  fig01_histogramme_age.png\n")

# boxplot de l'âge
med_age <- median(data$age, na.rm = TRUE)

p_box_age <- ggplot(data %>% filter(!is.na(age)), aes(x = "", y = age)) +
  geom_boxplot(fill = "#C0572B", color = "#7A3118",
               alpha = 0.72, width = 0.35,
               outlier.color = "#B0927A", outlier.alpha = 0.4,
               outlier.size  = 0.7) +
  geom_hline(yintercept = med_age,
             color = COL_ACCENT, linetype = "dashed", linewidth = 0.85) +
  annotate("label",
           x = 1.22, y = med_age,
           label = paste0("Méd. = ", med_age, " ans"),
           fill  = COL_FOND, color = "#8B5E2A",
           size  = 3.3, fontface = "bold", label.size = 0.2) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  labs(title    = "Boîte à moustaches de l'âge",
       subtitle = paste0("Médiane = ", med_age, " ans | Distribution fortement asymétrique"),
       y        = "Âge (années)",
       x        = NULL,
       caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
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

# pyramide des âges
p_pyramide <- data %>%
  filter(!is.na(age), !is.na(sexe), !is.na(grp_age)) %>%
  age_pyramid(age_group = "grp_age",
              split_by   = "sexe",
              proportion = FALSE) +
  scale_fill_manual(values = c("Homme" = "#C0572B", "Femme" = "#5B7A52"),
                    name = "Sexe") +
  labs(title    = "Pyramide des âges des membres des ménages",
       subtitle = "Nigeria – GHSP-Panel Wave 4 (2018/19)",
       x        = "Effectifs",
       y        = "Groupe d'âge",
       caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
  theme_tp1(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title    = element_text(face = "bold"))

ggsave("outputs/fig03_pyramide_ages.png", p_pyramide,
       width = 9, height = 8.5, dpi = 150, bg = COL_FOND)
cat("  fig03_pyramide_ages.png\n")

# LIEN DE PARENTÉ ET PROPORTIONS AVEC IC 95%
cat("\n Lien de parenté \n")

# couleurs par catégorie de parenté
pal_parente <- c(
  "Chef de ménage" = "#C0572B",
  "Conjoint(e)"    = "#5B7A52",
  "Enfant"         = "#E8A045",
  "Autre"          = "#9C8070"
)

p_parente <- ggplot(data,
                    aes(x = fct_rev(fct_infreq(lien_parente)),
                        fill = lien_parente)) +
  geom_bar(alpha = 0.88, color = COL_FOND, linewidth = 0.3) +
  geom_text(stat  = "count",
            aes(label = scales::comma(after_stat(count))),
            hjust = -0.15, size = 3.5, fontface = "bold",
            color = "#2E2319") +
  scale_fill_manual(values = pal_parente, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = scales::comma) +
  coord_flip() +
  labs(title   = "Fréquence du lien de parenté avec le chef de ménage",
       x       = NULL,
       y       = "Effectifs",
       caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
  theme_tp1(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig04_lien_parente.png", p_parente,
       width = 9, height = 5, dpi = 150, bg = COL_FOND)
cat("  fig04_lien_parente.png\n")

# proportions avec IC 95% exact (Clopper-Pearson)
tab_effectifs <- table(data$lien_parente)
tab_confiance <- lapply(tab_effectifs, function(x) {
  exactci(x, n = sum(tab_effectifs), conf.level = 0.95)
})
resultat_final <- data.frame(
  Categorie  = names(tab_effectifs),
  Effectif   = as.numeric(tab_effectifs),
  Proportion = round(as.numeric(tab_effectifs) / sum(tab_effectifs) * 100, 2),
  IC_lower   = round(sapply(tab_confiance, function(x) x$conf.int[1]) * 100, 2),
  IC_upper   = round(sapply(tab_confiance, function(x) x$conf.int[2]) * 100, 2)
)
cat("\nProportions avec IC 95% :\n"); print(resultat_final)
saveRDS(resultat_final, "data/processed/proportions_IC.rds")

# TAILLE DES MÉNAGES ET TEST DE WILCOXON
cat("\n Taille des ménages par secteur \n")

stats_menage <- base_menage %>%
  group_by(secteur) %>%
  summarise(
    N          = n(),
    Moyenne    = round(mean(taille), 2),
    Mediane    = median(taille),
    Q1         = quantile(taille, 0.25),
    Q3         = quantile(taille, 0.75),
    Ecart_type = round(sd(taille), 2),
    Min        = min(taille),
    Max        = max(taille)
  )
cat("\nStatistiques par secteur :\n"); print(stats_menage)

# test de Wilcoxon-Mann-Whitney
urbain     <- base_menage %>% filter(secteur == "Urbain") %>% pull(taille)
rural      <- base_menage %>% filter(secteur == "Rural")  %>% pull(taille)
wilcox_res <- wilcox.test(urbain, rural, alternative = "two.sided",
                          conf.int = TRUE, conf.level = 0.95)

# taille d'effet r de rang
N_total        <- length(urbain) + length(rural)
Z              <- qnorm(wilcox_res$p.value / 2)
r_rang         <- abs(Z) / sqrt(N_total)
interpretation <- case_when(
  r_rang < 0.1 ~ "Négligeable",
  r_rang < 0.3 ~ "Petit",
  r_rang < 0.5 ~ "Moyen",
  TRUE         ~ "Grand"
)
cat("\nTest de Wilcoxon-Mann-Whitney :\n")
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
moyennes <- base_menage %>%
  group_by(secteur) %>%
  summarise(moyenne = mean(taille))

# boxplot groupé
p_boxplot_menage <- ggplot(base_menage,
                           aes(x = secteur, y = taille, fill = secteur)) +
  geom_boxplot(alpha = 0.72,
               outlier.color = "#B0927A", outlier.size = 0.7,
               outlier.alpha = 0.45, width = 0.45,
               color = "#4A3C2F") +
  geom_jitter(width = 0.14, alpha = 0.07, size = 0.55,
              color = "#6B5C4A") +
  geom_point(data  = moyennes,
             aes(x = secteur, y = moyenne),
             shape = 18, size = 4.5, color = COL_ACCENT) +
  geom_text(data  = moyennes,
            aes(x = secteur, y = moyenne,
                label = paste0("Moy. = ", round(moyenne, 1))),
            vjust = -0.9, color = "#8B5E2A",
            size  = 3.5, fontface = "bold") +
  annotate("label",
           x = 1.5, y = max(base_menage$taille) * 0.93,
           label = label_wilcox,
           fill  = COL_FOND, color = "#5C4D3E",
           size  = 3.2, hjust = 0.5, fontface = "italic",
           label.size = 0.25) +
  scale_fill_manual(values = palette_secteur) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(
    title    = "Taille des ménages selon le secteur (Urbain vs Rural)",
    subtitle = paste0("n = ", scales::comma(nrow(base_menage)),
                      " ménages  |  ◆ = Moyenne  |  Test Wilcoxon-Mann-Whitney"),
    x        = NULL,
    y        = "Taille du ménage (nombre de membres)",
    caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
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

# TÂCHE 6 — TABLEAU GTSUMMARY STRATIFIÉ
cat("\n Tableau gtsummary stratifié \n")

data_tableau <- data_indiv %>%
  filter(!is.na(secteur)) %>%
  transmute(
    secteur       = secteur,
    age           = as.numeric(s1q4),
    sexe          = factor(as.numeric(s1q2), levels = c(1, 2),
                           labels = c("Masculin", "Féminin")),
    taille_menage = taille_menage
  )

tableau_gt <- data_tableau %>%
  tbl_summary(
    by      = secteur,
    include = c(age, sexe, taille_menage),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})\nMéd. {median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p}%)"
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
    test = list(age ~ "wilcox.test", sexe ~ "chisq.test",
                taille_menage ~ "wilcox.test"),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  add_n() %>%
  modify_header(
    label   ~ "**Variable**",
    stat_0  ~ "**Total**\nN = {N}",
    stat_1  ~ "**Urbain**\nN = {n}",
    stat_2  ~ "**Rural**\nN = {n}",
    p.value ~ "**p-valeur**"
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Secteur de résidence**") %>%
  modify_caption("**Tableau 1. Caractéristiques sociodémographiques par secteur**
   Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") %>%
  modify_footnote(p.value ~ "Wilcoxon-Mann-Whitney (continu) ; Chi-deux (catégoriel)") %>%
  bold_labels() %>%
  bold_p(t = 0.05)

tableau_gt %>%
  as_gt() %>%
  gt::gtsave("outputs/tableau_gtsummary.html")
cat("  tableau_gtsummary.html\n")

cat("\n Toutes les analyses terminées \n")
for (f in list.files("outputs/")) cat("  -", f, "\n")
