library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(rstatix)
library(gtsummary)
library(patchwork)
library(survey)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables",  showWarnings = FALSE, recursive = TRUE)

df_health    <- readRDS("data/processed/df_health_base.rds")
plan_health  <- readRDS("data/processed/plan_health.rds")
plan_malades <- readRDS("data/processed/plan_malades.rds")

# palette et thème — couleurs vives
COL_FOND    <- "#F0F4FF"
COL_GRILLE  <- "#D8E2F5"
COL_TEXTE   <- "#1A1A2E"
COL_MUTED   <- "#3D5A80"
COL_ACCENT  <- "#F03E3E"
PAL_SEXE    <- c("Homme" = "#339AF0", "Femme" = "#F06595")
PAL_MILIEU  <- c("Urbain" = "#339AF0", "Rural" = "#69DB7C")

theme_tp3 <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background  = element_rect(fill = COL_FOND, color = NA),
      panel.background = element_rect(fill = COL_FOND, color = NA),
      panel.grid.major = element_line(color = COL_GRILLE, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = base_size + 1,
                                      color = COL_TEXTE, margin = margin(b = 4)),
      plot.subtitle    = element_text(color = COL_MUTED, size = base_size - 1,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(color = "#7B8AB8", size = base_size - 3,
                                      hjust = 0, margin = margin(t = 6)),
      axis.title       = element_text(color = COL_MUTED, size = base_size - 1),
      axis.text        = element_text(color = "#2C3A6B"),
      legend.background = element_rect(fill = COL_FOND, color = NA),
      legend.key        = element_rect(fill = COL_FOND, color = NA),
      legend.title      = element_text(face = "bold", color = COL_TEXTE)
    )
}

# TAUX DE MORBIDITÉ PONDÉRÉS PAR SEXE ET GROUPE D'ÂGE
marge <- 0.012


# par sexe — via plan de sondage
# Approche svymean + subset : robuste quelle que soit la version de survey
niveaux_sexe <- c("Homme", "Femme")
taux_sexe <- do.call(rbind, lapply(niveaux_sexe, function(g) {
  res <- svymean(~malade,
                 subset(plan_health, sexe_label == g),
                 na.rm = TRUE)
  data.frame(
    sexe_label = g,
    taux       = as.numeric(coef(res)),
    se         = as.numeric(SE(res))
  )
})) %>%
  mutate(
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = taux + 1.96 * se
  )
print(taux_sexe)

p_sexe <- ggplot(taux_sexe, aes(x = sexe_label, y = taux, fill = sexe_label)) +
  geom_col(width = 0.5, show.legend = FALSE, color = "#1A1A2E", linewidth = 0.3) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.12, linewidth = 0.8, color = "#1A1A2E") +
  geom_text(aes(y = ic_high + marge, label = paste0(round(taux * 100, 1), "%")),
            vjust = 0, size = 5, fontface = "bold", color = COL_TEXTE) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(taux_sexe$ic_high) + 0.04),
                     expand = c(0, 0)) +
  scale_fill_manual(values = PAL_SEXE) +
  labs(title    = "Taux de morbidité pondéré par sexe",
       subtitle = "Estimations population — Wave 4 (2018) | Poids : wt_wave4",
       x = NULL, y = "Taux de morbidité pondéré",
       caption = "Source : Nigeria GHS Panel W4 | Barres d'erreur : IC à 95%") +
  theme_tp3() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"))

ggsave("output/figures/01a_morbidite_sexe.png", p_sexe,
       width = 7, height = 5.5, dpi = 300, bg = COL_FOND)
cat("  01a_morbidite_sexe.png\n")

# par groupe d'âge — pondéré
pal_age <- colorRampPalette(c("#74C0FC", "#1864AB"))(7)
names(pal_age) <- c("0-14","15-24","25-34","35-44","45-54","55-64","65+")

niveaux_age <- c("0-14","15-24","25-34","35-44","45-54","55-64","65+")
taux_age <- do.call(rbind, lapply(niveaux_age, function(g) {
  res <- svymean(~malade,
                 subset(plan_health, as.character(groupe_age) == g),
                 na.rm = TRUE)
  data.frame(
    groupe_age = g,
    taux       = as.numeric(coef(res)),
    se         = as.numeric(SE(res))
  )
})) %>%
  mutate(
    groupe_age = factor(groupe_age, levels = niveaux_age),
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = taux + 1.96 * se
  )

p_age <- ggplot(taux_age, aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_col(width = 0.65, show.legend = FALSE, color = "#1A1A2E", linewidth = 0.3) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.2, linewidth = 0.7, color = "#1A1A2E") +
  geom_text(aes(y = ic_high + marge, label = paste0(round(taux * 100, 1), "%")),
            vjust = 0, size = 4, fontface = "bold", color = COL_TEXTE) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(taux_age$ic_high) + 0.05),
                     expand = c(0, 0)) +
  scale_fill_manual(values = pal_age) +
  labs(title    = "Taux de morbidité pondéré par groupe d'âge",
       subtitle = "Estimations population — Wave 4 (2018) | Poids : wt_wave4",
       x = "Groupe d'âge", y = "Taux de morbidité pondéré",
       caption = "Source : Nigeria GHS Panel W4 | IC à 95%") +
  theme_tp3() + theme(panel.grid.major.x = element_blank())

ggsave("output/figures/01b_morbidite_age.png", p_age,
       width = 9, height = 5.5, dpi = 300, bg = COL_FOND)
cat("  01b_morbidite_age.png\n")

# croisé sexe × âge — pondéré
niveaux_age <- c("0-14","15-24","25-34","35-44","45-54","55-64","65+")
niveaux_sexe <- c("Homme", "Femme")
taux_sa <- do.call(rbind, lapply(niveaux_age, function(a) {
  do.call(rbind, lapply(niveaux_sexe, function(s) {
    res <- svymean(~malade,
                   subset(plan_health,
                          as.character(groupe_age) == a & as.character(sexe_label) == s),
                   na.rm = TRUE)
    data.frame(
      groupe_age = a,
      sexe_label = s,
      taux       = as.numeric(coef(res)),
      se         = as.numeric(SE(res))
    )
  }))
})) %>%
  mutate(
    groupe_age = factor(groupe_age, levels = niveaux_age),
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = taux + 1.96 * se
  )

dw <- 0.7
p_sa <- ggplot(taux_sa, aes(x = groupe_age, y = taux, fill = sexe_label)) +
  geom_col(position = position_dodge(dw), width = 0.65,
           color = "#1A1A2E", linewidth = 0.25) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                position = position_dodge(dw), width = 0.18,
                linewidth = 0.65, color = "#1A1A2E") +
  geom_text(aes(y = ic_high + marge, label = paste0(round(taux * 100, 1), "%")),
            position = position_dodge(dw), vjust = 0, size = 2.8,
            fontface = "bold", color = COL_TEXTE) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(taux_sa$ic_high) + 0.05),
                     expand = c(0, 0)) +
  scale_fill_manual(values = PAL_SEXE, name = "Sexe") +
  labs(title    = "Taux de morbidité pondéré par groupe d'âge et par sexe",
       subtitle = "Estimations population — Wave 4 (2018) | Poids : wt_wave4",
       x = "Groupe d'âge", y = "Taux de morbidité pondéré",
       caption = "Source : Nigeria GHS Panel W4 | IC à 95%") +
  theme_tp3() +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave("output/figures/01c_morbidite_sexe_age.png", p_sa,
       width = 11, height = 5.5, dpi = 300, bg = COL_FOND)
cat("  01c_morbidite_sexe_age.png\n")

# TYPES DE MALADIES — effectifs pondérés
cat("\n--- Types de maladies (pondéré) ---\n")

maladie_labels <- c(
  "1"="Paludisme","2"="Tuberculose","3"="Fièvre jaune","4"="Typhoïde",
  "5"="Choléra","6"="Diarrhée","7"="Méningite","8"="Varicelle",
  "9"="Pneumonie","10"="Rhume commun","11"="Blessure/Traumatisme",
  "12"="Autre","13"="Hypertension","14"="Grippe","15"="Rhinite/Catarrhe",
  "16"="Toux","17"="Céphalée","18"="Diabète","19"="Ver de Guinée",
  "20"="Dysenterie","21"="Infection cutanée","22"="Rougeole",
  "23"="Infection urinaire","24"="Douleurs articulaires",
  "25"="Fièvre non spécifiée","26"="Trouble digestif","27"="Faiblesse/Fatigue"
)
categorie_maladie <- c(
  "1"="Infectieuse","2"="Infectieuse","3"="Infectieuse","4"="Infectieuse",
  "5"="Infectieuse","6"="Infectieuse","7"="Infectieuse","8"="Infectieuse",
  "9"="Infectieuse","10"="Infectieuse","11"="Traumatique","12"="Autre",
  "13"="Chronique","14"="Infectieuse","15"="Infectieuse","16"="Infectieuse",
  "17"="Autre","18"="Chronique","19"="Infectieuse","20"="Infectieuse",
  "21"="Infectieuse","22"="Infectieuse","23"="Infectieuse","24"="Chronique",
  "25"="Infectieuse","26"="Autre","27"="Autre"
)

top10 <- df_health %>%
  filter(!is.na(s4aq3b_1), s4aq3 == 1, !is.na(wt_wave4)) %>%
  mutate(code_str  = as.character(as.integer(s4aq3b_1)),
         maladie   = maladie_labels[code_str],
         categorie = categorie_maladie[code_str]) %>%
  filter(!is.na(maladie)) %>%
  group_by(maladie, categorie) %>%
  summarise(n = sum(wt_wave4), .groups = "drop") %>%    # effectif pondéré
  mutate(pct = n / sum(n) * 100,
         maladie = fct_reorder(maladie, n)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

print(top10)
write.csv(top10, "output/tables/02_top10_maladies.csv", row.names = FALSE)

pal_cat <- c("Infectieuse"="#20C997","Chronique"="#F03E3E",
             "Traumatique"="#FFD43B","Autre"="#748FFC")

p_maladies <- ggplot(top10, aes(x = maladie, y = n, fill = categorie)) +
  geom_col(width = 0.68, color = COL_FOND, linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 3.8, fontface = "bold", color = COL_TEXTE) +
  coord_flip() +
  scale_fill_manual(values = pal_cat, name = "Catégorie") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = label_comma()) +
  labs(title    = "Dix affections pondérées les plus fréquemment déclarées",
       subtitle = "Effectifs estimés dans la population | Poids : wt_wave4",
       x = NULL, y = "Effectif pondéré (population estimée)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_tp3() +
  theme(legend.position = "top", panel.grid.major.y = element_blank())

ggsave("output/figures/02_types_maladies.png", p_maladies,
       width = 10, height = 6, dpi = 300, bg = COL_FOND)
cat("  02_types_maladies.png\n")

# RECOURS AUX SOINS PAR PRESTATAIRE — pondéré
groupe_praticien <- c(
  "0"="Aucun recours","1"="Tradipraticien","2"="Hôpital/Clinique (médecin)",
  "3"="Hôpital/Clinique (médecin)","4"="Hôpital/Clinique (infirmier)",
  "5"="Hôpital/Clinique (infirmier)","6"="Hôpital/Clinique (infirmier)",
  "7"="Pharmacie","8"="Pharmacie","9"="Tradipraticien","10"="Tradipraticien",
  "11"="Pharmacie","13"="Autre","14"="Agent de santé comm.",
  "15"="Agent de santé comm."
)

taux_prat <- df_health %>%
  filter(!is.na(s4aq3), s4aq3 == 1, !is.na(wt_wave4)) %>%
  mutate(code_prat = as.character(if_else(is.na(s4aq6a) | s4aq6a == 0,
                                          0, as.integer(s4aq6a))),
         praticien = groupe_praticien[code_prat]) %>%
  filter(!is.na(praticien)) %>%
  group_by(praticien) %>%
  summarise(n = sum(wt_wave4), .groups = "drop") %>%   # effectif pondéré
  mutate(pct = n / sum(n) * 100,
         praticien_ord = fct_reorder(praticien, n)) %>%
  arrange(desc(n))

print(taux_prat)
write.csv(taux_prat, "output/tables/03_recours_prestataires.csv", row.names = FALSE)

pal_prat <- c("Aucun recours"="#F03E3E","Hôpital/Clinique (médecin)"="#1864AB",
              "Hôpital/Clinique (infirmier)"="#74C0FC","Pharmacie"="#69DB7C",
              "Tradipraticien"="#FFD43B","Agent de santé comm."="#FF922B",
              "Autre"="#ADB5BD")

p_recours <- ggplot(taux_prat, aes(x = praticien_ord, y = pct, fill = praticien)) +
  geom_col(width = 0.68, show.legend = FALSE, color = COL_FOND, linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 4, fontface = "bold", color = COL_TEXTE) +
  coord_flip() +
  scale_fill_manual(values = pal_prat) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title    = "Recours pondéré aux soins selon le type de prestataire",
       subtitle = "Estimations population parmi les individus malades | Poids : wt_wave4",
       x = NULL, y = "Part pondérée des individus malades (%)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_tp3() + theme(panel.grid.major.y = element_blank())

ggsave("output/figures/03_recours_prestataires.png", p_recours,
       width = 10, height = 6, dpi = 300, bg = COL_FOND)
cat("  03_recours_prestataires.png\n")

# DÉPENSES DE SANTÉ — pondérées
df_dep <- df_health %>%
  mutate(dep_consultation = if_else(is.na(s4aq9),  0, as.numeric(s4aq9)),
         dep_medicaments  = if_else(is.na(s4aq14), 0, as.numeric(s4aq14)),
         dep_hopital      = if_else(is.na(s4aq17), 0, as.numeric(s4aq17)),
         dep_totale       = dep_consultation + dep_medicaments + dep_hopital) %>%
  filter(dep_totale > 0, !is.na(wt_wave4)) %>%
  mutate(decile = ntile(dep_totale, 10))

cat("  Individus avec dépenses > 0 :", nrow(df_dep), "\n")

# seuil outlier Tukey étendu
q1 <- quantile(df_dep$dep_totale, 0.25)
q3 <- quantile(df_dep$dep_totale, 0.75)
seuil_haut <- q3 + 3 * (q3 - q1)

# statistiques pondérées par décile
plan_dep <- svydesign(ids = ~cluster, strata = ~strata, weights = ~wt_wave4,
                      data = df_dep, nest = TRUE)

stats_decile <- df_dep %>%
  group_by(decile) %>%
  summarise(
    n       = n(),
    min     = min(dep_totale),
    mediane = median(dep_totale),
    moyenne = round(weighted.mean(dep_totale, wt_wave4)),
    max     = max(dep_totale),
    .groups = "drop"
  )
print(stats_decile)
write.csv(stats_decile, "output/tables/04_depenses_decile.csv", row.names = FALSE)

# médiane pondérée globale
med_val_pond <- as.numeric(svyquantile(~dep_totale, plan_dep,
                                       quantiles = 0.5, na.rm = TRUE)[[1]])

p_histo <- ggplot(df_dep, aes(x = dep_totale, weight = wt_wave4)) +
  geom_histogram(bins = 40, fill = "#20C997", color = COL_FOND,
                 alpha = 0.88, linewidth = 0.3) +
  geom_vline(xintercept = med_val_pond, linetype = "dashed",
             color = COL_ACCENT, linewidth = 1) +
  annotate("label", x = quantile(df_dep$dep_totale, 0.02), y = Inf,
           label = paste0("Médiane pondérée : ", format(round(med_val_pond), big.mark = " ")),
           hjust = 0, vjust = 1.3, color = COL_ACCENT, size = 4,
           fill = COL_FOND, label.size = 0.25, fontface = "bold") +
  scale_x_log10(labels = label_comma(big.mark = " ")) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Distribution pondérée des dépenses de santé (échelle logarithmique)",
       subtitle = "Individus ayant engagé des dépenses — Wave 4 (2018) | Poids : wt_wave4",
       x = "Dépense totale (Naira, échelle log)", y = "Effectif pondéré",
       caption = "Source : Nigeria GHS Panel W4 | Ligne pointillée = médiane pondérée") +
  theme_tp3()

ggsave("output/figures/04a_depenses_histogramme.png", p_histo,
       width = 10, height = 5.5, dpi = 300, bg = COL_FOND)
cat("  04a_depenses_histogramme.png\n")

# boxplot par prestataire
pal_prat2 <- c("Hôpital / Clinique"="#1864AB","Pharmacie"="#69DB7C",
               "Tradipraticien"="#FFD43B","Agent de santé comm."="#FF922B",
               "Autre"="#ADB5BD")

gp2 <- c("0"="Aucun recours","1"="Tradipraticien","2"="Hôpital / Clinique",
         "3"="Hôpital / Clinique","4"="Hôpital / Clinique",
         "5"="Hôpital / Clinique","6"="Hôpital / Clinique",
         "7"="Pharmacie","8"="Pharmacie","9"="Tradipraticien",
         "10"="Tradipraticien","11"="Pharmacie","13"="Autre",
         "14"="Agent de santé comm.","15"="Agent de santé comm.")

df_box <- df_dep %>%
  mutate(code_prat = as.character(if_else(is.na(s4aq6a) | s4aq6a == 0,
                                          0, as.integer(s4aq6a))),
         praticien = gp2[code_prat]) %>%
  filter(!is.na(praticien), praticien != "Aucun recours",
         dep_totale <= seuil_haut)

p_box <- ggplot(df_box, aes(x = reorder(praticien, dep_totale, median),
                            y = dep_totale, fill = praticien)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.25, outlier.size = 1,
               show.legend = FALSE, width = 0.55, color = "#1A1A2E") +
  stat_summary(fun = median, geom = "point", shape = 18,
               size = 3.5, color = COL_ACCENT) +
  scale_y_log10(labels = label_comma(big.mark = " ")) +
  scale_fill_manual(values = pal_prat2) +
  coord_flip() +
  labs(title    = "Dépenses de santé par type de prestataire",
       subtitle = "Échelle log — outliers extrêmes exclus (> Q3 + 3×IQR) | ◆ = médiane",
       x = NULL, y = "Dépense totale (Naira, échelle log)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_tp3() + theme(panel.grid.major.y = element_blank())

ggsave("output/figures/04b_depenses_boxplot_prestataire.png", p_box,
       width = 10, height = 5.5, dpi = 300, bg = COL_FOND)
cat("  04b_depenses_boxplot_prestataire.png\n")

saveRDS(list(seuil_haut = seuil_haut, gp2 = gp2, med_pond = med_val_pond),
        "data/processed/params_depenses.rds")

# TEST D'INDÉPENDANCE RECOURS × QUINTILE — Chi-deux pondéré (Rao-Scott)
df_q <- df_health %>%
  filter(!is.na(s4aq3), s4aq3 == 1, !is.na(wt_wave4)) %>%
  mutate(consulte = factor(if_else(s4aq1 == 1, "Consulté", "Non consulté",
                                   missing = "Non consulté"),
                           levels = c("Consulté", "Non consulté"))) %>%
  filter(!is.na(quintile_label), !is.na(consulte))

plan_q <- svydesign(ids = ~cluster, strata = ~strata, weights = ~wt_wave4,
                    data = df_q, nest = TRUE)

chi2_res  <- svychisq(~consulte + quintile_label, plan_q, statistic = "Chisq")
tab_pond  <- svytable(~consulte + quintile_label, plan_q)
n_total   <- sum(tab_pond)
chi2_val  <- chi2_res$statistic
v_cramer  <- sqrt(chi2_val / (n_total * (min(nrow(tab_pond), ncol(tab_pond)) - 1)))

cat("Chi² pondéré =", round(chi2_val, 3), "| p =", format.pval(chi2_res$p.value, digits = 3),
    "| V de Cramer =", round(v_cramer, 4), "\n")

tab_df        <- as.data.frame.matrix(round(tab_pond))
tab_df$Recours <- rownames(tab_df)
write.csv(tab_df, "output/tables/05_contingence_recours_quintile.csv", row.names = FALSE)

df_plot_q <- df_q %>%
  group_by(quintile_label, consulte) %>%
  summarise(n_pond = sum(wt_wave4), .groups = "drop") %>%
  group_by(quintile_label) %>%
  mutate(pct = n_pond / sum(n_pond) * 100) %>% ungroup()

p_quintile <- ggplot(df_plot_q, aes(x = quintile_label, y = pct, fill = consulte)) +
  geom_col(position = "stack", width = 0.62, color = COL_FOND, linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.8, color = "white", fontface = "bold") +
  annotate("label", x = 3, y = 105,
           label = paste0("χ² pondéré = ", round(chi2_val, 2),
                          "  |  V = ", round(v_cramer, 3),
                          "  |  p < 0.001"),
           fill = COL_FOND, color = COL_MUTED, size = 3.2,
           fontface = "italic", label.size = 0.2) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_fill_manual(values = c("Consulté" = "#20C997", "Non consulté" = "#F03E3E"),
                    name = "Recours aux soins") +
  labs(title    = "Recours pondéré aux soins selon le quintile de consommation",
       subtitle = "Parmi les individus malades — estimations population | Poids : wt_wave4",
       x = "Quintile de consommation", y = "Part pondérée des individus malades (%)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_tp3() +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave("output/figures/05_recours_quintile.png", p_quintile,
       width = 10, height = 6, dpi = 300, bg = COL_FOND)
cat("  05_recours_quintile.png\n")

# VIOLIN PLOTS DÉPENSES RURAL / URBAIN — Wilcoxon pondéré
df_vio <- df_health %>%
  mutate(dep_consultation = if_else(is.na(s4aq9),  0, as.numeric(s4aq9)),
         dep_medicaments  = if_else(is.na(s4aq14), 0, as.numeric(s4aq14)),
         dep_hopital      = if_else(is.na(s4aq17), 0, as.numeric(s4aq17)),
         dep_totale       = dep_consultation + dep_medicaments + dep_hopital) %>%
  filter(dep_totale > 0, !is.na(milieu_label), !is.na(wt_wave4))

# Wilcoxon pondéré par réplication proportionnelle aux poids
wt_sc    <- min(df_vio$wt_wave4)
df_rep   <- df_vio %>% mutate(rep_n = pmax(1, round(wt_wave4 / wt_sc))) %>%
  slice(rep(seq_len(n()), times = rep_n))
set.seed(123)
if (nrow(df_rep) > 100000) df_rep <- df_rep[sample(nrow(df_rep), 100000), ]

wx <- wilcox.test(dep_totale ~ milieu_label, data = df_rep,
                  exact = FALSE, conf.int = TRUE)
r_eff <- abs(qnorm(wx$p.value / 2)) / sqrt(nrow(df_rep))

cat("Wilcoxon pondéré W =", wx$statistic, "| p =", format.pval(wx$p.value, digits = 4),
    "| r =", round(r_eff, 4), "\n")

write.csv(data.frame(W = wx$statistic, p_value = wx$p.value, r_effet = r_eff),
          "output/tables/06_wilcoxon_rural_urbain.csv", row.names = FALSE)

p_vio <- ggplot(df_vio, aes(x = milieu_label, y = dep_totale,
                            fill = milieu_label, weight = wt_wave4)) +
  geom_violin(trim = TRUE, alpha = 0.55, color = NA) +
  geom_boxplot(aes(weight = NULL), width = 0.12, fill = "white",
               color = "#1A1A2E", outlier.shape = NA, linewidth = 0.65) +
  stat_summary(aes(weight = NULL), fun = median, geom = "point",
               shape = 18, size = 4.5, color = COL_ACCENT) +
  annotate("label", x = 1.5, y = Inf,
           label = paste0("Wilcoxon pondéré  p < 0.001  |  r = ", round(r_eff, 3)),
           vjust = 1.3, size = 3.8, fill = COL_FOND, label.size = 0.25,
           fontface = "italic", color = COL_MUTED) +
  scale_y_log10(labels = label_comma(big.mark = " "),
                breaks = c(10, 100, 500, 1000, 5000, 10000, 50000)) +
  scale_fill_manual(values = PAL_MILIEU) +
  labs(title    = "Distribution pondérée des dépenses de santé : rural vs urbain",
       subtitle = "Échelle log — Wave 4 (2018) | ◆ rouge = médiane | Poids : wt_wave4",
       x = NULL, y = "Dépense totale (Naira, échelle log)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_tp3() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 13, face = "bold"))

ggsave("output/figures/06a_violin_rural_urbain.png", p_vio,
       width = 8, height = 6, dpi = 300, bg = COL_FOND)
cat("  06a_violin_rural_urbain.png\n")

# violin par quintile — pondéré
df_vio_q <- df_vio %>% filter(!is.na(quintile_label))
pal_quint <- c("Q1 (plus pauvres)"="#F03E3E","Q2"="#FF922B",
               "Q3"="#FFD43B","Q4"="#69DB7C","Q5 (plus riches)"="#339AF0")

p_vio_q <- ggplot(df_vio_q, aes(x = quintile_label, y = dep_totale,
                                fill = quintile_label, weight = wt_wave4)) +
  geom_violin(trim = TRUE, alpha = 0.62, color = NA) +
  geom_boxplot(aes(weight = NULL), width = 0.1, fill = "white",
               color = "#1A1A2E", outlier.shape = NA, linewidth = 0.55) +
  stat_summary(aes(weight = NULL), fun = median, geom = "point",
               shape = 18, size = 3.5, color = COL_ACCENT) +
  scale_y_log10(labels = label_comma(big.mark = " "),
                breaks = c(10, 100, 500, 1000, 5000, 10000, 50000)) +
  scale_fill_manual(values = pal_quint, guide = "none") +
  labs(title    = "Dépenses de santé pondérées par quintile de consommation",
       subtitle = "Échelle log — Wave 4 (2018) | ◆ rouge = médiane | Poids : wt_wave4",
       x = "Quintile de consommation", y = "Dépense totale (Naira, échelle log)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_tp3() + theme(legend.position = "none")

ggsave("output/figures/06b_violin_quintiles.png", p_vio_q,
       width = 10, height = 6, dpi = 300, bg = COL_FOND)
cat("  06b_violin_quintiles.png\n")

# combiné
p_combined <- p_vio / p_vio_q +
  plot_annotation(
    title    = "Dépenses de santé pondérées — disparités par milieu et niveau de richesse",
    subtitle = "Nigeria GHS Panel — Wave 4, Post-Harvest 2018 | Poids : wt_wave4",
    theme    = theme(
      plot.background = element_rect(fill = COL_FOND, color = NA),
      plot.title      = element_text(face = "bold", size = 15, color = COL_TEXTE),
      plot.subtitle   = element_text(color = COL_MUTED, size = 12)
    )
  )

ggsave("output/figures/06c_violin_combined.png", p_combined,
       width = 10, height = 12, dpi = 300, bg = COL_FOND)
cat("  06c_violin_combined.png\n")

cat("\n Toutes les analyses pondérées terminées \n")
for (f in list.files("output/figures", pattern = "\\.png$")) cat(" -", f, "\n")
