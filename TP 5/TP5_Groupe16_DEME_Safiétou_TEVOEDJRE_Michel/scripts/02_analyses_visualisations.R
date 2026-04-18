library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(patchwork)
library(ggpubr)
library(rstatix)
library(survey)
library(srvyr)
library(tidyr)
library(stringr)

cat("Chargement des données traitées...\n")

wgt_data             <- readRDS("data/processed/wgt_data.rds")
crop_dict            <- readRDS("data/processed/crop_dict.rds")
tbl_cultures         <- readRDS("data/processed/tbl_cultures.rds")
tbl_diversification  <- readRDS("data/processed/tbl_diversification.rds")
tbl_intrants_parcelle <- readRDS("data/processed/tbl_intrants_parcelle.rds")
tbl_intrants_menage  <- readRDS("data/processed/tbl_intrants_menage.rds")
tbl_production       <- readRDS("data/processed/tbl_production.rds")
effectif_pondere     <- readRDS("data/processed/effectif_pondere.rds")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Palette et thème cohérents
pal_type <- c(
  "Céréale"    = "#2C7BB6",
  "Légumineuse"= "#1A9641",
  "Tubercule"  = "#D7191C",
  "Rente"      = "#FDAE61",
  "Autre"      = "#AAAAAA"
)
pal_milieu  <- c("Urbain" = "#FDAE61", "Rural" = "#1A9641")
pal_engrais <- c("Avec engrais inorg." = "#1A9641",
                 "Sans engrais inorg." = "#FDAE61")
pal_intrant <- c(
  "Engrais inorganique" = "#2C7BB6",
  "dont NPK"            = "#4DAEDB",
  "dont Urée"           = "#A8DAEE",
  "Engrais organique"   = "#1A9641",
  "Pesticide"           = "#D7191C",
  "Herbicide"           = "#FDAE61"
)

theme_tp5 <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8.5, color = "grey45",
                                    hjust = 1, margin = margin(t = 8)),
    axis.title       = element_text(size = 10),
    legend.title     = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )


# 15 cultures les plus fréquentes (W4)
cat("\nQ25 : Top 15 cultures les plus fréquentes...\n")

# Fréquence pondérée = somme des poids des ménages cultivant chaque culture
freq_cultures <- tbl_cultures %>%
  filter(!is.na(wt_wave4)) %>%
  group_by(cropcode, crop_name, crop_type) %>%
  summarise(freq_pond = sum(wt_wave4), .groups = "drop")

classement_15 <- freq_cultures %>%
  arrange(desc(freq_pond)) %>%
  slice_head(n = 15) %>%
  mutate(
    proportion  = freq_pond / effectif_pondere * 100,
    n_menages   = sapply(cropcode, function(cc)
      n_distinct(tbl_cultures$hhid[tbl_cultures$cropcode == cc])),
    crop_name   = reorder(crop_name, proportion),
    crop_type   = factor(crop_type,
                         levels = c("Céréale", "Légumineuse",
                                    "Tubercule", "Rente", "Autre"))
  )

cat("  Classement (% ménages, pondéré) :\n")
print(classement_15 %>% arrange(desc(proportion)) %>%
        select(crop_name, crop_type, proportion) %>%
        mutate(proportion = round(proportion, 1)))

write.csv(
  classement_15 %>% select(crop_name, crop_type, n_menages, proportion),
  "outputs/25_top15_cultures.csv", row.names = FALSE
)

p25 <- ggplot(classement_15, aes(x = proportion, y = crop_name, fill = crop_type)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.1, size = 3.2, fontface = "bold", color = "#2C3E50") +
  scale_fill_manual(values = pal_type, name = "Type de culture") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(
    title    = "Top 15 des cultures pratiquées — Vague 4, 2018",
    subtitle = paste0("Part des ménages agricoles cultivant chaque espèce\n",
                      "Effectif pondéré = ",
                      format(round(effectif_pondere), big.mark = " ")),
    x        = "Part des ménages agricoles (%)",
    y        = NULL,
    caption  = "Source : GHS Panel W4, Nigeria 2018/2019 | Pondération : wt_wave4"
  ) +
  theme_tp5 +
  theme(panel.grid.major.y = element_blank(),
        legend.key.size    = unit(0.4, "cm"))

ggsave("outputs/25_top15_cultures.png", p25,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Enregistré : outputs/25_top15_cultures.png\n")

# Q26 — Diversification culturale par ménage (W4)
cat("\nQ26 : Indice de diversification culturale...\n")

df_div <- tbl_diversification %>%
  filter(!is.na(wt_wave4), !is.na(milieu))

# Plan de sondage avec srvyr pour IC pondérés
plan_div <- df_div %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

resume_div <- plan_div %>%
  group_by(milieu) %>%
  summarise(
    moyenne = survey_mean(nb_cultures, vartype = "ci"),
    .groups = "drop"
  ) %>% as.data.frame()

cat("  Résumé pondéré par milieu :\n")
print(resume_div)

# Moyennes pondérées (pour annotation)
moy_milieu <- df_div %>%
  group_by(milieu) %>%
  summarise(moy = weighted.mean(nb_cultures, wt_wave4, na.rm = TRUE),
            .groups = "drop")

# Test de Wilcoxon rural vs urbain
test_wilcox_div <- wilcox.test(
  nb_cultures ~ milieu,
  data = df_div,
  conf.int = TRUE,
  exact = FALSE
)
n_div <- nrow(df_div)
r_div <- abs(qnorm(test_wilcox_div$p.value / 2)) / sqrt(n_div)

cat("  Wilcoxon W =", round(test_wilcox_div$statistic, 0),
    "| p =", format(test_wilcox_div$p.value, scientific = TRUE, digits = 3),
    "| r =", round(r_div, 3), "\n")

wilcox_tab_div <- data.frame(
  Comparaison  = "Rural vs Urbain",
  W            = round(test_wilcox_div$statistic, 2),
  p_value      = signif(test_wilcox_div$p.value, 4),
  r_effet      = round(r_div, 4),
  Significatif = ifelse(test_wilcox_div$p.value < 0.05, "Oui", "Non")
)
write.csv(wilcox_tab_div, "outputs/26_wilcoxon_diversification.csv",
          row.names = FALSE)

# Sous-figure a : histogramme pondéré
p26a <- ggplot(df_div, aes(x = nb_cultures, weight = wt_wave4)) +
  geom_histogram(binwidth = 1, fill = "#2C7BB6", color = "white", alpha = 0.85) +
  scale_x_continuous(breaks = 1:max(df_div$nb_cultures, na.rm = TRUE)) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  labs(
    title    = "Répartition du nombre de cultures",
    subtitle = "Wave 4 (2018) — effectifs pondérés",
    x        = "Nombre de cultures différentes",
    y        = "Effectifs pondérés",
    caption  = "Source : GHS Panel W4 | Pondération : wt_wave4"
  ) +
  theme_tp5

# Sous-figure b : violin + boxplot + point moyenne
p26b <- ggplot(df_div, aes(x = milieu, y = nb_cultures, fill = milieu)) +
  geom_violin(aes(weight = wt_wave4), alpha = 0.65, trim = FALSE) +
  geom_boxplot(aes(weight = wt_wave4), width = 0.14, fill = "white",
               outlier.size = 0.8, outlier.alpha = 0.4) +
  geom_point(data = moy_milieu, aes(x = milieu, y = moy),
             shape = 18, size = 4, color = "#D7191C", inherit.aes = FALSE) +
  annotate("text", x = 2.35,
           y = max(df_div$nb_cultures, na.rm = TRUE) * 0.88,
           label = paste0("Wilcoxon p = ",
                          format(test_wilcox_div$p.value, digits = 2, scientific = TRUE),
                          "\nr = ", round(r_div, 3)),
           size = 3.3, color = "grey25", fontface = "italic", hjust = 1) +
  scale_fill_manual(values = pal_milieu, guide = "none") +
  scale_y_continuous(breaks = seq(0, max(df_div$nb_cultures, na.rm = TRUE), by = 2)) +
  labs(
    title    = "Urbain vs Rural",
    subtitle = "Losange rouge = moyenne pondérée",
    x        = NULL,
    y        = "Nombre de cultures",
    caption  = "Source : GHS Panel W4 | Pondération : wt_wave4"
  ) +
  theme_tp5 +
  theme(panel.grid.major.x = element_blank())

p26 <- p26a + p26b +
  plot_annotation(
    title = "Diversification culturale des ménages — Wave 4 (2018)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("outputs/26_diversification_culturale.png", p26,
       width = 13, height = 6, dpi = 200, bg = "white")
cat("  Enregistré : outputs/26_diversification_culturale.png\n")

# Taux d'utilisation des intrants agricoles (W4)
# Source : secta11c2 (intrants réels par parcelle)
cat("\nQ27 : Analyse des intrants agricoles...\n")

# Plan de sondage pondéré
plan_intrants <- tbl_intrants_menage %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

colonnes_intrants   <- c("engrais_inorg", "npk", "urea",
                         "engrais_org", "pesticide", "herbicide")
etiquettes_intrants <- c("Engrais inorganique", "dont NPK", "dont Urée",
                         "Engrais organique",   "Pesticide", "Herbicide")

# Taux d'adoption avec IC 95% (via srvyr)
taux_adoption <- purrr::map2_dfr(colonnes_intrants, etiquettes_intrants,
                                 function(col, lbl) {
                                   plan_intrants %>%
                                     summarise(prop = survey_mean(!!sym(col), na.rm = TRUE, vartype = "ci")) %>%
                                     mutate(intrant = lbl)
                                 }) %>%
  mutate(
    pct    = prop * 100,
    ic_inf = prop_low * 100,
    ic_sup = prop_upp * 100,
    intrant = factor(intrant, levels = rev(etiquettes_intrants))
  )

cat("  Taux pondérés :\n")
print(taux_adoption %>% select(intrant, pct, ic_inf, ic_sup) %>%
        mutate(across(c(pct, ic_inf, ic_sup), ~round(., 1))))

# Barplot horizontal avec IC 95%
p27a <- ggplot(taux_adoption,
               aes(x = pct, y = intrant, fill = intrant,
                   xmin = ic_inf, xmax = ic_sup)) +
  geom_col(width = 0.65, alpha = 0.88) +
  geom_errorbarh(height = 0.25, color = "grey30", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 3.5, fontface = "bold", color = "#2C3E50") +
  scale_fill_manual(values = pal_intrant, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux d'adoption des intrants agricoles — Wave 4 (2018)",
    subtitle = "Part des ménages utilisateurs | Barres d'erreur : IC à 95%",
    x        = "Part des ménages (%)",
    y        = NULL,
    caption  = "Source : GHS Panel W4, secta11c2_harvestw4 | Pondération : wt_wave4"
  ) +
  theme_tp5 +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/27_intrants_taux_global.png", p27a,
       width = 10, height = 6, dpi = 200, bg = "white")

# Taux d'engrais inorganique par zone et milieu (barplot groupé avec IC 95%)
freq_zone_engrais <- tbl_intrants_menage %>%
  filter(!is.na(zone_label), !is.na(milieu), !is.na(wt_wave4)) %>%
  group_by(zone_label, milieu) %>%
  summarise(
    total    = n(),
    adopteurs = sum(engrais_inorg, na.rm = TRUE),
    taux      = adopteurs / total,
    ic_bas    = pmax(0, taux - 1.96 * sqrt(taux * (1 - taux) / total)),
    ic_haut   = pmin(1, taux + 1.96 * sqrt(taux * (1 - taux) / total)),
    .groups   = "drop"
  )

# Test du chi-deux zone × utilisation engrais inorganique
tab_chi2 <- xtabs(
  ~ zone_label + engrais_inorg,
  data = tbl_intrants_menage %>% filter(!is.na(zone_label))
)
chi2_res <- chisq.test(tab_chi2)
chi2_df  <- data.frame(
  Statistique  = round(chi2_res$statistic, 3),
  ddl          = chi2_res$parameter,
  p_value      = signif(chi2_res$p.value, 4),
  Significatif = ifelse(chi2_res$p.value < 0.05, "Oui", "Non")
)
write.csv(chi2_df, "outputs/27_chi2_zone_engrais.csv", row.names = FALSE)
cat("  Chi-deux zone × engrais : X² =", round(chi2_res$statistic, 2),
    "| p =", format(chi2_res$p.value, digits = 3), "\n")

p27b <- ggplot(freq_zone_engrais,
               aes(x = zone_label, y = taux * 100, fill = milieu,
                   ymin = ic_bas * 100, ymax = ic_haut * 100)) +
  geom_col(position = position_dodge(0.8), width = 0.72, alpha = 0.88) +
  geom_errorbar(position = position_dodge(0.8), width = 0.2,
                color = "grey30", linewidth = 0.6) +
  scale_fill_manual(values = pal_milieu, name = "Milieu") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
  labs(
    title    = "Adoption de l'engrais inorganique par zone et milieu",
    subtitle = paste0("Wave 4 (2018) | Chi-deux zone × adoption : X² = ",
                      round(chi2_res$statistic, 2),
                      ", p = ", format(chi2_res$p.value, digits = 2,
                                       scientific = TRUE),
                      " | IC 95%"),
    x        = NULL,
    y        = "Taux d'adoption (%)",
    caption  = "Source : GHS Panel W4, secta11c2_harvestw4 | Pondération : wt_wave4"
  ) +
  theme_tp5 +
  theme(axis.text.x = element_text(size = 9, lineheight = 0.9))

ggsave("outputs/27_intrants_par_zone.png", p27b,
       width = 11, height = 6.5, dpi = 200, bg = "white")
cat("  Enregistré : outputs/27_intrants_taux_global.png\n")
cat("  Enregistré : outputs/27_intrants_par_zone.png\n")

# Sauvegarde résultats
saveRDS(
  list(taux = taux_adoption, zone = freq_zone_engrais, chi2 = chi2_res),
  "data/processed/resultats_q27.rds"
)


#  Production maïs/millet par État (W4)
# Indicateur : production en kg/parcelle (secta3i × sa3iq6_conv)
cat("\nQ28 : Production maïs/millet par État...\n")

# Exclusion des valeurs extrêmes (IQR × 3), par culture
tbl_prod_propre <- tbl_production %>%
  filter(!is.na(production_kg), !is.na(nom_etat)) %>%
  group_by(culture) %>%
  mutate(
    q1_ref  = quantile(production_kg, 0.25, na.rm = TRUE),
    q3_ref  = quantile(production_kg, 0.75, na.rm = TRUE),
    iqr_ref = q3_ref - q1_ref,
    extreme = production_kg < (q1_ref - 3 * iqr_ref) |
      production_kg > (q3_ref + 3 * iqr_ref)
  ) %>%
  filter(!extreme) %>%
  ungroup()

n_out <- nrow(tbl_production) - nrow(tbl_prod_propre)
cat("  Observations retenues :", nrow(tbl_prod_propre),
    "/", nrow(tbl_production), "(", n_out, "outliers écartés)\n")

resume_prod_propre <- tbl_prod_propre %>%
  group_by(culture) %>%
  summarise(
    n       = n(),
    moy     = round(mean(production_kg,            na.rm = TRUE), 1),
    med     = round(median(production_kg,          na.rm = TRUE), 1),
    ecart_t = round(sd(production_kg,              na.rm = TRUE), 1),
    q1      = round(quantile(production_kg, 0.25,  na.rm = TRUE), 1),
    q3      = round(quantile(production_kg, 0.75,  na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n  Résumé production nettoyée :\n")
print(resume_prod_propre)

# Test de Kruskal-Wallis (maïs ~ État)
test_kw <- tbl_prod_propre %>%
  filter(culture == "Maïs") %>%
  kruskal_test(production_kg ~ nom_etat)
cat("\n  Kruskal-Wallis maïs ~ État : H =",
    round(test_kw$statistic, 2), "| p =",
    format(test_kw$p, digits = 3), "\n")

# Garder uniquement les États avec au moins 5 obs de maïs
etats_ok <- tbl_prod_propre %>%
  filter(culture == "Maïs") %>%
  count(nom_etat) %>%
  filter(n >= 5) %>%
  pull(nom_etat)

tbl_graphe28 <- tbl_prod_propre %>%
  filter(culture %in% c("Maïs", "Mil"),
         nom_etat %in% etats_ok) %>%
  mutate(nom_etat = fct_reorder(nom_etat, production_kg, .fun = median))

p28 <- ggplot(tbl_graphe28,
              aes(x = nom_etat, y = production_kg, fill = culture)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.5, outlier.alpha = 0.25,
               width = 0.6, position = position_dodge(0.8)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2,
               color = "#D7191C", position = position_dodge(0.8),
               show.legend = FALSE) +
  scale_fill_manual(values = c("Maïs" = "#2C7BB6", "Mil" = "#FDAE61"),
                    name = "Culture") +
  scale_y_continuous(labels = label_number(big.mark = " "),
                     expand = expansion(mult = c(0, 0.08))) +
  coord_flip() +
  labs(
    title    = "Production de maïs et mil par État — Wave 4 (2018)",
    subtitle = paste0("kg/parcelle | Valeurs extrêmes IQR×3 retirées (n=", n_out, " écartés)\n",
                      "Losange rouge = moyenne | Kruskal-Wallis maïs : H = ",
                      round(test_kw$statistic, 1), ", p < 0,001"),
    x        = NULL,
    y        = "Production (kg/parcelle)",
    caption  = "Source : GHS Panel W4, secta3i_harvestw4 | Pondération : wt_wave4"
  ) +
  theme_tp5 +
  theme(axis.text.y  = element_text(size = 8.5),
        legend.position = "top")

ggsave("outputs/28_production_par_etat.png", p28,
       width = 11, height = 14, dpi = 200, bg = "white")
cat("  Enregistré : outputs/28_production_par_etat.png\n")

saveRDS(list(kw = test_kw, stats = resume_prod_propre),
        "data/processed/resultats_q28.rds")

# Effet de l'engrais inorganique sur la production (W4)
cat("\nQ29 : Engrais inorganique et production...\n")

# Données nettoyées 
tbl_engrais_propre <- tbl_production %>%
  filter(!is.na(production_kg), !is.na(engrais_inorg)) %>%
  group_by(culture) %>%
  mutate(
    q1_ref  = quantile(production_kg, 0.25, na.rm = TRUE),
    q3_ref  = quantile(production_kg, 0.75, na.rm = TRUE),
    iqr_ref = q3_ref - q1_ref,
    extreme = production_kg < (q1_ref - 3 * iqr_ref) |
      production_kg > (q3_ref + 3 * iqr_ref)
  ) %>%
  filter(!extreme) %>%
  ungroup() %>%
  mutate(groupe_engrais = if_else(engrais_inorg,
                                  "Avec engrais inorg.",
                                  "Sans engrais inorg."))

# Fonction taille d'effet r de Rosenthal
calcul_r <- function(test, n) abs(qnorm(test$p.value / 2)) / sqrt(n)

# Tests de Wilcoxon par culture
test_w_mais   <- wilcox.test(
  production_kg ~ engrais_inorg,
  data = tbl_engrais_propre %>% filter(culture == "Maïs"),
  exact = FALSE
)
test_w_mil   <- wilcox.test(
  production_kg ~ engrais_inorg,
  data = tbl_engrais_propre %>% filter(culture == "Mil"),
  exact = FALSE
)

n_mais  <- nrow(tbl_engrais_propre %>% filter(culture == "Maïs"))
n_mil   <- nrow(tbl_engrais_propre %>% filter(culture == "Mil"))
r_mais  <- calcul_r(test_w_mais, n_mais)
r_mil   <- calcul_r(test_w_mil,  n_mil)

cat("  Wilcoxon maïs : W =", round(test_w_mais$statistic, 0),
    "| p =", format(test_w_mais$p.value, digits = 3, scientific = TRUE),
    "| r =", round(r_mais, 3), "\n")
cat("  Wilcoxon mil  : W =", round(test_w_mil$statistic, 0),
    "| p =", format(test_w_mil$p.value, digits = 3, scientific = TRUE),
    "| r =", round(r_mil, 3), "\n")

resume_engrais <- tbl_engrais_propre %>%
  group_by(culture, groupe_engrais) %>%
  summarise(
    n   = n(),
    med = round(median(production_kg, na.rm = TRUE), 1),
    moy = round(mean(production_kg,   na.rm = TRUE), 1),
    q1  = round(quantile(production_kg, 0.25, na.rm = TRUE), 1),
    q3  = round(quantile(production_kg, 0.75, na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n  Résumé production × engrais :\n")
print(resume_engrais)

note_mais <- paste0("Maïs : p = ",
                    format(test_w_mais$p.value, digits = 2, scientific = TRUE),
                    " | r = ", round(r_mais, 3))
note_mil  <- paste0("Mil  : p = ",
                    format(test_w_mil$p.value,  digits = 2, scientific = TRUE),
                    " | r = ", round(r_mil, 3))

p29 <- ggplot(tbl_engrais_propre,
              aes(x = groupe_engrais, y = production_kg, fill = groupe_engrais)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.18, fill = "white",
               outlier.size = 0.7, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "#D7191C") +
  facet_wrap(~culture, scales = "free_y") +
  scale_fill_manual(values = pal_engrais, guide = "none") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title    = "Production selon l'engrais inorganique — Wave 4 (2018)",
    subtitle = "kg/parcelle | Extrêmes IQR×3 retirés | Losange rouge = moyenne",
    x        = NULL,
    y        = "Production (kg/parcelle)",
    caption  = paste0("Source : GHS Panel W4, secta3i + secta11c2 | Pondération : wt_wave4\n",
                      note_mais, "   |   ", note_mil)
  ) +
  theme_tp5 +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("outputs/29_effet_engrais.png", p29,
       width = 10, height = 6, dpi = 200, bg = "white")
cat("  Enregistré : outputs/29_effet_engrais.png\n")

saveRDS(
  list(wilcox_mais = test_w_mais, wilcox_mil = test_w_mil,
       r_mais = r_mais, r_mil = r_mil, stats = resume_engrais),
  "data/processed/resultats_q29.rds"
)

cat("\nToutes les analyses terminées. Résultats dans outputs/\n")