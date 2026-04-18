# ===========================================================================
# Script 02 : Statistiques descriptives et visualisations
# Thème 5 — Analyse des cultures, intrants et rendements agricoles
# Enquête GHS-Panel Nigeria — Vague 4 (2018/19)
#
# Sorties produites :
#   graph01 — 15 cultures les plus répandues (Q25)
#   graph02 — Diversification culturale : histogramme + violin (Q26)
#   graph03 — Taux d'usage des intrants par catégorie (Q27)
#   graph04 — Adoption d'engrais par zone et milieu (Q27)
#   graph05 — Rendement maïs/millet par État en kg/ha (Q28)
#   graph06 — Impact de l'engrais inorganique sur le rendement kg/ha (Q29)
#   synthese_intrants.xlsx — tableaux récapitulatifs (3 onglets)
#
# Réalisé par : David Landry AGNANGMA SANAM | Hamadou DICKO
# ===========================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(rstatix)
library(ggpubr)
library(patchwork)
library(survey)
library(srvyr)
library(openxlsx)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ── Récupération des objets préparés ──────────────────────────────────────
if (!file.exists("data/processed/ref_poids.rds")) {
  cat(">> Données absentes — lancement du script 01...\n")
  source("scripts/01_chargement_preparation.R")
}

ref_poids            <- readRDS("data/processed/ref_poids.rds")
nomenclature_cultures<- readRDS("data/processed/nomenclature_cultures.rds")
tbl_cultures         <- readRDS("data/processed/tbl_cultures.rds")
tbl_diversification  <- readRDS("data/processed/tbl_diversification.rds")
tbl_intrants_parcelle<- readRDS("data/processed/tbl_intrants_parcelle.rds")
tbl_intrants_menage  <- readRDS("data/processed/tbl_intrants_menage.rds")
tbl_production       <- readRDS("data/processed/tbl_production.rds")
tbl_parcelle_surface <- readRDS("data/processed/tbl_parcelle_surface.rds")
effectif_pondere     <- readRDS("data/processed/effectif_pondere.rds")

# ── Jeu de couleurs et thème graphique ────────────────────────────────────
couleurs_famille <- c(
  "Céréale"          = "#1A5276",
  "Légumineuse"      = "#1E8449",
  "Tubercule"        = "#D35400",
  "Culture de rente" = "#8E44AD",
  "Légume"           = "#117A65",
  "Autre"            = "#7F8C8D"
)
couleurs_milieu  <- c("Urbain" = "#C0392B", "Rural" = "#1A5276")
couleurs_engrais <- c("Avec engrais inorg." = "#1A5276",
                      "Sans engrais inorg." = "#E67E22")
couleurs_intrant <- c(
  "Engrais inorganique" = "#1A5276",
  "dont NPK"            = "#2E86C1",
  "dont Urée"           = "#85C1E9",
  "Engrais organique"   = "#1E8449",
  "Pesticide"           = "#8E44AD",
  "Herbicide"           = "#D35400"
)

theme_projet <- theme_light(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", colour = "#2C3E50",
                                     size = 13, margin = margin(b = 6)),
    plot.subtitle     = element_text(colour = "#5D6D7E", size = 10,
                                      margin = margin(b = 10)),
    plot.caption      = element_text(colour = "grey45", size = 8.5, hjust = 1,
                                      margin = margin(t = 10)),
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    plot.margin       = margin(12, 15, 10, 10)
  )

# ═══════════════════════════════════════════════════════════════════════════
# Q25 — Les 15 cultures les plus répandues (W4)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n>> Q25 : Classement des cultures les plus pratiquées...\n")

freq_cultures <- tbl_cultures %>%
  filter(!is.na(wt_wave4)) %>%
  group_by(cropcode, nom_culture, famille) %>%
  summarise(freq_pond = sum(wt_wave4), .groups = "drop")

classement_15 <- freq_cultures %>%
  arrange(desc(freq_pond)) %>%
  slice_head(n = 15) %>%
  mutate(
    proportion = freq_pond / effectif_pondere * 100,
    nom_culture = reorder(nom_culture, proportion),
    famille = factor(famille,
                     levels = c("Céréale","Légumineuse","Tubercule",
                                "Culture de rente","Légume","Autre"))
  )

graph01 <- ggplot(classement_15, aes(x = proportion, y = nom_culture, fill = famille)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.1, size = 3.3, fontface = "bold", color = "#2C3E50") +
  scale_fill_manual(values = couleurs_famille, name = "Famille de culture") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Les 15 cultures les plus pratiquées — Wave 4, 2018",
    subtitle = paste0("Part des ménages agricoles cultivant chaque espèce\n",
                      "Effectif pondéré = ", format(round(effectif_pondere), big.mark = " ")),
    x = "Part des ménages agricoles (%)", y = NULL,
    caption  = "Source : GHS-W4, secta3i_harvestw4 | Pondération : wt_wave4 | Groupe 2"
  ) +
  theme_projet +
  theme(panel.grid.major.y = element_blank(),
        legend.key.size    = unit(0.4, "cm"))

ggsave("outputs/graph01_classement_cultures.png", graph01,
       width = 10, height = 7, dpi = 150)
cat("   [OK] graph01\n")

# ═══════════════════════════════════════════════════════════════════════════
# Q26 — Diversification culturale (W4)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n>> Q26 : Analyse de la diversification culturale...\n")

plan_diversif <- tbl_diversification %>%
  filter(!is.na(wt_wave4), !is.na(type_milieu)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

resume_diversif <- plan_diversif %>%
  group_by(type_milieu) %>%
  summarise(
    moyenne = survey_mean(nb_cultures, vartype = "ci"),
    mediane = survey_quantile(nb_cultures, quantiles = 0.5, vartype = NULL),
    .groups = "drop"
  ) %>% as.data.frame()

test_wilcox <- wilcox.test(
  nb_cultures ~ type_milieu,
  data = tbl_diversification %>% filter(!is.na(type_milieu)),
  conf.int = TRUE
)
n_obs_wilcox <- nrow(tbl_diversification %>% filter(!is.na(type_milieu)))
effet_r <- abs(qnorm(test_wilcox$p.value / 2)) / sqrt(n_obs_wilcox)

moyennes_milieu <- tbl_diversification %>%
  filter(!is.na(type_milieu), !is.na(wt_wave4)) %>%
  group_by(type_milieu) %>%
  summarise(moy = weighted.mean(nb_cultures, wt_wave4, na.rm = TRUE),
            .groups = "drop")

graph02a <- ggplot(tbl_diversification, aes(x = nb_cultures, weight = wt_wave4)) +
  geom_histogram(binwidth = 1, fill = "#1A5276", color = "white", alpha = 0.85) +
  scale_x_continuous(breaks = 1:max(tbl_diversification$nb_cultures, na.rm = TRUE)) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Répartition du nombre de cultures",
       subtitle = "Wave 4 (2018) — effectifs pondérés",
       x = "Nombre de cultures", y = "Effectifs pondérés",
       caption = "Source : GHS-W4, secta3i | Pondération : wt_wave4 | Groupe 2") +
  theme_projet

graph02b <- ggplot(
  tbl_diversification %>% filter(!is.na(type_milieu)),
  aes(x = type_milieu, y = nb_cultures, fill = type_milieu)
) +
  geom_violin(aes(weight = wt_wave4), alpha = 0.65, trim = FALSE) +
  geom_boxplot(aes(weight = wt_wave4), width = 0.14, fill = "white",
               outlier.size = 0.8, outlier.alpha = 0.4) +
  geom_point(data = moyennes_milieu, aes(x = type_milieu, y = moy),
             shape = 18, size = 4, color = "#E74C3C", inherit.aes = FALSE) +
  annotate("text", x = 2.35,
           y = max(tbl_diversification$nb_cultures, na.rm = TRUE) * 0.88,
           label = paste0("Wilcoxon p = ",
                          format(test_wilcox$p.value, digits = 2, scientific = TRUE),
                          "\nr = ", round(effet_r, 3)),
           size = 3.3, color = "grey25", fontface = "italic", hjust = 1) +
  scale_fill_manual(values = couleurs_milieu, guide = "none") +
  scale_y_continuous(breaks = seq(0, max(tbl_diversification$nb_cultures, na.rm = TRUE), by = 2)) +
  labs(title    = "Urbain vs Rural",
       subtitle = "Losange rouge = moyenne pondérée",
       x = NULL, y = "Nombre de cultures",
       caption = "Source : GHS-W4 | Pondération : wt_wave4 | Groupe 2") +
  theme_projet +
  theme(panel.grid.major.x = element_blank())

graph02 <- graph02a + graph02b +
  plot_annotation(
    title = "Diversification culturale des ménages — Wave 4 (2018)",
    theme = theme(plot.title = element_text(face = "bold", size = 14,
                                             colour = "#2C3E50"))
  )

ggsave("outputs/graph02_diversification.png", graph02,
       width = 13, height = 6, dpi = 150)
cat("   [OK] graph02\n")

saveRDS(list(wilcox = test_wilcox, r = effet_r, stats = resume_diversif),
        "data/processed/resultats_q26.rds")

# ═══════════════════════════════════════════════════════════════════════════
# Q27 — Taux d'adoption des intrants (W4)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n>> Q27 : Taux d'utilisation des intrants...\n")

plan_intrants <- tbl_intrants_menage %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(ids = hhid, weights = wt_wave4, nest = TRUE)

colonnes_intrants <- c("engrais_inorg", "npk", "urea",
                       "engrais_org",   "pesticide", "herbicide")
etiquettes_intrants <- c("Engrais inorganique", "dont NPK", "dont Urée",
                         "Engrais organique",   "Pesticide", "Herbicide")

taux_adoption <- purrr::map2_dfr(colonnes_intrants, etiquettes_intrants, function(col, lbl) {
  plan_intrants %>%
    summarise(prop = survey_mean(!!sym(col), na.rm = TRUE, vartype = "ci")) %>%
    mutate(intrant = lbl)
}) %>%
  mutate(
    pct     = prop * 100,
    ic_inf  = prop_low * 100,
    ic_sup  = prop_upp * 100,
    intrant = factor(intrant, levels = rev(etiquettes_intrants))
  )

graph03 <- ggplot(taux_adoption,
                  aes(x = pct, y = intrant, fill = intrant,
                      xmin = ic_inf, xmax = ic_sup)) +
  geom_col(width = 0.65, alpha = 0.88) +
  geom_errorbarh(height = 0.25, color = "grey30", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 3.5, fontface = "bold", color = "#2C3E50") +
  scale_fill_manual(values = couleurs_intrant, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux d'adoption des intrants agricoles — Wave 4 (2018)",
    subtitle = "Part des ménages utilisateurs | Barres d'erreur : IC à 95%",
    x = "Part des ménages (%)", y = NULL,
    caption  = "Source : GHS-W4, secta11c2_harvestw4 | Pondération : wt_wave4 | Groupe 2"
  ) +
  theme_projet +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/graph03_intrants.png", graph03,
       width = 10, height = 6, dpi = 150)
cat("   [OK] graph03\n")

# Graph04 — Engrais inorganique par zone et milieu
freq_zone_engrais <- tbl_intrants_menage %>%
  filter(!is.na(nom_zone), !is.na(type_milieu), !is.na(wt_wave4)) %>%
  group_by(nom_zone, type_milieu) %>%
  summarise(
    total    = n(),
    adopteurs = sum(engrais_inorg, na.rm = TRUE),
    taux      = adopteurs / total,
    ic_bas    = pmax(0, taux - 1.96 * sqrt(taux * (1 - taux) / total)),
    ic_haut   = pmin(1, taux + 1.96 * sqrt(taux * (1 - taux) / total)),
    .groups   = "drop"
  )

test_chi2 <- chisq.test(
  table(tbl_intrants_menage$type_milieu,
        tbl_intrants_menage$engrais_inorg)
)

graph04 <- ggplot(freq_zone_engrais,
                  aes(x = nom_zone, y = taux * 100, fill = type_milieu,
                      ymin = ic_bas * 100, ymax = ic_haut * 100)) +
  geom_col(position = position_dodge(0.8), width = 0.72, alpha = 0.88) +
  geom_errorbar(position = position_dodge(0.8), width = 0.2,
                color = "grey30", linewidth = 0.6) +
  scale_fill_manual(values = couleurs_milieu, name = "Milieu") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
  labs(
    title    = "Adoption de l'engrais inorganique par zone et milieu",
    subtitle = paste0("Wave 4 (2018) | Chi-deux milieu × adoption : X² = ",
                      round(test_chi2$statistic, 2), ", p < 0,001 | IC 95%"),
    x = NULL, y = "Taux d'adoption (%)",
    caption  = "Source : GHS-W4, secta11c2_harvestw4 | Pondération : wt_wave4 | Groupe 2"
  ) +
  theme_projet +
  theme(axis.text.x = element_text(size = 9, lineheight = 0.9))

ggsave("outputs/graph04_engrais_zone.png", graph04,
       width = 11, height = 6.5, dpi = 150)
cat("   [OK] graph04\n")

saveRDS(list(tab_taux = taux_adoption, tab_eng_zone = freq_zone_engrais, chi_eng = test_chi2),
        "data/processed/resultats_q27.rds")

# ═══════════════════════════════════════════════════════════════════════════
# Q28 — Rendement maïs/millet par État en kg/ha (W4)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n>> Q28 : Rendement par État (kg/ha)...\n")

# Filtrer les parcelles avec rendement disponible
tbl_rendement <- tbl_production %>%
  filter(!is.na(rendement_kg_ha), !is.na(nom_etat))

# Nettoyage des valeurs extrêmes (IQR × 3)
tbl_rdt_propre <- tbl_rendement %>%
  group_by(culture) %>%
  mutate(
    q1_ref   = quantile(rendement_kg_ha, 0.25, na.rm = TRUE),
    q3_ref   = quantile(rendement_kg_ha, 0.75, na.rm = TRUE),
    iqr_ref  = q3_ref - q1_ref,
    extreme  = rendement_kg_ha < (q1_ref - 3 * iqr_ref) |
               rendement_kg_ha > (q3_ref + 3 * iqr_ref)
  ) %>%
  filter(!extreme) %>%
  ungroup()

cat("   Observations retenues :",
    nrow(tbl_rdt_propre), "/", nrow(tbl_rendement), "\n")

resume_rendement <- tbl_rdt_propre %>%
  group_by(culture) %>%
  summarise(
    n       = n(),
    moy     = round(mean(rendement_kg_ha,            na.rm = TRUE), 1),
    med     = round(median(rendement_kg_ha,          na.rm = TRUE), 1),
    ecart_t = round(sd(rendement_kg_ha,              na.rm = TRUE), 1),
    q1      = round(quantile(rendement_kg_ha, 0.25,  na.rm = TRUE), 1),
    q3      = round(quantile(rendement_kg_ha, 0.75,  na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n   Résumé rendement nettoyé (kg/ha) :\n"); print(resume_rendement)

# Test de Kruskal-Wallis (maïs)
test_kw <- tbl_rdt_propre %>%
  filter(culture == "Maïs") %>%
  kruskal_test(rendement_kg_ha ~ nom_etat)
cat("\n   Kruskal-Wallis rendement maïs ~ État : H =",
    round(test_kw$statistic, 2), "| p =",
    format(test_kw$p, digits = 3), "\n")

etats_suffisants <- tbl_rdt_propre %>%
  filter(culture == "Maïs") %>%
  count(nom_etat) %>% filter(n >= 5) %>% pull(nom_etat)

tbl_pour_graphe <- tbl_rdt_propre %>%
  filter(culture %in% c("Maïs", "Millet"),
         nom_etat %in% etats_suffisants) %>%
  mutate(nom_etat = fct_reorder(nom_etat, rendement_kg_ha, .fun = median))

graph05 <- ggplot(tbl_pour_graphe,
                  aes(x = nom_etat, y = rendement_kg_ha, fill = culture)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.5, outlier.alpha = 0.25,
               width = 0.6, position = position_dodge(0.8)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2,
               color = "#E74C3C", position = position_dodge(0.8),
               show.legend = FALSE) +
  scale_fill_manual(values = c("Maïs" = "#1A5276", "Millet" = "#E67E22"),
                    name = "Culture") +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.08))) +
  coord_flip() +
  labs(
    title    = "Rendement du maïs et du millet par État — Wave 4 (2018)",
    subtitle = paste0("kg/ha (superficie GPS ou auto-déclarée) | Extrêmes IQR×3 retirés\n",
                      "Losange rouge = moyenne | Kruskal-Wallis maïs : H = ",
                      round(test_kw$statistic, 1), ", p < 0,001"),
    x = NULL, y = "Rendement (kg/ha)",
    caption  = "Source : GHS-W4, secta3i + secta1 | Traitement : Groupe 2"
  ) +
  theme_projet +
  theme(axis.text.y = element_text(size = 8.5),
        legend.position = "top")

ggsave("outputs/graph05_rendement_etats.png", graph05,
       width = 11, height = 14, dpi = 150)
cat("   [OK] graph05\n")

saveRDS(list(kw_mais = test_kw, stats = resume_rendement),
        "data/processed/resultats_q28.rds")

# ═══════════════════════════════════════════════════════════════════════════
# Q29 — Rendement selon l'engrais inorganique (W4)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n>> Q29 : Impact de l'engrais inorganique sur le rendement...\n")

tbl_eng_propre <- tbl_production %>%
  filter(!is.na(rendement_kg_ha), !is.na(engrais_inorg)) %>%
  group_by(culture) %>%
  mutate(
    q1_ref  = quantile(rendement_kg_ha, 0.25, na.rm = TRUE),
    q3_ref  = quantile(rendement_kg_ha, 0.75, na.rm = TRUE),
    iqr_ref = q3_ref - q1_ref,
    extreme = rendement_kg_ha < (q1_ref - 3 * iqr_ref) |
              rendement_kg_ha > (q3_ref + 3 * iqr_ref)
  ) %>%
  filter(!extreme) %>%
  ungroup()

calcul_r <- function(test, n) abs(qnorm(test$p.value / 2)) / sqrt(n)

test_w_mais <- wilcox.test(
  rendement_kg_ha ~ engrais_inorg,
  data = tbl_eng_propre %>% filter(culture == "Maïs")
)
test_w_millet <- wilcox.test(
  rendement_kg_ha ~ engrais_inorg,
  data = tbl_eng_propre %>% filter(culture == "Millet")
)

n_mais   <- nrow(tbl_eng_propre %>% filter(culture == "Maïs"))
n_millet <- nrow(tbl_eng_propre %>% filter(culture == "Millet"))
r_mais   <- calcul_r(test_w_mais,   n_mais)
r_millet <- calcul_r(test_w_millet, n_millet)

cat("   Wilcoxon maïs   : W =", round(test_w_mais$statistic, 0),
    "| p =", format(test_w_mais$p.value, digits = 3, scientific = TRUE),
    "| r =", round(r_mais, 3), "\n")
cat("   Wilcoxon millet : W =", round(test_w_millet$statistic, 0),
    "| p =", format(test_w_millet$p.value, digits = 3, scientific = TRUE),
    "| r =", round(r_millet, 3), "\n")

resume_engrais <- tbl_eng_propre %>%
  mutate(groupe_engrais = ifelse(engrais_inorg,
                                 "Avec engrais inorg.",
                                 "Sans engrais inorg.")) %>%
  group_by(culture, groupe_engrais) %>%
  summarise(
    n   = n(),
    med = round(median(rendement_kg_ha, na.rm = TRUE), 1),
    moy = round(mean(rendement_kg_ha,   na.rm = TRUE), 1),
    q1  = round(quantile(rendement_kg_ha, 0.25, na.rm = TRUE), 1),
    q3  = round(quantile(rendement_kg_ha, 0.75, na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n   Résumé rendement × engrais :\n"); print(resume_engrais)

note_mais   <- paste0("Maïs : p = ",
                       format(test_w_mais$p.value, digits = 2, scientific = TRUE),
                       " | r = ", round(r_mais, 3))
note_millet <- paste0("Millet : p = ",
                       format(test_w_millet$p.value, digits = 2, scientific = TRUE),
                       " | r = ", round(r_millet, 3))

graph06 <- ggplot(
  tbl_eng_propre %>%
    mutate(groupe_engrais = ifelse(engrais_inorg,
                                   "Avec engrais inorg.",
                                   "Sans engrais inorg.")),
  aes(x = groupe_engrais, y = rendement_kg_ha, fill = groupe_engrais)
) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.18, fill = "white",
               outlier.size = 0.7, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, fill = "#E74C3C", color = "#E74C3C") +
  facet_wrap(~culture, scales = "free_y") +
  scale_fill_manual(values = couleurs_engrais, guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Rendement selon l'engrais inorganique — Wave 4 (2018)",
    subtitle = "kg/ha (superficie GPS/auto-déclarée) | Extrêmes IQR×3 retirés | Losange = moyenne",
    x = NULL, y = "Rendement (kg/ha)",
    caption  = paste0("Source : GHS-W4, secta3i + secta1 + secta11c2 | Groupe 2\n",
                       note_mais, "   |   ", note_millet)
  ) +
  theme_projet +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("outputs/graph06_rendement_engrais.png", graph06,
       width = 10, height = 6, dpi = 150)
cat("   [OK] graph06\n")

saveRDS(list(wilcox_mais   = test_w_mais,
             wilcox_millet = test_w_millet,
             r_mais        = r_mais,
             r_millet      = r_millet,
             stats         = resume_engrais),
        "data/processed/resultats_q29.rds")

# ═══════════════════════════════════════════════════════════════════════════
# EXPORT EXCEL — 3 onglets
# ═══════════════════════════════════════════════════════════════════════════
cat("\n>> Création du fichier Excel récapitulatif...\n")

wb <- createWorkbook()

s_titre  <- createStyle(fontName = "Arial", fontSize = 13, textDecoration = "bold",
                         fontColour = "#1A5276")
s_source <- createStyle(fontName = "Arial", fontSize = 9, fontColour = "grey50",
                         textDecoration = "italic")
s_entete <- createStyle(fontName = "Arial", fontSize = 11, textDecoration = "bold",
                         halign = "center", fgFill = "#1A5276", fontColour = "white",
                         border = "TopBottomLeftRight", borderColour = "white")
s_corps  <- createStyle(fontName = "Arial", fontSize = 10, halign = "center",
                         border = "TopBottomLeftRight", borderColour = "#CCCCCC",
                         wrapText = TRUE)
s_alterne <- createStyle(fgFill = "#EAF2F8")
s_note   <- createStyle(fontName = "Arial", fontSize = 9, fontColour = "grey50",
                         textDecoration = "italic")

creer_onglet <- function(wb, onglet, titre, source, donnees, annotation = NULL) {
  addWorksheet(wb, onglet)
  writeData(wb, onglet, titre,  startRow = 1, startCol = 1)
  writeData(wb, onglet, source, startRow = 2, startCol = 1)
  writeData(wb, onglet, donnees, startRow = 4, startCol = 1, headerStyle = s_entete)
  nl <- nrow(donnees); nc <- ncol(donnees)
  addStyle(wb, onglet, s_corps, rows = 4:(nl + 4), cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  for (i in seq(2, nl, by = 2))
    addStyle(wb, onglet, s_alterne, rows = i + 4, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, onglet, s_titre,  rows = 1, cols = 1)
  addStyle(wb, onglet, s_source, rows = 2, cols = 1)
  if (!is.null(annotation)) {
    writeData(wb, onglet, annotation, startRow = nl + 6, startCol = 1)
    addStyle(wb, onglet, s_note, rows = nl + 6, cols = 1)
  }
}

# Onglet 1 : Adoption
onglet1 <- taux_adoption %>%
  as.data.frame() %>%
  transmute(
    Intrant              = as.character(intrant),
    `Taux pondéré (%)`   = round(pct, 1),
    `IC inf. 95% (%)`    = round(ic_inf, 1),
    `IC sup. 95% (%)`    = round(ic_sup, 1)
  ) %>%
  arrange(desc(`Taux pondéré (%)`))

creer_onglet(wb, "Adoption_Intrants",
             "Taux d'adoption des intrants — Wave 4 (2018)",
             "Source : GHS-W4, secta11c2 | Pondération : wt_wave4 | Groupe 2",
             onglet1,
             "Note : Adoption définie au niveau ménage (au moins une parcelle).")
setColWidths(wb, "Adoption_Intrants", cols = 1:4, widths = c(25, 18, 15, 15))

# Onglet 2 : Rendement
onglet2 <- resume_rendement %>%
  rename(Culture = culture, N = n, `Moyenne (kg/ha)` = moy, `Médiane (kg/ha)` = med,
         `Éc. type` = ecart_t, Q1 = q1, Q3 = q3)

creer_onglet(wb, "Rendement_Mais_Millet",
             "Rendement maïs et millet (kg/ha) — Wave 4 (2018)",
             "Source : GHS-W4, secta3i + secta1 | Superficie GPS/auto-déclarée | Groupe 2",
             onglet2)
setColWidths(wb, "Rendement_Mais_Millet", cols = 1:8, widths = c(10, 8, 16, 16, 10, 8, 8, 8))

# Onglet 3 : Rendement × engrais + tests
onglet3 <- resume_engrais %>%
  rename(Culture = culture, `Groupe` = groupe_engrais,
         N = n, `Médiane (kg/ha)` = med, `Moyenne (kg/ha)` = moy, Q1 = q1, Q3 = q3)

tbl_tests <- data.frame(
  Culture = c("Maïs", "Millet"),
  `Stat. W` = c(round(test_w_mais$statistic, 0),
                round(test_w_millet$statistic, 0)),
  `p-valeur` = c(format(test_w_mais$p.value, digits = 3, scientific = TRUE),
                 format(test_w_millet$p.value, digits = 3, scientific = TRUE)),
  `r de Rosenthal` = c(round(r_mais, 3), round(r_millet, 3)),
  `Taille d'effet` = c(
    ifelse(r_mais   < 0.1, "Négligeable", ifelse(r_mais   < 0.3, "Petite",
           ifelse(r_mais   < 0.5, "Moyenne", "Grande"))),
    ifelse(r_millet < 0.1, "Négligeable", ifelse(r_millet < 0.3, "Petite",
           ifelse(r_millet < 0.5, "Moyenne", "Grande")))
  ),
  check.names = FALSE
)

creer_onglet(wb, "Rendement_Engrais",
             "Rendement (kg/ha) selon l'engrais inorganique — W4 (2018)",
             "Source : GHS-W4, secta3i + secta1 + secta11c2 | Extrêmes retirés | Groupe 2",
             onglet3)

nl3 <- nrow(onglet3)
writeData(wb, "Rendement_Engrais",
          "Tests de Wilcoxon-Mann-Whitney (avec vs sans engrais inorganique)",
          startRow = nl3 + 7, startCol = 1)
addStyle(wb, "Rendement_Engrais",
         createStyle(fontName = "Arial", textDecoration = "bold", fontColour = "#1A5276"),
         rows = nl3 + 7, cols = 1)
writeData(wb, "Rendement_Engrais", tbl_tests,
          startRow = nl3 + 8, startCol = 1, headerStyle = s_entete)
addStyle(wb, "Rendement_Engrais", s_corps,
         rows = (nl3 + 8):(nl3 + 10), cols = 1:5, gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Rendement_Engrais", cols = 1:7,
             widths = c(10, 22, 8, 16, 16, 8, 8))

saveWorkbook(wb, "outputs/synthese_intrants.xlsx", overwrite = TRUE)
cat("   [OK] synthese_intrants.xlsx (3 onglets)\n")

cat("\n====== Analyses TP5 terminées avec succès ======\n")
cat("Graphiques dans outputs/ :\n")
for (f in list.files("outputs/", pattern = "\\.png$")) cat("  >>", f, "\n")
cat("Tableau Excel : outputs/synthese_intrants.xlsx\n")
