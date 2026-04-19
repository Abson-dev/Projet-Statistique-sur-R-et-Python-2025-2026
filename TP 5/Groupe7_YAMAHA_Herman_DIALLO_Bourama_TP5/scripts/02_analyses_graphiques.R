# =============================================================================
# Script 02 : Analyses descriptives et représentations graphiques
# Thème 5 — Cultures pratiquées, intrants utilisés et rendements agricoles
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Figures produites :
#   fig01 — Top 15 cultures les plus fréquentes (Q25)
#   fig02 — Diversification culturale : histogramme + violin plot (Q26)
#   fig03 — Taux d'adoption des intrants par type (Q27)
#   fig04 — Adoption de l'engrais inorganique par zone et milieu (Q27)
#   fig05 — Rendements maïs/mil par État — boxplots (Q28)
#   fig06 — Rendements selon utilisation engrais inorganique — violin (Q29)
#   tableau_intrants.xlsx — récapitulatif pondéré (3 feuilles)
#
# Corrections apportées :
#   - Noms des cultures traduits en français (via cultures_dict du script 01)
#   - fig02 : pourcentages affichés au-dessus de chaque barre de l'histogramme
#   - fig03 : labels % positionnés après ic_sup (plus de chevauchement avec les
#             segments d'IC)
#   - fig04 : pourcentages affichés sur les barres (au-dessus de ic_sup)
#   - fig06 : losange de la moyenne centré dans la boîte (nudge supprimé)
#
# Auteurs : Herman YAMAHA | Bourama DIALLO
# =============================================================================

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
library(purrr)
library(stringr)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ── Chargement des objets ────────────────────────────────────────────────────
if (!file.exists("data/processed/df_poids.rds")) {
  cat("Objets absents — exécution de 01_import_nettoyage.R...\n")
  source("scripts/01_import_nettoyage.R")
}

df_poids           <- readRDS("data/processed/df_poids.rds")
cultures_dict      <- readRDS("data/processed/cultures_dict.rds")
df_cultures_w4     <- readRDS("data/processed/df_cultures_w4.rds")
df_diversif_w4     <- readRDS("data/processed/df_diversif_w4.rds")
df_intrants_w4     <- readRDS("data/processed/df_intrants_w4.rds")
df_intrants_menage <- readRDS("data/processed/df_intrants_menage.rds")
df_crop_intrants   <- readRDS("data/processed/df_crop_intrants.rds")
N_pond_total       <- readRDS("data/processed/N_pond_total.rds")
meta_sup           <- readRDS("data/processed/meta_superficie.rds")

# ── Palettes et thème ────────────────────────────────────────────────────────
palette_type <- c(
  "Céréale"          = "#1A5276",
  "Légumineuse"      = "#1E8449",
  "Tubercule"        = "#D35400",
  "Culture de rente" = "#8E44AD",
  "Légume"           = "#117A65",
  "Autre"            = "#7F8C8D"
)
palette_milieu  <- c("Urbain" = "#C0392B", "Rural" = "#1A5276")
palette_engrais <- c("Avec engrais inorg." = "#1A5276",
                     "Sans engrais inorg." = "#E67E22")
palette_intrant <- c(
  "Engrais inorganique" = "#1A5276",
  "dont NPK"            = "#2E86C1",
  "dont Urée"           = "#85C1E9",
  "Engrais organique"   = "#1E8449",
  "Pesticide"           = "#8E44AD",
  "Herbicide"           = "#D35400"
)

# Thème commun — lisible en PDF A4
theme_tp5 <- theme_light(base_size = 11) +
  theme(
    plot.title       = element_text(face="bold", colour="#2C3E50", size=12,
                                    margin=margin(b=4)),
    plot.subtitle    = element_text(colour="#5D6D7E", size=9, lineheight=1.3,
                                    margin=margin(b=6)),
    plot.caption     = element_text(colour="grey45", size=7.5, hjust=1,
                                    margin=margin(t=5)),
    plot.margin      = margin(8, 16, 8, 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour="grey90", linewidth=0.4),
    legend.position  = "bottom",
    legend.key.size  = unit(0.42, "cm"),
    legend.text      = element_text(size=8.5),
    legend.title     = element_text(size=9, face="bold"),
    axis.text        = element_text(size=8.5, colour="grey25"),
    axis.title       = element_text(size=9)
  )

# stat_summary helper — losange orange = moyenne, centré sur les boxplots
mean_point <- stat_summary(
  fun = mean, geom = "point",
  shape = 23, size = 3.0,
  fill = "#F39C12", colour = "#2C3E50", stroke = 0.5
)

# =============================================================================
# Q25 — Top 15 cultures les plus fréquentes (W4)
# =============================================================================
cat("\n--- Q25 : Top 15 cultures (W4) ---\n")

tab_cult <- df_cultures_w4 %>%
  filter(!is.na(wt_wave4)) %>%
  group_by(cropcode, crop_name, crop_type) %>%
  summarise(n_pond = sum(wt_wave4), .groups = "drop")

top15 <- tab_cult %>%
  arrange(desc(n_pond)) %>%
  slice_head(n = 15) %>%
  mutate(
    pct       = n_pond / N_pond_total * 100,
    crop_name = reorder(crop_name, pct),
    crop_type = factor(crop_type, levels = c("Céréale","Légumineuse","Tubercule",
                                             "Culture de rente","Légume","Autre"))
  )

cat("Top 15 :\n")
print(top15 %>% arrange(desc(pct)) %>%
        select(crop_name, crop_type, pct) %>% mutate(pct=round(pct,1)))

fig01 <- ggplot(top15, aes(x=pct, y=crop_name, fill=crop_type)) +
  geom_col(width=0.70, colour="white", linewidth=0.25) +
  geom_text(aes(label=paste0(round(pct,1),"%")),
            hjust=-0.12, size=3.1, fontface="bold", colour="#2C3E50") +
  scale_fill_manual(values=palette_type, name="Type de culture") +
  scale_x_continuous(expand=expansion(mult=c(0, 0.22)),
                     labels=function(x) paste0(x,"%")) +
  labs(
    title   = "Top 15 des cultures pratiquées — Wave 4 (2018)",
    subtitle= paste0("% des ménages agricoles pratiquant chaque culture | N pondéré = ",
                     format(round(N_pond_total), big.mark="\u2009")),
    x="% des ménages agricoles", y=NULL,
    caption ="Source : GHS-W4, secta3i_harvestw4 | Pondération : wt_wave4 | Traitement : Groupe 7"
  ) +
  theme_tp5 +
  theme(panel.grid.major.y=element_blank(),
        legend.position="right", legend.direction="vertical")

ggsave("outputs/fig01_top15_cultures.png", fig01, width=10, height=7, dpi=180)
cat("  \u2713 fig01 exportée\n")

# =============================================================================
# Q26 — Diversification culturale (W4)
# =============================================================================
cat("\n--- Q26 : Diversification culturale ---\n")

design_div <- df_diversif_w4 %>%
  filter(!is.na(wt_wave4), !is.na(milieu)) %>%
  as_survey_design(ids=hhid, weights=wt_wave4, nest=TRUE)

stats_div <- design_div %>%
  group_by(milieu) %>%
  summarise(
    moy_pond = survey_mean(n_cultures, vartype="ci"),
    med_pond = survey_quantile(n_cultures, quantiles=0.5, vartype=NULL),
    .groups="drop"
  ) %>% as.data.frame()

res_wilcox_div <- wilcox.test(
  n_cultures ~ milieu,
  data=df_diversif_w4 %>% filter(!is.na(milieu)),
  conf.int=TRUE
)
N_div <- nrow(df_diversif_w4 %>% filter(!is.na(milieu)))
r_div <- abs(qnorm(res_wilcox_div$p.value / 2)) / sqrt(N_div)
moy_urb <- round(stats_div$moy_pond[stats_div$milieu=="Urbain"], 2)
moy_rur <- round(stats_div$moy_pond[stats_div$milieu=="Rural"],  2)
cat("Wilcoxon W=", round(res_wilcox_div$statistic, 0),
    "| p=", format(res_wilcox_div$p.value, digits=3, scientific=TRUE),
    "| r=", round(r_div, 3), "\n")

# ── fig02a : histogramme avec % au-dessus de chaque barre ───────────────────
# On calcule les fréquences pondérées par nombre de cultures, puis les %,
# pour pouvoir annoter chaque barre avec son pourcentage.
df_hist_data <- df_diversif_w4 %>%
  mutate(wt = ifelse(is.na(wt_wave4), 0, wt_wave4)) %>%
  group_by(n_cultures) %>%
  summarise(freq_pond = sum(wt), .groups = "drop") %>%
  mutate(pct_lab = freq_pond / sum(freq_pond) * 100)

fig02a <- ggplot(df_hist_data, aes(x = n_cultures, y = freq_pond)) +
  geom_col(fill = "#1A5276", colour = "white", alpha = 0.85) +
  # Pourcentages au-dessus de chaque barre
  geom_text(aes(label = paste0(round(pct_lab, 1), "%")),
            vjust = -0.45, size = 2.9, fontface = "bold", colour = "#2C3E50") +
  scale_x_continuous(breaks = df_hist_data$n_cultures) +
  # Expansion verticale pour que les labels ne soient pas coupés
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.14))) +
  labs(title    = "Distribution du nombre de cultures par ménage",
       subtitle = "Effectifs pondérés — Wave 4 (2018)",
       x = "Nombre de cultures", y = "Effectifs pondérés",
       caption = "Source : GHS-W4, secta3i | Pondération : wt_wave4") +
  theme_tp5

fig02b <- ggplot(
  df_diversif_w4 %>% filter(!is.na(milieu)),
  aes(x=milieu, y=n_cultures, fill=milieu)
) +
  geom_violin(aes(weight=wt_wave4), alpha=0.60, trim=FALSE, width=0.90) +
  geom_boxplot(aes(weight=wt_wave4), width=0.15, fill="white",
               outlier.size=0.7, outlier.alpha=0.35) +
  mean_point +
  annotate("label",
           x=1.5,
           y=max(df_diversif_w4$n_cultures, na.rm=TRUE) - 0.2,
           label=paste0("Wilcoxon p = ",
                        format(res_wilcox_div$p.value, digits=2, scientific=TRUE),
                        "\nr = ", round(r_div, 3)),
           size=3.0, colour="grey20", fontface="italic",
           fill="white", label.size=0.3, alpha=0.9) +
  scale_fill_manual(values=palette_milieu, guide="none") +
  scale_y_continuous(breaks=1:max(df_diversif_w4$n_cultures, na.rm=TRUE)) +
  labs(title="Diversification : Urbain vs Rural",
       subtitle=paste0("Losange orange = moyenne | Urbain : ",
                       moy_urb, " | Rural : ", moy_rur),
       x=NULL, y="Nombre de cultures",
       caption="Source : GHS-W4 | Pondération : wt_wave4") +
  theme_tp5 + theme(panel.grid.major.x=element_blank())

fig02 <- fig02a + fig02b +
  plot_annotation(
    title="Indice de diversification culturale — Wave 4 (2018)",
    theme=theme(plot.title=element_text(face="bold", size=13, colour="#2C3E50"))
  )

ggsave("outputs/fig02_diversification_culturale.png", fig02,
       width=12, height=6, dpi=180)
cat("  \u2713 fig02 exportée\n")

saveRDS(list(wilcox=res_wilcox_div, r=r_div, stats=stats_div),
        "data/processed/resultats_q26.rds")

# =============================================================================
# Q27 — Taux d'adoption des intrants (W4)
# =============================================================================
cat("\n--- Q27 : Adoption des intrants ---\n")

design_int <- df_intrants_menage %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(ids=hhid, weights=wt_wave4, nest=TRUE)

vars_intrants   <- c("engrais_inorg","npk","urea","engrais_org","pesticide","herbicide")
labels_intrants <- c("Engrais inorganique","dont NPK","dont Urée",
                     "Engrais organique","Pesticide","Herbicide")

tab_taux <- map2_dfr(vars_intrants, labels_intrants, function(v, lbl) {
  design_int %>%
    summarise(prop=survey_mean(!!sym(v), na.rm=TRUE, vartype="ci")) %>%
    mutate(intrant=lbl)
}) %>%
  mutate(
    pct    = prop * 100,
    ic_inf = prop_low * 100,
    ic_sup = prop_upp * 100,
    intrant= factor(intrant, levels=rev(labels_intrants))
  )

cat("Taux pondérés :\n")
print(tab_taux %>% select(intrant, pct, ic_inf, ic_sup) %>%
        mutate(across(c(pct,ic_inf,ic_sup), ~round(.,1))))

# ── fig03 : label % positionné après ic_sup pour éviter le chevauchement ────
# En plaçant x = ic_sup (la borne haute de l'IC) au lieu de x = pct (la barre),
# le label part toujours de la fin du segment d'erreur et ne le chevauche jamais.
fig03 <- ggplot(tab_taux,
                aes(x=pct, y=intrant, fill=intrant, xmin=ic_inf, xmax=ic_sup)) +
  geom_col(width=0.62, alpha=0.88) +
  geom_errorbarh(height=0.28, colour="grey30", linewidth=0.65) +
  # x = ic_sup : le label part de la fin du segment d'erreur, jamais en chevauchement
  geom_text(aes(x=ic_sup, label=paste0(round(pct,1),"%")),
            hjust=-0.18, size=3.2, fontface="bold", colour="#2C3E50") +
  scale_fill_manual(values=palette_intrant, guide="none") +
  # Expansion à droite augmentée pour accueillir les labels après ic_sup
  scale_x_continuous(expand=expansion(mult=c(0, 0.30)),
                     labels=function(x) paste0(x,"%")) +
  labs(
    title  ="Taux d'adoption des intrants agricoles — Wave 4 (2018)",
    subtitle="% des ménages agricoles utilisant chaque intrant | Barres d'erreur : IC 95%",
    x="% des ménages", y=NULL,
    caption="Source : GHS-W4, secta11c2_harvestw4 | Pondération : wt_wave4 | Traitement : Groupe 7"
  ) +
  theme_tp5 + theme(panel.grid.major.y=element_blank())

ggsave("outputs/fig03_adoption_intrants.png", fig03, width=10, height=6, dpi=180)
cat("  \u2713 fig03 exportée\n")

# ── fig04 : pourcentages sur les barres ─────────────────────────────────────
tab_eng_zone <- df_intrants_menage %>%
  filter(!is.na(zone_label), !is.na(milieu), !is.na(wt_wave4)) %>%
  group_by(zone_label, milieu) %>%
  summarise(
    n_total = n(),
    taux    = sum(engrais_inorg, na.rm=TRUE) / n_total,
    ic_inf  = pmax(0, taux - 1.96*sqrt(taux*(1-taux)/n_total)),
    ic_sup  = pmin(1, taux + 1.96*sqrt(taux*(1-taux)/n_total)),
    .groups ="drop"
  )

chi_eng <- chisq.test(table(df_intrants_menage$milieu,
                            df_intrants_menage$engrais_inorg))
cat("Chi-deux engrais x milieu : X2=", round(chi_eng$statistic,2),
    "| p=", format(chi_eng$p.value, digits=3), "\n")

fig04 <- ggplot(tab_eng_zone,
                aes(x=zone_label, y=taux*100, fill=milieu,
                    ymin=ic_inf*100, ymax=ic_sup*100)) +
  geom_col(position=position_dodge(0.78), width=0.70, alpha=0.88) +
  geom_errorbar(position=position_dodge(0.78), width=0.22,
                colour="grey30", linewidth=0.65) +
  # Pourcentages positionnés au-dessus de ic_sup pour ne pas chevaucher les segments
  geom_text(aes(y=ic_sup*100, label=paste0(round(taux*100, 1), "%")),
            position=position_dodge(0.78),
            vjust=-0.45, size=2.6, fontface="bold", colour="#2C3E50") +
  scale_fill_manual(values=palette_milieu, name="Milieu") +
  scale_y_continuous(labels=function(x) paste0(x,"%"),
                     # Expansion verticale augmentée pour accueillir les labels
                     expand=expansion(mult=c(0, 0.22))) +
  scale_x_discrete(labels=function(x) str_wrap(x, width=10)) +
  labs(
    title  ="Adoption de l'engrais inorganique par zone géopolitique et milieu",
    subtitle=paste0("Wave 4 (2018) | Chi-deux milieu \u00d7 adoption : X\u00b2 = ",
                    round(chi_eng$statistic,2), ", p < 0,001 | Barres d'erreur : IC 95%"),
    x=NULL, y="Taux d'adoption (%)",
    caption="Source : GHS-W4, secta11c2_harvestw4 | Pondération : wt_wave4 | Traitement : Groupe 7"
  ) +
  theme_tp5 +
  theme(axis.text.x=element_text(size=8.5, lineheight=1.2))

ggsave("outputs/fig04_engrais_zone_milieu.png", fig04, width=11, height=6, dpi=180)
cat("  \u2713 fig04 exportée\n")

saveRDS(list(tab_taux=tab_taux, tab_eng_zone=tab_eng_zone, chi_eng=chi_eng),
        "data/processed/resultats_q27.rds")

# =============================================================================
# Q28 — Rendements maïs/mil par État (kg/ha)
# =============================================================================
cat("\n--- Q28 : Rendements par État (kg/ha) ---\n")
cat("  Couverture superficie :", meta_sup$couverture, "%\n")

df_rend_clean <- df_crop_intrants %>%
  filter(!is.na(rendement_kgha), !is.na(state_name)) %>%
  group_by(crop_label) %>%
  mutate(
    Q1_r    = quantile(rendement_kgha, 0.25, na.rm=TRUE),
    Q3_r    = quantile(rendement_kgha, 0.75, na.rm=TRUE),
    IQR_r   = Q3_r - Q1_r,
    outlier = rendement_kgha < (Q1_r - 3*IQR_r) |
      rendement_kgha > (Q3_r + 3*IQR_r)
  ) %>%
  filter(!outlier) %>% ungroup()

cat("Obs. après suppression outliers :",
    nrow(df_rend_clean), "/",
    nrow(df_crop_intrants %>% filter(!is.na(rendement_kgha))), "\n")

stats_rend <- df_rend_clean %>%
  group_by(crop_label) %>%
  summarise(
    n   = n(),
    moy = round(mean(rendement_kgha,   na.rm=TRUE), 0),
    med = round(median(rendement_kgha, na.rm=TRUE), 0),
    et  = round(sd(rendement_kgha,     na.rm=TRUE), 0),
    q1  = round(quantile(rendement_kgha, 0.25, na.rm=TRUE), 0),
    q3  = round(quantile(rendement_kgha, 0.75, na.rm=TRUE), 0),
    .groups="drop"
  )
cat("\nStatistiques rendement nettoyé :\n"); print(stats_rend)

kw_mais <- df_rend_clean %>%
  filter(crop_label=="Maïs") %>%
  kruskal_test(rendement_kgha ~ state_name)
cat("Kruskal-Wallis maïs : H=", round(kw_mais$statistic,2),
    "| p=", format(kw_mais$p, digits=3), "\n")

etats_ok <- df_rend_clean %>%
  filter(crop_label=="Maïs") %>%
  count(state_name) %>% filter(n>=5) %>% pull(state_name)

df_rend_fig <- df_rend_clean %>%
  filter(crop_label %in% c("Maïs","Mil"), state_name %in% etats_ok) %>%
  mutate(state_name=fct_reorder(state_name, rendement_kgha, .fun=median))

fig05 <- ggplot(df_rend_fig,
                aes(x=state_name, y=rendement_kgha, fill=crop_label)) +
  geom_boxplot(alpha=0.72, outlier.size=0.5, outlier.alpha=0.25,
               width=0.56, position=position_dodge(0.76)) +
  stat_summary(aes(group=crop_label),
               fun=mean, geom="point", shape=23, size=2.0,
               fill="#F39C12", colour="#2C3E50", stroke=0.4,
               position=position_dodge(0.76)) +
  scale_fill_manual(values=c("Maïs"="#1A5276","Mil"="#E67E22"),
                    name="Culture") +
  scale_y_continuous(labels=comma, expand=expansion(mult=c(0.02, 0.06))) +
  coord_flip() +
  labs(
    title  ="Rendements de maïs et de mil par État — Wave 4 (2018)",
    subtitle=paste0("kg/ha | Outliers IQR\u00d73 supprimés | Losange orange = moyenne\n",
                    "Kruskal-Wallis ma\u00efs : H = ", round(kw_mais$statistic,1),
                    ", p < 0,001 | Superficies : secta1_harvestw4 (GPS)"),
    x=NULL, y="Rendement (kg/ha)",
    caption="Source : GHS-W4, secta3i + secta1_harvestw4 | Traitement : Groupe 7"
  ) +
  theme_tp5 +
  theme(axis.text.y=element_text(size=7.5),
        legend.position="bottom")

ggsave("outputs/fig05_rendement_etat.png", fig05, width=11, height=15, dpi=180)
cat("  \u2713 fig05 exportée\n")

saveRDS(list(kw_mais=kw_mais, stats=stats_rend),
        "data/processed/resultats_q28.rds")

# =============================================================================
# Q29 — Rendements selon l'utilisation d'engrais inorganique (W4)
# =============================================================================
cat("\n--- Q29 : Rendements selon engrais inorganique ---\n")

df_eng_clean <- df_crop_intrants %>%
  filter(!is.na(rendement_kgha), !is.na(engrais_inorg)) %>%
  group_by(crop_label) %>%
  mutate(
    Q1_r    = quantile(rendement_kgha, 0.25, na.rm=TRUE),
    Q3_r    = quantile(rendement_kgha, 0.75, na.rm=TRUE),
    IQR_r   = Q3_r - Q1_r,
    outlier = rendement_kgha < (Q1_r - 3*IQR_r) |
      rendement_kgha > (Q3_r + 3*IQR_r)
  ) %>%
  filter(!outlier) %>% ungroup()

calc_r <- function(test, n) abs(qnorm(test$p.value / 2)) / sqrt(n)

res_w_mais   <- wilcox.test(rendement_kgha ~ engrais_inorg,
                            data=df_eng_clean %>% filter(crop_label=="Maïs"))
res_w_millet <- wilcox.test(rendement_kgha ~ engrais_inorg,
                            data=df_eng_clean %>% filter(crop_label=="Mil"))

n_mais   <- nrow(df_eng_clean %>% filter(crop_label=="Maïs"))
n_millet <- nrow(df_eng_clean %>% filter(crop_label=="Mil"))
r_mais   <- calc_r(res_w_mais,   n_mais)
r_millet <- calc_r(res_w_millet, n_millet)

cat("Wilcoxon maïs : W=", round(res_w_mais$statistic,0),
    "| p=", format(res_w_mais$p.value,   digits=3, scientific=TRUE),
    "| r=", round(r_mais,   3), "\n")
cat("Wilcoxon mil  : W=", round(res_w_millet$statistic,0),
    "| p=", format(res_w_millet$p.value, digits=3, scientific=TRUE),
    "| r=", round(r_millet, 3), "\n")

stats_eng <- df_eng_clean %>%
  mutate(engrais_label=ifelse(engrais_inorg,
                              "Avec engrais inorg.","Sans engrais inorg.")) %>%
  group_by(crop_label, engrais_label) %>%
  summarise(
    n   = n(),
    med = round(median(rendement_kgha, na.rm=TRUE), 0),
    moy = round(mean(rendement_kgha,   na.rm=TRUE), 0),
    q1  = round(quantile(rendement_kgha, 0.25, na.rm=TRUE), 0),
    q3  = round(quantile(rendement_kgha, 0.75, na.rm=TRUE), 0),
    .groups="drop"
  )
cat("\nStatistiques rendement x engrais :\n"); print(stats_eng)

annot_mais   <- paste0("p = ", format(res_w_mais$p.value,   digits=2, scientific=TRUE),
                       "  |  r = ", round(r_mais,   3))
annot_millet <- paste0("p = ", format(res_w_millet$p.value, digits=2, scientific=TRUE),
                       "  |  r = ", round(r_millet, 3))

df_annot <- data.frame(
  crop_label    = c("Maïs", "Mil"),
  engrais_label = c("Avec engrais inorg.", "Avec engrais inorg."),
  label_text    = c(annot_mais, annot_millet),
  y_pos = c(
    quantile(df_eng_clean$rendement_kgha[df_eng_clean$crop_label=="Maïs"], 0.94, na.rm=TRUE),
    quantile(df_eng_clean$rendement_kgha[df_eng_clean$crop_label=="Mil"],  0.94, na.rm=TRUE)
  )
)

# ── fig06 : losange de la moyenne centré dans la boîte ──────────────────────
# Le stat_summary pour la médiane (losange rouge) et celui pour la moyenne
# (losange orange) sont tous deux centrés sur x (pas de nudge).
# Médiane et moyenne étant à des valeurs y différentes, ils ne se chevauchent
# pas. Le nudge horizontal a été supprimé pour recentrer le losange orange.
fig06 <- ggplot(
  df_eng_clean %>%
    mutate(engrais_label=ifelse(engrais_inorg,
                                "Avec engrais inorg.","Sans engrais inorg.")),
  aes(x=engrais_label, y=rendement_kgha, fill=engrais_label)
) +
  geom_violin(alpha=0.55, trim=FALSE, width=0.92) +
  geom_boxplot(width=0.16, fill="white",
               outlier.size=0.6, outlier.alpha=0.25) +
  # Losange rouge = médiane — centré sur la boîte
  stat_summary(fun=median, geom="point", shape=23, size=3.0,
               fill="#E74C3C", colour="#2C3E50", stroke=0.5) +
  # Losange orange = moyenne — centré sur la boîte (pas de position_nudge)
  stat_summary(fun=mean, geom="point", shape=23, size=3.0,
               fill="#F39C12", colour="#2C3E50", stroke=0.5) +
  geom_label(data=df_annot,
             aes(x=1.5, y=y_pos, label=label_text),
             inherit.aes=FALSE,
             size=2.8, colour="grey20", fontface="italic",
             fill="white", label.size=0.25, alpha=0.92) +
  facet_wrap(~crop_label, scales="free_y") +
  scale_fill_manual(values=palette_engrais, guide="none") +
  scale_y_continuous(labels=comma) +
  labs(
    title  ="Rendements selon l'utilisation d'engrais inorganique — Wave 4 (2018)",
    subtitle="kg/ha | Outliers IQR\u00d73 supprimés | Losange rouge = médiane, orange = moyenne",
    x=NULL, y="Rendement (kg/ha)",
    caption="Source : GHS-W4, secta3i + secta11c2 + secta1_harvestw4 | Traitement : Groupe 7"
  ) +
  theme_tp5 + theme(strip.text=element_text(face="bold", size=10.5))

ggsave("outputs/fig06_rendement_engrais.png", fig06, width=10, height=6, dpi=180)
cat("  \u2713 fig06 exportée\n")

saveRDS(list(wilcox_mais=res_w_mais, wilcox_millet=res_w_millet,
             r_mais=r_mais, r_millet=r_millet, stats=stats_eng),
        "data/processed/resultats_q29.rds")

# =============================================================================
# EXPORT EXCEL — 3 feuilles
# =============================================================================
cat("\n--- Export tableau Excel ---\n")

wb <- createWorkbook()
sty_titre  <- createStyle(fontName="Arial", fontSize=13, textDecoration="bold",
                          fontColour="#1A5276")
sty_source <- createStyle(fontName="Arial", fontSize=9, fontColour="grey50",
                          textDecoration="italic")
sty_header <- createStyle(fontName="Arial", fontSize=11, textDecoration="bold",
                          halign="center", fgFill="#1A5276", fontColour="white",
                          border="TopBottomLeftRight", borderColour="white")
sty_cell   <- createStyle(fontName="Arial", fontSize=10, halign="center",
                          border="TopBottomLeftRight", borderColour="#CCCCCC",
                          wrapText=TRUE)
sty_alt    <- createStyle(fgFill="#EAF2F8")
sty_note   <- createStyle(fontName="Arial", fontSize=9, fontColour="grey50",
                          textDecoration="italic")

add_feuille <- function(wb, nom, titre, source_txt, df_data, note=NULL) {
  addWorksheet(wb, nom)
  writeData(wb, nom, titre,      startRow=1, startCol=1)
  writeData(wb, nom, source_txt, startRow=2, startCol=1)
  writeData(wb, nom, df_data, startRow=4, startCol=1, headerStyle=sty_header)
  nr <- nrow(df_data); nc <- ncol(df_data)
  addStyle(wb, nom, sty_cell, rows=4:(nr+4), cols=1:nc, gridExpand=TRUE, stack=TRUE)
  for (i in seq(2, nr, by=2))
    addStyle(wb, nom, sty_alt, rows=i+4, cols=1:nc, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, nom, sty_titre,  rows=1, cols=1)
  addStyle(wb, nom, sty_source, rows=2, cols=1)
  if (!is.null(note)) {
    writeData(wb, nom, note, startRow=nr+6, startCol=1)
    addStyle(wb, nom, sty_note, rows=nr+6, cols=1)
  }
}

# Feuille 1 : Taux d'adoption
df_f1 <- tab_taux %>% as.data.frame() %>%
  transmute(Intrant=as.character(intrant),
            `Taux pondéré (%)`=round(pct,1),
            `IC inf. 95% (%)`=round(ic_inf,1),
            `IC sup. 95% (%)`=round(ic_sup,1)) %>%
  arrange(desc(`Taux pondéré (%)`))
add_feuille(wb, "Adoption_Intrants",
            "Taux d'adoption des intrants agricoles — Wave 4 (2018)",
            "Source : GHS-W4, secta11c2_harvestw4 | Pondération : wt_wave4 | Groupe 7",
            df_f1,
            "Note : Effectifs pondérés. Adoption = au moins une parcelle utilise l'intrant.")
setColWidths(wb, "Adoption_Intrants", cols=1:4, widths=c(25,18,15,15))

# Feuille 2 : Rendements par culture
df_f2 <- stats_rend %>%
  rename(Culture=crop_label, N=n, `Moyenne (kg/ha)`=moy, `Médiane (kg/ha)`=med,
         `Éc. type`=et, Q1=q1, Q3=q3)
add_feuille(wb, "Rendements_Mais_Mil",
            "Rendements du maïs et du mil (kg/ha) — Wave 4 (2018)",
            "Source : GHS-W4, secta3i + secta1_harvestw4 | Outliers IQR×3 supprimés | Groupe 7",
            df_f2,
            paste0("Note : superficies GPS issues de secta1_harvestw4. Couverture : ",
                   meta_sup$couverture, "% des parcelles productives."))
setColWidths(wb, "Rendements_Mais_Mil", cols=1:7, widths=c(10,8,16,16,10,8,8))

# Feuille 3 : Rendements x engrais + tests
df_f3 <- stats_eng %>%
  rename(Culture=crop_label, `Groupe engrais`=engrais_label,
         N=n, `Médiane (kg/ha)`=med, `Moyenne (kg/ha)`=moy, Q1=q1, Q3=q3)
df_tests <- data.frame(
  Culture          = c("Maïs","Mil"),
  `Stat. W`        = c(round(res_w_mais$statistic,0), round(res_w_millet$statistic,0)),
  `p-valeur`       = c(format(res_w_mais$p.value,   digits=3, scientific=TRUE),
                       format(res_w_millet$p.value, digits=3, scientific=TRUE)),
  `r de Rosenthal` = c(round(r_mais,3), round(r_millet,3)),
  `Taille d'effet` = c(
    ifelse(r_mais<0.1,"Négligeable",ifelse(r_mais<0.3,"Petite",
                                           ifelse(r_mais<0.5,"Moyenne","Grande"))),
    ifelse(r_millet<0.1,"Négligeable",ifelse(r_millet<0.3,"Petite",
                                             ifelse(r_millet<0.5,"Moyenne","Grande")))),
  check.names=FALSE
)
add_feuille(wb, "Rendements_Engrais",
            "Rendements (kg/ha) selon engrais inorganique — W4 (2018)",
            "Source : GHS-W4, secta3i + secta11c2 + secta1_harvestw4 | Groupe 7",
            df_f3)
nr3 <- nrow(df_f3)
writeData(wb, "Rendements_Engrais",
          "Tests de Wilcoxon-Mann-Whitney (rendement avec vs sans engrais inorganique)",
          startRow=nr3+7, startCol=1)
addStyle(wb, "Rendements_Engrais",
         createStyle(fontName="Arial", textDecoration="bold", fontColour="#1A5276"),
         rows=nr3+7, cols=1)
writeData(wb, "Rendements_Engrais", df_tests, startRow=nr3+8,
          startCol=1, headerStyle=sty_header)
addStyle(wb, "Rendements_Engrais", sty_cell,
         rows=(nr3+8):(nr3+10), cols=1:5, gridExpand=TRUE, stack=TRUE)
setColWidths(wb, "Rendements_Engrais", cols=1:7, widths=c(10,22,8,16,16,8,8))

saveWorkbook(wb, "outputs/tableau_intrants.xlsx", overwrite=TRUE)
cat("  \u2713 tableau_intrants.xlsx exporté (3 feuilles)\n")

cat("\n====== Analyses TP5 terminées ======\n")
cat("Figures dans outputs/ :\n")
for (f in list.files("outputs/", pattern="\\.png$")) cat("  -", f, "\n")
cat("Tableau Excel : outputs/tableau_intrants.xlsx\n")