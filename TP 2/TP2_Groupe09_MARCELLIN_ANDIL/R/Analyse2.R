#==================================================
# TP ISE1 - ANALYSE 2
# Éducation et alphabétisation des membres
# Nigeria GHS Panel - Wave 4 (2018)
# Poids : wt_wave4 issu de sectaa_harvestw4.dta
#==================================================

setwd("D:/ENSAE/ISE_1_SEMESTRE_2/R/mon_projet")

library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(gtsummary)
library(rstatix)
library(ggpubr)
library(viridis)
library(patchwork)
library(purrr)
library(scales)

# ── CHARGEMENT DES POIDS ─────────────────────────────────────────────────────
sectaa_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sectaa_harvestw4.dta"
)

poids <- sectaa_harvestw4 %>%
  select(hhid, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

cat("Poids chargés :", nrow(poids), "ménages\n")

# ── TÂCHE 7 : Chargement et jointure ─────────────────────────────────────────
sect2_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect2_harvestw4.dta"
)
sect1_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta"
)

# MERGE : sect2 + sect1 + poids
educ_data <- sect2_harvestw4 %>%
  left_join(
    sect1_harvestw4 %>% select(hhid, indiv, s1q2, s1q4),
    by = c("hhid", "indiv")
  ) %>%
  left_join(poids, by = "hhid") %>%
  mutate(
    sexe = case_when(
      s1q2 == 1 ~ "Homme",
      s1q2 == 2 ~ "Femme",
      TRUE       ~ NA_character_
    ),
    age  = as.numeric(s1q4),
    zone = case_when(
      sector == 1 ~ "Rural",
      sector == 2 ~ "Urbain",
      TRUE         ~ NA_character_
    )
  )

cat("Individus sans poids :", sum(is.na(educ_data$wt_wave4)), "\n")
cat("Valeurs manquantes s2aq9 :", sum(is.na(educ_data$s2aq9)), "/", nrow(educ_data), "\n")

# ── TÂCHE 8 : Variable niveau_educ à 5 catégories ────────────────────────────
educ_data <- educ_data %>%
  mutate(
    niveau_educ = case_when(
      s2aq9 %in% c(0,1,2,3,51,52,61)       ~ "Aucun",
      s2aq9 %in% 11:16                      ~ "Primaire",
      s2aq9 %in% c(21,22,23,321)            ~ "Junior Secondary",
      s2aq9 %in% c(24,25,26,27,28,322)      ~ "Senior Secondary",
      s2aq9 %in% c(31,33,34,35,41,43,
                   411,412,421,422,
                   423,424)                 ~ "Tertiaire",
      TRUE                                  ~ NA_character_
    ),
    niveau_educ = factor(
      niveau_educ,
      levels  = c("Aucun","Primaire","Junior Secondary",
                  "Senior Secondary","Tertiaire"),
      ordered = TRUE
    )
  )

# Fréquences pondérées
freq_educ <- educ_data %>%
  filter(!is.na(niveau_educ), !is.na(wt_wave4)) %>%
  group_by(niveau_educ) %>%
  summarise(n_pondere = sum(wt_wave4), .groups = "drop") %>%
  mutate(prop = n_pondere / sum(n_pondere) * 100) %>%
  arrange(desc(prop))
print(freq_educ)

ggplot(freq_educ,
       aes(x = prop, y = fct_reorder(niveau_educ, prop), fill = niveau_educ)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), hjust = -0.2, size = 3.5) +
  scale_fill_manual(
    values = c(
      "Aucun"            = "#c0392b",
      "Primaire"         = "#e67e22",
      "Junior Secondary" = "#f1c40f",
      "Senior Secondary" = "#27ae60",
      "Tertiaire"        = "#1a5276"
    ), guide = "none"
  ) +
  scale_x_continuous(limits = c(0, max(freq_educ$prop) * 1.2),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Distribution pondérée du niveau d'éducation",
    subtitle = "Poids : wt_wave4 | Wave 4 (2018)",
    x        = "Proportion pondérée (%)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# ── TÂCHE 9 : Éducation par sexe (adultes 18+) ───────────────────────────────
educ_adultes <- educ_data %>%
  filter(!is.na(niveau_educ), !is.na(sexe), age >= 18)

tab_cont <- table(educ_adultes$sexe, educ_adultes$niveau_educ)
chi2     <- chisq.test(tab_cont)
V_cramer <- sqrt(chi2$statistic / (sum(tab_cont) * (min(dim(tab_cont)) - 1)))
cat("Chi-deux p =", chi2$p.value, "| V de Cramér =", round(V_cramer, 4), "\n")

# Barplot 100% pondéré
educ_adultes %>%
  filter(!is.na(wt_wave4)) %>%
  group_by(sexe, niveau_educ) %>%
  summarise(n_pondere = sum(wt_wave4), .groups = "drop") %>%
  group_by(sexe) %>%
  mutate(prop = n_pondere / sum(n_pondere)) %>%
  ggplot(aes(x = sexe, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(
    aes(label = ifelse(prop >= 0.05, paste0(round(prop * 100, 1), "%"), "")),
    position = position_fill(vjust = 0.5),
    size = 3.5, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Aucun"="#c0392b","Primaire"="#e67e22",
               "Junior Secondary"="#f1c40f",
               "Senior Secondary"="#27ae60","Tertiaire"="#1a5276"),
    name = "Niveau d'éducation"
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Niveau d'éducation pondéré par sexe (adultes 18+)",
    subtitle = paste0("Chi-deux p = ", format.pval(chi2$p.value, digits = 3),
                      " | V de Cramér = ", round(V_cramer, 3)),
    x = "Sexe", y = "Proportion pondérée (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# ── TÂCHE 10 : Âge × niveau d'éducation ──────────────────────────────────────
educ_adultes <- educ_adultes %>%
  mutate(
    groupe_age = case_when(
      age >= 18 & age <= 30 ~ "18-30",
      age >= 31 & age <= 45 ~ "31-45",
      age >= 46 & age <= 60 ~ "46-60",
      age >  60             ~ "60+",
      TRUE                  ~ NA_character_
    ),
    groupe_age      = factor(groupe_age, levels = c("18-30","31-45","46-60","60+")),
    niveau_educ_num = as.integer(niveau_educ)
  )

ggplot(educ_adultes %>% filter(!is.na(groupe_age)),
       aes(x = groupe_age, y = niveau_educ_num, fill = groupe_age)) +
  geom_boxplot(aes(weight = wt_wave4), alpha = 0.75, outlier.alpha = 0.3) +
  scale_y_continuous(breaks = 1:5,
                     labels = c("Aucun","Primaire","JSS","SSS","Tertiaire")) +
  scale_fill_manual(
    values = c("18-30"="#1a5276","31-45"="#2874a6","46-60"="#85c1e9","60+"="#aed6f1"),
    guide  = "none"
  ) +
  labs(
    title = "Niveau d'éducation par groupe d'âge (adultes 18+)",
    x     = "Groupe d'âge",
    y     = "Niveau d'éducation"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

kw <- kruskal.test(niveau_educ_num ~ groupe_age,
                   data = educ_adultes %>% filter(!is.na(groupe_age)))
print(kw)

dunn <- rstatix::dunn_test(
  data            = educ_adultes %>% filter(!is.na(groupe_age)),
  formula         = niveau_educ_num ~ groupe_age,
  p.adjust.method = "bonferroni"
)
print(dunn)

# ── TÂCHE 11 : Scolarisation 6-17 ans rural vs urbain ────────────────────────
scol_data <- educ_data %>%
  filter(age >= 6, age <= 17, !is.na(zone), !is.na(wt_wave4)) %>%
  mutate(
    scolarise = case_when(
      s2aq3 == 1 ~ "Scolarisé",
      s2aq3 == 2 ~ "Non scolarisé",
      TRUE        ~ NA_character_
    )
  ) %>%
  filter(!is.na(scolarise))

tab_scol  <- table(scol_data$zone, scol_data$scolarise)
chi2_scol <- chisq.test(tab_scol)
print(chi2_scol)

scol_data %>%
  group_by(zone, scolarise) %>%
  summarise(n_pondere = sum(wt_wave4), n_brut = n(), .groups = "drop") %>%
  group_by(zone) %>%
  mutate(
    prop    = n_pondere / sum(n_pondere),
    n_tot   = sum(n_brut),
    ic_low  = map2_dbl(n_brut, n_tot, ~ binom.test(.x, .y)$conf.int[1]),
    ic_high = map2_dbl(n_brut, n_tot, ~ binom.test(.x, .y)$conf.int[2])
  ) %>%
  filter(scolarise == "Scolarisé") %>%
  ggplot(aes(x = zone, y = prop, fill = zone)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Rural" = "#2874a6", "Urbain" = "#e74c3c"),
                    guide = "none") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title    = "Taux de scolarisation pondéré des 6-17 ans par zone",
    subtitle = paste0("Chi-deux p = ", format.pval(chi2_scol$p.value, digits = 3)),
    x = "Zone", y = "Taux de scolarisation"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# ── TÂCHE 12 : Heatmap État × niveau d'éducation ─────────────────────────────
heatmap_data <- educ_data %>%
  filter(!is.na(niveau_educ), age >= 18, !is.na(wt_wave4)) %>%
  group_by(state) %>%
  summarise(
    taux_aucun = sum(wt_wave4[niveau_educ == "Aucun"]) / sum(wt_wave4) * 100,
    .groups    = "drop"
  ) %>%
  mutate(
    state = as_factor(state),
    state = fct_reorder(state, taux_aucun)
  )

ggplot(heatmap_data,
       aes(x = "Taux sans instruction", y = state, fill = taux_aucun)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = paste0(round(taux_aucun, 1), "%")),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", name = "% sans\ninstruction") +
  labs(
    title    = "Taux d'analphabétisme pondéré par État nigérian",
    subtitle = "Part des adultes (18+) sans instruction -- Wave 4 (2018)",
    x        = NULL,
    y        = "État"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_blank())
