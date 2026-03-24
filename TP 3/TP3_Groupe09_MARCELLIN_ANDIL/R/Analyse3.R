#==================================================
# TP ISE1 - ANALYSE 3
# Accès aux services de santé et chocs sanitaires
# Nigeria GHS Panel - Wave 4 (2018)
# Poids : wt_wave4 issu de sectaa_harvestw4.dta
#==================================================

setwd("D:/ENSAE/ISE_1_SEMESTRE_2/R/mon_projet")

library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(rstatix)
library(patchwork)
library(forcats)
library(gtsummary)
library(naniar)

# ── CHARGEMENT DES POIDS ─────────────────────────────────────────────────────
sectaa_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sectaa_harvestw4.dta"
)

poids <- sectaa_harvestw4 %>%
  select(hhid, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

cat("Poids chargés :", nrow(poids), "ménages\n")

# ── CHARGEMENT DES DONNÉES ────────────────────────────────────────────────────
sect4a_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect4a_harvestw4.dta"
)
sect1_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta"
)
totcons <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/totcons_final.dta"
)

# ── MERGE : sect4a + sect1 + poids ───────────────────────────────────────────
sante_data <- sect4a_harvestw4 %>%
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
      sector == 1 ~ "Urbain",
      sector == 2 ~ "Rural",
      TRUE         ~ NA_character_
    ),
    malade = case_when(
      s4aq3 == 1 ~ "Oui",
      s4aq3 == 2 ~ "Non",
      TRUE        ~ NA_character_
    ),
    consulte = case_when(
      s4aq1 == 1 ~ "Oui",
      s4aq1 == 2 ~ "Non",
      TRUE        ~ NA_character_
    ),
    type_maladie = case_when(
      s4aq3b_1 == 1  ~ "Paludisme",
      s4aq3b_1 == 4  ~ "Typhoide",
      s4aq3b_1 == 6  ~ "Diarrhee",
      s4aq3b_1 == 9  ~ "Pneumonie",
      s4aq3b_1 == 10 ~ "Rhume",
      s4aq3b_1 == 11 ~ "Blessure",
      s4aq3b_1 == 13 ~ "Hypertension",
      s4aq3b_1 == 15 ~ "Catarrhe",
      s4aq3b_1 == 16 ~ "Toux",
      s4aq3b_1 == 17 ~ "Maux de tete",
      s4aq3b_1 == 18 ~ "Diabete",
      s4aq3b_1 == 24 ~ "Ulcere/Estomac",
      s4aq3b_1 == 25 ~ "Probleme oculaire",
      s4aq3b_1 == 27 ~ "Douleurs corporelles",
      !is.na(s4aq3b_1) ~ "Autre",
      TRUE              ~ NA_character_
    ),
    prestataire = case_when(
      s4aq6a == 0  ~ "Aucun recours",
      s4aq6a == 1  ~ "Guerisseur traditionnel",
      s4aq6a == 2  ~ "Medecin",
      s4aq6a == 4  ~ "Infirmier",
      s4aq6a == 7  ~ "Pharmacien",
      s4aq6a == 8  ~ "Chimiste",
      s4aq6a == 11 ~ "PMV",
      s4aq6a %in% c(3,5,6,9,10,13,14,15) ~ "Autre",
      TRUE ~ NA_character_
    ),
    depense_sante = rowSums(
      cbind(
        ifelse(is.na(s4aq9),  0, s4aq9),
        ifelse(is.na(s4aq14), 0, s4aq14),
        ifelse(is.na(s4aq17), 0, s4aq17)
      )
    ),
    depense_sante = ifelse(depense_sante == 0, NA_real_, depense_sante),
    groupe_age = case_when(
      age <  5             ~ "0-4 ans",
      age >= 5  & age < 15 ~ "5-14 ans",
      age >= 15 & age < 25 ~ "15-24 ans",
      age >= 25 & age < 45 ~ "25-44 ans",
      age >= 45 & age < 65 ~ "45-64 ans",
      age >= 65            ~ "65+ ans",
      TRUE                 ~ NA_character_
    ),
    groupe_age = factor(groupe_age,
                        levels = c("0-4 ans","5-14 ans","15-24 ans",
                                   "25-44 ans","45-64 ans","65+ ans"))
  )

cat("Individus sans poids :", sum(is.na(sante_data$wt_wave4)), "\n")

# Quintiles de consommation + merge
totcons <- totcons %>%
  mutate(
    quintile = ntile(totcons_pc, 5),
    quintile = factor(quintile,
                      labels = c("Q1 (Pauvre)","Q2","Q3","Q4","Q5 (Riche)"))
  )

sante_data <- sante_data %>%
  left_join(totcons %>% select(hhid, quintile, totcons_pc), by = "hhid")

# ── TÂCHE 13 : Taux de morbidité pondéré ─────────────────────────────────────
taux_global_w <- sante_data %>%
  filter(!is.na(malade), !is.na(wt_wave4)) %>%
  summarise(taux_w = sum(wt_wave4[malade == "Oui"]) / sum(wt_wave4) * 100)
cat("Taux de morbidité pondéré :", round(taux_global_w$taux_w, 1), "%\n")

morb_sexe <- sante_data %>%
  filter(!is.na(malade), !is.na(sexe)) %>%
  group_by(sexe) %>%
  summarise(
    n       = n(),
    malades = sum(malade == "Oui"),
    taux_w  = sum(wt_wave4[malade == "Oui"], na.rm = TRUE) / sum(wt_wave4, na.rm = TRUE),
    ic_low  = mapply(function(x, n) binom.test(x, n)$conf.int[1], malades, n),
    ic_high = mapply(function(x, n) binom.test(x, n)$conf.int[2], malades, n),
    .groups = "drop"
  )

morb_age <- sante_data %>%
  filter(!is.na(malade), !is.na(groupe_age)) %>%
  group_by(groupe_age) %>%
  summarise(
    n       = n(),
    malades = sum(malade == "Oui"),
    taux_w  = sum(wt_wave4[malade == "Oui"], na.rm = TRUE) / sum(wt_wave4, na.rm = TRUE),
    ic_low  = mapply(function(x, n) binom.test(x, n)$conf.int[1], malades, n),
    ic_high = mapply(function(x, n) binom.test(x, n)$conf.int[2], malades, n),
    .groups = "drop"
  )

p_sexe <- ggplot(morb_sexe, aes(x = sexe, y = taux_w, fill = sexe)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux_w * 100, 1), "%")),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Homme" = "#2874a6", "Femme" = "#e74c3c"),
                    guide = "none") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.20)) +
  labs(title = "Taux de morbidité pondéré par sexe",
       x = "Sexe", y = "Taux (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_age <- ggplot(morb_age, aes(x = groupe_age, y = taux_w, fill = groupe_age)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux_w * 100, 1), "%")),
            vjust = -1.5, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Blues", guide = "none") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25)) +
  labs(title = "Taux de morbidité pondéré par groupe d'âge",
       x = "Groupe d'âge", y = "Taux (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1))

p_sexe + p_age

# ── TÂCHE 14 : Top 10 types de maladies pondéré ──────────────────────────────
top10_maladies <- sante_data %>%
  filter(!is.na(type_maladie), !is.na(wt_wave4)) %>%
  group_by(type_maladie) %>%
  summarise(n_pondere = sum(wt_wave4), .groups = "drop") %>%
  arrange(desc(n_pondere)) %>%
  slice_head(n = 10) %>%
  mutate(
    prop         = n_pondere / sum(n_pondere) * 100,
    type_maladie = fct_reorder(type_maladie, prop),
    categorie    = case_when(
      type_maladie %in% c("Paludisme","Typhoide","Diarrhee",
                          "Pneumonie","Rhume","Toux","Catarrhe") ~ "Infectieuse",
      type_maladie == "Blessure" ~ "Traumatique",
      TRUE ~ "Chronique/Autre"
    )
  )

ggplot(top10_maladies, aes(x = prop, y = type_maladie, fill = categorie)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), hjust = -0.2, size = 3.5) +
  scale_fill_manual(
    values = c("Infectieuse"="#2874a6","Traumatique"="#e74c3c",
               "Chronique/Autre"="#27ae60"),
    name = "Catégorie"
  ) +
  scale_x_continuous(limits = c(0, max(top10_maladies$prop) * 1.2)) +
  labs(
    title    = "Top 10 des maladies pondérées",
    subtitle = "Poids : wt_wave4 | Wave 4 (2018)",
    x        = "Proportion pondérée (%)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

# ── TÂCHE 15 : Recours aux soins par prestataire pondéré ─────────────────────
freq_prest <- sante_data %>%
  filter(!is.na(prestataire), malade == "Oui", !is.na(wt_wave4)) %>%
  group_by(prestataire) %>%
  summarise(n_pondere = sum(wt_wave4), .groups = "drop") %>%
  mutate(
    prop        = n_pondere / sum(n_pondere) * 100,
    prestataire = fct_reorder(prestataire, prop)
  )

ggplot(freq_prest, aes(x = prop, y = prestataire)) +
  geom_col(fill = "#2874a6", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), hjust = -0.2, size = 3.5) +
  scale_x_continuous(limits = c(0, max(freq_prest$prop) * 1.2)) +
  labs(
    title    = "Recours aux soins pondéré par type de prestataire",
    subtitle = "Parmi les individus malades -- Wave 4 (2018)",
    x        = "Proportion pondérée (%)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

# ── TÂCHE 16 : Distribution des dépenses de santé ────────────────────────────
sante_depenses <- sante_data %>%
  filter(!is.na(depense_sante), depense_sante > 0, !is.na(wt_wave4))

deciles_dep <- sante_depenses %>%
  summarise(
    N       = n(),
    Min     = min(depense_sante),
    Q1      = quantile(depense_sante, 0.25),
    Mediane = median(depense_sante),
    Moyenne = weighted.mean(depense_sante, wt_wave4),
    Q3      = quantile(depense_sante, 0.75),
    Max     = max(depense_sante)
  )
print(deciles_dep)

p_hist <- ggplot(sante_depenses, aes(x = depense_sante, weight = wt_wave4)) +
  geom_histogram(bins = 40, fill = "#2874a6", color = "white", alpha = 0.8) +
  scale_x_log10(labels = comma_format()) +
  labs(
    title = "Distribution pondérée des dépenses de santé",
    x     = "Dépenses (Naira, échelle log)",
    y     = "Effectif pondéré"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_box_prest <- sante_data %>%
  filter(!is.na(depense_sante), !is.na(prestataire),
         depense_sante > 0, prestataire != "Aucun recours") %>%
  ggplot(aes(x = fct_reorder(prestataire, depense_sante, median),
             y = depense_sante, fill = prestataire)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_y_log10(labels = comma_format()) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  coord_flip() +
  labs(
    title = "Dépenses de santé par type de prestataire",
    x     = NULL,
    y     = "Dépenses (Naira, échelle log)"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

p_hist + p_box_prest

# ── TÂCHE 17 : Recours aux soins × quintile de consommation ──────────────────
sante_quint <- sante_data %>%
  filter(!is.na(consulte), !is.na(quintile), malade == "Oui")

tab_quint  <- table(sante_quint$quintile, sante_quint$consulte)
chi2_quint <- chisq.test(tab_quint)
V_quint    <- sqrt(chi2_quint$statistic /
                     (sum(tab_quint) * (min(dim(tab_quint)) - 1)))
cat("Chi-deux p =", chi2_quint$p.value, "| V de Cramér =", round(V_quint, 4), "\n")

sante_quint %>%
  filter(!is.na(wt_wave4)) %>%
  group_by(quintile, consulte) %>%
  summarise(n_pondere = sum(wt_wave4), .groups = "drop") %>%
  group_by(quintile) %>%
  mutate(prop = n_pondere / sum(n_pondere)) %>%
  ggplot(aes(x = quintile, y = prop, fill = consulte)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(
    aes(label = ifelse(prop >= 0.05, paste0(round(prop * 100, 1), "%"), "")),
    position = position_fill(vjust = 0.5),
    size = 3.5, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Oui" = "#27ae60", "Non" = "#e74c3c"),
                    name = "A consulté") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Recours pondéré aux soins selon le quintile de consommation",
    subtitle = paste0("Chi-deux p = ", format.pval(chi2_quint$p.value, digits = 3),
                      " | V de Cramér = ", round(V_quint, 3)),
    x = "Quintile de consommation",
    y = "Proportion pondérée (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

# ── TÂCHE 18 : Dépenses médianes rural vs urbain ─────────────────────────────
sante_zone <- sante_data %>%
  filter(!is.na(depense_sante), !is.na(zone), depense_sante > 0)

wilcox_zone <- wilcox.test(depense_sante ~ zone,
                           data = sante_zone, exact = FALSE)
print(wilcox_zone)

effet_zone <- sante_zone %>%
  wilcox_effsize(depense_sante ~ zone)
print(effet_zone)

ggplot(sante_zone, aes(x = zone, y = depense_sante, fill = zone)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, alpha = 0.9,
               outlier.colour = "red", outlier.alpha = 0.3) +
  scale_y_log10(labels = comma_format()) +
  scale_fill_manual(values = c("Rural" = "#27ae60", "Urbain" = "#2874a6"),
                    guide = "none") +
  labs(
    title    = "Dépenses de santé pondérées selon la zone",
    subtitle = paste0("Wilcoxon p = ",
                      format.pval(wilcox_zone$p.value, digits = 3),
                      " | r = ", round(effet_zone$effsize, 3)),
    x        = "Zone",
    y        = "Dépenses de santé (Naira, échelle log)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
