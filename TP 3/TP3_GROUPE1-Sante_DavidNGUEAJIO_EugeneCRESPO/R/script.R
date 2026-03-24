source("R/fonctions.R")

# 1. CHARGEMENT DES DONNÉES
# Adapter les chemins selon votre répertoire de travail

sect2  <- read_dta("data/raw/NGA-GHSP-W4/sect2_harvestw4.dta")
sect1  <- read_dta("data/raw/NGA-GHSP-W4/sect1_harvestw4.dta")
secta  <- read_dta("data/raw/NGA-GHSP-W4/secta_harvestw4.dta")

# 2. CONSTRUCTION DU FICHIER INDIVIDUEL MAÎTRE

# 2.1  Roster individuel : sexe + âge (sect1)
roster <- sect1 %>%
  select(hhid, indiv, sexe = s1q2, age = s1q4) %>%
  mutate(
    sexe = factor(sexe, levels = 1:2, labels = c("Homme", "Femme")),
    age  = as.numeric(age)
  )
View (roster)
# 2.2  Variables ménage : pondération + stratification + zone (secta)
# IMPORTANT : sect2, sect1 et secta partagent les colonnes zone/state/sector/ea.
# On renomme celles issues de secta pour éviter les conflits lors du left_join.
menage <- secta %>%
  select(hhid, wt_wave4, strata,
         psu     = ea,       # Unité primaire de sondage (zone de dénombrement)
         sector_hh = sector, # Milieu urbain/rural
         state_hh  = state,  # État nigérian
         zone_hh   = zone    # Zone géographique
  ) %>%
  mutate(
    milieu   = factor(sector_hh, levels = 1:2, labels = c("Urbain", "Rural")),
    state_hh = factor(state_hh, levels = 1:37,
                      labels = c("Abia","Adamawa","Akwa Ibom","Anambra",
                                 "Bauchi","Bayelsa","Benue","Borno",
                                 "Cross River","Delta","Ebonyi","Edo",
                                 "Ekiti","Enugu","Gombe","Imo","Jigawa",
                                 "Kaduna","Kano","Katsina","Kebbi","Kogi",
                                 "Kwara","Lagos","Nasarawa","Niger","Ogun",
                                 "Ondo","Osun","Oyo","Plateau","Rivers",
                                 "Sokoto","Taraba","Yobe","Zamfara","FCT")),
    zone_hh  = factor(zone_hh, levels = 1:6,
                      labels = c("North Central","North East","North West",
                                 "South East","South South","South West"))
  )

# 2.3  Éducation (sect2)
educ <- sect2 %>%
  select(hhid, indiv,
         alphabetise  = s2aq5,    # 1=oui, 2=non
         jamais_scol  = s2aq6,    # 1=oui (a fréquenté), 2=non
         niveau_code  = s2aq9,    # niveau le plus élevé atteint
         diplome_code = s2aq10,   # diplôme obtenu
         scol_actuel  = s2aq13a,  # scolarisé 2018/19 : 1=oui, 2=non
         type_etab    = s2aq13c,  # type établissement
         age_min3     = s2aq2     # filtre : ≥3 ans (1=oui)
  )

# 2.4  Fusion
df <- educ %>%
  left_join(roster, by = c("hhid", "indiv")) %>%
  left_join(menage, by = "hhid") %>%
  filter(!is.na(wt_wave4),   # exclure ménages sans poids (N=49)
         age_min3 == 1)      # garder individus ≥ 3 ans

# 3. RECODAGES

# 3.1  niveau_educ — variable ordinale à 5 catégories
#      Basé sur s2aq9 (niveau de classe le plus élevé atteint)
#      Aucun   : 0 (NONE) + NA si jamais scolarisé
#      Coranique/adulte assimilé à Aucun formel (codes 51,52,61)
df <- df %>%
  mutate(
    niveau_educ = case_when(
      # Aucun : pas de scolarisation formelle
      niveau_code == 0                          ~ "Aucun",
      jamais_scol == 2                          ~ "Aucun",   # jamais allé à l'école
      niveau_code %in% c(51, 52, 61)            ~ "Aucun",   # coranique / adulte
      # Préscolaire/maternelle → Aucun formel
      niveau_code %in% c(1, 2, 3)               ~ "Aucun",
      # Primaire : PRIMARY 1-6
      niveau_code %in% 11:16                    ~ "Primaire",
      # Junior Secondary : JSS 1-3
      niveau_code %in% 21:23                    ~ "Junior Secondary",
      # Senior Secondary : SS 1-3 + Higher School
      niveau_code %in% c(24:26, 27, 28)         ~ "Senior Secondary",
      # Vocationnel secondaire
      niveau_code == 321                        ~ "Senior Secondary",
      # Tertiaire : Teacher Training, NCE, Nursing, Polytechnic,
      #             OND, HND, Université, Post-grad, Voc Tertiaire
      niveau_code %in% c(31, 33, 34, 35,
                         41, 43,
                         322, 411, 412,
                         421, 422, 423, 424)   ~ "Tertiaire",
      TRUE                                      ~ NA_character_
    ),
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun","Primaire",
                                    "Junior Secondary",
                                    "Senior Secondary","Tertiaire"),
                         ordered = TRUE)
  )

# 3.2  Groupes d'âge
df <- df %>%
  mutate(
    grp_age = case_when(
      age >= 18 & age <= 30 ~ "18-30",
      age >= 31 & age <= 45 ~ "31-45",
      age >= 46 & age <= 60 ~ "46-60",
      age >  60             ~ "60+",
      TRUE                  ~ NA_character_
    ),
    grp_age = factor(grp_age, levels = c("18-30","31-45","46-60","60+"))
  )

# 3.3  Alphabétisation (binaire)
df <- df %>%
  mutate(
    alpha_bin = case_when(
      alphabetise == 1 ~ 1L,
      alphabetise == 2 ~ 0L,
      TRUE             ~ NA_integer_
    )
  )

# 3.4  Scolarisation 6-17 ans
df <- df %>%
  mutate(
    scol_617 = case_when(
      age >= 6 & age <= 17 & scol_actuel == 1 ~ 1L,
      age >= 6 & age <= 17 & scol_actuel == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Aperçu du recodage
cat("=== Distribution niveau_educ (non pondérée) ===\n")
print(table(df$niveau_educ, useNA = "ifany"))

cat("\n=== Valeurs manquantes niveau_educ ===\n")
cat(sum(is.na(df$niveau_educ)), "individus sans information\n")

# 4. DÉCLARATION DU PLAN D'ENQUÊTE
design <- df %>%
  as_survey_design(
    ids     = psu,         # PSU = zone de dénombrement (ea renommé pour éviter conflit)
    strata  = strata,      # 6 strates géographiques
    weights = wt_wave4,    # Poids ménage appliqué à l'individu
    nest    = TRUE
  )

# Population totale représentée (individus ≥ 3 ans)
pop_totale <- design %>% summarise(n = survey_total(vartype = "ci")) %>% pull(n)
cat(sprintf("\nPopulation représentée (≥3 ans) : %s individus\n",
            format(round(pop_totale), big.mark = " ")))

# 5. TÂCHE 7 — INSPECTION DES VALEURS MANQUANTES

miss_summary <- df %>%
  summarise(
    n_total        = n(),
    miss_niveau    = sum(is.na(niveau_educ)),
    pct_miss_niv   = mean(is.na(niveau_educ)) * 100,
    miss_age       = sum(is.na(age)),
    pct_miss_age   = mean(is.na(age)) * 100,
    miss_sexe      = sum(is.na(sexe)),
    miss_alpha     = sum(is.na(alpha_bin))
  )
print(miss_summary)

# 6. TÂCHE 8 — FRÉQUENCES & PROPORTIONS (pondérées) + BARPLOT PAR SEXE

# Fréquences pondérées globales
freq_niv <- design %>%
  filter(!is.na(niveau_educ)) %>%
  group_by(niveau_educ) %>%
  summarise(
    effectif_pond = survey_total(vartype = "ci"),
    proportion    = survey_prop(vartype = "ci")
  )
print(freq_niv)

# Fréquences par sexe
freq_sexe <- design %>%
  filter(!is.na(niveau_educ), !is.na(sexe)) %>%
  group_by(sexe, niveau_educ) %>%
  summarise(
    effectif = survey_total(vartype = NULL),
    prop     = survey_prop(vartype = "ci")
  )

# --- GRAPHIQUE 1 : Barplot 100% empilées niveau_educ par sexe ---
p1 <- freq_sexe %>%
  ggplot(aes(x = sexe, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", width = 0.6, color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(prop > 0.03,
                               paste0(round(prop * 100, 1), "%"), "")),
            position = position_fill(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_viridis_d(option = "D", direction = -1,
                       name = "Niveau d'éducation") +
  labs(
    title    = "Distribution du niveau d'éducation par sexe",
    subtitle = "Adultes nigérians — GHS-Panel W4 (2018-2019), estimations pondérées",
    x = NULL, y = "Proportion pondérée (%)",
    caption  = "Source : NBS/World Bank GHS-Panel W4. Pondération : wt_wave4."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    panel.grid.major.x = element_blank()
  )

print(p1)
ggsave("fig1_barplot_educ_sexe.png", p1, width = 8, height = 5.5, dpi = 300)
cat("Figure 1 sauvegardée : fig1_barplot_educ_sexe.png\n")

# 7. TÂCHE 9 — COMPARAISON HOMME/FEMME (adultes 18+)
#    Test du chi-deux de Rao-Scott + V de Cramér + tableau de contingence

design_adultes <- design %>%
  filter(age >= 18, !is.na(sexe), !is.na(niveau_educ))

# Tableau de contingence pondéré
tab_cont <- design_adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(n_pond = survey_total(vartype = NULL)) %>%
  tidyr::pivot_wider(names_from = niveau_educ, values_from = n_pond)
cat("\nTableau de contingence pondéré (effectifs en milliers) :\n")
print(tab_cont)

# Test du chi-deux de Rao-Scott (adapté aux données d'enquête complexe)
# svychisq tient compte du plan de sondage (strates + clusters)
chi2_test <- svychisq(~ sexe + niveau_educ,
                      design = design_adultes,
                      statistic = "Chisq")   # Rao-Scott F-adjusted
cat("\nTest du chi-deux de Rao-Scott :\n")
print(chi2_test)

# V de Cramér (calculé sur effectifs pondérés)
tab_matrix <- design_adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(n = survey_total(vartype = NULL)) %>%
  tidyr::pivot_wider(names_from = niveau_educ, values_from = n) %>%
  select(-sexe) %>%
  as.matrix()

n_total <- sum(tab_matrix, na.rm = TRUE)
chi2_val <- chisq.test(tab_matrix)$statistic
k <- min(nrow(tab_matrix), ncol(tab_matrix)) - 1
v_cramer <- sqrt(chi2_val / (n_total * k))
cat(sprintf("\nV de Cramér : %.4f\n", v_cramer))
cat(sprintf("(Interprétation : <0.1 faible | 0.1-0.3 modéré | >0.3 fort)\n"))

# Barplot 100% empilées côte à côte (même graphique que Fig1 mais adultes 18+)
freq_adultes <- design_adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(prop = survey_prop(vartype = "ci"))

p2 <- freq_adultes %>%
  ggplot(aes(x = niveau_educ, y = prop, fill = sexe,
             ymin = prop_low, ymax = prop_upp)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  geom_errorbar(position = position_dodge(0.7), width = 0.25,
                linewidth = 0.6) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0),
                     limits = c(0, NA)) +
  scale_fill_manual(values = c("#1f77b4","#e377c2"), name = "Sexe") +
  labs(
    title    = "Niveau d'éducation par sexe — Adultes 18+ (estimations pondérées)",
    subtitle = sprintf("Chi-deux Rao-Scott p = %.4f | V de Cramér = %.3f",
                       chi2_test$p.value, v_cramer),
    x = "Niveau d'éducation", y = "Proportion pondérée (%)",
    caption  = "Barres d'erreur = IC 95%. Source : GHS-Panel W4."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank()
  )

print(p2)
ggsave("fig2_educ_sexe_adultes.png", p2, width = 9, height = 5.5, dpi = 300)
cat("Figure 2 sauvegardée : fig2_educ_sexe_adultes.png\n")

# Tableau de contingence commenté (gtsummary)
tbl_chi2 <- design_adultes %>%
  tbl_svysummary(
    include   = niveau_educ,
    by        = sexe,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
    label     = niveau_educ ~ "Niveau d'éducation"
  ) %>%
  add_overall() %>%
  add_p(test = list(all_categorical() ~ "svy.chisq.test")) %>%
  bold_labels() %>%
  modify_caption("**Tableau 1. Niveau d'éducation par sexe — Adultes 18+**")

print(tbl_chi2)

# 8. TÂCHE 10 — ÂGE ET NIVEAU D'ÉDUCATION
#    Boxplot + Kruskal-Wallis + post-hoc Dunn

df_adultes <- df %>%
  filter(age >= 18, !is.na(grp_age), !is.na(niveau_educ), !is.na(wt_wave4))

# Boxplot pondéré : on utilise les poids comme aes(weight=)
# pour que la médiane visuelle reflète la distribution pondérée
p3 <- df_adultes %>%
  ggplot(aes(x = grp_age, y = as.numeric(niveau_educ),
             fill = grp_age, weight = wt_wave4)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.3) +
  scale_y_continuous(
    breaks = 1:5,
    labels = levels(df_adultes$niveau_educ)
  ) +
  scale_fill_viridis_d(option = "C", guide = "none") +
  labs(
    title    = "Distribution du niveau d'éducation par groupe d'âge",
    subtitle = "Adultes 18+ — estimations pondérées (wt_wave4)",
    x = "Groupe d'âge", y = "Niveau d'éducation",
    caption  = "Source : GHS-Panel W4 Nigeria."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p3)
ggsave("fig3_boxplot_age_educ.png", p3, width = 8, height = 5.5, dpi = 300)
cat("Figure 3 sauvegardée : fig3_boxplot_age_educ.png\n")

# Test de Kruskal-Wallis (sur valeurs numériques de niveau_educ)
# Note : on utilise l'échantillon avec poids — le KW pondéré est approximé
# par les poids comme fréquences répétées (arrondi à l'entier le plus proche)
df_kw <- df_adultes %>%
  filter(!is.na(grp_age), !is.na(niveau_educ)) %>%
  mutate(
    niv_num  = as.numeric(niveau_educ),
    w_int    = round(wt_wave4 / min(wt_wave4, na.rm = TRUE))  # normalisation
  )

kw_result <- kruskal.test(niv_num ~ grp_age, data = df_kw,
                          weights = df_kw$w_int)
cat("\nKruskal-Wallis (niveau_educ ~ groupe d'âge) :\n")
print(kw_result)

# Post-hoc Dunn avec correction de Bonferroni
dunn_result <- df_kw %>%
  rstatix::dunn_test(niv_num ~ grp_age, p.adjust.method = "bonferroni")
cat("\nTest post-hoc de Dunn (Bonferroni) :\n")
print(dunn_result)

# Distributions pondérées par groupe d'âge
dist_age <- design %>%
  filter(age >= 18, !is.na(grp_age), !is.na(niveau_educ)) %>%
  group_by(grp_age, niveau_educ) %>%
  summarise(prop = survey_prop(vartype = "ci"))

p3b <- dist_age %>%
  ggplot(aes(x = grp_age, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_viridis_d(option = "D", direction = -1,
                       name = "Niveau d'éducation") +
  labs(
    title    = "Composition éducative par groupe d'âge (pondérée)",
    x = "Groupe d'âge", y = "Proportion (%)",
    caption  = sprintf("Kruskal-Wallis p < %.4f. Source : GHS-Panel W4.",
                       kw_result$p.value)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p3b)
ggsave("fig3b_dist_age_educ.png", p3b, width = 8, height = 5, dpi = 300)

# 9. TÂCHE 11 — TAUX DE SCOLARISATION 6-17 ANS : URBAIN vs RURAL
#    Tableau de contingence + chi-deux + graphique barres groupées IC 95%

design_617 <- design %>%
  filter(age >= 6, age <= 17, !is.na(milieu), !is.na(scol_617))

# Taux pondérés par milieu
taux_scol <- design_617 %>%
  group_by(milieu) %>%
  summarise(
    taux     = survey_mean(scol_617, vartype = "ci"),
    n_pond   = survey_total(vartype = NULL),
    n_scol   = survey_total(scol_617, vartype = NULL)
  )
cat("\nTaux de scolarisation pondéré (6-17 ans) :\n")
print(taux_scol)

# Test chi-deux Rao-Scott
chi2_scol <- svychisq(~ milieu + scol_617, design = design_617,
                      statistic = "Chisq")
cat("\nChi-deux Rao-Scott Urbain/Rural :\n")
print(chi2_scol)

# Graphique barres groupées avec IC 95%
p4 <- taux_scol %>%
  ggplot(aes(x = milieu, y = taux, fill = milieu,
             ymin = taux_low, ymax = taux_upp)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_errorbar(width = 0.2, linewidth = 0.8, color = "grey30") +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")),
            vjust = -0.8, fontface = "bold", size = 4.5) +
  scale_y_continuous(labels = label_percent(),
                     limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2196F3","#4CAF50")) +
  labs(
    title    = "Taux de scolarisation des 6-17 ans par milieu (pondéré)",
    subtitle = sprintf("Chi-deux Rao-Scott p = %.4f", chi2_scol$p.value),
    x = NULL, y = "Taux de scolarisation (%)",
    caption  = "IC 95% basés sur le plan de sondage (strates + clusters). Source : GHS-Panel W4."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p4)
ggsave("fig4_scolarisation_urbain_rural.png", p4, width = 7, height = 5, dpi = 300)
cat("Figure 4 sauvegardée : fig4_scolarisation_urbain_rural.png\n")

# Tableau commenté avec type d'établissement (sect2a → s2aq13c)
design_scol_type <- design %>%
  filter(age >= 6, age <= 17, scol_actuel == 1, !is.na(milieu)) %>%
  mutate(
    type_etab_lbl = factor(type_etab,
                           levels = 1:8,
                           labels = c("Fédéral","État","Local","Communauté",
                                      "Religieux","Privé","ONG","Autre"))
  )

taux_type <- design_scol_type %>%
  group_by(milieu, type_etab_lbl) %>%
  summarise(prop = survey_prop(vartype = "ci")) %>%
  filter(!is.na(type_etab_lbl))

p4b <- taux_type %>%
  ggplot(aes(x = type_etab_lbl, y = prop, fill = milieu,
             ymin = prop_low, ymax = prop_upp)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  geom_errorbar(position = position_dodge(0.7), width = 0.25, linewidth = 0.5) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("#2196F3","#4CAF50"), name = "Milieu") +
  labs(
    title = "Type d'établissement fréquenté par milieu (6-17 ans scolarisés)",
    x = "Type d'établissement", y = "Proportion (%)",
    caption = "IC 95%. Source : GHS-Panel W4."
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title  = element_text(face = "bold"))

print(p4b)
ggsave("fig4b_type_etab_milieu.png", p4b, width = 9, height = 5, dpi = 300)

# 10. TÂCHE 12 — HEATMAP : État × Niveau d'éducation
#     Part d'adultes SANS instruction par État, colorée par quintile

# Taux d'adultes sans instruction par État
taux_aucun_state <- design %>%
  filter(age >= 18, !is.na(state_hh), !is.na(niveau_educ)) %>%
  group_by(state_hh) %>%
  summarise(
    pct_aucun = survey_mean(niveau_educ == "Aucun", vartype = "ci")
  ) %>%
  mutate(
    quintile = ntile(pct_aucun, 5),
    quintile = factor(quintile,
                      labels = c("Q1 (le plus bas)","Q2","Q3","Q4",
                                 "Q5 (le plus élevé)"))
  )

# Taux par État ET niveau (pour la heatmap complète)
taux_state_niv <- design %>%
  filter(age >= 18, !is.na(state_hh), !is.na(niveau_educ)) %>%
  group_by(state_hh, niveau_educ) %>%
  summarise(prop = survey_prop(vartype = NULL)) %>%
  left_join(taux_aucun_state %>% select(state_hh, quintile), by = "state_hh")

# Ordonner les États par part croissante de "Aucun"
order_states <- taux_aucun_state %>%
  arrange(pct_aucun) %>%
  pull(state_hh)

taux_state_niv <- taux_state_niv %>%
  mutate(state_hh = factor(state_hh, levels = order_states))

p5 <- taux_state_niv %>%
  ggplot(aes(x = niveau_educ, y = state_hh, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       labels = label_percent(),
                       name = "Proportion (%)") +
  facet_grid(rows  = vars(quintile),
             scales = "free_y", space = "free_y") +
  labs(
    title    = "Heatmap : Niveau d'éducation des adultes (18+) par État nigérian",
    subtitle = "États ordonnés par taux croissant de sans-instruction, regroupés par quintile",
    x = "Niveau d'éducation", y = NULL,
    caption  = "Proportions pondérées (wt_wave4). Source : GHS-Panel W4 Nigeria (2018-2019)."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(angle = 20, hjust = 1),
    strip.text.y     = element_text(face = "bold", size = 8),
    legend.position  = "right",
    panel.spacing    = unit(0.3, "lines")
  )

print(p5)
ggsave("fig5_heatmap_state_educ.png", p5, width = 10, height = 13, dpi = 300)
cat("Figure 5 sauvegardée : fig5_heatmap_state_educ.png\n")

# Tableau des taux par État (classé du plus élevé au plus bas)
cat("\nTaux de sans-instruction par État (adultes 18+, pondéré) :\n")
taux_aucun_state %>%
  arrange(desc(pct_aucun)) %>%
  mutate(
    pct_fmt = paste0(round(pct_aucun * 100, 1), "%"),
    IC_95   = paste0("[", round(pct_aucun_low * 100, 1),
                     "% ; ", round(pct_aucun_upp * 100, 1), "%]")
  ) %>%
  select(État = state_hh, `Sans instruction (%)` = pct_fmt,
         `IC 95%` = IC_95, Quintile = quintile) %>%
  print(n = 37)

# 11. SYNTHÈSE : TABLEAU RÉCAPITULATIF GLOBAL (gtsummary)

tbl_global <- design %>%
  filter(age >= 18, !is.na(sexe), !is.na(niveau_educ)) %>%
  tbl_svysummary(
    include   = c(niveau_educ, sexe, grp_age, milieu),
    by        = sexe,
    statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
    label     = list(
      niveau_educ ~ "Niveau d'éducation",
      grp_age     ~ "Groupe d'âge",
      milieu      ~ "Milieu (Urbain/Rural)"
    ),
    missing   = "no"
  ) %>%
  add_overall() %>%
  add_p(test = list(all_categorical() ~ "svy.chisq.test")) %>%
  bold_labels() %>%
  modify_caption("**Tableau 2. Profil éducatif des adultes nigérians (18+) — GHS-Panel W4**")

print(tbl_global)

# 12. EXPORT DES RÉSULTATS NUMÉRIQUES
cat("\n========== EXPORT DES RÉSULTATS ==========\n")

# Sauvegarder les principaux objets dans un fichier RData
save(
  df, design,
  freq_niv, freq_sexe,
  chi2_test, v_cramer,
  kw_result, dunn_result,
  taux_scol, chi2_scol,
  taux_aucun_state,
  file = "resultats_analyse2_education.RData"
)

