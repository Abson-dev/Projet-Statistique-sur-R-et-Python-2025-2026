# =============================================================
# 03_analyse.R
# Tâche 2 : Analyse univariée de l'âge des membres
# =============================================================

# -- Extraire l'âge sans valeurs manquantes
age <- sect1_w4_clean %>%
  filter(!is.na(s1q4)) %>%
  pull(s1q4)

# -- Statistiques descriptives
stats_age <- data.frame(
  moyenne   = mean(age),
  mediane   = median(age),
  Q1        = quantile(age, 0.25),
  Q3        = quantile(age, 0.75),
  CV        = sd(age) / mean(age) * 100,  # coefficient de variation en %
  asymetrie = skewness(age)               # du package moments
)
sauvegarder_tab(stats_age, "T2_stats_age.csv")

# -- Histogramme (binwidth = 5)
p_hist <- ggplot(sect1_w4_clean, aes(x = s1q4)) +
  geom_histogram(binwidth = 5, fill = "#2196F3", color = "white") +
  labs(title = "Distribution de l'âge des membres – W4",
       x = "Âge (années)", y = "Effectif")

sauvegarder_fig(p_hist, "T2_histogramme_age.png")

# -- Boîte à moustaches
p_box <- ggplot(sect1_w4_clean, aes(y = s1q4)) +
  geom_boxplot(fill = "#42A5F5") +
  labs(title = "Boîte à moustaches de l'âge – W4",
       y = "Âge (années)")

sauvegarder_fig(p_box, "T2_boxplot_age.png")

# -- Test de normalité Shapiro-Wilk
# (limité à 5000 observations car c'est la limite de la fonction)
set.seed(123)
shapiro_sample <- if (length(age) > 5000) sample(age, 5000) else age
shapiro.test(shapiro_sample)






# -- Tâche 3 : Pyramide des âges par sexe (groupes de 5 ans) – W4

# -- Créer les groupes d'âge de 5 ans et recoder le sexe
df_pyramide <- sect1_w4_clean %>%
  filter(!is.na(s1q4), !is.na(s1q2)) %>%
  mutate(
    groupe_age = cut(s1q4,
                     breaks = seq(0, 100, by = 5),
                     right  = FALSE,
                     labels = paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-")),
    sexe = case_when(
      as.numeric(s1q2) == 1 ~ "Masculin",
      as.numeric(s1q2) == 2 ~ "Féminin"
    )
  ) %>%
  filter(!is.na(groupe_age), !is.na(sexe))

# -- Pyramide des âges avec apyramid
p_pyramide <- age_pyramid(
  data        = df_pyramide,
  age_group   = "groupe_age",
  split_by    = "sexe",
  proportional = TRUE
) +
  labs(title    = "Pyramide des âges – Nigeria GHS Panel W4 (2018)",
       subtitle = "Groupes quinquennaux par sexe",
       x        = "Proportion (%)",
       y        = "Groupe d'âge",
       fill     = "Sexe",
       caption  = "Source : Nigeria GHS Panel W4")

sauvegarder_fig(p_pyramide, "T3_pyramide_ages_W4.png",
                largeur = 10, hauteur = 12)







# -- Tâche 4 : Fréquences du lien de parenté avec IC à 95%

# -- Recoder le lien de parenté (s1q3)
df_parente <- sect1_w4_clean %>%
  mutate(
    parente = case_when(
      as.numeric(s1q3) == 1 ~ "Chef de ménage",
      as.numeric(s1q3) == 2 ~ "Conjoint(e)",
      as.numeric(s1q3) %in% c(3, 4, 5) ~ "Enfant",
      TRUE ~ "Autre"
    )
  ) %>%
  filter(!is.na(parente))

# -- Calculer fréquences + IC à 95% avec binom.test
n_total <- nrow(df_parente)

freq_parente <- df_parente %>%
  count(parente) %>%
  mutate(
    prop    = n / n_total,
    ic_bas  = mapply(function(x) binom.test(x, n_total)$conf.int[1], n),
    ic_haut = mapply(function(x) binom.test(x, n_total)$conf.int[2], n),
    parente = fct_reorder(parente, prop)
  )

sauvegarder_tab(freq_parente, "T4_frequences_parente.csv")

# -- Diagramme en barres horizontales
p_parente <- ggplot(freq_parente, aes(x = prop, y = parente)) +
  geom_col(fill = "#26A69A", alpha = 0.85) +
  geom_errorbar(aes(xmin = ic_bas, xmax = ic_haut),
                width = 0.2, color = "#004D40") +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            hjust = -0.2, size = 3.8) +
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title   = "Lien de parenté des membres – W4",
       x       = "Proportion (%)",
       y       = NULL,
       caption = "Source : Nigeria GHS Panel W4")

sauvegarder_fig(p_parente, "T4_lien_parente.png")





# -- Tâche 5 : Taille des ménages rural/urbain + test Wilcoxon

# -- Charger secta_w4 pour récupérer la variable zone
secta_w4 <- charger_dta("secta_harvest", 4)

# -- Calculer la taille de chaque ménage
taille_menage <- sect1_w4_clean %>%
  group_by(hhid) %>%
  summarise(taille = n(), .groups = "drop")

# -- Joindre avec secta pour avoir la zone rural/urbain
df_taille <- taille_menage %>%
  left_join(secta_w4 %>% select(hhid, zone), by = "hhid") %>%
  mutate(zone_label = case_when(
    as.numeric(zone) == 1 ~ "Urbain",
    as.numeric(zone) == 2 ~ "Rural",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(zone_label))

# -- Test de Wilcoxon-Mann-Whitney
wilcox_res <- wilcox.test(taille ~ zone_label, data = df_taille)
print(wilcox_res)

# -- Taille d'effet r de rang
r_effet <- abs(qnorm(wilcox_res$p.value / 2)) / sqrt(nrow(df_taille))
message("Taille d'effet r = ", round(r_effet, 3))

# -- Boxplot groupé
p_taille <- ggplot(df_taille, aes(x = zone_label, y = taille, fill = zone_label)) +
  geom_boxplot(alpha = 0.8) +
  annotate("text", x = 1.5, y = max(df_taille$taille) * 0.95,
           label = paste0("p = ", round(wilcox_res$p.value, 4)),
           size = 4) +
  scale_fill_manual(values = c("Rural" = "#66BB6A", "Urbain" = "#42A5F5")) +
  labs(title   = "Taille des ménages par zone – W4",
       x       = NULL,
       y       = "Nombre de membres",
       fill    = "Zone",
       caption = "Source : Nigeria GHS Panel W4")

sauvegarder_fig(p_taille, "T5_boxplot_taille_zone.png")







# -- Tâche 6 : Tableau gtsummary stratifié par zone (rural/urbain)

# -- Préparer le dataset avec âge, sexe, taille du ménage et zone
df_summary <- sect1_w4_clean %>%
  mutate(
    sexe_label = case_when(
      as.numeric(s1q2) == 1 ~ "Masculin",
      as.numeric(s1q2) == 2 ~ "Féminin"
    )
  ) %>%
  left_join(taille_menage, by = "hhid") %>%
  left_join(secta_w4 %>% select(hhid, zone_secta = zone), by = "hhid") %>%
  mutate(zone_label = case_when(
    as.numeric(zone_secta) == 1 ~ "Urbain",
    as.numeric(zone_secta) == 2 ~ "Rural"
  )) %>%
  filter(!is.na(zone_label)) %>%
  select(s1q4, sexe_label, taille, zone_label)

# -- Tableau gtsummary stratifié par zone
tableau <- df_summary %>%
  tbl_summary(
    by    = zone_label,
    label = list(
      s1q4       ~ "Âge (années)",
      sexe_label ~ "Sexe",
      taille     ~ "Taille du ménage"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_p(test = list(
    all_continuous()  ~ "wilcox.test",
    all_categorical() ~ "chisq.test"
  )) %>%
  add_overall() %>%
  bold_labels()

# -- Afficher
print(tableau)

# -- Exporter en HTML
# -- Exporter en IMAGE (PNG)
tableau %>%
  as_gt() %>%
  gt::gtsave(here("output", "tables", "T6_tableau_gtsummary.png"))

message("Tableau sauvegardé : T6_tableau_gtsummary.png")





