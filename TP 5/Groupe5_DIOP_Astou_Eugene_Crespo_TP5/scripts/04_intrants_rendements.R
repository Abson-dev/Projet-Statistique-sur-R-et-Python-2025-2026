# =============================================================================
# TP5 - Script 04 : Intrants agricoles et rendements
# Tâche 27 : Utilisation des engrais par type et zone
# Tâche 28 : Rendement à l'hectare (maïs et mil)
# Tâche 29 : Engrais chimique vs rendement
# =============================================================================
# Note méthodologique : Les données sur les engrais (secta11c2_harvestw4)
# couvrent la saison 2018/2019 (W4). Les tâches 27-29 sont donc entièrement
# réalisées sur W4, contrairement à ce qui pouvait être envisagé initialement.
# =============================================================================

message("--- Tâches 27, 28, 29 : Intrants et Rendements ---")

# =============================================================================
# TÂCHE 27 : Taux d'utilisation des engrais par type et par zone
# =============================================================================

# df_intrants_clean contient déjà milieu (joint dans 02_donnees.R)
df_intrants_milieu <- df_intrants_clean

taux_intrants <- df_intrants_milieu %>%
  filter(!is.na(milieu)) %>%
  group_by(milieu) %>%
  summarise(
    N              = n(),
    taux_inorg     = mean(engrais_inorg == 1, na.rm = TRUE) * 100,
    taux_npk       = mean(npk == 1,           na.rm = TRUE) * 100,
    taux_uree      = mean(uree == 1,          na.rm = TRUE) * 100,
    taux_org       = mean(engrais_org == 1,   na.rm = TRUE) * 100,
    taux_pesticide = mean(pesticide == 1,     na.rm = TRUE) * 100,
    taux_herbicide = mean(herbicide == 1,     na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Format long pour le graphique
taux_long <- taux_intrants %>%
  pivot_longer(
    cols      = starts_with("taux_"),
    names_to  = "type_intrant",
    values_to = "taux"
  ) %>%
  mutate(
    type_intrant = recode(type_intrant,
                          "taux_inorg"     = "Engrais inorganique",
                          "taux_npk"       = "NPK",
                          "taux_uree"      = "Urée",
                          "taux_org"       = "Engrais organique",
                          "taux_pesticide" = "Pesticide",
                          "taux_herbicide" = "Herbicide"
    ),
    type_intrant = factor(type_intrant,
                          levels = c("Engrais inorganique", "NPK", "Urée",
                                     "Engrais organique", "Pesticide", "Herbicide"))
  )

save_table(taux_intrants, "T27_taux_utilisation_intrants")

# Test chi-deux : association zone × utilisation engrais inorganique
tab_chi2 <- df_intrants_milieu %>%
  filter(!is.na(milieu), !is.na(engrais_inorg)) %>%
  mutate(engrais_inorg_f = factor(engrais_inorg,
                                  levels = c(0, 1),
                                  labels = c("Non", "Oui")))

chi2_result <- chisq.test(tab_chi2$engrais_inorg_f, tab_chi2$milieu)
print(chi2_result)
chi2_df <- data.frame(
  statistic = chi2_result$statistic,
  df        = chi2_result$parameter,
  p         = chi2_result$p.value
)
save_table(chi2_df, "T27_chi2_engrais_milieu")

# IC à 95% par type et milieu - calcul manuel par variable
calc_ic <- function(data, var, milieu_val) {
  x <- data %>% filter(milieu == milieu_val) %>% pull(!!sym(var))
  p <- mean(x == 1, na.rm = TRUE)
  n <- sum(!is.na(x))
  data.frame(
    milieu   = milieu_val,
    intrant  = var,
    taux     = p * 100,
    n        = n,
    ic_l     = max(0, (p - 1.96 * sqrt(p * (1 - p) / n)) * 100),
    ic_u     = min(100, (p + 1.96 * sqrt(p * (1 - p) / n)) * 100)
  )
}

vars_intrants <- c("engrais_inorg", "npk", "uree",
                   "engrais_org", "pesticide", "herbicide")
milieux <- c("Rural", "Urbain")

df_base_ic <- df_intrants_milieu %>% filter(!is.na(milieu))

taux_ic_long <- do.call(rbind, lapply(milieux, function(m) {
  do.call(rbind, lapply(vars_intrants, function(v) {
    calc_ic(df_base_ic, v, m)
  }))
})) %>%
  mutate(
    intrant = recode(intrant,
                     "engrais_inorg" = "Engrais\ninorganique",
                     "npk"           = "NPK",
                     "uree"          = "Urée",
                     "engrais_org"   = "Engrais\norganique",
                     "pesticide"     = "Pesticide",
                     "herbicide"     = "Herbicide"
    )
  )

# Graphique tâche 27
p27 <- ggplot(taux_ic_long,
              aes(x = intrant, y = taux, fill = milieu,
                  ymin = ic_l, ymax = ic_u)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6, alpha = 0.9) +
  geom_errorbar(position = position_dodge(width = 0.7),
                width = 0.25, color = "grey40", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -1.8, size = 2.8) +
  scale_fill_manual(values = c("Rural" = "#4DAC26", "Urbain" = "#2166AC"),
                    name = "Milieu") +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux d'utilisation des intrants agricoles par milieu de résidence",
    subtitle = paste0("Nigeria GHS Panel W4 (2018/2019) — ",
                      "Test χ² engrais inorg. × milieu : p = ",
                      round(chi2_result$p.value, 4)),
    x        = "Type d'intrant",
    y        = "Taux d'utilisation (%) avec IC 95%",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5

save_plot(p27, "T27_taux_intrants_milieu", width = 12, height = 7)

# =============================================================================
# TÂCHE 28 : Rendement à l'hectare — Maïs (1080) et Mil (1100)
# =============================================================================

# Jointure récolte + superficie
df_rendement <- df_recolte_clean %>%
  filter(cropcode %in% c(1080, 1100)) %>%  # maïs et mil uniquement
  left_join(
    df_superficie_clean %>% select(hhid, plotid, superficie_ha),
    by = c("hhid", "plotid")
  ) %>%
  filter(!is.na(qte_kg), !is.na(superficie_ha),
         superficie_ha > 0, qte_kg > 0) %>%
  mutate(
    rendement_kgha = qte_kg / superficie_ha,
    culture_label  = case_when(
      cropcode == 1080 ~ "Maïs",
      cropcode == 1100 ~ "Mil",
      TRUE             ~ "Autre"
    )
  )

# Suppression des outliers (méthode IQR × 3)
df_rendement_clean <- df_rendement %>%
  group_by(culture_label) %>%
  mutate(
    Q1  = quantile(rendement_kgha, 0.25, na.rm = TRUE),
    Q3  = quantile(rendement_kgha, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    outlier = rendement_kgha < (Q1 - 3 * IQR) | rendement_kgha > (Q3 + 3 * IQR)
  ) %>%
  filter(!outlier) %>%
  ungroup()

message("Observations rendement avant nettoyage : ", nrow(df_rendement))
message("Observations rendement après nettoyage  : ", nrow(df_rendement_clean))

# Statistiques descriptives des rendements
stats_rdt <- df_rendement_clean %>%
  group_by(culture_label) %>%
  summarise(
    N         = n(),
    Min       = min(rendement_kgha),
    Q1        = quantile(rendement_kgha, 0.25),
    Médiane   = median(rendement_kgha),
    Moyenne   = mean(rendement_kgha),
    Q3        = quantile(rendement_kgha, 0.75),
    Max       = max(rendement_kgha),
    EcartType = sd(rendement_kgha),
    .groups   = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

print(stats_rdt)
save_table(stats_rdt, "T28_stats_rendement_mais_mil")

# Table de correspondance codes -> noms des États nigérians
etats_nigeria <- c(
  "1"  = "Abia",        "2"  = "Adamawa",    "3"  = "Akwa Ibom",
  "4"  = "Anambra",     "5"  = "Bauchi",      "6"  = "Bayelsa",
  "7"  = "Benue",       "8"  = "Borno",       "9"  = "Cross River",
  "10" = "Delta",       "11" = "Ebonyi",      "12" = "Edo",
  "13" = "Ekiti",       "14" = "Enugu",       "15" = "FCT Abuja",
  "16" = "Gombe",       "17" = "Imo",         "18" = "Jigawa",
  "19" = "Kaduna",      "20" = "Kano",        "21" = "Katsina",
  "22" = "Kebbi",       "23" = "Kogi",        "24" = "Kwara",
  "25" = "Lagos",       "26" = "Nasarawa",    "27" = "Niger",
  "28" = "Ogun",        "29" = "Ondo",        "30" = "Osun",
  "31" = "Oyo",         "32" = "Plateau",     "33" = "Rivers",
  "34" = "Sokoto",      "35" = "Taraba",      "36" = "Yobe",
  "37" = "Zamfara"
)

df_rendement_clean <- df_rendement_clean %>%
  mutate(
    nom_etat = etats_nigeria[as.character(state.x)],
    nom_etat = if_else(is.na(nom_etat), paste0("État ", state.x), nom_etat)
  )

# Boxplots des rendements par État
p28 <- ggplot(df_rendement_clean,
              aes(x = fct_reorder(nom_etat, rendement_kgha, median),
                  y = rendement_kgha, fill = culture_label)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.6,
               position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Maïs" = "#F1A340", "Mil" = "#92C5DE"),
                    name = "Culture") +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title    = "Distribution des rendements (kg/ha) par État — Maïs et Mil",
    subtitle = "Nigeria GHS Panel W4 (2018/2019) — Outliers exclus (IQR × 3)",
    x        = "État",
    y        = "Rendement (kg/ha) — échelle log",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5 +
  theme(axis.text.y = element_text(size = 7))

save_plot(p28, "T28_rendements_par_etat", width = 12, height = 10)

# =============================================================================
# TÂCHE 29 : Engrais chimique vs rendement (Wilcoxon + effet)
# =============================================================================

# Jointure rendement + utilisation engrais inorganique
df_rdt_engrais <- df_rendement_clean %>%
  left_join(
    df_intrants_clean %>% select(hhid, plotid, engrais_inorg),
    by = c("hhid", "plotid")
  ) %>%
  filter(!is.na(engrais_inorg)) %>%
  mutate(
    engrais_label = if_else(engrais_inorg == 1, "Avec engrais", "Sans engrais")
  )

# Test de Wilcoxon par culture
test_rdt_engrais <- df_rdt_engrais %>%
  group_by(culture_label) %>%
  wilcox_test(rendement_kgha ~ engrais_label) %>%
  add_significance()

print(test_rdt_engrais)
save_table(as.data.frame(test_rdt_engrais), "T29_wilcoxon_engrais_rendement")

# Taille d'effet (r de rang)
effet_rdt <- df_rdt_engrais %>%
  group_by(culture_label) %>%
  wilcox_effsize(rendement_kgha ~ engrais_label)

print(effet_rdt)
save_table(as.data.frame(effet_rdt), "T29_taille_effet_engrais_rendement")

# Statistiques médianes par groupe
stats_rdt_engrais <- df_rdt_engrais %>%
  group_by(culture_label, engrais_label) %>%
  summarise(
    N       = n(),
    Médiane = median(rendement_kgha, na.rm = TRUE),
    Moyenne = mean(rendement_kgha, na.rm = TRUE),
    .groups = "drop"
  )

save_table(stats_rdt_engrais, "T29_stats_rendement_par_engrais")

# Graphique : boxplots groupés engrais vs sans engrais
p29 <- ggplot(df_rdt_engrais,
              aes(x = engrais_label, y = rendement_kgha, fill = engrais_label)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 0.8, color = "grey40") +
  facet_wrap(~ culture_label, scales = "free_y") +
  scale_fill_manual(
    values = c("Avec engrais" = "#D01C8B", "Sans engrais" = "#92C5DE"),
    guide  = "none"
  ) +
  stat_summary(fun = median, geom = "text",
               aes(label = paste0("Méd. = ", round(after_stat(y), 0), " kg/ha")),
               vjust = -0.8, size = 3, color = "grey20") +
  labs(
    title    = "Rendements selon l'utilisation d'engrais chimique",
    subtitle = paste0("Test de Wilcoxon — Maïs : ",
                      filter(test_rdt_engrais, culture_label == "Maïs")$p.signif,
                      " | Mil : ",
                      filter(test_rdt_engrais, culture_label == "Mil")$p.signif),
    x        = NULL,
    y        = "Rendement (kg/ha)",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5

save_plot(p29, "T29_rendement_engrais", width = 12, height = 7)

message("=== Tâches 27, 28 & 29 terminées ===")