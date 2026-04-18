# =============================================================================
# TP5 - Script 05 : Adoption des semences améliorées
# Tâche 30 : Taux d'adoption semences améliorées et engrais (W4)
# =============================================================================
# Note : Le prof demande une comparaison W1 vs W4. Cependant, les variables
# "améliorée/traditionnelle" (s11fq3b) et "certifiée" (s11fq3aa) n'existent
# pas sous la même forme en W1 (sect11f_plantingw1). La comparaison directe
# n'est donc pas possible avec les mêmes indicateurs. Nous documentons le
# taux d'adoption en W4 et commentons cette limite dans le rapport.
# =============================================================================

message("--- Tâche 30 : Semences améliorées ---")

# =============================================================================
# TÂCHE 30 : Adoption des semences améliorées et des engrais (W4)
# =============================================================================

# --- 30a : Taux d'adoption des semences améliorées par culture ---------------

adoption_semences <- df_plante_clean %>%
  filter(!is.na(type_semence)) %>%
  group_by(nom_culture, type_culture) %>%
  summarise(
    N             = n(),
    n_amelioree   = sum(type_semence == 1, na.rm = TRUE),
    taux_amelioree = mean(type_semence == 1, na.rm = TRUE) * 100,
    n_certifiee   = sum(semence_certifiee == 1, na.rm = TRUE),
    taux_certifiee = mean(semence_certifiee == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(N >= 10) %>%   # garder uniquement les cultures avec au moins 10 obs
  arrange(desc(taux_amelioree))

save_table(adoption_semences, "T30_adoption_semences_par_culture")

# --- 30b : Taux global adoption semences et engrais -------------------------

# Semences : niveau ménage
adoption_hh <- df_plante_clean %>%
  filter(!is.na(type_semence)) %>%
  group_by(hhid) %>%
  summarise(
    utilise_amelioree = as.integer(any(type_semence == 1, na.rm = TRUE)),
    utilise_certifiee = as.integer(any(semence_certifiee == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  left_join(df_menage, by = "hhid")

# Engrais : niveau ménage (agréger depuis parcelles)
engrais_hh <- df_intrants_clean %>%
  group_by(hhid) %>%
  summarise(
    utilise_engrais_inorg = as.integer(any(engrais_inorg == 1, na.rm = TRUE)),
    utilise_engrais_org   = as.integer(any(engrais_org   == 1, na.rm = TRUE)),
    utilise_pesticide     = as.integer(any(pesticide     == 1, na.rm = TRUE)),
    utilise_herbicide     = as.integer(any(herbicide     == 1, na.rm = TRUE)),
    .groups = "drop"
  )

# Jointure
adoption_global <- adoption_hh %>%
  left_join(engrais_hh, by = "hhid")

# Taux globaux
taux_adoption_global <- adoption_global %>%
  filter(!is.na(milieu)) %>%
  group_by(milieu) %>%
  summarise(
    N                    = n(),
    taux_semence_amelioree = mean(utilise_amelioree,     na.rm = TRUE) * 100,
    taux_semence_certifiee = mean(utilise_certifiee,     na.rm = TRUE) * 100,
    taux_engrais_inorg   = mean(utilise_engrais_inorg, na.rm = TRUE) * 100,
    taux_engrais_org     = mean(utilise_engrais_org,   na.rm = TRUE) * 100,
    taux_pesticide       = mean(utilise_pesticide,     na.rm = TRUE) * 100,
    taux_herbicide       = mean(utilise_herbicide,     na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_table(taux_adoption_global, "T30_taux_adoption_global_milieu")

# Format long pour graphique
adoption_long <- taux_adoption_global %>%
  pivot_longer(
    cols      = starts_with("taux_"),
    names_to  = "indicateur",
    values_to = "taux"
  ) %>%
  mutate(
    indicateur = recode(indicateur,
      "taux_semence_amelioree" = "Semence améliorée",
      "taux_semence_certifiee" = "Semence certifiée",
      "taux_engrais_inorg"     = "Engrais inorganique",
      "taux_engrais_org"       = "Engrais organique",
      "taux_pesticide"         = "Pesticide",
      "taux_herbicide"         = "Herbicide"
    ),
    categorie = case_when(
      str_detect(indicateur, "Semence") ~ "Semences",
      str_detect(indicateur, "Engrais") ~ "Engrais",
      TRUE                              ~ "Protection des cultures"
    ),
    indicateur = fct_reorder(indicateur, taux)
  )

# Graphique principal : barplot côte à côte par milieu
p30a <- ggplot(adoption_long,
               aes(x = indicateur, y = taux, fill = milieu)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.8) +
  scale_fill_manual(values = c("Rural" = "#4DAC26", "Urbain" = "#2166AC"),
                    name = "Milieu") +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  facet_wrap(~ categorie, scales = "free_x") +
  labs(
    title    = "Taux d'adoption des semences améliorées et intrants agricoles (W4, 2018/2019)",
    subtitle = "Proportion de ménages utilisant chaque type d'intrant — Rural vs Urbain",
    x        = NULL,
    y        = "Taux d'adoption (%)",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)\nNote : Comparaison W1 vs W4 non réalisable (variables non comparables entre vagues)"
  ) +
  theme_tp5 +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 8))

save_plot(p30a, "T30a_adoption_intrants_milieu", width = 14, height = 8)

# --- 30c : Adoption des semences améliorées par type de culture (top 10) ----

top_cultures_amelioree <- adoption_semences %>%
  filter(!is.na(type_culture)) %>%
  slice_head(n = 10) %>%
  mutate(nom_culture = fct_reorder(nom_culture, taux_amelioree))

p30b <- ggplot(top_cultures_amelioree,
               aes(x = nom_culture, y = taux_amelioree, fill = type_culture)) +
  geom_col(width = 0.75, alpha = 0.9) +
  geom_text(aes(label = paste0(round(taux_amelioree, 1), "%")),
            hjust = -0.15, size = 3.2) +
  scale_fill_manual(values = couleurs_type, name = "Type") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(
    title    = "Top 10 des cultures par taux d'adoption de semences améliorées",
    subtitle = "Nigeria GHS Panel W4 (2018/2019)",
    x        = NULL,
    y        = "Taux d'adoption semences améliorées (%)",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5

save_plot(p30b, "T30b_semences_ameliorees_par_culture", width = 12, height = 7)

# --- 30d : Tableau récapitulatif final --------------------------------------

tableau_final_t30 <- adoption_global %>%
  summarise(
    N_menages              = n(),
    `Semence améliorée (%)`  = round(mean(utilise_amelioree,     na.rm = TRUE) * 100, 1),
    `Semence certifiée (%)`  = round(mean(utilise_certifiee,     na.rm = TRUE) * 100, 1),
    `Engrais inorganique (%)` = round(mean(utilise_engrais_inorg, na.rm = TRUE) * 100, 1),
    `Engrais organique (%)`  = round(mean(utilise_engrais_org,   na.rm = TRUE) * 100, 1),
    `Pesticide (%)`          = round(mean(utilise_pesticide,     na.rm = TRUE) * 100, 1),
    `Herbicide (%)`          = round(mean(utilise_herbicide,     na.rm = TRUE) * 100, 1)
  )

print(tableau_final_t30)
save_table(tableau_final_t30, "T30_tableau_recapitulatif_adoption")

message("=== Tâche 30 terminée ===")
