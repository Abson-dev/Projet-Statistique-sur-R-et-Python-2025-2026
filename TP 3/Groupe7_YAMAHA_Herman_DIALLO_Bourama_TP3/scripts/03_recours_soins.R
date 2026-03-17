# =============================================================================
# Analyse 3 - Script 03 : Recours aux soins selon le type de prestataire
# Tâche 15 : Fréquence de consultation selon le type de prestataire
#            (hôpital public, clinique privée, pharmacie, tradipraticien, aucun)
#            Barplot ordonné
# Auteurs   : Groupe 7 - Herman YAMAHA | Bourama DIALLO
# Données   : Nigeria GHS Panel - Wave 4 (2018), Post-Harvest
# =============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# --- 1. Chargement -----------------------------------------------------------

df_health <- readRDS("data/df_health_base.rds")

# --- 2. Dictionnaire prestataires (LABD + s4aq8) -----------------------------
# s4aq6a = qui a été consulté (praticien)
# s4aq8  = qui gère l'établissement (public/privé)
# On utilise s4aq6a pour le TYPE de praticien

# LABD codes
praticien_labels <- c(
  "0"  = "Aucun",
  "1"  = "Tradipraticien",
  "2"  = "Médecin",
  "3"  = "Dentiste",
  "4"  = "Infirmier(ère)",
  "5"  = "Assistant médical",
  "6"  = "Sage-femme",
  "7"  = "Pharmacien",
  "8"  = "Préparateur en pharmacie",
  "9"  = "Accoucheuse traditionnelle",
  "10" = "Spiritualiste",
  "11" = "Vendeur de médicaments (PMV)",
  "13" = "Autre",
  "14" = "Agent de santé comm. junior (JCHEW)",
  "15" = "Agent de santé comm. (CHEW)"
)

# Regroupement en catégories synthétiques pour le TP
groupe_praticien <- c(
  "0"  = "Aucun recours",
  "1"  = "Tradipraticien",
  "2"  = "Hôpital/Clinique (médecin)",
  "3"  = "Hôpital/Clinique (médecin)",
  "4"  = "Hôpital/Clinique (infirmier)",
  "5"  = "Hôpital/Clinique (infirmier)",
  "6"  = "Hôpital/Clinique (infirmier)",
  "7"  = "Pharmacie",
  "8"  = "Pharmacie",
  "9"  = "Tradipraticien",
  "10" = "Tradipraticien",
  "11" = "Pharmacie",
  "13" = "Autre",
  "14" = "Agent de santé comm.",
  "15" = "Agent de santé comm."
)

# --- 3. Préparation ----------------------------------------------------------

# Individus ayant déclaré maladie ET leur recours
df_recours <- df_health %>%
  filter(!is.na(s4aq3), s4aq3 == 1) %>%           # malades
  mutate(
    code_prat = as.character(
      if_else(is.na(s4aq6a) | s4aq6a == 0, 0, as.integer(s4aq6a))
    ),
    praticien_detail  = praticien_labels[code_prat],
    praticien_groupe  = groupe_praticien[code_prat]
  )

# --- 4. Tableau par type détaillé --------------------------------------------

taux_prat_detail <- df_recours %>%
  count(praticien_detail, sort = TRUE) %>%
  mutate(
    pct          = n / sum(n) * 100,
    praticien_detail = fct_reorder(praticien_detail, n)
  ) %>%
  filter(!is.na(praticien_detail))

cat("=== Recours par type de prestataire (détail) ===\n")
print(taux_prat_detail)

# --- 5. Tableau par groupe ---------------------------------------------------

taux_prat_groupe <- df_recours %>%
  filter(!is.na(praticien_groupe)) %>%
  count(praticien_groupe, sort = TRUE) %>%
  mutate(
    pct             = n / sum(n) * 100,
    praticien_ordre = fct_reorder(praticien_groupe, n)
  )

cat("\n=== Recours par groupe de prestataire ===\n")
print(taux_prat_groupe)

# Sauvegarde
write.csv(taux_prat_groupe, "outputs/tables/03_recours_prestataires.csv",
          row.names = FALSE)

# --- 6. Barplot ordonné (groupes) --------------------------------------------

couleurs_prat <- c(
  "Aucun recours"                   = "#E63946",
  "Hôpital/Clinique (médecin)"      = "#1D3557",
  "Hôpital/Clinique (infirmier)"    = "#457B9D",
  "Pharmacie"                        = "#2A9D8F",
  "Tradipraticien"                   = "#E9C46A",
  "Agent de santé comm."             = "#F4A261",
  "Autre"                            = "#999999"
)

p_recours <- ggplot(taux_prat_groupe,
                    aes(x = praticien_ordre, y = pct,
                        fill = praticien_groupe)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = couleurs_prat) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "Recours aux soins selon le type de prestataire",
    subtitle = "Parmi les individus ayant déclaré une maladie ou blessure — Wave 4 (2018)",
    x        = NULL,
    y        = "Part des individus malades (%)",
    caption  = "Source : Nigeria GHS Panel W4, sect4a_harvestw4"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40"),
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/figures/03_recours_prestataires.png", p_recours,
       width = 10, height = 6, dpi = 300)
cat("Figure sauvegardée : outputs/figures/03_recours_prestataires.png\n")

# --- 7. Taux de non-recours par milieu et sexe --------------------------------

non_recours_milieu <- df_recours %>%
  filter(!is.na(milieu_label)) %>%
  mutate(aucun = if_else(praticien_groupe == "Aucun recours", 1L, 0L,
                         missing = 0L)) %>%
  group_by(milieu_label) %>%
  summarise(
    n         = n(),
    taux_aucun = mean(aucun, na.rm = TRUE),
    .groups   = "drop"
  )

cat("\n=== Taux de non-recours par milieu ===\n")
print(non_recours_milieu)

cat("\n=== Script 03 terminé ===\n")
