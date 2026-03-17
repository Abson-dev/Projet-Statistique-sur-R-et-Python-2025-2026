# =============================================================================
# TABLEAU RÉCAPITULATIF GTSUMMARY - STRATIFIÉ PAR SECTEUR
# Variables : âge (s1q4), sexe (s1q2), taille du ménage
# Stratification : sector (1=Urbain, 2=Rural)
# =============================================================================


library(haven)
library(dplyr)
library(gtsummary)
library(gt)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET PRÉPARATION
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

# --- Taille du ménage (calculée à partir de sect1) --------------------------
taille_menage <- sect1 %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

# --- Données individuelles --------------------------------------------------
data_tableau <- sect1 %>%
  left_join(taille_menage, by = "hhid") %>%
  filter(!is.na(sector)) %>%
  transmute(
    # Secteur (variable de stratification)
    secteur = factor(
      as.numeric(sector),
      levels = c(1, 2),
      labels = c("Urbain", "Rural")
    ),

    # Âge (quantitative continue)
    age = as.numeric(s1q4),

    # Sexe (qualitative binaire)
    sexe = factor(
      as.numeric(s1q2),
      levels = c(1, 2),
      labels = c("Masculin", "Féminin")
    ),

    # Taille du ménage (quantitative discrète)
    taille_menage = taille_menage
  )

cat("Dimensions du tableau :", nrow(data_tableau), "x", ncol(data_tableau), "\n")
cat("Urbain :", sum(data_tableau$secteur == "Urbain", na.rm = TRUE), "\n")
cat("Rural  :", sum(data_tableau$secteur == "Rural",  na.rm = TRUE), "\n")


# -----------------------------------------------------------------------------
# 2. TABLEAU GTSUMMARY STRATIFIÉ
# -----------------------------------------------------------------------------
tableau <- data_tableau %>%
  tbl_summary(
    by = secteur,       # Stratification par secteur

    include = c(age, sexe, taille_menage),

    # Statistiques affichées
    statistic = list(
      all_continuous()  ~ "{mean} ({sd}) \n Méd. {median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),

    # Libellés des variables
    label = list(
      age           ~ "Âge (années)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du ménage (nb. membres)"
    ),

    # Chiffres significatifs
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 1)
    ),

    # Traitement des valeurs manquantes
    missing      = "ifany",
    missing_text = "Valeurs manquantes"
  ) %>%

  # Ajouter la colonne "Total"
  add_overall(last = FALSE, col_label = "**Total**") %>%

  # Ajouter les p-valeurs des tests de comparaison
  # - Wilcoxon pour les variables continues (non normales)
  # - Chi² pour les variables catégorielles
  add_p(
    test = list(
      age           ~ "wilcox.test",
      sexe          ~ "chisq.test",
      taille_menage ~ "wilcox.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%

  # Ajouter les effectifs par groupe en en-tête
  add_n() %>%

  # Mise en forme finale
  modify_header(
    label      ~ "**Variable**",
    stat_0     ~ "**Total**\nN = {N}",
    stat_1     ~ "**Urbain**\nN = {n}",
    stat_2     ~ "**Rural**\nN = {n}",
    p.value    ~ "**p-valeur**"
  ) %>%

  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Secteur de résidence**"
  ) %>%

  modify_caption(
    "**Tableau 1. Caractéristiques sociodémographiques selon le secteur**
     Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) %>%

  # Note de bas de tableau
  modify_footnote(
    p.value ~ "Wilcoxon-Mann-Whitney pour variables continues ;
               Chi² de Pearson pour variables catégorielles"
  ) %>%

  bold_labels() %>%
  bold_p(t = 0.05)    # Met en gras les p-valeurs significatives


# -----------------------------------------------------------------------------
# 3. AFFICHAGE
# -----------------------------------------------------------------------------
tableau


# -----------------------------------------------------------------------------
# 4. EXPORT
# -----------------------------------------------------------------------------

# --- Export en Xlsx ---------------------------

install.packages("openxlsx")  # si pas encore installé
library(openxlsx)


# Étape 1 : convertir en data frame
tableau_df <- tableau %>%
  as_tibble() %>%
  as.data.frame()

# Vérifier
print(head(tableau_df))

# Étape 2 : créer le fichier Excel
wb <- createWorkbook()

# Étape 3 : ajouter une feuille
addWorksheet(wb, "Statistiques")

# Étape 4 : écrire les données
writeData(wb,
          sheet   = "Statistiques",
          x       = tableau_df,
          startRow = 1,
          startCol = 1,
          rowNames = FALSE)

# Étape 5 : mise en forme en-tête
addStyle(wb,
         sheet = "Statistiques",
         style = createStyle(fontColour      = "white",
                             fgFill          = "#1E88E5",
                             textDecoration  = "bold",
                             halign          = "center"),
         rows      = 1,
         cols      = 1:ncol(tableau_df),
         gridExpand = TRUE)

# Étape 6 : ajuster largeur colonnes
setColWidths(wb,
             sheet  = "Statistiques",
             cols   = 1:ncol(tableau_df),
             widths = "auto")

# Étape 7 : sauvegarder
saveWorkbook(wb,
             "stats_descriptives_GHSP_W4.xlsx",
             overwrite = TRUE)

cat("Tableau exporté avec succès !\n")

