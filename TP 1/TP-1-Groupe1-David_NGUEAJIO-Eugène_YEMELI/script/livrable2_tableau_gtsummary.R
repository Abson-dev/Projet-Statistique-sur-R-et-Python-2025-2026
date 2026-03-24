# =============================================================================
# LIVRABLE 2 — TABLEAU GTSUMMARY EXPORTABLE
# Statistiques démographiques par zone (Urbain / Rural)
# Variables : âge (s1q4), sexe (s1q2), taille du ménage
# GHSP Nigeria — Wave 4 (2018/19) | sect1_harvestw4
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# install.packages(c("haven", "dplyr", "gtsummary", "gt", "flextable",
#                    "webshot2"))

library(haven)
library(dplyr)
library(gtsummary)
library(gt)
library(ggplot2)
library(scales)
library(forcats)
library(openxlsx)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET PRÉPARATION
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

# Taille du ménage
taille_menage <- sect1 %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

# Données pour le tableau
data_tab <- sect1 %>%
  left_join(taille_menage, by = "hhid") %>%
  filter(!is.na(sector)) %>%
  transmute(
    secteur = factor(
      as.numeric(sector),
      levels = c(1, 2),
      labels = c("Urbain", "Rural")
    ),
    age = as.numeric(s1q4),
    sexe = factor(
      as.numeric(s1q2),
      levels = c(1, 2),
      labels = c("Masculin", "Féminin")
    ),
    taille_menage = taille_menage
  )

cat("N total :", nrow(data_tab), "\n")
cat("Urbain  :", sum(data_tab$secteur == "Urbain", na.rm = TRUE), "\n")
cat("Rural   :", sum(data_tab$secteur == "Rural",  na.rm = TRUE), "\n")


# -----------------------------------------------------------------------------
# 2. CONSTRUCTION DU TABLEAU
# -----------------------------------------------------------------------------
tableau <- data_tab %>%
  tbl_summary(
    by      = secteur,
    include = c(age, sexe, taille_menage),

    statistic = list(
      all_continuous()  ~ "{mean} ({sd})\nMéd. {median} [{p25} – {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),

    label = list(
      age           ~ "Âge (années)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du ménage (membres)"
    ),

    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 1)
    ),

    missing      = "ifany",
    missing_text = "Manquants"
  ) %>%

  add_overall(last = FALSE, col_label = "**Total**\nN = {N}") %>%

  add_p(
    test = list(
      age           ~ "wilcox.test",
      sexe          ~ "chisq.test",
      taille_menage ~ "wilcox.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%

  add_n() %>%

  modify_header(
    label   ~ "**Variable**",
    stat_1  ~ "**Urbain**\nN = {n}",
    stat_2  ~ "**Rural**\nN = {n}",
    p.value ~ "**p-valeur**"
  ) %>%

  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Secteur de résidence**"
  ) %>%

  modify_caption(
    "**Tableau 1. Caractéristiques démographiques selon le secteur de résidence**
     GHSP Nigeria — Wave 4 (2018/19), Post-Harvest Visit"
  ) %>%

  modify_footnote(
    p.value ~ paste0(
      "Test de Wilcoxon-Mann-Whitney pour variables continues ; ",
      "Test du Chi² de Pearson pour variables catégorielles. ",
      "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)."
    )
  ) %>%

  bold_labels() %>%
  bold_p(t = 0.05)

# Affichage dans RStudio Viewer
tableau


# -----------------------------------------------------------------------------
# 3. EXPORTS
# -----------------------------------------------------------------------------
chemin <- "output/tablolib2"

# ── Export HTML ───────────────────────────────────────────────────────────
tableau %>%
  as_gt() %>%
  gt::gtsave(paste0(chemin, "tableau_demo_W4.html"))

cat("Export HTML : tableau_demo_W4.html\n")

