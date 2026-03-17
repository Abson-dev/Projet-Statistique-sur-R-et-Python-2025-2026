# ==============================================================================
#  TP2 — Éducation et alphabétisation des membres des ménages nigérians
#  Nigeria GHS Panel · Wave 4 (2018)
#  ENSAE ISE 1 · Projet Statistique R 2025-2026
# ==============================================================================


# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

# ── 0.1  Packages ────────────────────────────────────────────────────────────
packages <- c(
  "haven", "dplyr", "tidyr", "forcats", "ggplot2", "naniar", "gtsummary",
  "rstatix", "ggpubr", "viridis", "scales", "patchwork", "PropCIs", "gt",
  "RColorBrewer", "gridExtra"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ── 0.2  Dossiers de sortie ─────────────────────────────────────────────────
dir.create("outputs/graphiques", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tableaux", recursive = TRUE, showWarnings = FALSE)

# ── 0.3  Palette de couleurs harmonisée ─────────────────────────────────────
pal <- c(
  vert_nigeria   = "#008753",
  or              = "#FFB612",
  terre_cuite     = "#C44D34",
  indigo          = "#2E3B4E",
  rose_sahel      = "#E6716F",
  sable           = "#F5E6D3",
  fond            = "#FCF9F5",
  texte           = "#2C3E50"
)

pal_educ <- c(
  "Aucun"            = "#C44D34",
  "Primaire"         = "#E67E22",
  "Junior Secondary" = "#F1C40F",
  "Senior Secondary" = "#2ECC71",
  "Tertiaire"        = "#008753"
)

pal_sexe <- c("Homme" = "#2E3B4E", "Femme" = "#E6716F")
pal_zone <- c("Urbain" = "#008753", "Rural" = "#E67E22")

# ── 0.4  Thème personnalisé ─────────────────────────────────────────────────
theme_nigeria <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = pal["fond"], color = NA),
      panel.background = element_rect(fill = pal["fond"], color = NA),
      panel.grid.major = element_line(color = "#E8E2D9", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 4, 
                                color = pal["texte"], hjust = 0, margin = margin(b = 8)),
      plot.subtitle = element_text(size = base_size, color = "#7F8C8D",
                                   hjust = 0, margin = margin(b = 12)),
      plot.caption = element_text(size = base_size - 3, color = "#BDC3C7",
                                  hjust = 1, margin = margin(t = 10)),
      axis.title = element_text(size = base_size, color = pal["texte"], face = "bold"),
      axis.text = element_text(size = base_size - 1, color = "#5D6D7E"),
      legend.position = "bottom",
      legend.background = element_rect(fill = pal["fond"], color = NA),
      legend.title = element_text(size = base_size - 1, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.key.size = unit(0.8, "cm"),
      plot.margin = margin(15, 15, 10, 15)
    )
}

save_graph <- function(nom, largeur = 10, hauteur = 6, dpi = 300) {
  ggsave(file.path("outputs/graphiques", nom), width = largeur, 
         height = hauteur, dpi = dpi, bg = pal["fond"])
  message("  ✓ ", nom)
}


# ==============================================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ==============================================================================

message("\n", strrep("═", 70))
message("  📊 CHARGEMENT DES DONNÉES")
message(strrep("═", 70))

# Chargement
sect2 <- read_dta("data/sect2_harvestw4.dta")
sect1 <- read_dta("data/sect1_harvestw4.dta")
secta <- read_dta("data/secta_harvestw4.dta")

# Recodage éducation
sect2 <- sect2 %>%
  mutate(
    scolarise = case_when(
      as.numeric(s2aq2) == 1 ~ "Scolarisé",
      as.numeric(s2aq2) == 2 ~ "Non scolarisé",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Scolarisé", "Non scolarisé")),
    
    niv_code = as.numeric(s2aq15),
    niveau_educ = case_when(
      is.na(niv_code) ~ NA_character_,
      niv_code %in% c(0, 1, 2) ~ "Aucun",
      niv_code %in% 11:16 ~ "Primaire",
      niv_code %in% 21:23 ~ "Junior Secondary",
      niv_code %in% 24:28 ~ "Senior Secondary",
      niv_code >= 31 ~ "Tertiaire",
      niv_code %in% c(51, 52, 61) ~ "Aucun",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Aucun", "Primaire", "Junior Secondary", 
                            "Senior Secondary", "Tertiaire"), ordered = TRUE)
  ) %>%
  select(hhid, indiv, scolarise, niveau_educ, niv_code)

# Recodage démographie
sect1 <- sect1 %>%
  mutate(
    sexe = case_when(
      as.numeric(s1q2) == 1 ~ "Homme",
      as.numeric(s1q2) == 2 ~ "Femme",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Homme", "Femme")),
    age = as.numeric(s1q4)
  ) %>%
  select(hhid, indiv, sexe, age)

# Recodage géographie
secta <- secta %>%
  mutate(
    zone = case_when(
      as.numeric(sector) == 1 ~ "Urbain",
      as.numeric(sector) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Urbain", "Rural")),
    state = as_factor(state)
  ) %>%
  select(hhid, zone, state)

# Jointure
df <- sect2 %>%
  left_join(sect1, by = c("hhid", "indiv")) %>%
  left_join(secta, by = "hhid")

message("\n✓ Fichier final : ", nrow(df), " individus")


# ==============================================================================
# TÂCHE 1 : VALEURS MANQUANTES AVEC POURCENTAGES
# ==============================================================================

message("\n", strrep("═", 70))
message("  📋 TÂCHE 1 - Valeurs manquantes")
message(strrep("═", 70))

# Calcul des pourcentages de NA
na_df <- df %>%
  summarise(across(c(sexe, age, zone, scolarise, niveau_educ), 
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  mutate(
    variable = case_when(
      variable == "sexe" ~ "Sexe",
      variable == "age" ~ "Âge",
      variable == "zone" ~ "Zone",
      variable == "scolarise" ~ "Scolarisation",
      variable == "niveau_educ" ~ "Niveau d'éducation"
    ),
    pct_na = round(pct_na, 1),
    label = paste0(pct_na, "%"),
    # Colorer en rouge si > 5%
    couleur = ifelse(pct_na > 5, pal["terre_cuite"], pal["vert_nigeria"])
  )

message("\n📊 Pourcentages de valeurs manquantes :")
print(na_df %>% select(variable, pct_na))

# GRAPHIQUE 1 : Barplot des pourcentages de NA
p1 <- ggplot(na_df, aes(x = reorder(variable, -pct_na), y = pct_na, fill = variable)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Sexe" = pal["indigo"],
    "Âge" = pal["or"],
    "Zone" = pal["vert_nigeria"],
    "Scolarisation" = pal["terre_cuite"],
    "Niveau d'éducation" = pal["rose_sahel"]
  )) +
  labs(
    title = "📊 Taux de valeurs manquantes par variable",
    subtitle = "Pourcentage d'observations manquantes dans chaque variable",
    x = NULL,
    y = "Pourcentage manquant (%)",
    caption = "Source: GHS Panel Wave 4 (2018)"
  ) +
  theme_nigeria() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold")
  )

print(p1)
save_graph("01_valeurs_manquantes.png", largeur = 10, hauteur = 6)


# ==============================================================================
# TÂCHE 2 : DISTRIBUTION DU NIVEAU D'ÉDUCATION
# ==============================================================================

message("\n", strrep("═", 70))
message("  📊 TÂCHE 2 - Distribution de l'éducation")
message(strrep("═", 70))

freq_educ <- df %>%
  filter(!is.na(niveau_educ)) %>%
  count(niveau_educ) %>%
  mutate(
    prop = n / sum(n),
    pct = scales::percent(prop, accuracy = 0.1),
    label = paste0(pct, "\n(n=", scales::comma(n), ")"),
    niveau_educ = fct_rev(fct_infreq(niveau_educ))
  )

p2 <- ggplot(freq_educ, aes(x = prop, y = niveau_educ, fill = niveau_educ)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = label), hjust = -0.1, size = 4, lineheight = 0.9) +
  scale_fill_manual(values = pal_educ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.3))) +
  labs(
    title = "🎓 Niveau d'éducation des membres du ménage",
    subtitle = "Wave 4 (2018) - Classement par fréquence",
    x = "Proportion (%)",
    y = NULL,
    caption = "Source: sect2_harvestw4"
  ) +
  theme_nigeria() +
  theme(legend.position = "none", panel.grid.major.y = element_blank())

print(p2)
save_graph("02_niveau_education.png", largeur = 12, hauteur = 7)


# ==============================================================================
# TÂCHE 3 : ÉDUCATION PAR SEXE (AVEC TESTS VALIDES)
# ==============================================================================

message("\n", strrep("═", 70))
message("  👥 TÂCHE 3 - Éducation par sexe")
message(strrep("═", 70))

adultes <- df %>%
  filter(!is.na(age), age >= 18, !is.na(niveau_educ), !is.na(sexe))

message("\nAdultes 18+ : ", nrow(adultes))

# Tableau de contingence
tab_sexe <- table(adultes$sexe, adultes$niveau_educ)
print(tab_sexe)

# Tests statistiques (avec gestion d'erreur)
chi2 <- tryCatch(
  chisq.test(tab_sexe),
  error = function(e) {
    message("⚠️ Erreur chi-deux: ", e$message)
    return(list(statistic = NA, p.value = NA, parameter = NA))
  }
)

cramer <- if (!is.na(chi2$statistic)) {
  cramer_v(tab_sexe)
} else {
  NA
}

message("\n🔬 Résultats des tests :")
message("  • χ² = ", ifelse(is.na(chi2$statistic), "Non calculable", round(chi2$statistic, 2)))
message("  • p = ", ifelse(is.na(chi2$p.value), "Non calculable", format.pval(chi2$p.value, digits = 4)))
message("  • V de Cramér = ", ifelse(is.na(cramer), "Non calculable", round(cramer, 4)))

# Proportions
prop_sexe <- adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe) %>%
  mutate(
    prop = n / sum(n),
    pct = scales::percent(prop, accuracy = 1)
  )

# GRAPHIQUE 3 - Redimensionné pour être entièrement visible
p3 <- ggplot(prop_sexe, aes(x = sexe, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", alpha = 0.9, width = 0.5) +
  geom_text(
    aes(label = ifelse(prop > 0.05, pct, "")),
    position = position_fill(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = pal_educ, name = "Niveau d'éducation") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  
  # Annotation des tests en bas
  annotate(
    "text",
    x = 1.5,
    y = -0.08,
    label = paste0(
      "χ² = ", ifelse(is.na(chi2$statistic), "N/A", round(chi2$statistic, 1)),
      "  ·  p = ", ifelse(is.na(chi2$p.value), "N/A", format.pval(chi2$p.value, digits = 2)),
      "  ·  V = ", ifelse(is.na(cramer), "N/A", round(cramer, 3))
    ),
    size = 4,
    color = pal["texte"]
  ) +
  
  labs(
    title = "👫 Niveau d'éducation par sexe - Adultes 18+",
    subtitle = "Répartition en pourcentage",
    x = NULL,
    y = "Proportion (%)",
    caption = "Source: sect2_harvestw4 + sect1_harvestw4"
  ) +
  
  theme_nigeria() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(15, 15, 30, 15),  # Marge en bas pour l'annotation
    panel.grid.major.x = element_blank()
  )

print(p3)
save_graph("03_education_par_sexe.png", largeur = 10, hauteur = 8)


# ==============================================================================
# TÂCHE 4 : ÉDUCATION PAR GROUPE D'ÂGE
# ==============================================================================

message("\n", strrep("═", 70))
message("  📅 TÂCHE 4 - Éducation par âge")
message(strrep("═", 70))

adultes <- adultes %>%
  mutate(
    groupe_age = cut(age, breaks = c(18, 31, 46, 61, Inf),
                     labels = c("18-30 ans", "31-45 ans", "46-60 ans", "60+ ans"),
                     right = FALSE),
    educ_num = as.numeric(niveau_educ)
  )

# Test Kruskal-Wallis
kw <- kruskal.test(educ_num ~ groupe_age, data = adultes)
message("\nKruskal-Wallis : p = ", format.pval(kw$p.value, digits = 4))

# Post-hoc Dunn
dunn <- adultes %>%
  dunn_test(educ_num ~ groupe_age, p.adjust.method = "bonferroni")

print(dunn %>% select(group1, group2, p.adj, p.adj.signif))

p4 <- ggplot(adultes, aes(x = groupe_age, y = educ_num, fill = groupe_age)) +
  geom_violin(alpha = 0.3, trim = TRUE) +
  geom_boxplot(alpha = 0.7, width = 0.2, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "white", color = pal["texte"], stroke = 1.2) +
  scale_fill_manual(values = c(pal["vert_nigeria"], pal["or"], 
                               pal["terre_cuite"], pal["indigo"])) +
  scale_y_continuous(breaks = 1:5,
                     labels = c("Aucun", "Primaire", "Jr. Sec.", "Sr. Sec.", "Tertiaire")) +
  annotate("label", x = 3.5, y = 5.4,
           label = paste0("Kruskal-Wallis : p = ", format.pval(kw$p.value, digits = 3)),
           size = 4, fill = pal["sable"], color = pal["texte"]) +
  labs(title = "📊 Niveau d'éducation par groupe d'âge",
       x = NULL, y = "Niveau d'éducation",
       caption = "Source: sect2_harvestw4 + sect1_harvestw4") +
  theme_nigeria() +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

print(p4)
save_graph("04_education_par_age.png", largeur = 12, hauteur = 7)

# Sauvegarde Dunn
write.csv(dunn, "outputs/tableaux/04_dunn_results.csv", row.names = FALSE)


# ==============================================================================
# TÂCHE 5 : SCOLARISATION 6-17 ANS (CORRECTION DU χ²)
# ==============================================================================
# ==============================================================================
# TÂCHE 5 - SCOLARISATION 6-17 ANS (VERSION CORRIGÉE)
# ==============================================================================

message("\n", strrep("═", 70))
message("  🧒 TÂCHE 5 - Scolarisation des 6-17 ans")
message(strrep("═", 70))

enfants <- df %>%
  filter(!is.na(age), age >= 6, age <= 17, !is.na(scolarise), !is.na(zone))

message("\nEnfants 6-17 ans : ", nrow(enfants))
message("  • Urbain : ", sum(enfants$zone == "Urbain"))
message("  • Rural : ", sum(enfants$zone == "Rural"))

# Tableau de contingence
tab_zone_scol <- table(enfants$zone, enfants$scolarise)
cat("\n📋 Tableau de contingence :\n")
print(tab_zone_scol)

# Vérifier s'il y a des effectifs nuls
if (any(tab_zone_scol == 0)) {
  message("\n⚠️  Attention : Des cellules ont un effectif nul.")
  message("   Le test du chi-deux peut être instable.")
}

# Test du chi-deux avec simulation si nécessaire
chi2_enf <- tryCatch(
  chisq.test(tab_zone_scol, simulate.p.value = TRUE, B = 2000),
  error = function(e) {
    message("Erreur chi-deux: ", e$message)
    return(list(statistic = NaN, p.value = NA))
  }
)

# V de Cramér - avec gestion d'erreur
cramer_enf <- tryCatch(
  cramer_v(tab_zone_scol),
  error = function(e) NA
)

# Afficher les résultats
cat("\n🔬 Résultats des tests :\n")
cat("  • χ² = ", ifelse(is.nan(chi2_enf$statistic), "Non calculable", 
                        round(chi2_enf$statistic, 2)), "\n")
cat("  • p = ", ifelse(is.na(chi2_enf$p.value), "Non calculable", 
                       format.pval(chi2_enf$p.value, digits = 4)), "\n")

# V de Cramér - différentes structures possibles
if (!is.na(cramer_enf)) {
  if (is.list(cramer_enf) && !is.null(cramer_enf$Cramers_V)) {
    v_value <- round(cramer_enf$Cramers_V, 4)
  } else if (is.numeric(cramer_enf) && length(cramer_enf) == 1) {
    v_value <- round(cramer_enf, 4)
  } else {
    v_value <- "Non calculable"
  }
} else {
  v_value <- "Non calculable"
}
cat("  • V = ", v_value, "\n")

# Proportions avec intervalles de confiance
prop_enf <- enfants %>%
  group_by(zone, scolarise) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(zone) %>%
  mutate(total = sum(n)) %>%
  rowwise() %>%
  mutate(
    prop = n / total,
    # IC de Wilson (robuste même pour petites proportions)
    ic_low = PropCIs::scoreci(n, total, 0.95)$conf.int[1],
    ic_high = PropCIs::scoreci(n, total, 0.95)$conf.int[2]
  ) %>%
  ungroup()

cat("\n📊 Proportions calculées :\n")
print(prop_enf)

# GRAPHIQUE 5
p5 <- ggplot(prop_enf %>% filter(scolarise == "Scolarisé"),
             aes(x = zone, y = prop, fill = zone)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.15, linewidth = 0.8, color = pal["texte"]) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = pal_zone) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.1))) +
  
  # Annotation conditionnelle
  annotate("label", x = 1.5, y = 0.95,
           label = paste0(
             ifelse(!is.nan(chi2_enf$statistic), 
                    paste0("χ² = ", round(chi2_enf$statistic, 1)), "χ² = N/A"),
             "  ·  p = ", 
             ifelse(!is.na(chi2_enf$p.value), 
                    format.pval(chi2_enf$p.value, digits = 2), "N/A"),
             "  ·  V = ", v_value
           ),
           size = 4, fill = pal["sable"], color = pal["texte"]) +
  
  labs(title = "📚 Taux de scolarisation des 6-17 ans",
       subtitle = "Avec intervalles de confiance à 95% (méthode de Wilson)",
       x = NULL, y = "Taux de scolarisation",
       caption = "Source: sect2_harvestw4 + sect1_harvestw4 + secta_harvestw4") +
  theme_nigeria() +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

print(p5)
save_graph("05_scolarisation_zone.png", largeur = 8, hauteur = 7)

# ==============================================================================
# TÂCHE 6 : HEATMAP PAR ÉTAT
# ==============================================================================

message("\n", strrep("═", 70))
message("  🗺️ TÂCHE 6 - Heatmap par État")
message(strrep("═", 70))

heatmap <- adultes %>%
  filter(!is.na(state)) %>%
  mutate(
    state_clean = as.character(as_factor(state)),
    state_clean = stringr::str_remove(state_clean, "^\\d+\\.\\s*")
  ) %>%
  group_by(state_clean) %>%
  summarise(
    n_total = n(),
    n_aucun = sum(niveau_educ == "Aucun", na.rm = TRUE),
    taux_aucun = n_aucun / n_total,
    .groups = "drop"
  ) %>%
  filter(n_total >= 30) %>%
  mutate(
    state_clean = fct_reorder(state_clean, taux_aucun),
    label_taux = scales::percent(taux_aucun, accuracy = 1)
  )

p6 <- ggplot(heatmap, aes(x = 1, y = state_clean, fill = taux_aucun)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label_taux), size = 3.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "inferno", direction = -1,
                       labels = scales::percent_format(accuracy = 1),
                       name = "Taux d'analphabétisme") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "🗺️ Carte de l'analphabétisme au Nigeria",
       subtitle = "Part d'adultes sans instruction par État",
       x = NULL, y = NULL,
       caption = "Source: GHS Panel Wave 4 (2018)") +
  theme_nigeria(base_size = 10) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"))

print(p6)
save_graph("06_heatmap_analphabetisme.png", largeur = 10, hauteur = 12)


# ==============================================================================
# GRAPHIQUE SUPPLÉMENTAIRE : Évolution avec l'âge
# ==============================================================================

p7 <- adultes %>%
  filter(age <= 80) %>%
  ggplot(aes(x = age, y = educ_num, group = cut_width(age, 5))) +
  geom_violin(aes(fill = stat(x)), alpha = 0.7, scale = "width") +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              color = pal["terre_cuite"], fill = pal["sable"], linewidth = 1.2) +
  scale_fill_viridis_c(option = "viridis", name = "Âge") +
  scale_y_continuous(breaks = 1:5,
                     labels = c("Aucun", "Primaire", "Jr. Sec.", "Sr. Sec.", "Tertiaire")) +
  labs(title = "📈 Évolution du niveau d'éducation avec l'âge",
       x = "Âge (années)", y = "Niveau d'éducation",
       caption = "Source: sect2_harvestw4 + sect1_harvestw4") +
  theme_nigeria() +
  theme(legend.position = "none")

print(p7)
save_graph("07_education_age_tendance.png", largeur = 12, hauteur = 7)

# GRAPHIQUE 8 : Matrice de corrélation visuelle (pour le rapport)
p8 <- adultes %>%
  count(sexe, niveau_educ) %>%
  group_by(sexe) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = niveau_educ, y = sexe, fill = prop)) +
  
  # Tuiles
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Étiquettes
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  
  # Palette
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::percent_format(),
    name = "Proportion"
  ) +
  
  # Labels
  labs(
    title = "🔷 Matrice des proportions - Sexe × Niveau d'éducation",
    subtitle = "Lecture en ligne : 100% par sexe",
    x = "Niveau d'éducation",
    y = NULL,
    caption = "Source: sect2_harvestw4 + sect1_harvestw4"
  ) +
  
  # Thème
  theme_nigeria() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

print(p8)
save_graph("08_matrice_sexe_education.png", largeur = 10, hauteur = 6)



# ==============================================================================
# TABLEAUX RÉCAPITULATIFS
# ==============================================================================

message("\n", strrep("═", 70))
message("  📋 CRÉATION DES TABLEAUX")
message(strrep("═", 70))

# Tableau 1 : Par zone
tbl_zone <- adultes %>%
  select(zone, sexe, age, niveau_educ) %>%
  tbl_summary(
    by = zone,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label = list(age = "Âge", sexe = "Sexe", niveau_educ = "Niveau d'éducation"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_caption("**Tableau 1. Caractéristiques par zone**")

tbl_zone %>% as_gt() %>% gtsave("outputs/tableaux/01_tableau_zone.html")

# Tableau 2 : Par sexe
tbl_sexe <- adultes %>%
  select(sexe, niveau_educ) %>%
  tbl_summary(by = sexe, label = list(niveau_educ = "Niveau d'éducation"),
              missing = "no") %>%
  add_p() %>%
  modify_caption("**Tableau 2. Éducation par sexe**")

tbl_sexe %>% as_gt() %>% gtsave("outputs/tableaux/02_tableau_sexe.html")

# Tableau 3 : États
tbl_etats <- heatmap %>%
  arrange(desc(taux_aucun)) %>%
  select(État = state_clean, Population = n_total, 
         `Sans instruction` = n_aucun, Taux = taux_aucun) %>%
  gt() %>%
  fmt_percent(columns = Taux, decimals = 1) %>%
  fmt_number(columns = c(Population, `Sans instruction`), decimals = 0) %>%
  data_color(columns = Taux, palette = "Reds") %>%
  tab_header(title = "Taux d'analphabétisme par État")

gtsave(tbl_etats, "outputs/tableaux/03_tableau_etats.html")

message("\n✅ TP2 terminé !")
message("  • 7 graphiques dans outputs/graphiques/")
message("  • 3 tableaux dans outputs/tableaux/")