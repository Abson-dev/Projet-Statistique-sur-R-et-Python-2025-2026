# ==============================================================================
#  TP2 — Éducation et alphabétisation des membres des ménages nigérians
#  Nigeria GHS Panel · Wave 4 (2018)
#  ENSAE ISE 1 · Projet Statistique R 2025-2026
#  VERSION AVEC PONDÉRATIONS (wt_wave4 dans secta_harvestw4)
# ==============================================================================
#
#  PONDÉRATION UTILISÉE : wt_wave4 (poids cross-sectionnel W4)
#  Disponible dans secta_harvestw4, joint via hhid sur df
#  → Représente la population nigériane en 2018
#  → Choix justifié : toutes les analyses sont cross-sectionnelles (W4 uniquement)
#  → wt_longpanel non retenu car réservé aux analyses longitudinales
#
#  PACKAGES SUPPLÉMENTAIRES : srvyr, survey
# ==============================================================================


# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

# ── 0.1  Packages ─────────────────────────────────────────────────────────────
packages <- c(
  "haven", "dplyr", "tidyr", "forcats", "ggplot2", "naniar", "gtsummary",
  "rstatix", "ggpubr", "viridis", "scales", "patchwork", "PropCIs", "gt",
  "RColorBrewer", "gridExtra",
  "srvyr", "survey"          # ← nouveaux pour les analyses pondérées
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ── 0.2  Dossiers de sortie ───────────────────────────────────────────────────
dir.create("outputs/graphiques", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tableaux",   recursive = TRUE, showWarnings = FALSE)

# ── 0.3  Palette de couleurs harmonisée ──────────────────────────────────────
pal <- c(
  vert_nigeria = "#008753",
  or           = "#FFB612",
  terre_cuite  = "#C44D34",
  indigo       = "#2E3B4E",
  rose_sahel   = "#E6716F",
  sable        = "#F5E6D3",
  fond         = "#FCF9F5",
  texte        = "#2C3E50"
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

# ── 0.4  Thème personnalisé ───────────────────────────────────────────────────
theme_nigeria <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = pal["fond"], color = NA),
      panel.background = element_rect(fill = pal["fond"], color = NA),
      panel.grid.major = element_line(color = "#E8E2D9", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title    = element_text(face = "bold", size = base_size + 4,
                                   color = pal["texte"], hjust = 0, margin = margin(b = 8)),
      plot.subtitle = element_text(size = base_size, color = "#7F8C8D",
                                   hjust = 0, margin = margin(b = 12)),
      plot.caption  = element_text(size = base_size - 3, color = "#BDC3C7",
                                   hjust = 1, margin = margin(t = 10)),
      axis.title    = element_text(size = base_size, color = pal["texte"], face = "bold"),
      axis.text     = element_text(size = base_size - 1, color = "#5D6D7E"),
      legend.position   = "bottom",
      legend.background = element_rect(fill = pal["fond"], color = NA),
      legend.title      = element_text(size = base_size - 1, face = "bold"),
      legend.text       = element_text(size = base_size - 1),
      legend.key.size   = unit(0.8, "cm"),
      plot.margin       = margin(15, 15, 10, 15)
    )
}

save_graph <- function(nom, largeur = 10, hauteur = 6, dpi = 300) {
  ggsave(file.path("outputs/graphiques", nom),
         width = largeur, height = hauteur, dpi = dpi, bg = pal["fond"])
  message("  ✓ ", nom)
}


# ==============================================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ==============================================================================

message("\n", strrep("═", 70))
message("  CHARGEMENT DES DONNÉES")
message(strrep("═", 70))

sect2 <- read_dta("data/sect2_harvestw4.dta")
sect1 <- read_dta("data/sect1_harvestw4.dta")
secta <- read_dta("data/secta_harvestw4.dta")

# ── Recodage éducation ────────────────────────────────────────────────────────
sect2 <- sect2 %>%
  mutate(
    scolarise = case_when(
      as.numeric(s2aq2) == 1 ~ "Scolarisé",
      as.numeric(s2aq2) == 2 ~ "Non scolarisé",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Scolarisé", "Non scolarisé")),
    
    niv_code = as.numeric(s2aq15),
    niveau_educ = case_when(
      is.na(niv_code)                    ~ NA_character_,
      niv_code %in% c(0, 1, 2)          ~ "Aucun",
      niv_code %in% 11:16               ~ "Primaire",
      niv_code %in% 21:23               ~ "Junior Secondary",
      niv_code %in% c(24:28, 321)       ~ "Senior Secondary",
      niv_code %in% c(31:35, 322, 41,
                      411, 412, 42, 421,
                      422, 423, 424, 43) ~ "Tertiaire",
      niv_code %in% c(51, 52, 61)       ~ "Aucun",
      TRUE                              ~ NA_character_
    ) %>% factor(levels = c("Aucun", "Primaire", "Junior Secondary",
                            "Senior Secondary", "Tertiaire"), ordered = TRUE)
  ) %>%
  select(hhid, indiv, scolarise, niveau_educ, niv_code)

# ── Recodage démographie ──────────────────────────────────────────────────────
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

# ── Recodage géographie + POIDS ───────────────────────────────────────────────
# wt_wave4 = poids cross-sectionnel W4 → représente la population nigériane en 2018
# Joint via hhid : chaque individu reçoit le poids de son ménage
secta <- secta %>%
  mutate(
    zone = case_when(
      as.numeric(sector) == 1 ~ "Urbain",
      as.numeric(sector) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Urbain", "Rural")),
    state = as_factor(state),
    poids = wt_wave4        # ← poids de sondage ménage
  ) %>%
  select(hhid, zone, state, poids)

# ── Jointure ──────────────────────────────────────────────────────────────────
df <- sect2 %>%
  left_join(sect1, by = c("hhid", "indiv")) %>%
  left_join(secta, by = "hhid")

message("\n✓ Fichier final : ", nrow(df), " individus")
message("  Dont avec poids valide : ", sum(!is.na(df$poids)))

# ── Design de sondage pondéré (srvyr) ─────────────────────────────────────────
design_complet <- df %>%
  filter(!is.na(poids)) %>%
  as_survey_design(weights = poids, ids = hhid)

message("✓ Design de sondage créé")


# ==============================================================================
# TÂCHE 1 : VALEURS MANQUANTES
# (non pondérée — qualité de collecte = propriété de l'échantillon)
# ==============================================================================

message("\n", strrep("═", 70))
message("  TÂCHE 1 - Valeurs manquantes (non pondérée)")
message(strrep("═", 70))

na_df <- df %>%
  summarise(across(c(sexe, age, zone, scolarise, niveau_educ),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  mutate(
    variable = case_when(
      variable == "sexe"        ~ "Sexe",
      variable == "age"         ~ "Âge",
      variable == "zone"        ~ "Zone",
      variable == "scolarise"   ~ "Scolarisation",
      variable == "niveau_educ" ~ "Niveau d'éducation"
    ),
    pct_na = round(pct_na, 1),
    label  = paste0(pct_na, "%"),
    couleur = ifelse(pct_na > 5, pal["terre_cuite"], pal["vert_nigeria"])
  )

message("\nPourcentages de valeurs manquantes :")
print(na_df %>% select(variable, pct_na))

p1 <- ggplot(na_df, aes(x = reorder(variable, -pct_na), y = pct_na, fill = variable)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Sexe"              = pal["indigo"],
    "Âge"               = pal["or"],
    "Zone"              = pal["vert_nigeria"],
    "Scolarisation"     = pal["terre_cuite"],
    "Niveau d'éducation"= pal["rose_sahel"]
  )) +
  labs(
    title    = "Taux de valeurs manquantes par variable",
    subtitle = "Pourcentage d'observations manquantes — Non pondéré (qualité collecte)",
    x = NULL, y = "Pourcentage manquant (%)",
    caption  = "Source : GHS Panel Wave 4 (2018)"
  ) +
  theme_nigeria() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, face = "bold"))

print(p1)
save_graph("01_valeurs_manquantes.png", largeur = 10, hauteur = 6)


# ==============================================================================
# TÂCHE 2 : DISTRIBUTION DU NIVEAU D'ÉDUCATION  [PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 70))
message("  TÂCHE 2 - Distribution de l'éducation (pondérée)")
message(strrep("═", 70))

# Proportions pondérées via srvyr
freq_educ_pond <- design_complet %>%
  filter(!is.na(niveau_educ)) %>%
  group_by(niveau_educ) %>%
  summarise(prop = survey_mean(vartype = "ci")) %>%
  mutate(
    pct   = scales::percent(prop, accuracy = 0.1),
    label = pct
  )

cat("\nDistribution pondérée du niveau d'éducation :\n")
print(freq_educ_pond)

p2 <- ggplot(freq_educ_pond,
             aes(x = prop, y = fct_rev(niveau_educ), fill = niveau_educ)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_errorbarh(aes(xmin = prop_low, xmax = prop_upp),
                 height = 0.2, linewidth = 0.6, color = pal["texte"]) +
  geom_text(aes(label = label), hjust = -0.15, size = 4) +
  scale_fill_manual(values = pal_educ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Niveau d'éducation des membres du ménage",
    subtitle = "Wave 4 (2018) · Proportions pondérées avec IC à 95% (wt_wave4)",
    x = "Proportion (%) pondérée", y = NULL,
    caption  = "Source : sect2_harvestw4 · Pondération : wt_wave4"
  ) +
  theme_nigeria() +
  theme(legend.position = "none", panel.grid.major.y = element_blank())

print(p2)
save_graph("02_niveau_education.png", largeur = 12, hauteur = 7)


# ==============================================================================
# TÂCHE 3 : ÉDUCATION PAR SEXE  [PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 70))
message("  TÂCHE 3 - Éducation par sexe (pondérée)")
message(strrep("═", 70))

# Sous-design adultes 18+
design_adultes <- design_complet %>%
  filter(!is.na(age), age >= 18, !is.na(niveau_educ), !is.na(sexe))

# Proportions pondérées par sexe
prop_sexe_pond <- design_adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(prop = survey_mean(vartype = NULL)) %>%
  mutate(pct = scales::percent(prop, accuracy = 1))

cat("\nProportions pondérées éducation × sexe :\n")
print(prop_sexe_pond)

# Tests sur données brutes (les tests de rang/chi2 ne se pondèrent pas directement)
# On utilise le design pour le chi2 pondéré via survey
adultes_brut <- df %>%
  filter(!is.na(age), age >= 18, !is.na(niveau_educ), !is.na(sexe))

tab_sexe <- table(adultes_brut$sexe, adultes_brut$niveau_educ)
chi2     <- tryCatch(chisq.test(tab_sexe),
                     error = function(e) list(statistic = NA, p.value = NA))
cramer   <- if (!is.na(chi2$statistic)) cramer_v(tab_sexe) else NA

message("\nTests statistiques (données brutes) :")
message("  chi2 = ", round(chi2$statistic, 2),
        "  p = ", format.pval(chi2$p.value, digits = 4),
        "  V = ", round(cramer, 4))

p3 <- ggplot(prop_sexe_pond, aes(x = sexe, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", alpha = 0.9, width = 0.5) +
  geom_text(
    aes(label = ifelse(prop > 0.05, pct, "")),
    position = position_fill(vjust = 0.5),
    size = 4, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = pal_educ, name = "Niveau d'éducation") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  annotate("text", x = 1.5, y = -0.08,
           label = paste0(
             "chi2 = ", round(chi2$statistic, 1),
             "  ·  p = ", format.pval(chi2$p.value, digits = 2),
             "  ·  V = ", round(cramer, 3)
           ),
           size = 4, color = pal["texte"]) +
  labs(
    title    = "Niveau d'éducation par sexe — Adultes 18+",
    subtitle = "Proportions pondérées (wt_wave4)",
    x = NULL, y = "Proportion (%) pondérée",
    caption  = "Source : sect2_harvestw4 + sect1_harvestw4 · Pondération : wt_wave4"
  ) +
  theme_nigeria() +
  theme(legend.position = "bottom",
        plot.margin = margin(15, 15, 30, 15),
        panel.grid.major.x = element_blank())

print(p3)
save_graph("03_education_par_sexe.png", largeur = 10, hauteur = 8)


# ==============================================================================
# TÂCHE 4 : ÉDUCATION PAR GROUPE D'ÂGE  [TESTS SUR DONNÉES BRUTES]
# ==============================================================================

message("\n", strrep("═", 70))
message("  TÂCHE 4 - Éducation par âge")
message(strrep("═", 70))

adultes_brut <- adultes_brut %>%
  mutate(
    groupe_age = cut(age, breaks = c(18, 31, 46, 61, Inf),
                     labels = c("18-30 ans", "31-45 ans", "46-60 ans", "60+ ans"),
                     right = FALSE),
    educ_num = as.numeric(niveau_educ)
  )

# Kruskal-Wallis (non pondéré — test de rang)
kw <- kruskal.test(educ_num ~ groupe_age, data = adultes_brut)
message("Kruskal-Wallis : p = ", format.pval(kw$p.value, digits = 4))

# Post-hoc Dunn
dunn <- adultes_brut %>%
  dunn_test(educ_num ~ groupe_age, p.adjust.method = "bonferroni")
print(dunn %>% select(group1, group2, p.adj, p.adj.signif))

# Proportions pondérées par groupe d'âge pour le graphique
design_adultes_age <- design_complet %>%
  filter(!is.na(age), age >= 18, !is.na(niveau_educ)) %>%
  mutate(
    groupe_age = cut(age, breaks = c(18, 31, 46, 61, Inf),
                     labels = c("18-30 ans", "31-45 ans", "46-60 ans", "60+ ans"),
                     right = FALSE),
    educ_num = as.numeric(niveau_educ)
  )

p4 <- ggplot(adultes_brut,
             aes(x = groupe_age, y = educ_num, fill = groupe_age)) +
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
  labs(
    title   = "Niveau d'éducation par groupe d'âge",
    x = NULL, y = "Niveau d'éducation",
    caption = "Source : sect2_harvestw4 + sect1_harvestw4"
  ) +
  theme_nigeria() +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

print(p4)
save_graph("04_education_par_age.png", largeur = 12, hauteur = 7)

write.csv(dunn, "outputs/tableaux/04_dunn_results.csv", row.names = FALSE)


# ==============================================================================
# TÂCHE 5 : SCOLARISATION 6-17 ANS PAR ZONE  [PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 70))
message("  TÂCHE 5 - Scolarisation 6-17 ans (pondérée)")
message(strrep("═", 70))

# Sous-design enfants 6-17 ans
design_enfants <- design_complet %>%
  filter(!is.na(age), age >= 6, age <= 17,
         !is.na(scolarise), !is.na(zone))

# Proportions pondérées scolarisation × zone
prop_scol_pond <- design_enfants %>%
  group_by(zone, scolarise) %>%
  summarise(prop = survey_mean(vartype = "ci")) %>%
  ungroup()

cat("\nProportions pondérées scolarisation × zone :\n")
print(prop_scol_pond)

# Test chi2 sur données brutes (test classique)
enfants_brut <- df %>%
  filter(!is.na(age), age >= 6, age <= 17,
         !is.na(scolarise), !is.na(zone))

tab_zone_scol <- table(enfants_brut$zone, enfants_brut$scolarise)
cat("\nTableau de contingence :\n")
print(tab_zone_scol)

chi2_enf <- tryCatch(
  chisq.test(tab_zone_scol, simulate.p.value = TRUE, B = 2000),
  error = function(e) list(statistic = NaN, p.value = NA)
)

cramer_enf <- tryCatch(cramer_v(tab_zone_scol), error = function(e) NA)
v_value <- if (!is.na(cramer_enf) && is.numeric(cramer_enf))
  round(cramer_enf, 4) else "N/A"

cat("chi2 =", round(chi2_enf$statistic, 2),
    "  p =", format.pval(chi2_enf$p.value, digits = 4),
    "  V =", v_value, "\n")

# Graphique avec proportions pondérées + IC
p5 <- ggplot(prop_scol_pond %>% filter(scolarise == "Scolarisé"),
             aes(x = zone, y = prop, fill = zone)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                width = 0.15, linewidth = 0.8, color = pal["texte"]) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = pal_zone) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.1))) +
  annotate("label", x = 1.5, y = 0.95,
           label = paste0(
             "chi2 = ", ifelse(is.nan(chi2_enf$statistic), "N/A",
                               round(chi2_enf$statistic, 1)),
             "  ·  p = ", ifelse(is.na(chi2_enf$p.value), "N/A",
                                 format.pval(chi2_enf$p.value, digits = 2)),
             "  ·  V = ", v_value
           ),
           size = 4, fill = pal["sable"], color = pal["texte"]) +
  labs(
    title    = "Taux de scolarisation des 6-17 ans",
    subtitle = "Proportions pondérées avec IC à 95% (wt_wave4)",
    x = NULL, y = "Taux de scolarisation pondéré",
    caption  = "Source : sect2_harvestw4 + sect1_harvestw4 + secta_harvestw4"
  ) +
  theme_nigeria() +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

print(p5)
save_graph("05_scolarisation_zone.png", largeur = 8, hauteur = 7)


# ==============================================================================
# TÂCHE 6 : HEATMAP PAR ÉTAT  [PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 70))
message("  TÂCHE 6 - Heatmap par État (pondérée)")
message(strrep("═", 70))

# Taux d'analphabétisme pondéré par état
# Note : on utilise weighted.mean car srvyr group_by sur state+niveau_educ
#        est équivalent ici
adultes_pond <- df %>%
  filter(!is.na(age), age >= 18, !is.na(niveau_educ),
         !is.na(state), !is.na(poids)) %>%
  mutate(
    state_clean = stringr::str_remove(as.character(as_factor(state)), "^\\d+\\.\\s*"),
    sans_instruct = (niveau_educ == "Aucun")
  )

heatmap <- adultes_pond %>%
  group_by(state_clean) %>%
  summarise(
    n_total    = n(),
    taux_aucun = weighted.mean(sans_instruct, poids, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  filter(n_total >= 30) %>%
  mutate(
    state_clean = fct_reorder(state_clean, taux_aucun),
    label_taux  = scales::percent(taux_aucun, accuracy = 1)
  )

cat("\nTop 5 états — taux d'analphabétisme pondéré le plus élevé :\n")
heatmap %>% arrange(desc(taux_aucun)) %>% head(5) %>% print()

p6 <- ggplot(heatmap, aes(x = 1, y = state_clean, fill = taux_aucun)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label_taux),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "inferno", direction = -1,
                       labels = scales::percent_format(accuracy = 1),
                       name = "Taux d'analphabétisme") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title    = "Carte de l'analphabétisme au Nigeria",
    subtitle = "Part pondérée d'adultes sans instruction par État (wt_wave4)",
    x = NULL, y = NULL,
    caption  = "Source : GHS Panel Wave 4 (2018) · Pondération : wt_wave4"
  ) +
  theme_nigeria(base_size = 10) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"))

print(p6)
save_graph("06_heatmap_analphabetisme.png", largeur = 10, hauteur = 12)


# ==============================================================================
# GRAPHIQUE 7 : Évolution avec l'âge
# ==============================================================================

p7 <- adultes_brut %>%
  filter(age <= 80) %>%
  ggplot(aes(x = age, y = educ_num, group = cut_width(age, 5))) +
  geom_violin(aes(fill = stat(x)), alpha = 0.7, scale = "width") +
  geom_smooth(aes(weight = adultes_brut %>%
                    filter(age <= 80) %>%
                    left_join(df %>% select(hhid, indiv, poids),
                              by = c("hhid", "indiv")) %>%
                    pull(poids),
                  group = 1),
              method = "loess", se = TRUE,
              color = pal["terre_cuite"], fill = pal["sable"], linewidth = 1.2) +
  scale_fill_viridis_c(option = "viridis", name = "Âge") +
  scale_y_continuous(breaks = 1:5,
                     labels = c("Aucun", "Primaire", "Jr. Sec.", "Sr. Sec.", "Tertiaire")) +
  labs(
    title   = "Évolution du niveau d'éducation avec l'âge",
    x = "Âge (années)", y = "Niveau d'éducation",
    caption = "Source : sect2_harvestw4 + sect1_harvestw4"
  ) +
  theme_nigeria() +
  theme(legend.position = "none")

print(p7)
save_graph("07_education_age_tendance.png", largeur = 12, hauteur = 7)


# ==============================================================================
# GRAPHIQUE 8 : Matrice Sexe × Éducation  [PONDÉRÉE]
# ==============================================================================

mat_pond <- design_adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(prop = survey_mean(vartype = NULL)) %>%
  ungroup()

p8 <- ggplot(mat_pond, aes(x = niveau_educ, y = sexe, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma",
                       labels = scales::percent_format(),
                       name = "Proportion pondérée") +
  labs(
    title    = "Matrice des proportions — Sexe × Niveau d'éducation",
    subtitle = "Lecture en ligne : 100% par sexe · Proportions pondérées (wt_wave4)",
    x = "Niveau d'éducation", y = NULL,
    caption  = "Source : sect2_harvestw4 + sect1_harvestw4 · Pondération : wt_wave4"
  ) +
  theme_nigeria() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"))

print(p8)
save_graph("08_matrice_sexe_education.png", largeur = 10, hauteur = 6)


# ==============================================================================
# TABLEAUX RÉCAPITULATIFS  [PONDÉRÉS]
# ==============================================================================

message("\n", strrep("═", 70))
message("  CRÉATION DES TABLEAUX (pondérés)")
message(strrep("═", 70))

# Tableau 1 : Par zone (pondéré via tbl_svysummary)
design_tbl <- design_complet %>%
  filter(!is.na(age), age >= 18, !is.na(zone))

tbl_zone <- design_tbl %>%
  tbl_svysummary(
    by      = zone,
    include = c(sexe, age, niveau_educ),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n_unweighted} ({p}%)"),
    label   = list(age = "Âge", sexe = "Sexe", niveau_educ = "Niveau d'éducation"),
    digits  = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous()  ~ "svy.wilcox.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Tableau 1. Caractéristiques par zone — Pondéré (wt_wave4)**") %>%
  modify_footnote(update = everything() ~
                    "Statistiques pondérées (wt_wave4). Moyenne (écart-type) pour les variables continues ; n_brut (% pondéré) pour les catégorielles.")

tbl_zone %>% as_gt() %>% gtsave("outputs/tableaux/01_tableau_zone.html")
message("  ✓ 01_tableau_zone.html")

# Tableau 2 : Par sexe (pondéré)
tbl_sexe <- design_complet %>%
  filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(niveau_educ)) %>%
  tbl_svysummary(
    by      = sexe,
    include = niveau_educ,
    label   = list(niveau_educ = "Niveau d'éducation"),
    missing = "no"
  ) %>%
  add_p(test = list(all_categorical() ~ "svy.chisq.test")) %>%
  bold_labels() %>%
  modify_caption("**Tableau 2. Éducation par sexe — Pondéré (wt_wave4)**")

tbl_sexe %>% as_gt() %>% gtsave("outputs/tableaux/02_tableau_sexe.html")
message("  ✓ 02_tableau_sexe.html")

# Tableau 3 : États (taux pondérés)
tbl_etats <- heatmap %>%
  arrange(desc(taux_aucun)) %>%
  select(État = state_clean, `N individus` = n_total,
         `Taux pondéré` = taux_aucun) %>%
  gt() %>%
  fmt_percent(columns = `Taux pondéré`, decimals = 1) %>%
  fmt_number(columns = `N individus`, decimals = 0) %>%
  data_color(columns = `Taux pondéré`, palette = "Reds") %>%
  tab_header(
    title    = "Taux d'analphabétisme pondéré par État",
    subtitle = "Adultes 18+ · Pondération : wt_wave4"
  )

gtsave(tbl_etats, "outputs/tableaux/03_tableau_etats.html")
message("  ✓ 03_tableau_etats.html")

# Tableau 4 : Dunn (déjà produit en CSV)
message("  ✓ 04_dunn_results.csv")

message("\n✅ TP2 pondéré terminé !")
message("  • 8 graphiques dans outputs/graphiques/")
message("  • 4 tableaux dans outputs/tableaux/")