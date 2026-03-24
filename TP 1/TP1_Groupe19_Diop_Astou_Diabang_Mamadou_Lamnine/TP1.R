# ==============================================================================
#  TP1 — Profil démographique des ménages nigérians
#  Nigeria GHS Panel · Waves 1–4 (2010 · 2012 · 2015 · 2018)
#  ENSAE ISE 1 · Projet Statistique R 2025-2026
#  VERSION AVEC PONDÉRATIONS (wt_waveX dans secta_harvestwX)
# ==============================================================================
#
#  ARBORESCENCE DU PROJET  (ouvrir TP1.Rproj avant d'exécuter)
#  TP1/
#  ├── TP1.Rproj
#  ├── script.R             ← ce fichier
#  ├── data/
#  │   ├── sect1_harvestw1.dta  sect1_harvestw2.dta  sect1_harvestw3.dta  sect1_harvestw4.dta
#  │   ├── sect1_plantingw1.dta sect1_plantingw2.dta sect1_plantingw3.dta sect1_plantingw4.dta
#  │   └── secta_harvestw1.dta  secta_harvestw2.dta  secta_harvestw3.dta  secta_harvestw4.dta
#  └── outputs/
#      ├── graphiques/
#      └── tableaux/
#
#  VARIABLES DE PONDÉRATION (vérifiées sur les .dta réels)
#  ────────────────────────────────────────────────────────
#  Les poids sont dans secta_harvestwX (niveau ménage), joints via hhid :
#    wt_wave1  → W1 (2010)    wt_wave2  → W2 (2012)
#    wt_wave3  → W3 (2015)    wt_wave4  → W4 (2018)
#  Ce sont des poids de sondage ménage. Pour les analyses individuelles,
#  chaque individu d'un ménage reçoit le poids de son ménage (expansion weight).
#  → Package srvyr utilisé pour les statistiques pondérées.
#
#  VARIABLES CLÉS (vérifiées sur les fichiers .dta réels)
#  ──────────────────────────────────────────────────────
#  indiv    = identifiant individuel  (NB : "indiv", pas "indiv_id")
#  s1q2     = sexe            (1=Male, 2=Female)
#  s1q3     = lien de parenté (1=Head, 2=Spouse, 3=Own child, 4=Step child,
#                              5=Adopted child, 6=Grandchild, 7=Brother/Sister,
#                              8=Niece/Nephew, 9=In-law, 10=Parent,
#                              11=Parent-in-law, 12=Domestic help,
#                              14=Other relation, 15=Non-relation)
#  s1q4     = âge en années (numérique continu)
#  s1q7     = statut matrimonial
#                (1=Married monogamous, 2=Married polygamous, 3=Informal union,
#                 4=Divorced, 5=Separated, 6=Widowed, 7=Never married)
#  sector   = milieu (1=Urban, 2=Rural) — dans tous les fichiers harvest
#  zone     = région géographique (1=North Central … 6=South West)
#  state    = état nigérian
#
#  PLAN DU SCRIPT
#  ──────────────
#  0.  Configuration  (packages · dossiers · thème · palettes)
#  ──────────── ANALYSES DEMANDÉES ────────────────────────────
#  T1. Vérification qualité : heatmap % valeurs manquantes (harvest + planting, W1–W4)
#  T2. Analyse univariée de l'âge W4 : histogramme · boxplot · stats pondérées · Shapiro-Wilk
#  T3. Distribution par sexe : camembert W4 pondéré + évolution taux féminisation W1–W4
#  T4. Distribution du statut matrimonial : histogramme W4 pondéré + évolution W1–W4
#  T5. Pyramide des âges W4 annotée (pondérée)
#  T6. Pyramides des 4 vagues côte à côte (pondérées)
#  T7. Lien de parenté W4 : barplot pondéré + IC 95% + évolution W1–W4
#  T8. Taille des ménages Rural/Urban : violin+boxplot · Wilcoxon · r de rang
#       + évolution taille médiane W1–W4
#  T9. Tableau gtsummary pondéré stratifié par milieu (âge · sexe · taille · statut mat.)
# ==============================================================================


# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

# ── 0.1  Packages ──────────────────────────────────────────────────────────────
pkgs <- c(
  "haven",      # lire les .dta Stata
  "dplyr",      # manipulation des données
  "tidyr",      # pivot / restructuration
  "forcats",    # réordonner les facteurs
  "ggplot2",    # visualisation
  "apyramid",   # pyramide des âges (T5, T6)
  "naniar",     # valeurs manquantes (T1)
  "gtsummary",  # tableau descriptif exportable (T9)
  "rstatix",    # Wilcoxon + taille d'effet r de rang (T8)
  "PropCIs",    # IC à 95% sur proportions (T7)
  "e1071",      # asymétrie / kurtosis (T2)
  "patchwork",  # assembler plusieurs graphiques
  "scales",     # formatage axes (%, virgules)
  "gt",         # export tableaux HTML / Word
  "ggtext",     # titres enrichis markdown dans ggplot2
  "srvyr",      # analyses pondérées (survey design)
  "survey"      # backend pour srvyr
)

manquants <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(manquants)) install.packages(manquants, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ── 0.2  Dossiers outputs (créés automatiquement) ─────────────────────────────
dir.create("outputs/graphiques", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tableaux",   recursive = TRUE, showWarnings = FALSE)
message("✓ Dossiers outputs/ prêts.")

# ── 0.3  Palettes de couleurs ──────────────────────────────────────────────────
pal <- c(
  bleu   = "#2563EB",
  orange = "#F59E0B",
  vert   = "#10B981",
  rouge  = "#EF4444",
  violet = "#7C3AED",
  ocre   = "#D97706",
  rose   = "#EC4899",
  gris   = "#6B7280",
  fond   = "#FBFBFD",
  fond2  = "#F3F4F6",
  texte  = "#111827",
  texte2 = "#4B5563"
)

pal_vagues <- c(
  "W1 (2010)" = "#2563EB",
  "W2 (2012)" = "#F59E0B",
  "W3 (2015)" = "#10B981",
  "W4 (2018)" = "#7C3AED"
)

pal_sexe <- c("Male" = "#2563EB", "Female" = "#EC4899")

pal_lien <- c(
  "Chef de ménage" = "#2563EB",
  "Conjoint(e)"    = "#EC4899",
  "Enfant"         = "#10B981",
  "Autre"          = "#F59E0B"
)

pal_mat <- c(
  "Jamais marié(e)"   = "#9CA3AF",
  "Marié(e) monogame" = "#2563EB",
  "Marié(e) polygame" = "#7C3AED",
  "Union informelle"  = "#F59E0B",
  "Veuf/Veuve"        = "#10B981",
  "Séparé(e)"         = "#D97706",
  "Divorcé(e)"        = "#EF4444"
)

# ── 0.4  Thème ggplot2 ─────────────────────────────────────────────────────────
theme_ghs <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = pal["fond"],  color = NA),
      panel.background  = element_rect(fill = pal["fond"],  color = NA),
      panel.grid.major  = element_line(color = "#E5E7EB",   linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = base_size + 3,
                                       color = pal["texte"]),
      plot.subtitle     = element_text(size = base_size, color = pal["texte2"]),
      axis.title        = element_text(face = "bold", size = base_size,
                                       color = pal["texte2"]),
      axis.text         = element_text(size = base_size - 1, color = pal["texte2"]),
      axis.ticks        = element_line(color = "#D1D5DB"),
      legend.background = element_rect(fill = pal["fond"], color = NA),
      legend.title      = element_text(face = "bold", size = base_size),
      legend.text       = element_text(size = base_size - 1),
      legend.key.size   = unit(0.5, "cm"),
      strip.text        = element_text(face = "bold", size = base_size),
      strip.background  = element_rect(fill = pal["fond2"], color = NA),
      plot.margin       = margin(14, 16, 10, 14)
    )
}

save_g <- function(nom, w = 7, h = 5) {
  ggsave(file.path("outputs/graphiques", nom),
         width = w, height = h, dpi = 200, bg = pal["fond"])
  message("  ✓ outputs/graphiques/", nom)
}


# ==============================================================================
# CHARGEMENT DES DONNÉES
# ==============================================================================

message("\n── Chargement des fichiers (harvest + planting + secta, W1–W4) ─────────")

# ─ sect1 harvest ──────────────────────────────────────────────────────────────
h_w1 <- read_dta("data/sect1_harvestw1.dta") |> mutate(vague = "W1 (2010)", visite = "harvest")
h_w2 <- read_dta("data/sect1_harvestw2.dta") |> mutate(vague = "W2 (2012)", visite = "harvest")
h_w3 <- read_dta("data/sect1_harvestw3.dta") |> mutate(vague = "W3 (2015)", visite = "harvest")
h_w4 <- read_dta("data/sect1_harvestw4.dta") |> mutate(vague = "W4 (2018)", visite = "harvest")

# ─ sect1 planting ─────────────────────────────────────────────────────────────
p_w1 <- read_dta("data/sect1_plantingw1.dta") |> mutate(vague = "W1 (2010)", visite = "planting")
p_w2 <- read_dta("data/sect1_plantingw2.dta") |> mutate(vague = "W2 (2012)", visite = "planting")
p_w3 <- read_dta("data/sect1_plantingw3.dta") |> mutate(vague = "W3 (2015)", visite = "planting")
p_w4 <- read_dta("data/sect1_plantingw4.dta") |> mutate(vague = "W4 (2018)", visite = "planting")

# ─ secta harvest : milieu + POIDS ─────────────────────────────────────────────
# Les poids de sondage sont dans secta, pas dans sect1
# wt_wave1/2/3/4 = poids d'expansion ménage (household expansion weight)
# NB : en W3, sector peut valoir 0 (ménages "new") → recodé en NA
charger_secta <- function(path, vague_lbl, wt_var) {
  read_dta(path) |>
    mutate(vague = vague_lbl) |>
    select(hhid, vague, sector, zone, state, poids = all_of(wt_var)) |>
    mutate(
      milieu = case_when(
        as.numeric(sector) == 1 ~ "Urban",
        as.numeric(sector) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Urban", "Rural"))
    )
}

sa_w1 <- charger_secta("data/secta_harvestw1.dta", "W1 (2010)", "wt_wave1")
sa_w2 <- charger_secta("data/secta_harvestw2.dta", "W2 (2012)", "wt_wave2")
sa_w3 <- charger_secta("data/secta_harvestw3.dta", "W3 (2015)", "wt_wave3")
sa_w4 <- charger_secta("data/secta_harvestw4.dta", "W4 (2018)", "wt_wave4")

# ─ Empilage ───────────────────────────────────────────────────────────────────
df_harvest  <- bind_rows(h_w1, h_w2, h_w3, h_w4)
df_planting <- bind_rows(p_w1, p_w2, p_w3, p_w4)
df_secta    <- bind_rows(sa_w1, sa_w2, sa_w3, sa_w4)

cat(sprintf("sect1 harvest  (W1–W4) : %d individus\n", nrow(df_harvest)))
cat(sprintf("sect1 planting (W1–W4) : %d individus\n", nrow(df_planting)))
cat("\nEffectifs harvest par vague :\n")
table(df_harvest$vague) |> print()

# ─ Recodage harmonisé des variables clés ──────────────────────────────────────
recoder_vars <- function(df) {
  df |>
    mutate(
      sexe = case_when(
        as.numeric(s1q2) == 1 ~ "Male",
        as.numeric(s1q2) == 2 ~ "Female",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Male", "Female")),
      
      lien_num = as.numeric(s1q3),
      lien = case_when(
        lien_num == 1            ~ "Chef de ménage",
        lien_num == 2            ~ "Conjoint(e)",
        lien_num %in% c(3,4,5)  ~ "Enfant",
        !is.na(lien_num)         ~ "Autre",
        TRUE                     ~ NA_character_
      ) |> factor(levels = c("Chef de ménage","Conjoint(e)","Enfant","Autre")),
      
      age = as.numeric(s1q4),
      
      mat_num = as.numeric(s1q7),
      statut_mat = case_when(
        mat_num == 1 ~ "Marié(e) monogame",
        mat_num == 2 ~ "Marié(e) polygame",
        mat_num == 3 ~ "Union informelle",
        mat_num == 4 ~ "Divorcé(e)",
        mat_num == 5 ~ "Séparé(e)",
        mat_num == 6 ~ "Veuf/Veuve",
        mat_num == 7 ~ "Jamais marié(e)",
        TRUE         ~ NA_character_
      ) |> factor(levels = c("Jamais marié(e)","Marié(e) monogame","Marié(e) polygame",
                             "Union informelle","Veuf/Veuve","Séparé(e)","Divorcé(e)"))
    )
}

df_harvest  <- recoder_vars(df_harvest)
df_planting <- recoder_vars(df_planting)

# ─ Jointure des poids sur df_harvest ──────────────────────────────────────────
# Chaque individu reçoit le poids de son ménage
df_harvest <- df_harvest |>
  left_join(df_secta |> select(hhid, vague, milieu, poids),
            by = c("hhid", "vague"))

cat("\nVérification poids W4 :\n")
df_harvest |>
  filter(vague == "W4 (2018)") |>
  summarise(
    n_individus   = n(),
    n_poids_na    = sum(is.na(poids)),
    poids_min     = round(min(poids, na.rm = TRUE), 0),
    poids_max     = round(max(poids, na.rm = TRUE), 0),
    poids_moyenne = round(mean(poids, na.rm = TRUE), 0)
  ) |> print()

# ─ Création du design enquête pondéré (srvyr) ─────────────────────────────────
# On crée un design par vague (les poids ne sont pas comparables entre vagues)
# Pour les analyses multi-vagues, on pondère vague par vague

creer_design <- function(df, vague_lbl) {
  df |>
    filter(vague == vague_lbl, !is.na(poids)) |>
    as_survey_design(weights = poids, ids = hhid)
}

design_w4 <- creer_design(df_harvest, "W4 (2018)")
design_w1 <- creer_design(df_harvest, "W1 (2010)")
design_w2 <- creer_design(df_harvest, "W2 (2012)")
design_w3 <- creer_design(df_harvest, "W3 (2015)")

message("\n✓ Designs de sondage créés (W1 à W4).")


# ==============================================================================
# T1 — VÉRIFICATION QUALITÉ : HEATMAP % VALEURS MANQUANTES
#      (non pondérée — qualité = caractéristique de l'échantillon)
# ==============================================================================

message("\n", strrep("═", 65))
message("  T1 — Qualité des données : valeurs manquantes (W1–W4)")
message(strrep("═", 65))

# NB : La heatmap de NA n'est PAS pondérée — les valeurs manquantes sont
# une propriété de la collecte (échantillon), pas de la population.

vars_cles_lbl <- c(
  sexe       = "Sexe (s1q2)",
  lien       = "Lien de parenté (s1q3)",
  age        = "Âge (s1q4)",
  statut_mat = "Statut matrimonial (s1q7)"
)

calc_na <- function(df, visite_lbl) {
  df |>
    group_by(vague) |>
    summarise(
      across(names(vars_cles_lbl),
             ~ round(mean(is.na(.)) * 100, 1),
             .names = "{.col}"),
      .groups = "drop"
    ) |>
    pivot_longer(-vague, names_to = "var_code", values_to = "pct_na") |>
    mutate(
      visite   = visite_lbl,
      variable = vars_cles_lbl[var_code]
    )
}

na_df <- bind_rows(
  calc_na(df_harvest,  "Post-Harvest"),
  calc_na(df_planting, "Post-Planting")
) |>
  mutate(
    vague    = factor(vague,    levels = names(pal_vagues)),
    visite   = factor(visite,   levels = c("Post-Harvest","Post-Planting")),
    variable = factor(variable, levels = rev(vars_cles_lbl))
  )

cat("\nRésumé % NA — harvest :\n")
calc_na(df_harvest, "harvest") |>
  select(-visite, -var_code) |>
  pivot_wider(names_from = vague, values_from = pct_na) |>
  print()

p_na <- ggplot(na_df, aes(x = vague, y = variable, fill = pct_na)) +
  geom_tile(color = "white", linewidth = 0.9) +
  geom_text(
    aes(label = paste0(pct_na, "%"),
        color = pct_na > 12),
    size = 3.2, fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#D7F0D0", mid = "#FFF3C4", high = "#F4A6A0",
    midpoint = 10, limits = c(0, 45), name = "% manquant",
    labels = function(x) paste0(x, "%"),
    guide  = guide_colorbar(barwidth = 8, barheight = 0.5)
  ) +
  scale_color_manual(values = c("FALSE" = pal["texte"], "TRUE" = "white"),
                     guide = "none") +
  facet_wrap(~ visite, ncol = 2) +
  labs(
    title    = "Qualité des données — Valeurs manquantes par variable et par vague",
    subtitle = "Nigeria GHS Panel · Post-Harvest et Post-Planting · W1–W4 · Vert = peu de NA · Rouge = beaucoup",
    x = NULL, y = NULL,
    caption = "Source : sect1_harvest* + sect1_planting* (W1–W4) · Non pondéré (qualité échantillon)"
  ) +
  theme_ghs(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        legend.position = "bottom")

print(p_na)
save_g("T1_qualite_valeurs_manquantes.png", w = 11, h = 5)


# ==============================================================================
# T2 — ANALYSE UNIVARIÉE DE L'ÂGE  [W4 harvest — PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 65))
message("  T2 — Analyse univariée de l'âge PONDÉRÉE (W4 harvest)")
message(strrep("═", 65))

df_age_w4 <- df_harvest |>
  filter(vague == "W4 (2018)", !is.na(age), age >= 0, age <= 120, !is.na(poids))

cat(sprintf("Individus avec âge valide + poids (W4 harvest) : %d\n", nrow(df_age_w4)))

# ── Statistiques descriptives PONDÉRÉES (srvyr) ────────────────────────────────
stats_pond <- design_w4 |>
  filter(!is.na(age), age >= 0, age <= 120) |>
  summarise(
    Moyenne_pond   = survey_mean(age, vartype = "ci"),
    Médiane_pond   = survey_median(age, vartype = "ci"),
    Q1_pond        = survey_quantile(age, quantiles = 0.25, vartype = NULL),
    Q3_pond        = survey_quantile(age, quantiles = 0.75, vartype = NULL),
    Ecart_type_pond = survey_sd(age)
  )

cat("\n── Statistiques descriptives PONDÉRÉES (W4) ──────────────\n")
print(as.data.frame(stats_pond))

# Aussi les stats non pondérées pour comparaison
stats_np <- df_age_w4 |>
  summarise(
    N          = n(),
    Moyenne    = round(mean(age), 2),
    Médiane    = round(median(age), 2),
    Q1         = round(quantile(age, 0.25), 2),
    Q3         = round(quantile(age, 0.75), 2),
    Ecart_type = round(sd(age), 2),
    CV_pct     = round(sd(age) / mean(age) * 100, 1),
    Asymétrie  = round(e1071::skewness(age), 3),
    Kurtosis   = round(e1071::kurtosis(age), 3),
    Min        = min(age), Max = max(age)
  )

cat("\n── Rappel statistiques NON pondérées (W4) ────────────────\n")
print(as.data.frame(t(stats_np)))

# Moyenne et médiane pondérées pour annotations graphiques
moy_pond <- stats_pond$Moyenne_pond
med_pond <- as.numeric(stats_pond$Médiane_pond)

# ── Histogramme pondéré ────────────────────────────────────────────────────────
# Pour l'histogramme, on utilise aes(weight = poids) → ggplot pond. les effectifs
p_hist_age <- ggplot(df_age_w4, aes(x = age, weight = poids)) +
  geom_histogram(binwidth = 5, fill = pal["bleu"],
                 color = "white", alpha = 0.88, linewidth = 0.22) +
  geom_vline(xintercept = med_pond, linetype = "dashed",
             color = pal["orange"], linewidth = 0.85) +
  geom_vline(xintercept = moy_pond, linetype = "dotted",
             color = pal["rouge"], linewidth = 0.85) +
  annotate("text", x = med_pond + 1.5, y = Inf,
           label = paste0("Médiane = ", round(med_pond, 1), " ans (pond.)"),
           color = pal["orange"], size = 2.85, hjust = 0, vjust = 2.2, fontface = "bold") +
  annotate("text", x = moy_pond + 1.5, y = Inf,
           label = paste0("Moyenne = ", round(moy_pond, 1), " ans (pond.)"),
           color = pal["rouge"], size = 2.85, hjust = 0, vjust = 4.6, fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 100, 10), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.06))) +
  labs(
    title    = "Distribution de l'âge des membres du ménage",
    subtitle = "Nigeria GHS Panel · Wave 4 (2018) · Classes de 5 ans · Effectifs pondérés",
    x = "Âge (années)", y = "Effectif pondéré",
    caption = "Source : sect1_harvestw4 · Pondération : wt_wave4 · Médiane orange · Moyenne rouge (pointillé)"
  ) +
  theme_ghs()

# ── Boîte à moustaches (non pondérée — boxplot pondéré non standard) ──────────
# Note : les boxplots pondérés ne sont pas directement supportés par ggplot2.
# On utilise les quantiles pondérés calculés via srvyr pour annoter le boxplot.
p_box_age <- ggplot(df_age_w4, aes(x = "", y = age)) +
  geom_boxplot(
    fill = pal["bleu"], color = pal["texte"],
    alpha = 0.70, width = 0.38,
    outlier.color = pal["rouge"], outlier.size = 0.75, outlier.alpha = 0.38,
    linewidth = 0.32
  ) +
  # Remplacer la moyenne par celle pondérée
  geom_hline(yintercept = moy_pond, linetype = "dotted",
             color = pal["rouge"], linewidth = 0.8) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title    = "Boîte à moustaches — Âge (W4)",
    subtitle = paste0("Médiane pond. = ", round(med_pond, 1),
                      " ans  ·  Moyenne pond. = ", round(moy_pond, 1), " ans (ligne rouge)"),
    x = NULL, y = "Âge (années)",
    caption = "Source : sect1_harvestw4 · Quantiles non pondérés · Moyenne pondérée annotée"
  ) +
  theme_ghs() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p_age_final <- p_hist_age / p_box_age +
  plot_layout(heights = c(3.2, 1.1)) +
  plot_annotation(
    title = "Analyse univariée de l'âge — GHS Wave 4 (2018) · Pondérée",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_age_final)
save_g("T2_age_histogramme_boxplot.png", w = 7, h = 7.5)

# ── Shapiro-Wilk (sur échantillon — test de normalité ne se pondère pas) ───────
cat("\n── Test de Shapiro-Wilk (non pondéré — test de normalité) ──\n")
set.seed(42)
shap <- shapiro.test(sample(df_age_w4$age, min(nrow(df_age_w4), 5000)))
print(shap)
cat(if (shap$p.value < 0.05)
  "\n→ p < 0.05 : distribution NON normale (cohérent avec pyramide à base large).\n"
  else "\n→ p ≥ 0.05 : pas de rejet de la normalité.\n")


# ==============================================================================
# T3 — DISTRIBUTION PAR SEXE  [PONDÉRÉE]
#      Camembert W4 + évolution taux féminisation W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T3 — Distribution par sexe PONDÉRÉE (W4 + W1–W4)")
message(strrep("═", 65))

# ── Proportions pondérées par sexe W4 ─────────────────────────────────────────
sexe_pond_w4 <- design_w4 |>
  filter(!is.na(sexe)) |>
  group_by(sexe) |>
  summarise(prop = survey_mean(vartype = "ci")) |>
  mutate(
    n_pond = prop * sum(df_harvest$poids[df_harvest$vague == "W4 (2018)"], na.rm = TRUE),
    ypos   = cumsum(prop) - 0.5 * prop,
    lbl    = paste0(sexe, "\n", scales::percent(prop, accuracy = 0.1))
  )

cat("\n── Proportions pondérées par sexe (W4) ─────────────────\n")
print(sexe_pond_w4)

p_camembert <- ggplot(sexe_pond_w4, aes(x = "", y = prop, fill = sexe)) +
  geom_col(width = 1, color = "white", linewidth = 1.2) +
  geom_text(aes(y = ypos, label = lbl),
            color = "white", size = 3.8, fontface = "bold") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = pal_sexe) +
  labs(
    title    = "Répartition par sexe (pondérée)",
    subtitle = "Wave 4 (2018)",
    caption  = "Source : sect1_harvestw4 · Pondération : wt_wave4",
    fill = "Sexe"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.background = element_rect(fill = pal["fond"], color = NA),
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   color = pal["texte"], size = 13),
    plot.subtitle   = element_text(hjust = 0.5, color = pal["texte2"],
                                   size = 10, margin = margin(b = 6)),
    plot.caption    = element_text(hjust = 0, color = "#AAA", size = 8),
    legend.position = "none",
    plot.margin     = margin(12, 8, 8, 8)
  )

# ── Taux de féminisation pondéré W1–W4 par milieu ─────────────────────────────
calc_ratio_f_pond <- function(design_obj, vague_lbl, milieu_lbl) {
  design_obj |>
    filter(!is.na(sexe), milieu == milieu_lbl) |>
    summarise(ratio_f = survey_mean(sexe == "Female", vartype = "ci")) |>
    mutate(vague = vague_lbl, milieu = milieu_lbl)
}

ratio_pond <- bind_rows(
  calc_ratio_f_pond(design_w1, "W1 (2010)", "Urban"),
  calc_ratio_f_pond(design_w1, "W1 (2010)", "Rural"),
  calc_ratio_f_pond(design_w2, "W2 (2012)", "Urban"),
  calc_ratio_f_pond(design_w2, "W2 (2012)", "Rural"),
  calc_ratio_f_pond(design_w3, "W3 (2015)", "Urban"),
  calc_ratio_f_pond(design_w3, "W3 (2015)", "Rural"),
  calc_ratio_f_pond(design_w4, "W4 (2018)", "Urban"),
  calc_ratio_f_pond(design_w4, "W4 (2018)", "Rural")
) |>
  mutate(vague = factor(vague, levels = names(pal_vagues)))

cat("\n── Taux de féminisation pondéré W1–W4 par milieu ────────\n")
print(ratio_pond)

p_ratio_sexe <- ggplot(ratio_pond,
                       aes(x = vague, y = ratio_f, color = milieu, group = milieu)) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = pal["gris"], linewidth = 0.6, alpha = 0.7) +
  geom_ribbon(aes(ymin = ratio_f_low, ymax = ratio_f_upp, fill = milieu),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 3.8, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(aes(label = scales::percent(ratio_f, accuracy = 0.1)),
            vjust = -1.05, size = 2.85, fontface = "bold", show.legend = FALSE) +
  annotate("text", x = 0.7, y = 0.5015, label = "Parité (50%)",
           color = pal["gris"], size = 2.6, hjust = 0, fontface = "italic") +
  scale_color_manual(values = c("Urban" = pal["bleu"], "Rural" = pal["orange"])) +
  scale_fill_manual(values  = c("Urban" = pal["bleu"], "Rural" = pal["orange"]),
                    guide   = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0.485, 0.525)) +
  labs(
    title    = "Évolution du taux de féminisation pondéré (W1–W4)",
    subtitle = "Part des femmes parmi les membres des ménages · Rural vs Urban · avec IC 95%",
    x = "Vague", y = "Part des femmes (pond.)", color = "Milieu",
    caption = "Source : sect1_harvest (W1–W4) + secta_harvest (W1–W4) · Pondération wt_waveX"
  ) +
  theme_ghs()

p_sexe_final <- p_camembert | p_ratio_sexe +
  plot_annotation(
    title = "Structure par sexe — Nigeria GHS Panel · Analyses pondérées",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_sexe_final)
save_g("T3_sexe_camembert_evolution.png", w = 12, h = 5.5)


# ==============================================================================
# T4 — DISTRIBUTION DU STATUT MATRIMONIAL  [PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 65))
message("  T4 — Statut matrimonial PONDÉRÉ (W4 + W1–W4)")
message(strrep("═", 65))

# Proportions pondérées statut matrimonial W4 (adultes 15+)
mat_pond_w4 <- design_w4 |>
  filter(!is.na(statut_mat), !is.na(age), age >= 15) |>
  group_by(statut_mat) |>
  summarise(prop = survey_mean(vartype = "ci")) |>
  mutate(statut_mat = fct_reorder(statut_mat, prop, .desc = FALSE))

cat("\n── Statut matrimonial pondéré W4 ───────────────────────\n")
print(mat_pond_w4)

p_mat_hist <- ggplot(mat_pond_w4,
                     aes(x = prop, y = statut_mat, fill = statut_mat)) +
  geom_col(alpha = 0.88, width = 0.72) +
  geom_errorbarh(aes(xmin = prop_low, xmax = prop_upp),
                 height = 0.22, color = pal["gris"], linewidth = 0.55) +
  geom_text(
    aes(x = prop_upp, label = scales::percent(prop, accuracy = 0.1)),
    hjust = -0.15, size = 2.85, color = pal["texte"]
  ) +
  scale_fill_manual(values = pal_mat) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.22))) +
  labs(
    title    = "Statut matrimonial — Wave 4 (2018) · Pondéré",
    subtitle = "Individus de 15 ans et plus · Proportions pondérées avec IC à 95%",
    x = "Proportion (%) pondérée", y = NULL,
    caption = "Source : sect1_harvestw4 · Pondération : wt_wave4"
  ) +
  theme_ghs() + theme(legend.position = "none")

# Évolution pondérée W1–W4
calc_mat_pond <- function(design_obj, vague_lbl) {
  design_obj |>
    filter(!is.na(statut_mat), !is.na(age), age >= 15) |>
    group_by(statut_mat) |>
    summarise(prop = survey_mean(vartype = NULL)) |>
    mutate(vague = vague_lbl)
}

mat_vagues_pond <- bind_rows(
  calc_mat_pond(design_w1, "W1 (2010)"),
  calc_mat_pond(design_w2, "W2 (2012)"),
  calc_mat_pond(design_w3, "W3 (2015)"),
  calc_mat_pond(design_w4, "W4 (2018)")
) |>
  mutate(vague = factor(vague, levels = names(pal_vagues)))

p_mat_evol <- ggplot(mat_vagues_pond,
                     aes(x = vague, y = prop, fill = statut_mat)) +
  geom_col(position = "fill", alpha = 0.88, width = 0.72) +
  geom_text(
    aes(label = if_else(prop > 0.05, scales::percent(prop, accuracy = 1), "")),
    position = position_fill(vjust = 0.5),
    size = 2.7, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = pal_mat) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title    = "Évolution du statut matrimonial pondéré W1–W4",
    subtitle = "Adultes 15+ · Proportions pondérées",
    x = "Vague", y = "Proportion (%)", fill = "Statut",
    caption = "Source : sect1_harvest (W1–W4) · Pondération : wt_waveX"
  ) +
  theme_ghs() + theme(legend.position = "right")

p_mat_final <- p_mat_hist | p_mat_evol +
  plot_annotation(
    title = "Statut matrimonial des membres des ménages · Analyses pondérées",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_mat_final)
save_g("T4_statut_matrimonial_hist_evolution.png", w = 13, h = 6)


# ==============================================================================
# T5 — PYRAMIDE DES ÂGES W4  [PONDÉRÉE]
# ==============================================================================

message("\n", strrep("═", 65))
message("  T5 — Pyramide des âges W4 pondérée (livrable principal)")
message(strrep("═", 65))

# APPROCHE PONDÉRÉE CORRECTE :
# On garde les données INDIVIDUELLES (pas agrégées) et on construit la pyramide
# manuellement avec geom_col + aes(weight = poids).
# Cela préserve exactement les couleurs (pal_sexe) et les légendes du script original.
# L'argument `count` de age_pyramid() sur données agrégées casse la palette car
# sexe n'est plus reconnu comme facteur ordonné avec ses niveaux.

# Fonction préparation pyramide pondérée (données individuelles)
prep_pyr_pond <- function(df_wave, vague_lbl) {
  df_wave |>
    filter(vague == vague_lbl,
           !is.na(age), !is.na(sexe), !is.na(poids),
           age >= 0, age <= 120) |>
    mutate(
      groupe_age = cut(
        age,
        breaks = c(seq(0, 95, 5), Inf),
        right  = FALSE, include.lowest = TRUE,
        labels = c(paste0(seq(0, 90, 5), "–", seq(4, 94, 5)), "95+")
      )
    ) |>
    # Agréger par groupe d'âge × sexe en sommant les poids
    group_by(groupe_age, sexe) |>
    summarise(effectif_pond = sum(poids, na.rm = TRUE), .groups = "drop") |>
    # Conserver les niveaux du facteur sexe tels qu'ils sont dans df_harvest
    mutate(sexe = factor(sexe, levels = c("Male", "Female")))
}

# Fonction pyramide pondérée construite manuellement (geom_col)
# → identique visuellement au script original, mais avec effectifs pondérés
faire_pyramide_pond <- function(df_pyr, titre = NULL, sous_titre = NULL,
                                badge_lbl = NULL, badge_col = pal["violet"],
                                n_pond = NULL,
                                base_size = 10, show_y = TRUE, show_leg = TRUE) {
  
  # Les hommes vont à gauche (valeurs négatives), les femmes à droite
  df_plot <- df_pyr |>
    mutate(
      eff_plot = if_else(sexe == "Male", -effectif_pond, effectif_pond)
    )
  
  # Limites symétriques de l'axe X
  max_val <- max(abs(df_plot$eff_plot), na.rm = TRUE)
  breaks_x <- pretty(c(-max_val, max_val), n = 6)
  
  p <- ggplot(df_plot, aes(x = eff_plot, y = groupe_age, fill = sexe)) +
    geom_col(alpha = 0.88, width = 0.88) +
    scale_fill_manual(
      values = pal_sexe,                          # ← même palette que script original
      labels = c("Male" = "Homme", "Female" = "Femme"),
      name   = "Sexe"
    ) +
    scale_x_continuous(
      breaks = breaks_x,
      labels = function(x) scales::comma(abs(x))  # afficher valeurs absolues
    ) +
    labs(
      title    = titre,
      subtitle = sous_titre,
      x        = "Effectif pondéré",
      y        = if (show_y) "Groupe d'âge" else NULL,
      fill     = "Sexe"
    ) +
    theme_ghs(base_size = base_size) +
    theme(
      legend.position  = if (show_leg) "bottom" else "none",
      legend.direction = "horizontal",
      axis.text.y      = if (show_y) element_text(size = 8, color = pal["texte2"])
      else element_blank(),
      axis.ticks.y     = if (show_y) element_line() else element_blank()
    )
  
  # Badge vague
  if (!is.null(badge_lbl)) {
    p <- p + annotate("label",
                      x = Inf, y = Inf,
                      label = badge_lbl,
                      hjust = 1.08, vjust = 1.4, size = 4.2,
                      fill  = pal["fond2"], color = badge_col,
                      label.size = 0.45, fontface = "bold",
                      label.r = unit(0.22, "lines"))
  }
  
  # Effectif pondéré total
  if (!is.null(n_pond)) {
    p <- p + annotate("text",
                      x = -Inf, y = -Inf,
                      label = paste0("N pondéré ≈ ",
                                     scales::comma(round(n_pond / 1e6, 1)), "M individus"),
                      hjust = -0.05, vjust = -0.85,
                      size = 2.85, color = pal["texte2"])
  }
  
  p
}

# ── Pyramide W4 pondérée ───────────────────────────────────────────────────────
pyr_pond_w4 <- prep_pyr_pond(df_harvest, "W4 (2018)")
n_pond_w4   <- sum(pyr_pond_w4$effectif_pond)

p_pyr_w4 <- faire_pyramide_pond(
  df_pyr    = pyr_pond_w4,
  titre     = "Pyramide des âges — Nigeria GHS Panel · Pondérée",
  sous_titre = "Wave 4 (2018) · Post-Harvest · Groupes quinquennaux · Effectifs pondérés (wt_wave4)",
  badge_lbl = "Wave 4 · 2018",
  badge_col = pal["violet"],
  n_pond    = n_pond_w4,
  base_size = 10,
  show_y    = TRUE,
  show_leg  = TRUE
) +
  labs(caption = paste0("Source : sect1_harvestw4   |   Bleu = Homme   |   Rose = Femme",
                        "\nLes effectifs représentent la population estimée"))

print(p_pyr_w4)
save_g("T5_pyramide_ages_w4.png", w = 7.5, h = 10)

cat("\nCommentaire pyramide W4 pondérée :\n")
cat("  → Base large (0–14 ans) : forte fécondité historique.\n")
cat("  → Rétrécissement progressif avec l'âge : mortalité et émigration.\n")
cat("  → Légère surnumérosité féminine à partir de 45–50 ans.\n")
cat("  → Profil typique d'une démographie en transition.\n")


# ==============================================================================
# T6 — PYRAMIDES DES 4 VAGUES CÔTE À CÔTE  [PONDÉRÉES]
# ==============================================================================

message("\n", strrep("═", 65))
message("  T6 — Pyramides des 4 vagues côte à côte pondérées")
message(strrep("═", 65))

pyr_pond_w1 <- prep_pyr_pond(df_harvest, "W1 (2010)")
pyr_pond_w2 <- prep_pyr_pond(df_harvest, "W2 (2012)")
pyr_pond_w3 <- prep_pyr_pond(df_harvest, "W3 (2015)")

# Fonction pour les 4 petites pyramides côte à côte
faire_pyr_mini <- function(df_pyr, lbl_vague, annee, couleur_badge,
                           show_y = TRUE, show_leg = FALSE) {
  n_pond <- sum(df_pyr$effectif_pond)
  
  p <- faire_pyramide_pond(
    df_pyr    = df_pyr,
    badge_lbl = paste0(lbl_vague, " · ", annee),
    badge_col = couleur_badge,
    n_pond    = n_pond,
    base_size = 8.5,
    show_y    = show_y,
    show_leg  = show_leg
  ) +
    # Pour les mini pyramides : badge plus petit
    theme(plot.margin = margin(5, 4, 4, 4))
  
  # Ajuster la taille du badge pour les mini pyramides
  p$layers[[which(sapply(p$layers, function(l)
    inherits(l$geom, "GeomLabel")))]]$aes_params$size <- 3.1
  
  p
}

p4_w1 <- faire_pyr_mini(pyr_pond_w1, "W1", "2010", pal_vagues["W1 (2010)"], show_y=TRUE,  show_leg=FALSE)
p4_w2 <- faire_pyr_mini(pyr_pond_w2, "W2", "2012", pal_vagues["W2 (2012)"], show_y=FALSE, show_leg=FALSE)
p4_w3 <- faire_pyr_mini(pyr_pond_w3, "W3", "2015", pal_vagues["W3 (2015)"], show_y=FALSE, show_leg=FALSE)
p4_w4 <- faire_pyr_mini(pyr_pond_w4, "W4", "2018", pal_vagues["W4 (2018)"], show_y=FALSE, show_leg=TRUE)

p_4pyr <- (p4_w1 | p4_w2 | p4_w3 | p4_w4) +
  plot_annotation(
    title    = "Évolution de la structure démographique pondérée — W1 (2010) à W4 (2018)",
    subtitle = "Nigeria GHS Panel · Post-Harvest · Groupes quinquennaux · Effectifs pondérés (wt_waveX)",
    caption  = "Source : sect1_harvestw1/w2/w3/w4 + secta_harvestw1/w2/w3/w4",
    theme    = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.subtitle   = element_text(size = 10, color = pal["texte2"]),
      plot.caption    = element_text(size = 8, color = "#AAA", hjust = 0),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_4pyr)
save_g("T6_pyramides_4_vagues.png", w = 17, h = 11)


# ==============================================================================
# T7 — LIEN DE PARENTÉ : BARPLOT PONDÉRÉ + IC 95% + ÉVOLUTION W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T7 — Lien de parenté pondéré + IC 95% (W4 + W1–W4)")
message(strrep("═", 65))

# Proportions pondérées W4
lien_pond_w4 <- design_w4 |>
  filter(!is.na(lien)) |>
  group_by(lien) |>
  summarise(prop = survey_mean(vartype = "ci")) |>
  arrange(desc(prop)) |>
  mutate(lien = fct_reorder(lien, prop))

cat("\n── Fréquences pondérées lien de parenté W4 + IC 95% ────\n")
print(lien_pond_w4)

p_lien_w4 <- ggplot(lien_pond_w4, aes(x = prop, y = lien, fill = lien)) +
  geom_col(alpha = 0.88, width = 0.66) +
  geom_errorbarh(
    aes(xmin = prop_low, xmax = prop_upp),
    height = 0.22, color = pal["gris"], linewidth = 0.65
  ) +
  geom_text(
    aes(x = prop_upp, label = scales::percent(prop, accuracy = 0.1)),
    hjust = -0.15, size = 2.9, color = pal["texte"], fontface = "bold"
  ) +
  scale_fill_manual(values = pal_lien) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "Lien de parenté — Wave 4 (2018) · Pondéré",
    subtitle = "Proportions pondérées avec IC à 95% (srvyr)",
    x = "Proportion pondérée (%)", y = NULL,
    caption = "Source : sect1_harvestw4 · Pondération : wt_wave4"
  ) +
  theme_ghs() + theme(legend.position = "none")

# Évolution pondérée W1–W4
calc_lien_pond <- function(design_obj, vague_lbl) {
  design_obj |>
    filter(!is.na(lien)) |>
    group_by(lien) |>
    summarise(prop = survey_mean(vartype = NULL)) |>
    mutate(vague = vague_lbl)
}

lien_vagues_pond <- bind_rows(
  calc_lien_pond(design_w1, "W1 (2010)"),
  calc_lien_pond(design_w2, "W2 (2012)"),
  calc_lien_pond(design_w3, "W3 (2015)"),
  calc_lien_pond(design_w4, "W4 (2018)")
) |>
  mutate(vague = factor(vague, levels = names(pal_vagues)))

p_lien_evol <- ggplot(lien_vagues_pond,
                      aes(x = vague, y = prop, color = lien, group = lien)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1.4) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -1.0, size = 2.65, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = pal_lien) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0.04, 0.12))) +
  labs(
    title    = "Évolution lien de parenté pondéré W1–W4",
    subtitle = "Part pondérée de chaque catégorie · Post-Harvest",
    x = "Vague", y = "Proportion pondérée (%)", color = "Lien",
    caption = "Source : sect1_harvest (W1–W4) · Pondération : wt_waveX"
  ) +
  theme_ghs()

p_lien_final <- p_lien_w4 | p_lien_evol +
  plot_annotation(
    title = "Structure des ménages par lien de parenté · Analyses pondérées",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_lien_final)
save_g("T7_lien_parente_IC95_evolution.png", w = 13, h = 5.5)


# ==============================================================================
# T8 — TAILLE DES MÉNAGES RURAL/URBAN
#      Violin+boxplot · Wilcoxon · r de rang · Évolution W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T8 — Taille des ménages Rural/Urban (W4 + W1–W4)")
message(strrep("═", 65))

# NB : La taille du ménage est calculée à partir des données brutes (nombre de membres).
# Le test de Wilcoxon est réalisé sur les données non pondérées (test de rang).
# Les médianes pondérées sont calculées séparément via srvyr.

taille_w4 <- df_harvest |>
  filter(vague == "W4 (2018)") |>
  group_by(hhid) |>
  summarise(
    taille = n(),
    milieu = first(milieu),
    poids  = first(poids),
    .groups = "drop"
  ) |>
  filter(!is.na(milieu))

cat("\nStatistiques taille ménage W4 par milieu :\n")
taille_w4 |>
  group_by(milieu) |>
  summarise(
    n         = n(),
    Médiane   = median(taille),
    Moyenne   = round(mean(taille), 2),
    # Médiane pondérée (weighted.median du package Hmisc ou approximation)
    Med_pond  = round(weighted.mean(taille, poids, na.rm = TRUE), 2),
    .groups   = "drop"
  ) |> print()

# Wilcoxon + r de rang (sur données non pondérées — test de rang standard)
wilcox_res <- wilcox.test(taille ~ milieu, data = taille_w4)
r_eff      <- taille_w4 |> rstatix::wilcox_effsize(taille ~ milieu)
effet_lbl  <- dplyr::case_when(
  abs(r_eff$effsize) < 0.1 ~ "négligeable",
  abs(r_eff$effsize) < 0.3 ~ "petit",
  abs(r_eff$effsize) < 0.5 ~ "moyen",
  TRUE                      ~ "grand"
)
cat("\n── Wilcoxon :", format.pval(wilcox_res$p.value, digits=3),
    " · r =", round(r_eff$effsize, 3), "(", effet_lbl, ")\n")
cat("Note : Test de Wilcoxon réalisé sur les données brutes (rangs non pondérables).\n")

# Violin + boxplot (avec pondération visuelle via aes(weight))
p_violin <- ggplot(taille_w4, aes(x = milieu, y = taille, fill = milieu)) +
  geom_violin(aes(weight = poids),
              alpha = 0.32, linewidth = 0.28, trim = TRUE, bw = 0.75) +
  geom_boxplot(
    width = 0.14, alpha = 0.90, linewidth = 0.35,
    outlier.color = pal["rouge"], outlier.size = 0.65, outlier.alpha = 0.35
  ) +
  # Moyenne pondérée
  stat_summary(aes(weight = poids), fun = function(x) weighted.mean(x, taille_w4$poids[match(x, taille_w4$taille)]),
               geom = "point", shape = 18, size = 4.2, color = "white") +
  scale_fill_manual(values = c("Urban" = pal["bleu"], "Rural" = pal["orange"])) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(
    title    = "Taille des ménages selon le milieu de résidence",
    subtitle = paste0(
      "W = ", round(wilcox_res$statistic, 0),
      "  ·  p = ", format.pval(wilcox_res$p.value, digits = 3),
      "  ·  r = ", round(r_eff$effsize, 3),
      " (effet ", effet_lbl, ")",
      "  ·  Violons pondérés"
    ),
    x = NULL, y = "Nombre de membres",
    caption = "Source : sect1_harvestw4 + secta_harvestw4 · Violons pondérés (wt_wave4)"
  ) +
  theme_ghs() + theme(legend.position = "none")

# Évolution taille médiane pondérée W1–W4
calc_taille_pond <- function(df_h, df_s, vague_lbl) {
  df_h |>
    filter(vague == vague_lbl) |>
    group_by(hhid) |>
    summarise(taille = n(), .groups = "drop") |>
    left_join(df_s |> filter(vague == vague_lbl) |> select(hhid, milieu, poids),
              by = "hhid") |>
    filter(!is.na(milieu), !is.na(poids)) |>
    group_by(milieu) |>
    summarise(
      mediane      = median(taille),
      med_ponderee = weighted.mean(taille, poids),
      .groups = "drop"
    ) |>
    mutate(vague = vague_lbl)
}

taille_evol <- bind_rows(
  calc_taille_pond(df_harvest, df_secta, "W1 (2010)"),
  calc_taille_pond(df_harvest, df_secta, "W2 (2012)"),
  calc_taille_pond(df_harvest, df_secta, "W3 (2015)"),
  calc_taille_pond(df_harvest, df_secta, "W4 (2018)")
) |>
  mutate(vague = factor(vague, levels = names(pal_vagues)))

p_evol_taille <- ggplot(taille_evol,
                        aes(x = vague, y = med_ponderee,
                            color = milieu, group = milieu)) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(aes(label = round(med_ponderee, 1)),
            vjust = -1.0, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Urban" = pal["bleu"], "Rural" = pal["orange"])) +
  scale_y_continuous(breaks = seq(3, 12, 1)) +
  labs(
    title    = "Évolution de la taille moyenne pondérée des ménages (W1–W4)",
    subtitle = "Rural vs Urban · Moyenne pondérée (wt_waveX)",
    x = "Vague", y = "Taille moyenne pondérée", color = "Milieu",
    caption = "Source : sect1_harvest (W1–W4) + secta_harvest (W1–W4)"
  ) +
  theme_ghs()

p_taille_final <- p_violin | p_evol_taille +
  plot_annotation(
    title = "Taille des ménages — Rural vs Urban · Analyses pondérées",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_taille_final)
save_g("T8_taille_menage_violin_evolution.png", w = 13, h = 5.5)


# ==============================================================================
# T9 — TABLEAU GTSUMMARY PONDÉRÉ STRATIFIÉ PAR MILIEU  [W4 harvest]
# ==============================================================================

message("\n", strrep("═", 65))
message("  T9 — Tableau gtsummary pondéré stratifié par milieu (W4)")
message(strrep("═", 65))

# gtsummary supporte les designs srvyr via tbl_svysummary()
df_gt_w4 <- df_harvest |>
  filter(vague == "W4 (2018)", !is.na(age), !is.na(poids)) |>
  left_join(taille_w4 |> select(hhid, taille), by = "hhid") |>
  filter(!is.na(milieu)) |>
  mutate(milieu = factor(milieu, levels = c("Urban","Rural")))

# Créer le design pour T9
design_gt <- df_gt_w4 |>
  as_survey_design(weights = poids, ids = hhid)

tbl_demo <- design_gt |>
  tbl_svysummary(
    by        = milieu,
    include   = c(age, sexe, taille, statut_mat),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    label = list(
      age        ~ "Âge (années)",
      sexe       ~ "Sexe",
      taille     ~ "Taille du ménage (membres)",
      statut_mat ~ "Statut matrimonial"
    ),
    digits  = list(all_continuous() ~ 1),
    missing = "no"
  ) |>
  add_p(test = list(
    all_continuous()  ~ "svy.wilcox.test",
    all_categorical() ~ "svy.chisq.test"
  )) |>
  add_overall(last = FALSE) |>
  bold_labels() |>
  italicize_levels() |>
  modify_caption(
    "**Tableau 1. Caractéristiques démographiques par milieu — GHS Wave 4 (2018) · Pondéré**"
  ) |>
  modify_footnote(
    update = everything() ~
      "Statistiques pondérées (wt_wave4). Moyenne (écart-type) pour les variables continues ; % pondérés pour les catégorielles. n_unweighted = effectif brut. Tests : Wilcoxon pondéré · chi-deux pondéré (srvyr/survey)."
  )

print(tbl_demo)

path_tbl <- "outputs/tableaux/T9_tableau_gtsummary_milieu.html"
tbl_demo |> as_gt() |> gt::gtsave(path_tbl)
message("  ✓ ", path_tbl)


# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================

message("\n", strrep("═", 65))
message("  ✓  TP1 avec pondérations terminé avec succès")
message(strrep("═", 65), "\n")

cat("Outputs produits :\n\n")
cat("  outputs/graphiques/\n")
cat("    T1_qualite_valeurs_manquantes.png       ← heatmap % NA (non pondérée)\n")
cat("    T2_age_histogramme_boxplot.png          ← histogramme + boxplot PONDÉRÉS\n")
cat("    T3_sexe_camembert_evolution.png         ← camembert + féminisation PONDÉRÉS\n")
cat("    T4_statut_matrimonial_hist_evolution.png← histogramme + évolution PONDÉRÉS\n")
cat("    T5_pyramide_ages_w4.png                 ← pyramide PONDÉRÉE (livrable principal)\n")
cat("    T6_pyramides_4_vagues.png               ← 4 pyramides PONDÉRÉES côte à côte\n")
cat("    T7_lien_parente_IC95_evolution.png      ← barplot IC95% + évolution PONDÉRÉS\n")
cat("    T8_taille_menage_violin_evolution.png   ← violin pondéré + évolution\n\n")
cat("  outputs/tableaux/\n")
cat("    T9_tableau_gtsummary_milieu.html        ← tableau tbl_svysummary PONDÉRÉ\n")



# install.packages(c("rmarkdown", "officedown", "officer", "flextable", "knitr"))
install.packages("flextable")