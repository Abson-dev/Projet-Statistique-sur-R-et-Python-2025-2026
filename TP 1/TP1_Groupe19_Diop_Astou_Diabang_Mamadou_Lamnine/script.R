# ==============================================================================
#  TP1 — Profil démographique des ménages nigérians
#  Nigeria GHS Panel · Waves 1–4 (2010 · 2012 · 2015 · 2018)
#  ENSAE ISE 1 · Projet Statistique R 2025-2026
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
#  T2. Analyse univariée de l'âge W4 : histogramme · boxplot · stats · Shapiro-Wilk
#  T3. Distribution par sexe : camembert W4 + évolution taux féminisation W1–W4
#  T4. Distribution du statut matrimonial : histogramme W4 + évolution W1–W4
#  T5. Pyramide des âges W4 annotée (livrable principal de l'énoncé)
#  T6. Pyramides des 4 vagues côte à côte (W1 · W2 · W3 · W4)
#  T7. Lien de parenté W4 : barplot horizontal + IC 95% + évolution W1–W4
#  T8. Taille des ménages Rural/Urban : violin+boxplot · Wilcoxon · r de rang
#       + évolution taille médiane W1–W4
#  T9. Tableau gtsummary stratifié par milieu (âge · sexe · taille · statut mat.)
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
  "ggtext"      # titres enrichis markdown dans ggplot2
)

manquants <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(manquants)) install.packages(manquants, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ── 0.2  Dossiers outputs (créés automatiquement) ─────────────────────────────
dir.create("outputs/graphiques", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tableaux",   recursive = TRUE, showWarnings = FALSE)
message("✓ Dossiers outputs/ prêts.")

# ── 0.3  Palettes de couleurs (version améliorée)

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

pal_sexe <- c(
  "Male"   = "#2563EB",
  "Female" = "#EC4899"
)

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
      
      plot.background   = element_rect(fill = pal["fond"], color = NA),
      panel.background  = element_rect(fill = pal["fond"], color = NA),
      
      panel.grid.major  = element_line(color = "#E5E7EB", linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      
      plot.title = element_text(
        face = "bold",
        size = base_size + 3,
        color = pal["texte"]
      ),
      
      plot.subtitle = element_text(
        size = base_size,
        color = pal["texte2"]
      ),
      
      axis.title = element_text(
        face = "bold",
        size = base_size,
        color = pal["texte2"]
      ),
      
      axis.text = element_text(
        size = base_size - 1,
        color = pal["texte2"]
      ),
      
      axis.ticks = element_line(color = "#D1D5DB"),
      
      legend.background = element_rect(
        fill = pal["fond"],
        color = NA
      ),
      
      legend.title = element_text(
        face = "bold",
        size = base_size
      ),
      
      legend.text = element_text(
        size = base_size - 1
      ),
      
      legend.key.size = unit(0.5, "cm"),
      
      strip.text = element_text(
        face = "bold",
        size = base_size
      ),
      
      strip.background = element_rect(
        fill = pal["fond2"],
        color = NA
      ),
      
      plot.margin = margin(14,16,10,14)
      
    )
}
# Utilitaire : sauvegarder le dernier graphique
save_g <- function(nom, w = 7, h = 5) {
  ggsave(file.path("outputs/graphiques", nom),
         width = w, height = h, dpi = 200, bg = pal["fond"])
  message("  ✓ outputs/graphiques/", nom)
}


# ==============================================================================
# CHARGEMENT DES DONNÉES
# ==============================================================================

message("\n── Chargement des 12 fichiers (harvest + planting + secta, W1–W4) ──────")

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

# ─ secta harvest  (milieu = sector : 1=Urban, 2=Rural) ────────────────────────
# NB : en W3, sector peut valoir 0 (ménages "new") → recodé en NA
charger_secta <- function(path, vague_lbl) {
  read_dta(path) |>
    mutate(vague = vague_lbl) |>
    select(hhid, vague, sector, zone, state) |>
    mutate(
      milieu = case_when(
        as.numeric(sector) == 1 ~ "Urban",
        as.numeric(sector) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Urban", "Rural"))
    )
}

sa_w1 <- charger_secta("data/secta_harvestw1.dta", "W1 (2010)")
sa_w2 <- charger_secta("data/secta_harvestw2.dta", "W2 (2012)")
sa_w3 <- charger_secta("data/secta_harvestw3.dta", "W3 (2015)")
sa_w4 <- charger_secta("data/secta_harvestw4.dta", "W4 (2018)")

# ─ Empilage ───────────────────────────────────────────────────────────────────
df_harvest  <- bind_rows(h_w1, h_w2, h_w3, h_w4)
df_planting <- bind_rows(p_w1, p_w2, p_w3, p_w4)
df_secta    <- bind_rows(sa_w1, sa_w2, sa_w3, sa_w4)

cat(sprintf("sect1 harvest  (W1–W4) : %d individus\n", nrow(df_harvest)))
cat(sprintf("sect1 planting (W1–W4) : %d individus\n", nrow(df_planting)))
cat("\nEffectifs harvest par vague :\n")
table(df_harvest$vague) |> print()

# ─ Recodage harmonisé des variables clés (toutes vagues) ──────────────────────
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


# ==============================================================================
# T1 — VÉRIFICATION QUALITÉ : HEATMAP % VALEURS MANQUANTES
#      Variables clés · Harvest + Planting · Toutes vagues
# ==============================================================================

message("\n", strrep("═", 65))
message("  T1 — Qualité des données : valeurs manquantes (W1–W4)")
message(strrep("═", 65))

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
    low      = "#D7F0D0",   # vert très pâle = 0% NA
    mid      = "#FFF3C4",   # jaune = quelques NA
    high     = "#F4A6A0",   # rouge-saumon = beaucoup de NA
    midpoint = 10,
    limits   = c(0, 45),
    name     = "% manquant",
    labels   = function(x) paste0(x, "%"),
    guide    = guide_colorbar(barwidth = 8, barheight = 0.5)
  ) +
  scale_color_manual(values = c("FALSE" = pal["texte"], "TRUE" = "white"),
                     guide = "none") +
  facet_wrap(~ visite, ncol = 2) +
  labs(
    title    = "Qualité des données — Valeurs manquantes par variable et par vague",
    subtitle = "Nigeria GHS Panel · Post-Harvest et Post-Planting · W1–W4 · Vert = peu de NA · Rouge = beaucoup",
    x        = NULL, y = NULL,
    caption  = "Source : sect1_harvest* + sect1_planting* (W1–W4)"
  ) +
  theme_ghs(base_size = 10) +
  theme(
    panel.grid  = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    legend.position = "bottom"
  )

print(p_na)
save_g("T1_qualite_valeurs_manquantes.png", w = 11, h = 5)


# ==============================================================================
# T2 — ANALYSE UNIVARIÉE DE L'ÂGE  [W4 harvest]
#      Histogramme (binwidth=5) · Boîte à moustaches · Stats · Shapiro-Wilk
# ==============================================================================

message("\n", strrep("═", 65))
message("  T2 — Analyse univariée de l'âge (W4 harvest)")
message(strrep("═", 65))

df_age_w4 <- df_harvest |>
  filter(vague == "W4 (2018)", !is.na(age), age >= 0, age <= 120)

cat(sprintf("Individus avec âge valide (W4 harvest) : %d\n", nrow(df_age_w4)))

# Statistiques
stats_age <- df_age_w4 |>
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
cat("\n── Statistiques descriptives (W4) ───────────────────────\n")
print(as.data.frame(t(stats_age)))

med_age <- median(df_age_w4$age)
moy_age <- mean(df_age_w4$age)

# ── Histogramme ───────────────────────────────────────────────────────────────
p_hist_age <- ggplot(df_age_w4, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = pal["bleu"],
                 color = "white", alpha = 0.88, linewidth = 0.22) +
  geom_vline(xintercept = med_age, linetype = "dashed",
             color = pal["orange"], linewidth = 0.85) +
  geom_vline(xintercept = moy_age, linetype = "dotted",
             color = pal["rouge"],  linewidth = 0.85) +
  annotate("text", x = med_age + 1.5, y = Inf,
           label = paste0("Médiane = ", round(med_age, 1), " ans"),
           color = pal["orange"], size = 2.85, hjust = 0, vjust = 2.2, fontface = "bold") +
  annotate("text", x = moy_age + 1.5, y = Inf,
           label = paste0("Moyenne = ", round(moy_age, 1), " ans"),
           color = pal["rouge"], size = 2.85, hjust = 0, vjust = 4.6, fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 100, 10), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.06))) +
  labs(
    title    = "Distribution de l'âge des membres du ménage",
    subtitle = "Nigeria GHS Panel · Wave 4 (2018) · Classes de 5 ans",
    x        = "Âge (années)", y = "Effectif",
    caption  = "Source : sect1_harvestw4 · Trait orange = médiane · Pointillé rouge = moyenne"
  ) +
  theme_ghs()

# ── Boîte à moustaches ────────────────────────────────────────────────────────
p_box_age <- ggplot(df_age_w4, aes(x = "", y = age)) +
  geom_boxplot(
    fill = pal["bleu"], color = pal["texte"],
    alpha = 0.70, width = 0.38,
    outlier.color = pal["rouge"], outlier.size = 0.75, outlier.alpha = 0.38,
    linewidth = 0.32
  ) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = pal["orange"]) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title    = "Boîte à moustaches — Âge (W4)",
    subtitle = "◆ Moyenne   |   Barre centrale = médiane   |   Points rouges = valeurs aberrantes",
    x = NULL, y = "Âge (années)",
    caption = "Source : sect1_harvestw4"
  ) +
  theme_ghs() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Assemblage
p_age_final <- p_hist_age / p_box_age +
  plot_layout(heights = c(3.2, 1.1)) +
  plot_annotation(
    title = "Analyse univariée de l'âge — GHS Wave 4 (2018)",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_age_final)
save_g("T2_age_histogramme_boxplot.png", w = 7, h = 7.5)

# ── Shapiro-Wilk ──────────────────────────────────────────────────────────────
cat("\n── Test de Shapiro-Wilk ──────────────────────────────────\n")
set.seed(42)
shap <- shapiro.test(sample(df_age_w4$age, min(nrow(df_age_w4), 5000)))
print(shap)
cat(if (shap$p.value < 0.05)
  "\n→ p < 0.05 : distribution NON normale (cohérent avec pyramide à base large).\n"
  else "\n→ p ≥ 0.05 : pas de rejet de la normalité.\n")


# ==============================================================================
# T3 — DISTRIBUTION PAR SEXE
#      Camembert W4 + évolution taux de féminisation W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T3 — Distribution par sexe (W4 + évolution W1–W4)")
message(strrep("═", 65))

# ── Camembert W4 ──────────────────────────────────────────────────────────────
sexe_w4 <- df_harvest |>
  filter(vague == "W4 (2018)", !is.na(sexe)) |>
  count(sexe) |>
  mutate(
    prop = n / sum(n),
    ypos = cumsum(prop) - 0.5 * prop,
    lbl  = paste0(sexe, "\n", scales::percent(prop, accuracy = 0.1),
                  "\n(n=", scales::comma(n), ")")
  )

p_camembert <- ggplot(sexe_w4, aes(x = "", y = prop, fill = sexe)) +
  geom_col(width = 1, color = "white", linewidth = 1.2) +
  geom_text(aes(y = ypos, label = lbl),
            color = "white", size = 3.8, fontface = "bold") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = pal_sexe) +
  labs(
    title    = "Répartition par sexe",
    subtitle = "Wave 4 (2018)",
    caption  = "Source : sect1_harvestw4",
    fill     = "Sexe"
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

# ── Évolution W1–W4 (ratio femmes) par milieu ─────────────────────────────────
ratio_sexe <- df_harvest |>
  filter(!is.na(sexe)) |>
  left_join(df_secta |> select(hhid, vague, milieu), by = c("hhid","vague")) |>
  filter(!is.na(milieu)) |>
  group_by(vague, milieu) |>
  summarise(ratio_f = mean(sexe == "Female"), .groups = "drop") |>
  mutate(vague = factor(vague, levels = names(pal_vagues)))

p_ratio_sexe <- ggplot(ratio_sexe,
                       aes(x = vague, y = ratio_f, color = milieu, group = milieu)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = pal["gris"],
             linewidth = 0.6, alpha = 0.7) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 3.8, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(aes(label = scales::percent(ratio_f, accuracy = 0.1)),
            vjust = -1.05, size = 2.85, fontface = "bold", show.legend = FALSE) +
  annotate("text", x = 0.7, y = 0.5015, label = "Parité (50%)",
           color = pal["gris"], size = 2.6, hjust = 0, fontface = "italic") +
  scale_color_manual(values = c("Urban" = pal["bleu"], "Rural" = pal["orange"])) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0.49, 0.52)) +
  labs(
    title    = "Évolution du taux de féminisation (W1–W4)",
    subtitle = "Part des femmes parmi les membres des ménages · Rural vs Urban",
    x = "Vague", y = "Part des femmes", color = "Milieu",
    caption = "Source : sect1_harvest (W1–W4) + secta_harvest (W1–W4)"
  ) +
  theme_ghs()

p_sexe_final <- p_camembert | p_ratio_sexe +
  plot_annotation(
    title = "Structure par sexe — Nigeria GHS Panel",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_sexe_final)
save_g("T3_sexe_camembert_evolution.png", w = 12, h = 5.5)


# ==============================================================================
# T4 — DISTRIBUTION DU STATUT MATRIMONIAL
#      Histogramme W4 + évolution 100% empilé W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T4 — Statut matrimonial (W4 + comparaison W1–W4)")
message(strrep("═", 65))

# Histogramme W4 (adultes 15+)
mat_w4 <- df_harvest |>
  filter(vague == "W4 (2018)", !is.na(statut_mat), !is.na(age), age >= 15) |>
  count(statut_mat) |>
  mutate(prop = n / sum(n),
         statut_mat = fct_reorder(statut_mat, n, .desc = FALSE))

p_mat_hist <- ggplot(mat_w4, aes(x = n, y = statut_mat, fill = statut_mat)) +
  geom_col(alpha = 0.88, width = 0.72) +
  geom_text(
    aes(label = paste0(scales::comma(n), "  (", scales::percent(prop, accuracy = 0.1), ")")),
    hjust = -0.07, size = 2.85, color = pal["texte"]
  ) +
  scale_fill_manual(values = pal_mat) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.24))) +
  labs(
    title    = "Statut matrimonial — Wave 4 (2018)",
    subtitle = "Individus de 15 ans et plus · sect1_harvestw4",
    x = "Effectif", y = NULL,
    caption = "Source : sect1_harvestw4"
  ) +
  theme_ghs() + theme(legend.position = "none")

# Évolution 100% empilé W1–W4
mat_vagues <- df_harvest |>
  filter(!is.na(statut_mat), !is.na(age), age >= 15) |>
  group_by(vague, statut_mat) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(vague) |>
  mutate(prop = n / sum(n),
         vague = factor(vague, levels = names(pal_vagues))) |>
  ungroup()

p_mat_evol <- ggplot(mat_vagues,
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
    title    = "Évolution du statut matrimonial W1–W4",
    subtitle = "Adultes 15+ · Part de chaque statut",
    x = "Vague", y = "Proportion (%)", fill = "Statut",
    caption = "Source : sect1_harvest (W1–W4)"
  ) +
  theme_ghs() + theme(legend.position = "right")

p_mat_final <- p_mat_hist | p_mat_evol +
  plot_annotation(
    title = "Statut matrimonial des membres des ménages",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_mat_final)
save_g("T4_statut_matrimonial_hist_evolution.png", w = 13, h = 6)


# ==============================================================================
# T5 — PYRAMIDE DES ÂGES W4  (livrable principal de l'énoncé)
#      Groupes quinquennaux · Annotation "Wave 4 · 2018" · Couleurs Homme/Femme
# ==============================================================================

message("\n", strrep("═", 65))
message("  T5 — Pyramide des âges W4 annotée (livrable principal)")
message(strrep("═", 65))

# Fonction de préparation commune pyramide
prep_pyr <- function(df_wave) {
  df_wave |>
    filter(!is.na(age), !is.na(sexe), age >= 0, age <= 120) |>
    mutate(
      groupe_age = cut(
        age,
        breaks         = c(seq(0, 95, 5), Inf),
        right          = FALSE,
        include.lowest = TRUE,
        labels         = c(paste0(seq(0, 90, 5), "–", seq(4, 94, 5)), "95+")
      )
    )
}

df_pyr_w4 <- prep_pyr(df_harvest |> filter(vague == "W4 (2018)"))
n_w4 <- nrow(df_pyr_w4)

p_pyr_w4 <- age_pyramid(
  data          = df_pyr_w4,
  age_group     = "groupe_age",
  split_by      = "sexe",
  show_midpoint = FALSE,
  pal           = c(pal["bleu"], pal["rose"])
) +
  # Badge annotation vague (livrable demandé par l'énoncé)
  annotate("label",
           x = Inf, y = Inf,
           label = "Wave 4 · 2018",
           hjust = 1.08, vjust = 1.4, size = 4.2,
           fill  = pal["fond2"], color = pal["violet"],
           label.size = 0.45, fontface = "bold",
           label.r = unit(0.22, "lines")
  ) +
  # Effectif total
  annotate("text",
           x = -Inf, y = -Inf,
           label = paste0("N = ", scales::comma(n_w4), " individus"),
           hjust = -0.05, vjust = -0.85, size = 2.85, color = pal["texte2"]
  ) +
  labs(
    title    = "Pyramide des âges — Nigeria GHS Panel",
    subtitle = "Wave 4 (2018) · Post-Harvest · Groupes quinquennaux",
    x        = "Effectif",
    y        = "Groupe d'âge",
    fill     = "Sexe",
    caption  = paste0("Source : sect1_harvestw4",
                      "   |   Bleu = Homme   |   Rose = Femme")
  ) +
  theme_ghs(base_size = 10) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    axis.text.y      = element_text(size = 8, color = pal["texte2"])
  )

print(p_pyr_w4)
save_g("T5_pyramide_ages_w4.png", w = 7.5, h = 10)

cat("\nCommentaire pyramide W4 :\n")
cat("  → Base large (0–14 ans) : forte fécondité historique.\n")
cat("  → Rétrécissement progressif avec l'âge : mortalité et émigration.\n")
cat("  → Légère surnumérosité féminine à partir de 45–50 ans.\n")
cat("  → Profil typique d'une démographie en transition.\n")


# ==============================================================================
# T6 — PYRAMIDES DES 4 VAGUES CÔTE À CÔTE
# ==============================================================================

message("\n", strrep("═", 65))
message("  T6 — Pyramides des 4 vagues côte à côte (W1–W4)")
message(strrep("═", 65))

df_pyr_w1 <- prep_pyr(df_harvest |> filter(vague == "W1 (2010)"))
df_pyr_w2 <- prep_pyr(df_harvest |> filter(vague == "W2 (2012)"))
df_pyr_w3 <- prep_pyr(df_harvest |> filter(vague == "W3 (2015)"))

# Fonction pour construire une pyramide par vague
faire_pyr <- function(df_pyr, lbl_vague, annee, couleur_badge,
                      show_y = TRUE, show_leg = FALSE) {
  age_pyramid(
    data = df_pyr, age_group = "groupe_age", split_by = "sexe",
    show_midpoint = FALSE, pal = c(pal["bleu"], pal["rose"])
  ) +
    annotate("label",
             x = Inf, y = Inf,
             label = paste0(lbl_vague, " · ", annee),
             hjust = 1.1, vjust = 1.4, size = 3.1,
             fill  = pal["fond2"], color = couleur_badge,
             label.size = 0.38, fontface = "bold",
             label.r = unit(0.18, "lines")
    ) +
    annotate("text",
             x = -Inf, y = -Inf,
             label = paste0("N=", scales::comma(nrow(df_pyr))),
             hjust = -0.08, vjust = -0.75, size = 2.4, color = pal["texte2"]
    ) +
    labs(
      title = NULL,
      x     = "Effectif",
      y     = if (show_y) "Groupe d'âge" else NULL,
      fill  = "Sexe"
    ) +
    theme_ghs(base_size = 8.5) +
    theme(
      legend.position = if (show_leg) "bottom" else "none",
      axis.text.y     = if (show_y) element_text(size = 6.8, color = pal["texte2"])
      else element_blank(),
      axis.ticks.y    = if (show_y) element_line() else element_blank(),
      plot.margin     = margin(5, 4, 4, 4)
    )
}

p4_w1 <- faire_pyr(df_pyr_w1, "W1", "2010", pal_vagues["W1 (2010)"], show_y=TRUE,  show_leg=FALSE)
p4_w2 <- faire_pyr(df_pyr_w2, "W2", "2012", pal_vagues["W2 (2012)"], show_y=FALSE, show_leg=FALSE)
p4_w3 <- faire_pyr(df_pyr_w3, "W3", "2015", pal_vagues["W3 (2015)"], show_y=FALSE, show_leg=FALSE)
p4_w4 <- faire_pyr(df_pyr_w4, "W4", "2018", pal_vagues["W4 (2018)"], show_y=FALSE, show_leg=TRUE)

p_4pyr <- (p4_w1 | p4_w2 | p4_w3 | p4_w4) +
  plot_annotation(
    title    = "Évolution de la structure démographique — W1 (2010) à W4 (2018)",
    subtitle = "Nigeria GHS Panel · Post-Harvest · Groupes quinquennaux · Bleu = Homme · Rose = Femme",
    caption  = "Source : sect1_harvestw1 · sect1_harvestw2 · sect1_harvestw3 · sect1_harvestw4",
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
# T7 — LIEN DE PARENTÉ : BARPLOT + IC 95% + COMPARAISON W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T7 — Lien de parenté + IC 95% (W4 + comparaison W1–W4)")
message(strrep("═", 65))

# Fréquences W4 + IC (PropCIs::scoreci = Wilson)
lien_w4 <- df_harvest |> filter(vague == "W4 (2018)", !is.na(lien))
N_lien   <- nrow(lien_w4)

freq_lien <- lien_w4 |>
  count(lien) |>
  mutate(prop = n / N_lien) |>
  rowwise() |>
  mutate(
    ic_low  = PropCIs::scoreci(n, N_lien, 0.95)$conf.int[1],
    ic_high = PropCIs::scoreci(n, N_lien, 0.95)$conf.int[2]
  ) |>
  ungroup() |>
  arrange(desc(prop)) |>
  mutate(lien = fct_reorder(lien, prop))

cat("\n── Fréquences lien de parenté W4 + IC 95% ───────────────\n")
print(as.data.frame(freq_lien))

p_lien_w4 <- ggplot(freq_lien, aes(x = prop, y = lien, fill = lien)) +
  geom_col(alpha = 0.88, width = 0.66) +
  geom_errorbarh(
    aes(xmin = ic_low, xmax = ic_high),
    height = 0.22, color = pal["gris"], linewidth = 0.65
  ) +
  geom_text(
    aes(x = ic_high, label = scales::percent(prop, accuracy = 0.1)),
    hjust = -0.15, size = 2.9, color = pal["texte"], fontface = "bold"
  ) +
  scale_fill_manual(values = pal_lien) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title    = "Lien de parenté — Wave 4 (2018)",
    subtitle = "Proportions avec IC à 95% (méthode de Wilson)",
    x = "Proportion (%)", y = NULL,
    caption = "Source : sect1_harvestw4 · PropCIs::scoreci"
  ) +
  theme_ghs() + theme(legend.position = "none")

# Évolution W1–W4
lien_vagues <- df_harvest |>
  filter(!is.na(lien)) |>
  group_by(vague, lien) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(vague) |>
  mutate(prop = n / sum(n),
         vague = factor(vague, levels = names(pal_vagues))) |>
  ungroup()

p_lien_evol <- ggplot(lien_vagues,
                      aes(x = vague, y = prop, color = lien, group = lien)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1.4) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -1.0, size = 2.65, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = pal_lien) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0.04, 0.12))) +
  labs(
    title    = "Évolution lien de parenté W1–W4",
    subtitle = "Part de chaque catégorie · Post-Harvest",
    x = "Vague", y = "Proportion (%)", color = "Lien",
    caption = "Source : sect1_harvest (W1–W4)"
  ) +
  theme_ghs()

p_lien_final <- p_lien_w4 | p_lien_evol +
  plot_annotation(
    title = "Structure des ménages par lien de parenté",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_lien_final)
save_g("T7_lien_parente_IC95_evolution.png", w = 13, h = 5.5)


# ==============================================================================
# T8 — TAILLE DES MÉNAGES RURAL/URBAN
#      Violin+boxplot · Test de Wilcoxon · r de rang · Évolution W1–W4
# ==============================================================================

message("\n", strrep("═", 65))
message("  T8 — Taille des ménages Rural/Urban (W4 + W1–W4)")
message(strrep("═", 65))

# Taille ménage W4
taille_w4 <- df_harvest |>
  filter(vague == "W4 (2018)") |>
  group_by(hhid) |>
  summarise(taille = n(), .groups = "drop") |>
  left_join(df_secta |> filter(vague == "W4 (2018)") |> select(hhid, milieu),
            by = "hhid") |>
  filter(!is.na(milieu))

cat("\nStatistiques taille ménage W4 par milieu :\n")
taille_w4 |>
  group_by(milieu) |>
  summarise(n=n(), Médiane=median(taille), Moyenne=round(mean(taille),2),
            .groups="drop") |> print()

# Wilcoxon + r de rang
wilcox_res <- wilcox.test(taille ~ milieu, data = taille_w4)
r_eff      <- taille_w4 |> rstatix::wilcox_effsize(taille ~ milieu)
effet_lbl  <- dplyr::case_when(
  abs(r_eff$effsize) < 0.1 ~ "négligeable",
  abs(r_eff$effsize) < 0.3 ~ "petit",
  abs(r_eff$effsize) < 0.5 ~ "moyen",
  TRUE                      ~ "grand"
)
cat("\n── Wilcoxon :", format.pval(wilcox_res$p.value, digits=3),
    " · r =", round(r_eff$effsize,3), "(", effet_lbl, ")\n")

# Violin + boxplot
p_violin <- ggplot(taille_w4, aes(x = milieu, y = taille, fill = milieu)) +
  geom_violin(alpha = 0.32, linewidth = 0.28, trim = TRUE, bw = 0.75) +
  geom_boxplot(
    width = 0.14, alpha = 0.90, linewidth = 0.35,
    outlier.color = pal["rouge"], outlier.size = 0.65, outlier.alpha = 0.35
  ) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4.2, color = "white") +
  scale_fill_manual(values = c("Urban" = pal["bleu"], "Rural" = pal["orange"])) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(
    title    = "Taille des ménages selon le milieu de résidence",
    subtitle = paste0(
      "W = ", round(wilcox_res$statistic, 0),
      "  ·  p = ", format.pval(wilcox_res$p.value, digits = 3),
      "  ·  r = ", round(r_eff$effsize, 3),
      " (effet ", effet_lbl, ")"
    ),
    x = NULL, y = "Nombre de membres",
    caption = "Source : sect1_harvestw4 + secta_harvestw4 · Wave 4 (2018)"
  ) +
  theme_ghs() + theme(legend.position = "none")

# Évolution taille médiane W1–W4
taille_all <- df_harvest |>
  group_by(hhid, vague) |>
  summarise(taille = n(), .groups = "drop") |>
  left_join(df_secta |> select(hhid, vague, milieu), by = c("hhid","vague")) |>
  filter(!is.na(milieu)) |>
  mutate(vague = factor(vague, levels = names(pal_vagues)))

med_milieu_vague <- taille_all |>
  group_by(vague, milieu) |>
  summarise(mediane = median(taille), .groups = "drop")

p_evol_taille <- ggplot(med_milieu_vague,
                        aes(x = vague, y = mediane, color = milieu, group = milieu)) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(aes(label = round(mediane, 1)),
            vjust = -1.0, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Urban" = pal["bleu"], "Rural" = pal["orange"])) +
  scale_y_continuous(breaks = seq(3, 12, 1)) +
  labs(
    title    = "Évolution de la taille médiane des ménages (W1–W4)",
    subtitle = "Rural vs Urban · Post-Harvest",
    x = "Vague", y = "Taille médiane", color = "Milieu",
    caption = "Source : sect1_harvest (W1–W4) + secta_harvest (W1–W4)"
  ) +
  theme_ghs()

p_taille_final <- p_violin | p_evol_taille +
  plot_annotation(
    title = "Taille des ménages — Rural vs Urban",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = pal["texte"]),
      plot.background = element_rect(fill = pal["fond"], color = NA)
    )
  )
print(p_taille_final)
save_g("T8_taille_menage_violin_evolution.png", w = 13, h = 5.5)


# ==============================================================================
# T9 — TABLEAU GTSUMMARY STRATIFIÉ PAR MILIEU  [W4 harvest]
#      âge · sexe · taille du ménage · statut matrimonial
# ==============================================================================

message("\n", strrep("═", 65))
message("  T9 — Tableau gtsummary stratifié par milieu (W4)")
message(strrep("═", 65))

df_gt <- df_harvest |>
  filter(vague == "W4 (2018)", !is.na(age)) |>
  left_join(taille_w4 |> select(hhid, taille, milieu), by = "hhid") |>
  filter(!is.na(milieu)) |>
  mutate(milieu = factor(milieu, levels = c("Urban","Rural")))

tbl_demo <- df_gt |>
  select(age, sexe, taille, statut_mat, milieu) |>
  tbl_summary(
    by        = milieu,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
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
    all_continuous()  ~ "wilcox.test",
    all_categorical() ~ "chisq.test"
  )) |>
  add_overall(last = FALSE) |>
  bold_labels() |>
  italicize_levels() |>
  modify_caption(
    "**Tableau 1. Caractéristiques démographiques par milieu — GHS Wave 4 (2018)**"
  ) |>
  modify_footnote(
    update = everything() ~
      "Moyenne (écart-type) pour les variables continues ; n (%) pour les catégorielles. Tests : Wilcoxon-Mann-Whitney · chi-deux."
  )

print(tbl_demo)

path_tbl <- "outputs/tableaux/T9_tableau_gtsummary_milieu.html"
tbl_demo |> as_gt() |> gt::gtsave(path_tbl)
message("  ✓ ", path_tbl)

# Décommenter pour export Word :
# tbl_demo |> as_gt() |> gt::gtsave("outputs/tableaux/T9_tableau_gtsummary_milieu.docx")


# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================

message("\n", strrep("═", 65))
message("  ✓  TP1 terminé avec succès")
message(strrep("═", 65), "\n")

cat("Outputs produits :\n\n")
cat("  outputs/graphiques/\n")
cat("    T1_qualite_valeurs_manquantes.png       ← heatmap % NA harvest+planting W1–W4\n")
cat("    T2_age_histogramme_boxplot.png          ← histogramme + boxplot âge W4\n")
cat("    T3_sexe_camembert_evolution.png         ← camembert W4 + évolution taux féminisation\n")
cat("    T4_statut_matrimonial_hist_evolution.png← histogramme W4 + 100% empilé W1–W4\n")
cat("    T5_pyramide_ages_w4.png                 ← LIVRABLE PRINCIPAL (énoncé)\n")
cat("    T6_pyramides_4_vagues.png               ← 4 pyramides côte à côte W1–W4\n")
cat("    T7_lien_parente_IC95_evolution.png      ← barplot IC95% + évolution W1–W4\n")
cat("    T8_taille_menage_violin_evolution.png   ← violin+boxplot + évolution W1–W4\n\n")
cat("  outputs/tableaux/\n")
cat("    T9_tableau_gtsummary_milieu.html        ← LIVRABLE PRINCIPAL (énoncé)\n")
