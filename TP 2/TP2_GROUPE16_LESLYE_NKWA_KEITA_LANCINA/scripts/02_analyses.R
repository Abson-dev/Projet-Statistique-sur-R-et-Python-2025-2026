# =============================================================================
# TP2 GHS Nigeria Panel Wave 4 (2018-2019)
# Script 02 : Analyses | Tâches 8 à 12
# Thème : Capital humain | Éducation et alphabétisation
# Données : NBS Nigeria / World Bank LSMS-ISA
# =============================================================================

# Packages et graine chargés par main.R.

# Note : coin et rstatix partagent des noms de fonctions identiques:
# kruskal_test, chisq_test, wilcox_test, sign_test, friedman_test. 
# coin étant chargé après rstatix dans main.R,
# il masque ces fonctions quand les deux packages sont chargés (conflit de namespace)
#Préfixe rstatix:: utilisé systématiquement.

# =============================================================================
# BLOC 0 — Chargement et conventions graphiques
# =============================================================================

df_educ <- readRDS(here("data", "processed", "df_educ_clean.rds"))
cat("Données chargées :", nrow(df_educ), "x", ncol(df_educ), "\n")
# Attendu : 25 767 x 69

# --- Palette TP2 — violet UNESCO (SDG4 / Education for All) ---
# Source : Conventions colorimétriques — projet 2025-2026
# Couleur dominante : violet #5C2D91 — couleur institutionnelle UNESCO,
# utilisée dans tous les rapports EFA (Education for All) et SDG4.
# Règles transversales inter-TP :
#   Rural  -> #80BD41 (vert FAO/IFPRI)
#   Urbain -> #6C757D (gris HABITAT/ONU)
#   Homme  -> #0058AB (convention WHO/HMD pyramides des âges)
#   Femme  -> #E8416F (convention WHO/HMD pyramides des âges)

pal_tp2 <- list(
  primaire   = "#5C2D91",
  secondaire = "#9B59B6",
  superieur  = "#3D3B8E",
  faible     = "#D7BDE2",
  homme      = "#0058AB",
  femme      = "#C0392B",
  rural      = "#80BD41",
  urbain     = "#6C757D"
)

# Palette séquentielle niveau_educ : plus le niveau est élevé,
# plus la couleur est foncée — convention DIME Analytics / Banque Mondiale
# ("darker shades are higher values", dimewiki.worldbank.org, 2021)
pal_niveaux <- c(
  "Aucun"            = "#D7BDE2",
  "Primaire"         = "#C39BD3",
  "Secondaire"       = "#9B59B6",
  "Technique/Prof"   = "#7D3C98",
  "Tertiaire"        = "#5C2D91",
  "Coranique/Adulte" = "#7D6608"
)

# Caption standardisé défini dans main.R — disponible globalement

# Theme commun — appliqué sur toutes les figures
theme_tp2 <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.caption  = element_text(hjust  = 1, size = 8,
                                   color  = "grey50", face = "italic"),
      plot.title    = element_text(face   = "bold", size = 13),
      plot.subtitle = element_text(size   = 10, color = "grey40"),
      axis.text     = element_text(size   = 10),
      legend.title  = element_text(size   = 10, face = "bold"),
      legend.text   = element_text(size   = 9)
    )
}


# =============================================================================
# TÂCHE 8 — Distribution du niveau d'éducation
# =============================================================================
# Population : membres actifs éligibles avec niveau_educ renseigné
# (hors moins de 3 ans — s2aq2 == 2)
# Variable : niveau_educ (factor ordonné, 6 catégories)
# Visualisation : barplot horizontal ordonné par fréquence avec IC 95%
# IC calculés par la méthode exacte de Clopper-Pearson (PropCIs)

df_t8 <- df_educ |>
  filter(!is.na(niveau_educ))

n_t8 <- nrow(df_t8)

freq_t8 <- df_t8 |>
  count(niveau_educ) |>
  mutate(
    pct    = round(n / n_t8 * 100, 1))

cat("\n=== Tâche 8 — Distribution niveau_educ ===\n")
print(freq_t8)

p_t8 <- ggplot(freq_t8,
               aes(x = pct,
                   y = fct_reorder(niveau_educ, pct),
                   fill = niveau_educ)) +
  geom_col(alpha = 0.88) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.2, size = 3.5, color = pal_tp2$superieur) +
  scale_fill_manual(values = pal_niveaux, guide = "none") +
  scale_x_continuous(limits = c(0, 44)) +
  labs(
    title    = "Distribution du niveau d'éducation complété",
    subtitle = paste0("Nigeria GHS Panel W4 | n = ",
                      format(n_t8, big.mark = " "),
                      " membres éligibles (3 ans et plus)"),
    x        = "Proportion (%)",
    y        = NULL,
    caption  = source_ghs
  ) +
  theme_tp2()

ggsave(here("outputs", "figures", "fig01_niveau_educ_distribution.png"),
       plot = p_t8, width = 10, height = 7, dpi = 300)
cat("fig01_niveau_educ_distribution.png exporté\n")

# INTERPRÉTATION :
# Le primaire domine (33.4%) — beaucoup d'adultes nigérians ont fréquenté
# l'école sans aller au-delà du cycle primaire, point d'arrêt naturel
# lié aux coûts de transition vers le secondaire.
# Un adulte sur quatre (24.9%) n'a aucun niveau formel — taux élevé mais
# cohérent avec l'UNESCO qui estimait 38% d'analphabétisme adulte au
# Nigeria en 2018 (la différence tient à l'inclusion des 3-17 ans ici).
# Le coranique (6.1%) reflète l'importance de l'éducation islamique
# dans le Nord — à ne pas assimiler à l'absence d'instruction.

# =============================================================================
# TÂCHE 9 — Niveau d'éducation par sexe (adultes 18+)
# =============================================================================
# Population : adultes 18 ans et plus avec niveau_educ et sexe renseignés.
# Filtre adulte_18plus indispensable : inclure les enfants biaiserait
# l'analyse car leur niveau "Aucun" ou "Primaire" reflète leur âge
# et non une inégalité de genre.
#
# Test du chi-deux : H0 = indépendance entre sexe et niveau d'éducation
# V de Cramér : mesure de l'intensité de l'association (0 = indépendance,
# 1 = association parfaite). Seuils Cohen (1988) :
#   < 0.10 : négligeable | 0.10-0.30 : faible | > 0.30 : modéré à fort

df_t9 <- df_educ |>
  filter(adulte_18plus == TRUE,
         !is.na(niveau_educ),
         !is.na(sexe))

cat("\n=== Tâche 9 — Niveau éducation x Sexe ===\n")
cat("Adultes 18+ retenus :", nrow(df_t9), "\n")

# Tableau de contingence
tab_t9 <- table(df_t9$niveau_educ, df_t9$sexe)
cat("\nEffectifs :\n");     print(tab_t9)
cat("\nProportion par sexe (%) :\n")
print(round(prop.table(tab_t9, margin = 2) * 100, 1))

# Tests
chi2_t9 <- chisq.test(tab_t9)
cat("\nChi-deux :\n"); print(chi2_t9)

v_t9 <- rstatix::cramer_v(df_t9$niveau_educ, df_t9$sexe)
cat("V de Cramér :", round(v_t9, 4), "\n")

# Barplot 100% empilé
df_t9_plot <- df_t9 |>
  count(sexe, niveau_educ) |>
  group_by(sexe) |>
  mutate(pct = n / sum(n) * 100)

p_t9 <- ggplot(df_t9_plot,
               aes(x = sexe, y = pct / 100, fill = niveau_educ)) +
  geom_col(position = "fill", alpha = 0.88, width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = pal_niveaux, name = "Niveau") +
  scale_x_discrete(labels = c(Homme = "Homme", Femme = "Femme")) +
  labs(
    title    = "Niveau d'éducation par sexe (adultes 18 ans et plus)",
    subtitle = paste0(
      "Chi-deux : p ", ifelse(chi2_t9$p.value < 0.001, "< 0,001",
                              paste0("= ", round(chi2_t9$p.value, 3))),
      "  |  V de Cramér = ", round(v_t9, 3)
    ),
    x        = NULL,
    y        = "Proportion",
    caption  = source_ghs
  ) +
  theme_tp2()

ggsave(here("outputs", "figures", "fig02_niveau_educ_sexe.png"),
       plot = p_t9, width = 9, height = 7, dpi = 300)
cat("fig02_niveau_educ_sexe.png exporté\n")

# INTERPRÉTATION :
# L'association sexe-niveau est significative (p < 0.001) mais d'ampleur
# faible à modérée (V = 0.214). La différence la plus marquée est au
# niveau "Aucun" : 31.3% des femmes vs 14.7% des hommes n'ont aucune
# instruction — écart de 16.6 points caractéristique des inégalités
# de genre en Afrique subsaharienne (UNGEI, 2020).
# Au secondaire, les hommes dominent (39.2% vs 28.7%) mais l'écart
# se réduit — signe d'une progression féminine dans les générations
# récentes (DHS Nigeria 2018 confirme cette tendance).


# =============================================================================
# TÂCHE 10 — Âge x niveau d'éducation (adultes 18+)
# =============================================================================
# Test de Kruskal-Wallis : compare les distributions d'âge entre groupes
# de niveau d'éducation. Non paramétrique — normalité de l'âge rejetée
# au TP1 (Shapiro-Wilk, p < 0.001).
# Post-hoc Dunn avec correction Bonferroni si H0 rejetée.
# On analyse l'âge sur les adultes uniquement pour éviter que les enfants
# (tous à faible niveau par construction) ne dominent les résultats.

df_t10 <- df_educ |>
  filter(adulte_18plus == TRUE,
         !is.na(niveau_educ),
         !is.na(s1q4))

cat("\n=== Tâche 10 — Kruskal-Wallis âge x niveau_educ ===\n")

kw_t10 <- rstatix::kruskal_test(df_t10, s1q4 ~ niveau_educ)
print(kw_t10)

if (kw_t10$p < 0.05) {
  cat("\nPost-hoc Dunn (Bonferroni) :\n")
  dunn_t10 <- rstatix::dunn_test(df_t10, s1q4 ~ niveau_educ,
                        p.adjust.method = "bonferroni")
  print(dunn_t10)
}

p_t10 <- ggplot(df_t10,
                aes(x = fct_reorder(niveau_educ, s1q4, .fun = median),
                    y     = s1q4,
                    fill  = niveau_educ)) +
  geom_boxplot(alpha        = 0.82,
               outlier.size  = 0.6,
               outlier.color = "grey60",
               width         = 0.55) +
  scale_fill_manual(values = pal_niveaux, guide = "none") +
  labs(
    title    = "Distribution de l'âge par niveau d'éducation (adultes 18+)",
    subtitle = paste0(
      "Kruskal-Wallis : H(", kw_t10$df, ") = ",
      round(kw_t10$statistic, 2),
      ", p ", ifelse(kw_t10$p < 0.001, "< 0,001",
                     paste0("= ", round(kw_t10$p, 3)))
    ),
    x        = "Niveau d'éducation",
    y        = "Age (années complètes)",
    caption  = source_ghs
  ) +
  theme_tp2() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(here("outputs", "figures", "fig03_age_niveau_educ.png"),
       plot = p_t10, width = 10, height = 7, dpi = 300)
cat("fig03_age_niveau_educ.png exporté\n")

# INTERPRÉTATION :
# H0 rejetée massivement (H = 2174, p < 0.001) : les distributions
# d'âge diffèrent significativement entre tous les niveaux sauf
# Technique/Prof vs Tertiaire (p.adj = 1, ns).
# Le secondaire a la médiane d'âge la plus basse — les scolarisés
# récents s'arrêtent plus souvent au secondaire qu'aux générations
# précédentes qui s'arrêtaient au primaire.
# Le niveau "Aucun" concentre les adultes les plus âgés — les
# générations antérieures avaient moins accès à l'école formelle,
# cohérent avec l'expansion scolaire post-indépendance (1960-1990).


# =============================================================================
# TÂCHE 11 — Taux de scolarisation des 6-17 ans par milieu
# =============================================================================
# Population cible : enfants et adolescents 6-17 ans (enfant_6_17 == TRUE)
# avec scolarise_actuel renseigné (année 2018/2019).
# IC 95% exacts de Clopper-Pearson via binom.test().
# Chi-deux sur tableau milieu x scolarisation.

df_t11 <- df_educ |>
  filter(enfant_6_17 == TRUE,
         !is.na(scolarise_actuel),
         !is.na(milieu))

cat("\n=== Tâche 11 — Scolarisation 6-17 ans x Milieu ===\n")
cat("Enfants 6-17 ans retenus :", nrow(df_t11), "\n")

tab_t11 <- table(df_t11$milieu, df_t11$scolarise_actuel)
cat("\nEffectifs :\n"); print(tab_t11)
cat("\nProportion par milieu (%) :\n")
print(round(prop.table(tab_t11, margin = 1) * 100, 1))

chi2_t11 <- chisq.test(tab_t11)
cat("\nChi-deux :\n"); print(chi2_t11)

ic_t11 <- df_t11 |>
  group_by(milieu) |>
  summarise(
    n_total = n(),
    n_scol  = sum(scolarise_actuel),
    taux    = round(n_scol / n_total * 100, 1),
    ic_low  = round(binom.test(n_scol, n_total)$conf.int[1] * 100, 1),
    ic_up   = round(binom.test(n_scol, n_total)$conf.int[2] * 100, 1),
    .groups = "drop"
  )

cat("\nTaux de scolarisation avec IC 95% :\n")
print(ic_t11)

p_t11 <- ggplot(ic_t11,
                aes(x = milieu, y = taux, fill = milieu)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_up),
                width = 0.12, linewidth = 0.7,
                color = pal_tp2$superieur) +
  geom_text(aes(label = paste0(taux, "%")),
            vjust = -2, size = 4.2, fontface = "bold",
            color = pal_tp2$superieur) +
  scale_fill_manual(values = c(Urbain = pal_tp2$urbain,
                               Rural  = pal_tp2$rural),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Taux de scolarisation des 6-17 ans par milieu de résidence",
    subtitle = paste0(
      "Chi-deux : p ",
      ifelse(chi2_t11$p.value < 0.001, "< 0,001",
             paste0("= ", round(chi2_t11$p.value, 3))),
      "  |  IC 95% : méthode exacte de Clopper-Pearson"
    ),
    x        = NULL,
    y        = "Taux de scolarisation (%)",
    caption  = source_ghs
  ) +
  theme_tp2()

ggsave(here("outputs", "figures", "fig04_scolarisation_milieu.png"),
       plot = p_t11, width = 8, height = 7, dpi = 300)
cat("fig04_scolarisation_milieu.png exporté\n")

# INTERPRÉTATION :
# Les taux de scolarisation sont élevés dans les deux milieux
# (92.4% urbain, 88.5% rural) — progrès notable depuis les années 2000
# grâce à la politique UBE (Universal Basic Education, 2004).
# L'écart rural-urbain de 3.9 points est statistiquement significatif
# (p < 0.001) mais faible en ampleur — les IC ne se chevauchent pas
# (91.2-93.4 vs 87.6-89.4), confirmant une différence réelle et non
# un artefact d'échantillonnage.
# Le décrochage hors scolarité reste plus fréquent en rural (11.5% vs
# 7.6%) — lié à la distance aux établissements et aux travaux agricoles
# (FAO, 2018, The State of Food and Agriculture).

# =============================================================================
# TÂCHE 12 — Heatmap taux d'analphabétisme par État nigérian
# =============================================================================
# Pour chaque État (37 entités : 36 États + FCT Abuja), on calcule la part
# d'adultes 18+ sans instruction (niveau_educ == "Aucun") parmi les adultes
# avec niveau_educ renseigné.
# Les États avec effectif < 30 sont annotés (*) pour signaler l'instabilité
# de l'estimateur — seuil recommandé par la Banque Mondiale pour les
# statistiques désagrégées (DIME Analytics, 2021).
# Palette séquentielle blanc -> violet foncé : convention UNESCO pour
# les heatmaps d'analphabétisme (plus foncé = taux plus élevé = situation
# moins favorable).

df_t12 <- df_educ |>
  filter(adulte_18plus == TRUE, !is.na(niveau_educ)) |>
  mutate(state_label = as_factor(state))

heatmap_t12 <- df_t12 |>
  group_by(state_label) |>
  summarise(
    n_total              = n(),
    n_aucun              = sum(niveau_educ == "Aucun"),
    taux_analphabetisme  = round(n_aucun / n_total * 100, 1),
    instable             = n_total < 30,
    .groups              = "drop"
  ) |>
  mutate(
    label = if_else(instable,
                    paste0(taux_analphabetisme, "%*"),
                    paste0(taux_analphabetisme, "%"))
  )


# Tableau des 10 États avec le taux d'analphabétisme le plus élevé
tab_t12 <- heatmap_t12 |>
  arrange(desc(taux_analphabetisme)) |>
  select(state_label, n_total, n_aucun, taux_analphabetisme) |>
  rename(
    Etat                = state_label,
    N_adultes           = n_total,
    N_sans_instruction  = n_aucun,
    Taux_pct            = taux_analphabetisme
  ) |>
  head(10)

cat("\n=== Top 10 États — taux d'analphabétisme adulte ===\n")
print(tab_t12)

write_excel_csv(
  tab_t12,
  here("outputs", "tables", "tab02_top10_analphabetisme.csv")
)
cat("tab02_top10_analphabetisme.csv exporté\n")
p_t12 <- ggplot(heatmap_t12,
                aes(x    = 1,
                    y    = fct_reorder(state_label,
                                       taux_analphabetisme),
                    fill = taux_analphabetisme)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_gradient(low  = pal_tp2$faible,
                      high = pal_tp2$primaire,
                      name = "% sans\ninstruction") +
  labs(
    title    = "Part d'adultes sans instruction par État nigérian",
    subtitle = "Adultes 18 ans et plus | * Effectif < 30 : estimateur instable",
    x        = NULL,
    y        = NULL,
    caption  = source_ghs
  ) +
  theme_tp2() +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid   = element_blank())

ggsave(here("outputs", "figures", "fig05_heatmap_analphabetisme_etats.png"),
       plot = p_t12, width = 8, height = 14, dpi = 300)
cat("fig05_heatmap_analphabetisme_etats.png exporté\n")


# INTERPRÉTATION :
# La géographie de l'analphabétisme suit la fracture Nord-Sud nigériane.
# Les États du Nord-Ouest et du Nord-Est dominent (Kebbi 66.7%, Niger
# 55.6%, Bauchi 46.2%) — régions historiquement marquées par la
# prédominance de l'éducation coranique sur le système formel et par
# des taux de pauvreté plus élevés (NBS Poverty Report, 2020).
# Les États du Sud affichent les taux les plus bas : Rivers 2.1%,
# Lagos 4.7% — économies plus diversifiées, meilleure infrastructure
# scolaire et tradition d'alphabétisation plus ancienne.
# Cette disparité Nord-Sud persistante justifie des politiques
# éducatives différenciées par zone géopolitique.

# =============================================================================
# TÂCHE 13 — Tableau récapitulatif gtsummary
# =============================================================================
# Tableau stratifié par milieu (Urbain / Rural) pour les variables clés
# du TP2 : alphabétisation, niveau d'éducation, scolarisation actuelle,
# sexe. Produit avec tbl_summary() — exportable en Word via Quarto.
# Les p-valeurs sont calculées automatiquement par add_p() :
#   - chi-deux pour les variables catégorielles
#   - Wilcoxon pour les variables continues (non utilisées ici)

df_t13 <- df_educ |>
  filter(!is.na(milieu)) |>
  select(milieu, sexe, alphabetise, niveau_educ, scolarise_actuel)

tableau_t13 <- df_t13 |>
  tbl_summary(
    by           = milieu,
    label        = list(
      sexe             ~ "Sexe",
      alphabetise      ~ "Alphabétisé(e)",
      niveau_educ      ~ "Niveau d'éducation",
      scolarise_actuel ~ "Scolarisé(e) en 2018/2019"
    ),
    statistic    = list(all_categorical() ~ "{n} ({p}%)"),
    missing      = "ifany",
    missing_text = "Valeurs manquantes"
  ) |>
  add_overall(last = FALSE) |>
  add_p() |>
  modify_header(label ~ "**Variable**",
                all_stat_cols() ~ "**{level}**\nN = {n}") |>
  bold_labels()

print(tableau_t13)

# Export CSV (encodage Excel)
write_excel_csv(
  tableau_t13 |> as_tibble() |>
    mutate(across(everything(), as.character)),
  here("outputs", "tables", "tab01_gtsummary_education.csv")
)

# Export HTML
gt::gtsave(
  tableau_t13 |>
    as_gt() |>
    gt::tab_header(
      title    = "Caractéristiques éducatives par milieu de résidence",
      subtitle = "Nigeria GHS Panel W4 (2018-2019)"
    ) |>
    gt::tab_source_note(source_ghs) |>
    gt::tab_options(
      heading.background.color  = "#5C2D91",
      heading.title.font.size   = 16,
      heading.subtitle.font.size = 12,
      column_labels.font.weight = "bold"
    ),
  filename = here("outputs", "tables", "tab01_gtsummary_education.html")
)

cat("tab01_gtsummary_education exporté (CSV + HTML)\n")

# INTERPRÉTATION :
# Le tableau confirme les disparités rural-urbain sur toutes les
# dimensions éducatives simultanément (toutes p < 0.001).
# L'urbain devance le rural sur l'alphabétisation, le niveau
# d'éducation et la scolarisation actuelle — cohérent avec les
# analyses individuelles des tâches 9 à 12.
# Ce tableau récapitulatif constitue la pièce maîtresse du rapport :
# il synthétise l'ensemble des inégalités éducatives en une seule vue.

cat("\n======================================================\n")
cat("  Script 02_analyses.R terminé\n")
cat("  Figures  : outputs/figures/ (fig01 à fig05)\n")
cat("  Tableaux : outputs/tables/  (tab01)\n")
cat("======================================================\n")