# =============================================================================
# TP3 GHS Nigeria Panel Wave 4 (2018-2019)
# Script 02 : Analyses | Tâches 13 à 19
# Thème : Santé | Accès aux soins et dépenses de santé
# Données : NBS Nigeria / World Bank LSMS-ISA
# =============================================================================

# Packages et graine chargés par main.R.
# Note : rstatix:: utilisé systématiquement — conflit de namespace avec coin.


# =============================================================================
# BLOC 0 — Chargement et conventions graphiques
# =============================================================================

df_sante <- readRDS(here("data", "processed", "df_sante_clean.rds"))
cat("Données chargées :", nrow(df_sante), "x", ncol(df_sante), "\n")
# Attendu : 25 767 x 85

# --- Palette TP3 — turquoise OMS (WHO World Health Statistics) ---
# Source : Conventions_colorimétriques.docx — projet ENSAE ISE1 2025-2026
# Couleur dominante : turquoise #009688 — couleur institutionnelle OMS,
# associée universellement aux soins médicaux et systèmes de santé.
# Règles transversales inter-TP :
#   Rural  -> #80BD41 (vert FAO/IFPRI)
#   Urbain -> #6C757D (gris HABITAT/ONU)
#   Homme  -> #0058AB (convention WHO/HMD)
#   Femme  -> #E8416F (convention WHO/HMD)

pal_tp3 <- list(
  primaire   = "#009688",
  secondaire = "#00ACC1",
  urgence    = "#E53935",
  public     = "#1565C0",
  prive      = "#2E7D32",
  autre      = "#F9A825",
  depense    = "#EF6C00",
  homme      = "#0058AB",
  femme      = "#E8416F",
  rural      = "#80BD41",
  urbain     = "#6C757D"
)

pal_presta <- c(
  "Public" = "#1565C0",
  "Prive"  = "#2E7D32",
  "Autre"  = "#F9A825"
)

source_ghs <- paste0(
  "Source : Nigeria GHS Panel W4 (2018-2019), ",
  "NBS / World Bank LSMS-ISA. Calculs des auteurs."
)

theme_tp3 <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.caption  = element_text(hjust = 1, size = 8,
                                   color = "grey50", face = "italic"),
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      axis.text     = element_text(size = 10),
      legend.title  = element_text(size = 10, face = "bold"),
      legend.text   = element_text(size = 9)
    )
}


# =============================================================================
# TÂCHE 13 — Taux de morbidité par sexe et groupe d'âge
# =============================================================================
# Population : membres avec s4aq3 renseigné (hors enfants < 5 ans)
# Variable : malade (1/0) — s4aq3 == 1 dans sect4a
# Stratification : sexe et groupe_age
# IC 95% : méthode exacte de Clopper-Pearson (binom.test)
# Le taux de morbidité est calculé sur les 21 496 membres renseignés
# uniquement — les 4 271 enfants < 5 ans sont exclus du dénominateur.

df_t13 <- df_sante |>
  filter(!is.na(malade), !is.na(sexe), !is.na(groupe_age))

cat("\n=== Tâche 13 — Taux de morbidité ===\n")
cat("Population renseignée :", nrow(df_t13), "\n")
cat("Taux global :", round(mean(df_t13$malade) * 100, 1), "%\n")

ic_sexe <- df_t13 |>
  group_by(sexe) |>
  summarise(
    n_total   = n(),
    n_malades = sum(malade),
    taux      = round(n_malades / n_total * 100, 1),
    ic_low    = round(binom.test(n_malades, n_total)$conf.int[1] * 100, 1),
    ic_up     = round(binom.test(n_malades, n_total)$conf.int[2] * 100, 1),
    .groups   = "drop"
  )

cat("\nTaux de morbidité par sexe :\n")
print(ic_sexe)

ic_age <- df_t13 |>
  group_by(groupe_age) |>
  summarise(
    n_total   = n(),
    n_malades = sum(malade),
    taux      = round(n_malades / n_total * 100, 1),
    ic_low    = round(binom.test(n_malades, n_total)$conf.int[1] * 100, 1),
    ic_up     = round(binom.test(n_malades, n_total)$conf.int[2] * 100, 1),
    .groups   = "drop"
  )

cat("\nTaux de morbidité par groupe d'âge :\n")
print(ic_age)

p_t13a <- ggplot(ic_sexe, aes(x = sexe, y = taux, fill = sexe)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_up),
                width = 0.12, linewidth = 0.7,
                color = pal_tp3$primaire) +
  geom_text(aes(label = paste0(taux, "%")),
            vjust = -2, size = 4, fontface = "bold",
            color = pal_tp3$primaire) +
  scale_fill_manual(values = c(Homme = pal_tp3$homme,
                               Femme = pal_tp3$femme),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 15),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Taux de morbidité déclarée par sexe",
    subtitle = "4 semaines de référence | IC 95% : méthode exacte de Clopper-Pearson",
    x        = NULL,
    y        = "Taux de morbidité (%)",
    caption  = source_ghs
  ) +
  theme_tp3()
ggsave(here("outputs", "figures", "fig01a_morbidite_sexe.png"),
       plot = p_t13a, width = 9, height = 7, dpi = 300)
cat("fig01a_morbidite_sexe.png exporté\n")

#  limite axe Y portée à 30 pour inclure le groupe 60+
# dont le taux (23.5%) dépassait l'ancienne limite de 20.
p_t13b <- ggplot(ic_age, aes(x = groupe_age, y = taux,
                             fill = groupe_age)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_up),
                width = 0.15, linewidth = 0.7,
                color = pal_tp3$primaire) +
  geom_text(aes(label = paste0(taux, "%")),
            vjust = -2, size = 3.8, fontface = "bold",
            color = pal_tp3$primaire) +
  scale_fill_manual(
    values = c("0-14"  = "#80CBC4",
               "15-34" = "#26A69A",
               "35-59" = "#00897B",
               "60+"   = "#00695C"),
    guide  = "none"
  ) +
  scale_y_continuous(limits = c(0, 30),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Taux de morbidité déclarée par groupe d'âge",
    subtitle = "4 semaines de référence | IC 95% : méthode exacte de Clopper-Pearson",
    x        = "Groupe d'âge",
    y        = "Taux de morbidité (%)",
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig01b_morbidite_age.png"),
       plot = p_t13b, width = 9, height = 7, dpi = 300)
cat("fig01b_morbidite_age.png exporté\n")



# INTERPRÉTATION :
# Taux global : 9.3% — cohérent DHS Nigeria 2018 (8-12% sur 4 semaines).
# Femmes 10.2% vs Hommes 8.4% : écart réel (IC ne se chevauchent pas).
# Les femmes déclarent plus de morbidité dans les enquêtes LSMS —
# recours aux soins pré/post-nataux et moindre sous-déclaration.
# Gradient âge très net : 7.4% (0-14) -> 23.5% (60+) — les personnes
# âgées cumulent maladies chroniques et comorbidités.
# Note : les moins de 5 ans sont exclus du dénominateur (4 271 NA).


# =============================================================================
# TÂCHE 14 — Types de maladies déclarées
# =============================================================================
# Population : membres malades (malade == 1) avec s4aq3b_1 renseigné
# Les 10 affections les plus fréquentes, colorées par catégorie :
#   Infectieuse  : paludisme, TB, typhoïde, choléra, méningite,
#                  varicelle, pneumonie, diarrhée, dysenterie,
#                  hépatite B, ver de Guinée
#   Respiratoire : rhume, grippe, catarrhe, toux
#   Traumatique  : blessure
#   Chronique    : hypertension, diabète, ulcère
#   Autre        : maux de tête, douleurs corporelles, problèmes
#                  oculaires, dentaires, cutanés, autre

df_t14 <- df_sante |>
  filter(malade == 1, !is.na(s4aq3b_1)) |>
  mutate(
    type_maladie = as_factor(s4aq3b_1),
    categorie    = case_when(
      s4aq3b_1 %in% c(1, 2, 3, 4, 5, 6, 7, 8,
                      9, 19, 20, 23)      ~ "Infectieuse",
      s4aq3b_1 %in% c(10, 14, 15, 16)     ~ "Respiratoire",
      s4aq3b_1 == 11                       ~ "Traumatique",
      s4aq3b_1 %in% c(13, 18, 24)         ~ "Chronique",
      TRUE                                  ~ "Autre"
    )
  )

top10_t14 <- df_t14 |>
  count(type_maladie, categorie) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  slice_max(n, n = 10)

cat("\n=== Tâche 14 — Top 10 maladies ===\n")
print(top10_t14)

pal_cat <- c(
  "Infectieuse"  = pal_tp3$urgence,
  "Respiratoire" = pal_tp3$secondaire,
  "Traumatique"  = pal_tp3$depense,
  "Chronique"    = pal_tp3$public,
  "Autre"        = "grey60"
)

p_t14 <- ggplot(top10_t14,
                aes(x    = pct,
                    y    = fct_reorder(type_maladie, pct),
                    fill = categorie)) +
  geom_col(alpha = 0.87) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.15, size = 3.5,
            color = "grey30") +
  scale_fill_manual(values = pal_cat, name = "Catégorie") +
  scale_x_continuous(limits = c(0, 55)) +
  labs(
    title    = "Les 10 affections les plus fréquentes",
    subtitle = "Parmi les membres ayant déclaré une maladie/blessure (4 semaines)",
    x        = "Proportion (%)",
    y        = NULL,
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig02_types_maladies.png"),
       plot = p_t14, width = 11, height = 7, dpi = 300)
cat("fig02_types_maladies.png exporté\n")

# INTERPRÉTATION :
# Douleurs corporelles (25.5%) et paludisme (25.2%) codominent —
# écart marginal de 0.3 points. Le profil symptomatique (douleurs,
# céphalées) reflète la déclaration auto-rapportée sans diagnostic.
# Catarrhe et toux (Respiratoire) : 2e cause de mortalité infantile
# au Nigeria (WHO NCHS, 2018).
# Maladies chroniques sous-représentées — fenêtre de 4 semaines
# inadaptée à leur capture dans les enquêtes auto-déclarées.


# =============================================================================
# TÂCHE 15 — Recours aux soins par type de prestataire
# =============================================================================
# Population : membres ayant consulté (a_consulte == 1)
# Variable : type_presta (Public / Privé / Autre) depuis s4aq8
# Note : 729 consultants sans s4aq8 renseigné sont exclus (consultations
# à domicile ou chez chemist/PMV sans établissement identifiable).

df_t15 <- df_sante |>
  filter(a_consulte == 1, !is.na(type_presta))

cat("\n=== Tâche 15 — Recours aux soins par prestataire ===\n")
cat("Consultants avec prestataire identifié :", nrow(df_t15), "\n")
cat("Consultants sans prestataire identifié :",
    sum(df_sante$a_consulte == 1, na.rm = TRUE) - nrow(df_t15), "\n")

freq_t15 <- df_t15 |>
  count(type_presta) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

print(freq_t15)

p_t15 <- ggplot(freq_t15,
                aes(x    = pct,
                    y    = fct_reorder(type_presta, pct),
                    fill = type_presta)) +
  geom_col(alpha = 0.87, width = 0.55) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.15, size = 4, fontface = "bold",
            color = "grey30") +
  scale_fill_manual(values = pal_presta, guide = "none") +
  scale_x_continuous(limits = c(0, 85)) +
  labs(
    title    = "Recours aux soins selon le type de gestionnaire",
    subtitle = paste0("Parmi les membres ayant consulté (4 semaines) | ",
                      "729 sans établissement identifié exclus"),
    x        = "Proportion (%)",
    y        = NULL,
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig03_recours_soins_prestataire.png"),
       plot = p_t15, width = 10, height = 6, dpi = 300)
cat("fig03_recours_soins_prestataire.png exporté\n")

# INTERPRÉTATION :
# Privé 67.1%, Public 30.5%, Autre 2.4%.
# Dominance du secteur privé cohérente avec WHO Nigeria (2015) :
# sous-financement chronique du secteur public nigérian.
# Les chemist/PMV sont classés "Privé" dans s4aq8 — la distinction
# formel/informel n'est pas capturable via cette variable seule.


# =============================================================================
# TÂCHE 16 — Distribution des dépenses de santé
# =============================================================================
# Variable principale : s4aq14 (dépenses médicaments)
# Justification : depense_totale (somme s4aq9 + s4aq14 + s4aq17 avec
# na.rm=FALSE) ne donne que 397 observations car les 3 postes doivent
# être renseignés simultanément — cas très rare. s4aq14 est le poste
# le plus universel (achat de médicaments sans consultation formelle)
# et le mieux renseigné. Il sert de proxy des dépenses de santé.
# depense_totale est conservée pour la tâche 18 (comparaison milieux)
# avec winsorisation au 99e percentile pour robustesse.

df_t16 <- df_sante |>
  filter(!is.na(s4aq14), s4aq14 > 0)

cat("\n=== Tâche 16 — Dépenses médicaments (s4aq14) ===\n")
cat("Membres avec s4aq14 > 0 :", nrow(df_t16), "\n")

deciles_t16 <- quantile(df_t16$s4aq14,
                        probs = seq(0, 1, 0.1), na.rm = TRUE)
cat("\nDéciles s4aq14 (médicaments) :\n")
print(round(deciles_t16))

q3    <- quantile(df_t16$s4aq14, 0.75)
iqr   <- IQR(df_t16$s4aq14)
seuil <- q3 + 3 * iqr
cat("\nSeuil outliers (Q3 + 3*IQR) :", round(seuil), "nairas\n")
cat("Nombre d'outliers            :",
    sum(df_t16$s4aq14 > seuil), "\n")

p_t16a <- ggplot(df_t16, aes(x = log10(s4aq14))) +
  geom_histogram(bins = 40, fill = pal_tp3$depense,
                 color = "white", alpha = 0.85) +
  scale_x_continuous(
    breaks = c(2, 3, 4, 5, 6),
    labels = c("100", "1 000", "10 000", "100 000", "1 000 000")
  ) +
  labs(
    title    = "Distribution des dépenses en médicaments (échelle log)",
    subtitle = paste0("Membres avec s4aq14 > 0 | n = ",
                      format(nrow(df_t16), big.mark = " ")),
    x        = "Dépenses médicaments (nairas, échelle log)",
    y        = "Effectif",
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig04a_depenses_distribution.png"),
       plot = p_t16a, width = 10, height = 7, dpi = 300)
cat("fig04a_depenses_distribution.png exporté\n")

df_t16b <- df_sante |>
  filter(!is.na(s4aq14), s4aq14 > 0, !is.na(type_presta))

p_t16b <- ggplot(df_t16b,
                 aes(x    = type_presta,
                     y    = log10(s4aq14),
                     fill = type_presta)) +
  geom_boxplot(alpha = 0.82, outlier.size = 0.6,
               outlier.color = "grey60", width = 0.5) +
  scale_fill_manual(values = pal_presta, guide = "none") +
  scale_y_continuous(
    breaks = c(2, 3, 4, 5, 6),
    labels = c("100", "1 000", "10 000", "100 000", "1 000 000")
  ) +
  labs(
    title    = "Dépenses médicaments par type de prestataire",
    subtitle = "Échelle log | outliers affichés en gris",
    x        = NULL,
    y        = "Dépenses médicaments (nairas, échelle log)",
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig04b_depenses_prestataire.png"),
       plot = p_t16b, width = 10, height = 7, dpi = 300)
cat("fig04b_depenses_prestataire.png exporté\n")

# INTERPRÉTATION :
# Distribution asymétrique à droite — classique pour les dépenses de santé.
# Médiane et seuil outliers à commenter à l'exécution.
# Comparer les médianes par prestataire : public tend vers dépenses
# plus basses (subventions partielles), privé vers dépenses plus élevées.


# =============================================================================
# TÂCHE 17 — Recours aux soins x quintile de richesse
# =============================================================================
# Population : membres avec a_consulte et quintile renseignés
# Test du chi-deux : H0 = indépendance entre consultation et quintile
# V de Cramér : intensité de l'association (Cohen 1988 :
#   < 0.10 négligeable | 0.10-0.30 faible | > 0.30 modéré à fort)
# Fisher si effectifs attendus < 5

df_t17 <- df_sante |>
  filter(!is.na(a_consulte), !is.na(quintile))

cat("\n=== Tâche 17 — Recours aux soins x Quintile ===\n")
cat("Effectif retenu :", nrow(df_t17), "\n")

tab_t17 <- table(df_t17$quintile, df_t17$a_consulte)
cat("\nTableau de contingence quintile x consultation :\n")
print(tab_t17)
cat("\nProportion par quintile (%) :\n")
print(round(prop.table(tab_t17, margin = 1) * 100, 1))

chi2_exp <- chisq.test(tab_t17)$expected
cat("Effectifs attendus min :", round(min(chi2_exp), 1), "\n")

if (min(chi2_exp) >= 5) {
  chi2_t17 <- chisq.test(tab_t17)
  cat("\nChi-deux :\n"); print(chi2_t17)
} else {
  cat("Effectifs < 5 — test exact de Fisher utilisé\n")
  fisher_t17 <- fisher.test(tab_t17, simulate.p.value = TRUE)
  print(fisher_t17)
}

v_t17 <- rstatix::cramer_v(df_t17$a_consulte,
                           as.factor(df_t17$quintile))
cat("V de Cramér :", round(v_t17, 4), "\n")

df_t17_plot <- df_t17 |>
  mutate(consulte_label = if_else(a_consulte == 1,
                                  "A consulté", "N'a pas consulté")) |>
  count(quintile, consulte_label) |>
  group_by(quintile) |>
  mutate(pct = n / sum(n) * 100)

p_t17 <- ggplot(df_t17_plot,
                aes(x    = factor(quintile),
                    y    = pct / 100,
                    fill = consulte_label)) +
  geom_col(position = "fill", alpha = 0.87) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("A consulté"       = pal_tp3$primaire,
               "N'a pas consulté" = "grey80"),
    name   = NULL
  ) +
  labs(
    title    = "Recours aux soins selon le quintile de consommation",
    subtitle = paste0(
      "Chi-deux : p ",
      ifelse(exists("chi2_t17") && chi2_t17$p.value < 0.001,
             "< 0,001",
             "voir console"),
      "  |  V de Cramér = ", round(v_t17, 3)
    ),
    x        = "Quintile de consommation (1 = plus pauvre)",
    y        = "Proportion",
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig05_recours_quintile.png"),
       plot = p_t17, width = 10, height = 7, dpi = 300)
cat("fig05_recours_quintile.png exporté\n")

# INTERPRÉTATION :
# Gradient socioéconomique net : Q1 12.1% -> Q5 26.1% (écart 14 points).
# Chi-deux p < 0.001, V = 0.12 : association faible mais réelle.
# La richesse favorise le recours aux soins mais n'est pas le seul
# déterminant — maladie, distance géographique et culture jouent aussi.


# =============================================================================
# TÂCHE 18 — Dépenses médianes rural/urbain
# =============================================================================
# Variable : depense_totale winsorisée au 99e percentile
# Justification : les 15 outliers (> 98 450 N) sont réels (hospitalisations
# longues) mais distordent la moyenne et le test de Wilcoxon.
# La winsorisation plafonne sans supprimer, préservant la distribution.
# Test de Wilcoxon : H0 = égalité des distributions de dépenses
# Taille d'effet : r de rang via rstatix::wilcox_effsize

# Winsorisation au 99e percentile
p99 <- quantile(df_sante$depense_totale, 0.99, na.rm = TRUE)
df_sante <- df_sante |>
  mutate(depense_totale_w = if_else(!is.na(depense_totale) &
                                      depense_totale > p99,
                                    p99, depense_totale))

cat("\n99e percentile depense_totale :", round(p99), "nairas\n")
cat("Valeurs winsorisées :",
    sum(df_sante$depense_totale > p99, na.rm = TRUE), "\n")

df_t18 <- df_sante |>
  filter(!is.na(depense_totale_w), depense_totale_w > 0,
         !is.na(milieu))

cat("\n=== Tâche 18 — Dépenses médianes rural/urbain ===\n")

stats_t18 <- df_t18 |>
  group_by(milieu) |>
  summarise(
    n       = n(),
    mediane = round(median(depense_totale_w)),
    moyenne = round(mean(depense_totale_w)),
    q1      = round(quantile(depense_totale_w, 0.25)),
    q3      = round(quantile(depense_totale_w, 0.75)),
    .groups = "drop"
  )

cat("\nStatistiques par milieu (dépense winsorisée) :\n")
print(stats_t18)

wilcox_t18  <- rstatix::wilcox_test(df_t18, depense_totale_w ~ milieu)
effsize_t18 <- rstatix::wilcox_effsize(df_t18, depense_totale_w ~ milieu)

cat("\nTest de Wilcoxon :\n"); print(wilcox_t18)
cat("Taille d'effet (r) :", round(effsize_t18$effsize, 4), "\n")
cat("Magnitude          :", effsize_t18$magnitude, "\n")

p_t18 <- ggplot(df_t18,
                aes(x = milieu, y = log10(depense_totale_w),
                    fill = milieu)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, alpha = 0.9,
               outlier.size = 0.5, outlier.color = "grey60") +
  stat_summary(fun = median, geom = "point",
               size = 3, color = "white", shape = 21,
               fill = "white") +
  scale_fill_manual(values = c(Urbain = pal_tp3$urbain,
                               Rural  = pal_tp3$rural),
                    guide  = "none") +
  scale_y_continuous(
    breaks = c(2, 3, 4, 5, 6),
    labels = c("100", "1 000", "10 000", "100 000", "1 000 000")
  ) +
  labs(
    title    = "Distribution des dépenses de santé par milieu de résidence",
    subtitle = paste0(
      "Dépense winsorisée au 99e pctile | Wilcoxon : p ",
      ifelse(wilcox_t18$p < 0.001, "< 0,001",
             paste0("= ", round(wilcox_t18$p, 3))),
      "  |  r = ", round(effsize_t18$effsize, 3),
      " (", effsize_t18$magnitude, ")"
    ),
    x        = NULL,
    y        = "Dépense totale winsorisée (nairas, échelle log)",
    caption  = source_ghs
  ) +
  theme_tp3()

ggsave(here("outputs", "figures", "fig06_depenses_milieu.png"),
       plot = p_t18, width = 9, height = 7, dpi = 300)
cat("fig06_depenses_milieu.png exporté\n")

# INTERPRÉTATION :
# Résultat précédent : p = 0.936, r = 0.004 sans winsorisation.
# Avec winsorisation, comparer à l'exécution — si toujours non
# significatif, cela confirme l'absence de différence rurale/urbaine
# sur les dépenses, probablement due au recours universel aux chemist/PMV
# à tarifs comparables dans les deux milieux.
# n = 397 (108 urbains, 289 ruraux) — puissance limitée à documenter
# honnêtement dans le rapport.


# =============================================================================
# TÂCHE 19 — Tableau récapitulatif gtsummary
# =============================================================================
# Tableau stratifié par milieu pour les variables clés du TP3.
# Produit avec tbl_summary() — exportable en Word via Quarto.

df_t19 <- df_sante |>
  filter(!is.na(milieu)) |>
  select(milieu, sexe, groupe_age, malade, a_consulte,
         type_presta, nhis_couvert, quintile)

tableau_t19 <- df_t19 |>
  tbl_summary(
    by           = milieu,
    label        = list(
      sexe         ~ "Sexe",
      groupe_age   ~ "Groupe d'âge",
      malade       ~ "Maladie/blessure (4 sem.)",
      a_consulte   ~ "A consulté un praticien",
      type_presta  ~ "Type de prestataire",
      nhis_couvert ~ "Couvert par le NHIS",
      quintile     ~ "Quintile de consommation"
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

print(tableau_t19)

write_excel_csv(
  tableau_t19 |> as_tibble() |>
    mutate(across(everything(), as.character)),
  here("outputs", "tables", "tab01_gtsummary_sante.csv")
)

gt::gtsave(
  tableau_t19 |>
    as_gt() |>
    gt::tab_header(
      title    = "Caractéristiques de santé par milieu de résidence",
      subtitle = "Nigeria GHS Panel W4 (2018-2019)"
    ) |>
    gt::tab_source_note(source_ghs) |>
    gt::tab_options(
      heading.background.color   = "#009688",
      heading.title.font.size    = 16,
      heading.subtitle.font.size = 12,
      column_labels.font.weight  = "bold"
    ),
  filename = here("outputs", "tables", "tab01_gtsummary_sante.html")
)

cat("tab01_gtsummary_sante exporté (CSV + HTML)\n")

# INTERPRÉTATION :
# Le tableau synthétise toutes les disparités rural/urbain simultanement.
# Attendu : toutes les variables significatives (p < 0.05) sauf
# éventuellement le sexe — la distribution H/F ne devrait pas varier
# selon le milieu.

cat("\n======================================================\n")
cat("  Script 02_analyses.R terminé\n")
cat("  Figures  : outputs/figures/ (fig01 à fig06)\n")
cat("  Tableaux : outputs/tables/  (tab01)\n")
cat("======================================================\n")
