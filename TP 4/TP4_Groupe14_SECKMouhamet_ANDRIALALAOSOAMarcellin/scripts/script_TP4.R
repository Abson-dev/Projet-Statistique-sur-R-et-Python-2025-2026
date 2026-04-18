# ==============================================================================
# TP4 : Analyse des parcelles agricoles — Superficie, tenure foncière
#        et utilisation des terres (Vague 4)
# ==============================================================================


# ==============================================================================
# ÉTAPE 0 : INITIALISATION — LIBRAIRIES ET DOSSIERS
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, tidyr, sf, scales, rstatix,
       viridis, ggrepel, patchwork)

#dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
#dir.create("outputs/graphs", recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# ÉTAPE 1 : CHARGEMENT ET EXPLORATION DES DONNÉES
# ==============================================================================

# 1.1. Chargement des fichiers

secta1  <- read_dta("data/raw/secta1_harvestw4.dta")
secta   <- read_dta("data/raw/secta_harvestw4.dta")
geovars <- read_dta("data/raw/nga_plotgeovariables_y4.dta")
sect11b1 <- read_dta("data/raw/sect11b1_plantingw4.dta")


# 1.2. Examen de la structure

cat("\n Structure de secta1_harvestw4 \n")
str(secta1, max.level = 1)
glimpse(secta1)

cat("\n Structure de secta_harvestw4 \n")
str(secta, max.level = 1)

cat("\n Structure de nga_plotgeovariables_y4 \n")
str(geovars, max.level = 1)


# 1.3. Vérification des doublons (clé : hhid + plotid)

doublons_s1 <- secta1 %>%
  group_by(hhid, plotid) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons dans secta1 (hhid, plotid) :", doublons_s1, "\n")

doublons_geo <- geovars %>%
  group_by(hhid, plotid) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons dans geovars (hhid, plotid) :", doublons_geo, "\n")


# 1.4. Cohérence des identifiants

communs_s1_secta <- intersect(unique(secta1$hhid), unique(secta$hhid))
communs_s1_geo   <- intersect(
  paste(secta1$hhid, secta1$plotid),
  paste(geovars$hhid, geovars$plotid)
)
cat("\nMénages communs entre secta1 et secta      :", length(communs_s1_secta), "\n")
cat("Parcelles communes entre secta1 et geovars :", length(communs_s1_geo), "\n")


# 1.5. Inspection des variables clés de superficie

cat("\n sa1q11 (superficie déclarée) \n")
cat("Label :", attr(secta1$sa1q11, "label"), "\n")
summary(secta1$sa1q11)

cat("\n prefilled_gps_area (superficie GPS) \n")
cat("Label :", attr(secta1$prefilled_gps_area, "label"), "\n")
summary(secta1$prefilled_gps_area)

# Les deux variables sont en m² (médiane ~2 000 m² = 0,2 ha)
# -> conversion : / 10 000 pour obtenir des hectares

cat("\n--- sa1q4 (accès à la parcelle) ---\n")
cat("Label :", attr(secta1$sa1q4, "label"), "\n")
attr(secta1$sa1q4, "labels")
table(secta1$sa1q4, useNA = "ifany")

cat("\n--- sa1q7 (raison perte d'accès) ---\n")
cat("Label :", attr(secta1$sa1q7, "label"), "\n")
attr(secta1$sa1q7, "labels")
table(secta1$sa1q7, useNA = "ifany")

cat("\n--- sector (milieu de résidence) ---\n")
attr(secta1$sector, "labels")
table(secta1$sector, useNA = "ifany")


# ==============================================================================
# ÉTAPE 2 : CONSTRUCTION DE LA BASE ANALYTIQUE
# ==============================================================================

# 2.1. Intégration des poids et des variables géographiques
# Left join : secta1 est la table principale (niveau parcelle)
# wt_wave4 propagé au niveau parcelle via hhid
# NA sur wt_wave4 : mécanisme MCAR -> exclus des estimations pondérées

parcelles <- secta1 %>%
  left_join(secta %>% select(hhid, wt_wave4), by = "hhid") %>%
  left_join(geovars %>% select(hhid, plotid,
                               dist_household, srtm_nga, srtmslp_nga),
            by = c("hhid", "plotid")) %>%
  left_join(sect11b1 %>% select(hhid, plotid, s11b1q4),
            by = c("hhid", "plotid"))

# Conversion m² -> hectares ; pente character -> numérique
# s11b1q4 recodée en 4 catégories de tenure
parcelles <- parcelles %>%
  mutate(
    superf_declaree_ha = sa1q11 / 10000,
    superf_gps_ha      = prefilled_gps_area / 10000,
    pente_num          = as.numeric(srtmslp_nga),
    milieu             = ifelse(sector == 1, "Urbain", "Rural"),
    acces_parcelle     = case_when(
      sa1q4 == 1 ~ "A encore acces",
      sa1q4 == 2 ~ "N'a plus acces",
      TRUE       ~ NA_character_
    ),
    tenure = case_when(
      s11b1q4 == 1 ~ "Achat",
      s11b1q4 == 2 ~ "Location",
      s11b1q4 == 3 ~ "Usage gratuit",
      s11b1q4 == 4 ~ "Attribution communautaire",
      s11b1q4 == 5 ~ "Heritage familial",
      s11b1q4 == 6 ~ "Metayage",
      s11b1q4 == 7 ~ "Echange temporaire",
      TRUE         ~ NA_character_
    )
  )


na_poids <- sum(is.na(parcelles$wt_wave4))
cat("\nNA sur wt_wave4 :", na_poids,
    "(", round(na_poids / nrow(parcelles) * 100, 2), "%)\n")


# 2.2. Conversion des superficies en hectares et recodages

parcelles <- parcelles %>%
  mutate(
    superf_declaree_ha = sa1q11 / 10000,
    superf_gps_ha      = prefilled_gps_area / 10000,
    pente_num          = suppressWarnings(as.numeric(srtmslp_nga)),
    milieu             = ifelse(sector == 1, "Urbain", "Rural"),
    acces_parcelle     = case_when(
      sa1q4 == 1 ~ "A encore acces",
      sa1q4 == 2 ~ "N'a plus acces",
      TRUE       ~ NA_character_
    )
  )


# 2.3. Diagnostic qualité : valeurs manquantes et aberrantes

na_superf_dec  <- sum(is.na(parcelles$superf_declaree_ha))
na_superf_gps  <- sum(is.na(parcelles$superf_gps_ha))
aberrants_neg  <- sum(parcelles$superf_declaree_ha < 0,   na.rm = TRUE)
aberrants_sup  <- sum(parcelles$superf_declaree_ha > 500, na.rm = TRUE)

cat("\nNA superficie declaree :", na_superf_dec,
    "(", round(na_superf_dec / nrow(parcelles) * 100, 1), "%)\n")
cat("NA superficie GPS      :", na_superf_gps,
    "(", round(na_superf_gps  / nrow(parcelles) * 100, 1), "%)\n")
cat("Superficies < 0 ha     :", aberrants_neg, "\n")
cat("Superficies > 500 ha   :", aberrants_sup, "\n")

qa_table <- data.frame(
  Variable  = c("Superficie declaree (ha)", "Superficie GPS (ha)", "Poids wt_wave4"),
  NA_count  = c(na_superf_dec, na_superf_gps, na_poids),
  NA_pct    = c(round(na_superf_dec / nrow(parcelles) * 100, 1),
                round(na_superf_gps  / nrow(parcelles) * 100, 1),
                round(na_poids       / nrow(parcelles) * 100, 1)),
  Aberrants = c(aberrants_neg + aberrants_sup, NA, NA),
  Seuil     = c("< 0 ou > 500 ha", "--", "--")
)
write.csv(qa_table, "outputs/tables/qualite_donnees_tp4.csv", row.names = FALSE)


# 2.4. Base parcelles valides

parcelles_val <- parcelles %>%
  filter(
    !is.na(superf_declaree_ha),
    superf_declaree_ha >= 0,
    superf_declaree_ha <= 500
  )

cat("\nParcelles valides :", nrow(parcelles_val), "/", nrow(parcelles), "\n")


# 2.5. Superficie totale par ménage (agrégation)

superf_menage <- parcelles_val %>%
  group_by(hhid) %>%
  summarise(
    nb_parcelles     = n(),
    superf_totale_ha = sum(superf_declaree_ha, na.rm = TRUE),
    superf_moy_ha    = mean(superf_declaree_ha, na.rm = TRUE),
    wt_wave4         = first(wt_wave4),
    milieu           = first(milieu),
    state            = first(state),
    .groups          = "drop"
  )

cat("Menages agriculteurs (>= 1 parcelle valide) :", nrow(superf_menage), "\n")
write.csv(superf_menage, "outputs/tables/superficie_par_menage.csv",
          row.names = FALSE)


# ==============================================================================
# ÉTAPE 3 : ANALYSE UNIVARIÉE DE LA SUPERFICIE
# ==============================================================================

# 3.1. Statistiques descriptives par décile

deciles_parcelle <- quantile(parcelles_val$superf_declaree_ha,
                             probs = seq(0, 1, 0.1), na.rm = TRUE)

stats_parcelle <- parcelles_val %>%
  summarise(
    n      = n(),
    mean   = mean(superf_declaree_ha, na.rm = TRUE),
    sd     = sd(superf_declaree_ha, na.rm = TRUE),
    median = median(superf_declaree_ha, na.rm = TRUE),
    Q1     = quantile(superf_declaree_ha, 0.25, na.rm = TRUE),
    Q3     = quantile(superf_declaree_ha, 0.75, na.rm = TRUE),
    min    = min(superf_declaree_ha, na.rm = TRUE),
    max    = max(superf_declaree_ha, na.rm = TRUE)
  )

cat("\nStatistiques superficie declaree par parcelle (ha) :\n")
print(stats_parcelle)
cat("\nDeciles :\n")
print(deciles_parcelle)

write.csv(stats_parcelle,
          "outputs/tables/stats_superf_parcelle.csv", row.names = FALSE)
write.csv(data.frame(decile = names(deciles_parcelle),
                     valeur_ha = round(deciles_parcelle, 4)),
          "outputs/tables/deciles_superf_parcelle.csv", row.names = FALSE)

# Statistiques superficie par ménage

stats_menage <- superf_menage %>%
  summarise(
    n      = n(),
    mean   = mean(superf_totale_ha, na.rm = TRUE),
    sd     = sd(superf_totale_ha, na.rm = TRUE),
    median = median(superf_totale_ha, na.rm = TRUE),
    Q1     = quantile(superf_totale_ha, 0.25, na.rm = TRUE),
    Q3     = quantile(superf_totale_ha, 0.75, na.rm = TRUE),
    min    = min(superf_totale_ha, na.rm = TRUE),
    max    = max(superf_totale_ha, na.rm = TRUE)
  )

cat("\nStatistiques superficie totale par menage (ha) :\n")
print(stats_menage)
write.csv(stats_menage,
          "outputs/tables/stats_superf_menage.csv", row.names = FALSE)


# 3.2. Graphique : Histogramme par parcelle (echelle log)

p_hist_parcelle <- ggplot(
  parcelles_val %>% filter(superf_declaree_ha > 0),
  aes(x = superf_declaree_ha)
) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "Superficie declaree par parcelle (ha, echelle log)",
    y = "Effectif"
  ) +
  theme_minimal()

ggsave("outputs/graphs/hist_superf_parcelle_log.png", p_hist_parcelle,
       width = 6, height = 4, dpi = 300)
print(p_hist_parcelle)


# 3.3. Graphique : Histogramme par menage (echelle log)

p_hist_menage <- ggplot(
  superf_menage %>% filter(superf_totale_ha > 0),
  aes(x = superf_totale_ha)
) +
  geom_histogram(bins = 40, fill = "#76B7B2", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "Superficie totale par menage (ha, echelle log)",
    y = "Effectif"
  ) +
  theme_minimal()

ggsave("outputs/graphs/hist_superf_menage_log.png", p_hist_menage,
       width = 6, height = 4, dpi = 300)
print(p_hist_menage)


# 3.4. Graphique : Boxplot superficie par parcelle

p_boxplot_parcelle <- ggplot(
  parcelles_val %>% filter(superf_declaree_ha > 0),
  aes(y = superf_declaree_ha)
) +
  geom_boxplot(fill = "steelblue", width = 0.4, outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) +
  labs(x = NULL, y = "Superficie declaree par parcelle (ha, echelle log)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggsave("outputs/graphs/boxplot_superf_parcelle.png", p_boxplot_parcelle,
       width = 4, height = 5, dpi = 300)
print(p_boxplot_parcelle)


# ==============================================================================
# ÉTAPE 4 : COMPARAISON SUPERFICIE DÉCLARÉE VS SUPERFICIE GPS
# ==============================================================================

# 4.1. Base commune (deux mesures disponibles)

parcelles_comp <- parcelles_val %>%
  filter(
    !is.na(superf_gps_ha),
    superf_gps_ha      > 0,
    superf_declaree_ha > 0
  )

cat("\nParcelles avec les deux mesures :", nrow(parcelles_comp), "\n")


# 4.2. Corrélation de Spearman

spearman_gps <- cor.test(
  parcelles_comp$superf_declaree_ha,
  parcelles_comp$superf_gps_ha,
  method = "spearman"
)
cat("\nCorrelation de Spearman (declaree vs GPS) :\n")
print(spearman_gps)
capture.output(print(spearman_gps),
               file = "outputs/tables/spearman_declaree_gps.txt")

# Biais de declaration
parcelles_comp <- parcelles_comp %>%
  mutate(ratio_dec_gps = superf_declaree_ha / superf_gps_ha)

cat("\nRapport declaree/GPS :\n")
print(summary(parcelles_comp$ratio_dec_gps))
cat("% de parcelles sur-declarees (ratio > 1) :",
    round(mean(parcelles_comp$ratio_dec_gps > 1, na.rm = TRUE) * 100, 1), "%\n")


# 4.3. Scatter plot declaree vs GPS + ligne 45°

p_scatter_gps <- ggplot(
  parcelles_comp,
  aes(x = superf_gps_ha, y = superf_declaree_ha)
) +
  geom_point(alpha = 0.3, size = 1.2, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0,
              color = "#E15759", linewidth = 0.8, linetype = "dashed") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Superficie GPS (ha, echelle log)",
    y = "Superficie declaree (ha, echelle log)"
  ) +
  annotate("text", x = Inf, y = -Inf,
           label = paste0("Spearman r = ",
                          round(spearman_gps$estimate, 3),
                          "\np < 2.2e-16"),
           hjust = 1.1, vjust = -0.5, size = 3.5) +
  theme_minimal()

ggsave("outputs/graphs/scatter_declaree_vs_gps.png", p_scatter_gps,
       width = 6, height = 5, dpi = 300)
print(p_scatter_gps)


# ==============================================================================
# ÉTAPE 5 : ANALYSE DU RÉGIME D'ACCÈS AUX PARCELLES (PROXY TENURE)
# ==============================================================================

# Note : La variable de tenure foncière complète (sa1q12 : mode d'acquisition —
# propriete, heritage, location, metayage, pret) se trouve dans
# secta1_plantingw4, non disponible dans le package public de la vague 4
# sur microdata.worldbank.org. On utilise sa1q4 (acces a la parcelle : Oui/Non)
# comme proxy du statut d'acces foncier, completee par sa1q7 (raison de la
# perte d'acces) pour les parcelles dont l'acces a ete perdu.

# 5.1. Tableau des fréquences d'accès

acces_table <- parcelles %>%
  filter(!is.na(acces_parcelle)) %>%
  count(acces_parcelle, name = "effectif") %>%
  mutate(
    prop       = effectif / sum(effectif),
    prop_label = scales::percent(prop, accuracy = 0.1)
  ) %>%
  arrange(desc(effectif))

print(acces_table)
write.csv(acces_table, "outputs/tables/acces_parcelles.csv", row.names = FALSE)


# 5.2. Raisons de la perte d'accès

raisons_perte <- parcelles %>%
  filter(sa1q4 == 2, !is.na(sa1q7)) %>%
  mutate(
    raison_lib = case_when(
      sa1q7 == 1  ~ "Obtenir meilleure terre",
      sa1q7 == 2  ~ "Obtenir terre plus proche",
      sa1q7 == 3  ~ "Non rentable",
      sa1q7 == 4  ~ "Ne peut plus cultiver",
      sa1q7 == 5  ~ "Investir dans affaires",
      sa1q7 == 6  ~ "Besoin argent (education)",
      sa1q7 == 7  ~ "Besoin argent (autre)",
      sa1q7 == 8  ~ "Remboursement dette",
      sa1q7 == 10 ~ "Autre",
      TRUE        ~ paste0("Code ", sa1q7)
    )
  ) %>%
  count(raison_lib, name = "effectif") %>%
  mutate(
    prop_label = scales::percent(effectif / sum(effectif), accuracy = 0.1)
  ) %>%
  arrange(desc(effectif))

print(raisons_perte)
write.csv(raisons_perte, "outputs/tables/raisons_perte_acces.csv",
          row.names = FALSE)


# 5.3. Graphique : Barplot accès aux parcelles

p_acces <- ggplot(acces_table,
                  aes(x = reorder(acces_parcelle, effectif), y = effectif)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = paste0(effectif, " (", prop_label, ")")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = NULL, y = "Effectif") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)))

ggsave("outputs/graphs/acces_parcelles.png", p_acces,
       width = 6, height = 3, dpi = 300)
print(p_acces)


# 5.4. Test du Chi-deux : accès × milieu (rural/urbain)

tab_acces_milieu <- parcelles %>%
  filter(!is.na(acces_parcelle), !is.na(milieu)) %>%
  { table(.$milieu, .$acces_parcelle) }

cat("\nTableau de contingence acces x milieu :\n")
print(addmargins(tab_acces_milieu))

eff_th <- chisq.test(tab_acces_milieu)$expected
cat("Effectif theorique minimal :", round(min(eff_th), 1), "\n")

chi_acces <- chisq.test(tab_acces_milieu)
print(chi_acces)
capture.output(print(chi_acces),
               file = "outputs/tables/chi2_acces_milieu.txt")

cramer_acces <- cramer_v(tab_acces_milieu)
cat("V de Cramer :", round(cramer_acces, 3), "\n")
write(paste("V de Cramer :", round(cramer_acces, 3)),
      file = "outputs/tables/cramer_acces_milieu.txt")




# ==============================================================================
# ÉTAPE 5bis : ANALYSE DE LA TENURE FONCIÈRE (s11b1q4)
# ==============================================================================

# 5b.1. Tableau des fréquences de tenure (toutes parcelles avec s11b1q4 renseigné)

tenure_table <- parcelles %>%
  filter(!is.na(tenure)) %>%
  count(tenure, name = "effectif") %>%
  mutate(
    prop       = effectif / sum(effectif),
    prop_label = scales::percent(prop, accuracy = 0.1)
  ) %>%
  arrange(desc(effectif))

print(tenure_table)
write.csv(tenure_table, "outputs/tables/tenure_fonciere.csv", row.names = FALSE)


# 5b.2. Graphique : Barplot horizontal tenure

p_tenure <- ggplot(tenure_table,
                   aes(x = reorder(tenure, effectif), y = effectif)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = paste0(effectif, " (", prop_label, ")")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = NULL, y = "Effectif") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)))

ggsave("outputs/graphs/tenure_fonciere.png", p_tenure,
       width = 7, height = 4, dpi = 300)
print(p_tenure)


# 5b.3. Tableau croisé tenure × milieu (effectifs bruts)

tab_tenure_milieu <- parcelles %>%
  filter(!is.na(tenure), !is.na(milieu)) %>%
  count(milieu, tenure) %>%
  pivot_wider(names_from = milieu, values_from = n, values_fill = 0)

print(tab_tenure_milieu)
write.csv(tab_tenure_milieu,
          "outputs/tables/tenure_par_milieu.csv", row.names = FALSE)


# 5b.4. Test du Chi-deux tenure × milieu

tab_mat_tenure <- parcelles %>%
  filter(!is.na(tenure), !is.na(milieu)) %>%
  { table(.$milieu, .$tenure) }

chi_tenure <- chisq.test(tab_mat_tenure)
print(chi_tenure)
capture.output(print(chi_tenure),
               file = "outputs/tables/chi2_tenure_milieu.txt")

cramer_tenure <- cramer_v(tab_mat_tenure)
cat("V de Cramer :", round(cramer_tenure, 3), "\n")
write(paste("V de Cramer :", round(cramer_tenure, 3)),
      file = "outputs/tables/cramer_tenure_milieu.txt")



# ==============================================================================
# ÉTAPE 6 : RELATION SUPERFICIE TOTALE × NOMBRE DE PARCELLES
# ==============================================================================

# 6.1. Corrélation de Spearman

spearman_nb <- cor.test(
  superf_menage$nb_parcelles,
  superf_menage$superf_totale_ha,
  method = "spearman"
)
cat("\nCorrelation de Spearman (superficie totale ~ nb parcelles) :\n")
print(spearman_nb)
capture.output(print(spearman_nb),
               file = "outputs/tables/spearman_superf_nb_parcelles.txt")


# 6.2. Scatter plot + courbe loess

p_scatter_nb <- ggplot(
  superf_menage,
  aes(x = nb_parcelles, y = superf_totale_ha)
) +
  geom_point(alpha = 0.3, size = 1.2, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE,
              color = "#E15759", fill = "#E15759", alpha = 0.15) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Nombre de parcelles par menage",
    y = "Superficie totale du menage (ha, echelle log)"
  ) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Spearman r = ",
                          round(spearman_nb$estimate, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  theme_minimal()

ggsave("outputs/graphs/scatter_superf_nb_parcelles.png", p_scatter_nb,
       width = 6, height = 5, dpi = 300)
print(p_scatter_nb)




# ==============================================================================
# ÉTAPE 7 : CARTE DE LA SUPERFICIE MÉDIANE PAR ÉTAT (VAGUE 4)
# ==============================================================================

# 7.1. Chargement du shapefile

shapefile_path <- "data/raw/gadm41_NGA_1.shp"
if (!file.exists(shapefile_path)) {
  stop("Shapefile non trouvé. Vérifiez le chemin.")
}
nigeria_states <- st_read(shapefile_path)


# 7.2. Correspondance code -> nom d'État

state_labels      <- attr(secta1$state, "labels")
state_codes       <- as.numeric(state_labels)
state_names_raw   <- names(state_labels)
state_names_clean <- gsub("^[0-9]+\\.\\s*", "", state_names_raw)
code_to_name      <- data.frame(
  state_code = state_codes,
  state_name = state_names_clean,
  stringsAsFactors = FALSE
)


# 7.3. Superficie médiane par État (données brutes — non pondérées)
# IMPORTANT : Le plan de sondage du GHS-Panel n'est pas conçu pour produire
# des estimations représentatives au niveau des États (représentatif au niveau
# national et par zone géopolitique uniquement). Les superficianes médianes
# ci-dessous sont calculées sur données brutes et fournies à titre indicatif.

superf_etat <- parcelles_val %>%
  group_by(state_code = state) %>%
  summarise(
    superf_mediane_ha = median(superf_declaree_ha, na.rm = TRUE),
    n_parcelles       = n(),
    .groups           = "drop"
  ) %>%
  left_join(code_to_name, by = "state_code") %>%
  mutate(
    state_name = case_when(
      state_name == "Federal Capital Territory" ~ "FCT",
      state_name == "Akwa Ibom"                ~ "Akwa Ibom",
      TRUE                                     ~ state_name
    )
  )

print(superf_etat)
write.csv(superf_etat, "outputs/tables/superf_mediane_par_etat.csv",
          row.names = FALSE)


# 7.4. Jointure avec le shapefile

map_data <- nigeria_states %>%
  left_join(superf_etat, by = c("NAME_1" = "state_name"))


# 7.5. Calcul des centroïdes pour les étiquettes

centroids <- st_centroid(map_data)


# 7.6. Carte choroplèthe

p_carte <- ggplot(data = map_data) +
  geom_sf(aes(fill = superf_mediane_ha), color = "white", size = 0.2) +
  geom_sf_text(data = centroids, aes(label = NAME_1),
               size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option   = "plasma",
                       na.value = "grey50",
                       name     = "Superficie\nmediane (ha)") +
  theme_minimal() +
  theme(
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

print(p_carte)
ggsave("outputs/graphs/carte_superf_mediane_par_etat.png", p_carte,
       width = 7, height = 4, dpi = 300)

