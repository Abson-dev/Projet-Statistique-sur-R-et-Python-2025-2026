# ============================================================
# TP3 — Accès aux services de santé et chocs sanitaires
# Nigeria GHS Panel Wave 4 (2018)
# ISE 1 — ENSAE Pierre NDIAYE | Projet Statistique R 2025-2026
# VERSION AVEC PONDÉRATIONS (wt_wave4)
# ============================================================
# Structure du projet :
#   projet/
#   ├── scripts/   ← ce fichier .R
#   ├── outputs/    ← graphiques et tableaux
#   └── data/  ← fichiers .dta
#
# Fichiers nécessaires :
#   sect4a_harvestw4.dta      (santé)
#   sect1_harvestw4.dta       (démographie)
#   secta_harvestw4.dta       (géographie + poids de sondage wt_wave4)
#   cons_agg_wave3_visit2.dta (proxy niveau de vie)
# ============================================================

# ── 0. Chemins ────────────────────────────────────────────────
path_raw <- "../data/"
path_out <- "../outputs/"
if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)

# ── 1. Packages ───────────────────────────────────────────────
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(rstatix)
library(gtsummary)
library(naniar)
library(patchwork)
library(gt)
library(survey)      # ← NOUVEAU : estimations pondérées

theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = "grey40"),
          axis.title    = element_text(size = 11),
          legend.position = "bottom")
)
palette_zone <- c("Rural" = "#2E75B6", "Urban" = "#ED7D31")

# ── 2. Import ─────────────────────────────────────────────────
# Les variables sont stockées en numérique dans ces fichiers.
# NE PAS utiliser as_factor() — on compare directement aux codes.
sante <- read_dta(paste0(path_raw, "sect4a_harvestw4.dta"))
demo  <- read_dta(paste0(path_raw, "sect1_harvestw4.dta"))
geo   <- read_dta(paste0(path_raw, "secta_harvestw4.dta"))   # ← contient wt_wave4

# Wave 4 sans cons_agg : on utilise Wave 3 (approche standard LSMS-ISA)
conso <- read_dta(paste0(path_raw, "cons_agg_wave3_visit2.dta"))

# Diagnostic appariement hhid W3 <-> W4
n_commun <- length(intersect(unique(sante$hhid), unique(conso$hhid)))
cat(sprintf("Menages W4: %d | W3: %d | Commun: %d (%.1f%%)\n",
    length(unique(sante$hhid)), length(unique(conso$hhid)),
    n_commun, n_commun / length(unique(sante$hhid)) * 100))

# ── 3. Exploration initiale ───────────────────────────────────
glimpse(sante)

# Vérification de la variable de pondération dans secta
cat("\nVariables de pondération disponibles dans secta :\n")
grep("wt|weight|pond", names(geo), value = TRUE, ignore.case = TRUE) |> print()
cat("\nRésumé du poids wt_wave4 :\n")
summary(geo$wt_wave4)
cat("Nb observations avec poids non manquants :", sum(!is.na(geo$wt_wave4)), "\n")

p_miss <- vis_miss(
  sante |> select(hhid, indiv, s4aq3, s4aq1, s4aq9, s4aq6a),
  warn_large_data = FALSE)
ggsave(paste0(path_out, "00_valeurs_manquantes.png"),
       plot = p_miss, width = 10, height = 5, dpi = 300)
message("OK : 00_valeurs_manquantes.png")

# ── 4. Construction du dataframe analytique ───────────────────
# CODAGES RÉELS vérifiés dans les données :
#   s1q2   : 1 = Male,  2 = Female
#   s4aq1  : 1 = YES (a consulté),  2 = NO
#   s4aq3  : 1 = YES (malade),      2 = NO
#   sector : 1 = Urban,             2 = Rural
#   s4aq6a : 0 = No one, 2 = Doctor, 8 = Chemist, etc. (numérique)
#   s4aq3b_1 : 1 = Malaria, 27 = Body pains, etc. (numérique)

# Dictionnaire des types de maladies
labels_maladie <- c(
  "1"  = "Malaria",        "2"  = "TB",
  "3"  = "Fievre jaune",   "4"  = "Typhoide",
  "5"  = "Cholera",        "6"  = "Diarrhee",
  "7"  = "Meningite",      "8"  = "Varicelle",
  "9"  = "Pneumonie",      "10" = "Rhume",
  "11" = "Blessure",       "12" = "Autre",
  "13" = "Hypertension",   "14" = "Grippe",
  "15" = "Catarrhe",       "16" = "Toux",
  "17" = "Cephalee",       "18" = "Diabete",
  "19" = "Ver de Guinee",  "20" = "Dysenterie",
  "21" = "Gale",           "22" = "Teigne",
  "23" = "Hepatite B",     "24" = "Ulcere/douleur estomac",
  "25" = "Pb oculaire",    "26" = "Pb dentaire",
  "27" = "Douleurs corporelles"
)

# Dictionnaire des prestataires
labels_presta <- c(
  "0"  = "Aucun",          "1"  = "Guerisseur traditionnel",
  "2"  = "Medecin",        "3"  = "Dentiste",
  "4"  = "Infirmier(e)",   "5"  = "Assistant medical",
  "6"  = "Sage-femme",     "7"  = "Pharmacien",
  "8"  = "Chimiste",       "9"  = "Accoucheuse trad.",
  "10" = "Spiritualiste",  "11" = "Vendeur med. brevetes",
  "13" = "Autre",          "14" = "JCHEW",
  "15" = "CHEW"
)

# ── Extraction des poids depuis secta (niveau ménage) ─────────
# secta contient un enregistrement par ménage : on extrait hhid + wt_wave4
poids <- geo |>
  select(hhid, wt_wave4) |>
  distinct(hhid, .keep_all = TRUE)

demo_sel <- demo |>
  select(hhid, indiv, s1q2, s1q4) |>
  mutate(
    sexe = case_when(
      s1q2 == 1 ~ "Homme",
      s1q2 == 2 ~ "Femme",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Homme", "Femme")),
    age = as.numeric(s1q4),
    groupe_age = case_when(
      age <  15             ~ "< 15 ans",
      age >= 15 & age <= 29 ~ "15-29 ans",
      age >= 30 & age <= 44 ~ "30-44 ans",
      age >= 45 & age <= 59 ~ "45-59 ans",
      age >= 60             ~ "60 ans +",
      TRUE ~ NA_character_
    ) |> factor(levels = c("< 15 ans","15-29 ans","30-44 ans","45-59 ans","60 ans +"))
  ) |>
  select(hhid, indiv, sexe, age, groupe_age)

df <- sante |>
  left_join(demo_sel, by = c("hhid", "indiv")) |>
  left_join(poids, by = "hhid") |>          # ← AJOUT DES POIDS
  left_join(
    conso |>
      select(hhid, welfare = totcons) |>
      mutate(quintile = ntile(welfare, 5) |> factor(
        labels = c("Q1 (plus pauvre)","Q2","Q3","Q4","Q5 (plus riche)"))),
    by = "hhid"
  ) |>
  mutate(
    malade = case_when(
      s4aq3 == 1 ~ 1L,
      s4aq3 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    consulte = case_when(
      s4aq1 == 1 ~ 1L,
      s4aq1 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    zone = case_when(
      sector == 1 ~ "Urban",
      sector == 2 ~ "Rural",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Urban", "Rural")),
    type_maladie1 = labels_maladie[as.character(s4aq3b_1)],
    type_maladie2 = labels_maladie[as.character(s4aq3b_2)],
    prestataire1  = labels_presta[as.character(s4aq6a)],
    prestataire2  = labels_presta[as.character(s4aq6b)],
    dep_consultation    = as.numeric(s4aq9),
    dep_medicaments     = as.numeric(s4aq14),
    dep_hospitalisation = as.numeric(s4aq17),
    dep_totale_sante    = rowSums(
      cbind(dep_consultation, dep_medicaments, dep_hospitalisation),
      na.rm = TRUE),
    dep_totale_sante = ifelse(dep_totale_sante == 0, NA_real_, dep_totale_sante),
    log_dep = log(dep_totale_sante)
  )

cat("Nb observations     :", nrow(df), "\n")
cat("Nb avec poids valide:", sum(!is.na(df$wt_wave4)), "\n")
cat("Taux morbidite (non pondere)   :", round(mean(df$malade,   na.rm=TRUE)*100, 1), "%\n")
cat("Taux consultation  (non pondere):", round(mean(df$consulte, na.rm=TRUE)*100, 1), "%\n")
cat("Zone - Urban/Rural  :") ; print(table(df$zone, useNA="always"))
cat("Sexe                :") ; print(table(df$sexe, useNA="always"))

# ── 5. Design d'enquête ───────────────────────────────────────
# On filtre les observations avec poids non manquants pour le design
df_pond <- df |> filter(!is.na(wt_wave4))

svy_design <- svydesign(
  ids     = ~hhid,            # unité primaire de sondage = ménage
  weights = ~wt_wave4,        # poids de sondage Wave 4
  data    = df_pond,
  nest    = TRUE
)

cat("\n=== Design d'enquête créé ===\n")
cat("N non pondéré :", nrow(df_pond), "\n")
cat("N pondéré (estimé) :", round(sum(weights(svy_design))), "\n")


# ══════════════════════════════════════════════════════════════
# TACHE 13 — Taux de morbidité pondéré par sexe et groupe d'âge
# ══════════════════════════════════════════════════════════════

# Fonction IC pondéré via design survey
ic_morbidite_pond <- function(design, group_var) {
  formula_group <- as.formula(paste0("~", group_var))
  res <- svyby(~malade, formula_group, design,
               svymean, na.rm = TRUE, vartype = "ci")
  res |>
    rename(taux = malade, ic_bas = ci_l, ic_haut = ci_u) |>
    mutate(groupe = as.character(.data[[group_var]]))
}

# Filtres pour chaque sous-groupe (exclure NA)
design_sexe <- subset(svy_design, !is.na(sexe) & !is.na(malade))
design_age  <- subset(svy_design, !is.na(groupe_age) & !is.na(malade))
design_zone <- subset(svy_design, !is.na(zone) & !is.na(malade))

morb_sexe_pond <- ic_morbidite_pond(design_sexe, "sexe")
morb_age_pond  <- ic_morbidite_pond(design_age,  "groupe_age")
morb_zone_pond <- ic_morbidite_pond(design_zone, "zone")

cat("\nMorbidite PONDEREE par sexe :\n")      ; print(morb_sexe_pond)
cat("\nMorbidite PONDEREE par groupe d'age :\n") ; print(morb_age_pond)
cat("\nMorbidite PONDEREE par zone :\n")      ; print(morb_zone_pond)

# Taux global pondéré
taux_global_pond <- svymean(~malade, subset(svy_design, !is.na(malade)), na.rm=TRUE)
cat("\nTaux morbidite PONDERE global :", round(coef(taux_global_pond)*100, 1), "%\n")
cat("IC 95% :", round(confint(taux_global_pond)*100, 1), "\n")

# Taux consultation pondéré
taux_consult_pond <- svymean(~consulte, subset(svy_design, !is.na(consulte)), na.rm=TRUE)
cat("Taux consultation PONDERE      :", round(coef(taux_consult_pond)*100, 1), "%\n")

# Figures pondérées
morb_sexe_pond$sexe <- factor(morb_sexe_pond$sexe, levels = c("Homme","Femme"))

p1 <- morb_sexe_pond |>
  ggplot(aes(x = sexe, y = taux, fill = sexe)) +
  geom_col(width = 0.5, colour = "white") +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux*100,1),"%")),
            vjust = -0.8, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(morb_sexe_pond$ic_haut) + 0.04)) +
  scale_fill_manual(values = c("Homme"="#2E75B6","Femme"="#C00000")) +
  labs(title="Taux de morbidite PONDERE par sexe",
       subtitle="GHS Panel Wave 4 (2018) — IC 95% (pondéré)",
       x=NULL, y="Part des individus malades/blesses") +
  theme(legend.position = "none")

morb_age_pond$groupe_age <- factor(morb_age_pond$groupe_age,
  levels = c("< 15 ans","15-29 ans","30-44 ans","45-59 ans","60 ans +"))

p2 <- morb_age_pond |>
  ggplot(aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_col(width = 0.6, colour = "white") +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux*100,1),"%")),
            vjust = -0.8, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(morb_age_pond$ic_haut) + 0.04)) +
  scale_fill_brewer(palette = "Blues") +
  labs(title="Taux de morbidite PONDERE par groupe d'age", x=NULL,
       y="Part des individus malades/blesses") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=20, hjust=1))

fig1 <- p1 + p2 + plot_annotation(
  title = "Figure 1. Morbidite declaree — estimations PONDEREES (wt_wave4)",
  theme = theme(plot.title = element_text(face="bold", size=14))
)
print(fig1)
ggsave(paste0(path_out,"fig1_morbidite_sexe_age.png"),
       plot=fig1, width=12, height=6, dpi=300)
message("OK : fig1_morbidite_sexe_age.png (pondéré)")


# ══════════════════════════════════════════════════════════════
# TACHE 14 — Types de maladies déclarées (pondéré)
# ══════════════════════════════════════════════════════════════

# Pour les fréquences de maladies, on utilise les poids pour estimer
# les effectifs "représentatifs" nationaux
maladies_long_pond <- df_pond |>
  filter(malade == 1) |>
  select(hhid, indiv, wt_wave4, type_maladie1, type_maladie2) |>
  pivot_longer(cols = c(type_maladie1, type_maladie2),
               names_to = "rang", values_to = "type_maladie") |>
  filter(!is.na(type_maladie))

# Somme des poids par type de maladie (effectif pondéré)
top10_pond <- maladies_long_pond |>
  group_by(type_maladie) |>
  summarise(n_pond = sum(wt_wave4), n_brut = n(), .groups = "drop") |>
  arrange(desc(n_pond)) |>
  slice_head(n = 10) |>
  mutate(
    type_maladie = fct_reorder(type_maladie, n_pond),
    categorie = case_when(
      type_maladie %in% c("Malaria","TB","Typhoide","Cholera","Diarrhee",
                          "Pneumonie","Fievre jaune","Hepatite B",
                          "Meningite","Varicelle") ~ "Infectieuse",
      type_maladie %in% c("Blessure")             ~ "Traumatique",
      type_maladie %in% c("Hypertension","Diabete","Ulcere/douleur estomac") ~ "Chronique",
      TRUE ~ "Autre/Symptome"
    )
  )

fig2 <- ggplot(top10_pond, aes(x=type_maladie, y=n_pond/1000, fill=categorie)) +
  geom_col(width=0.7) +
  geom_text(aes(label=paste0(round(n_pond/1000,0),"k")), hjust=-0.2, size=3.5) +
  coord_flip() +
  scale_fill_manual(values=c("Infectieuse"="#C00000","Traumatique"="#ED7D31",
                              "Chronique"="#4472C4","Autre/Symptome"="#A9A9A9")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
  labs(title="Figure 2. Top 10 des affections déclarées (effectifs PONDÉRÉS)",
       subtitle="Individus malades/blesses — GHS W4 2018 | Poids : wt_wave4",
       x=NULL, y="Effectif pondéré (milliers)", fill="Categorie")
print(fig2)
ggsave(paste0(path_out,"fig2_top10_affections.png"),
       plot=fig2, width=11, height=6, dpi=300)
message("OK : fig2_top10_affections.png (pondéré)")


# ══════════════════════════════════════════════════════════════
# TACHE 15 — Recours aux soins (pondéré)
# ══════════════════════════════════════════════════════════════

prestataires_pond <- df_pond |>
  filter(!is.na(prestataire1)) |>
  select(hhid, indiv, wt_wave4, prestataire1, prestataire2) |>
  pivot_longer(cols = c(prestataire1, prestataire2),
               names_to = "rang", values_to = "prestataire") |>
  filter(!is.na(prestataire))

freq_presta_pond <- prestataires_pond |>
  group_by(prestataire) |>
  summarise(n_pond = sum(wt_wave4), .groups = "drop") |>
  mutate(prop = n_pond / sum(n_pond),
         prestataire = fct_reorder(prestataire, n_pond))

fig3 <- ggplot(freq_presta_pond, aes(x=prestataire, y=prop)) +
  geom_col(fill="#2E75B6", width=0.65) +
  geom_text(aes(label=paste0(round(prop*100,1),"%")), hjust=-0.2, size=3.5) +
  coord_flip() +
  scale_y_continuous(labels=percent_format(), expand=expansion(mult=c(0,0.15))) +
  labs(title="Figure 3. Recours aux soins par type de prestataire (PONDÉRÉ)",
       subtitle="GHS Panel Wave 4 (2018) | Poids : wt_wave4",
       x=NULL, y="Part des consultations (pondérée)")
print(fig3)
ggsave(paste0(path_out,"fig3_recours_prestataire.png"),
       plot=fig3, width=11, height=7, dpi=300)
message("OK : fig3_recours_prestataire.png (pondéré)")


# ══════════════════════════════════════════════════════════════
# TACHE 16 — Distribution des dépenses de santé (pondéré)
# ══════════════════════════════════════════════════════════════

# Statistiques descriptives pondérées
design_dep <- subset(svy_design, !is.na(dep_totale_sante) & dep_totale_sante > 0)

cat("\nStatistiques des depenses de sante PONDÉRÉES (Naira) :\n")
dep_stat_pond <- svymean(~dep_totale_sante, design_dep, na.rm=TRUE)
dep_quantiles_pond <- svyquantile(~dep_totale_sante, design_dep,
                                  quantiles=c(0.25,0.50,0.75), na.rm=TRUE)
cat("Moyenne pondérée :", round(coef(dep_stat_pond)), "Naira\n")
cat("Médiane pondérée (Q50) :", round(coef(dep_quantiles_pond)[2]), "Naira\n")
cat("Q25 pondéré :", round(coef(dep_quantiles_pond)[1]), "Naira\n")
cat("Q75 pondéré :", round(coef(dep_quantiles_pond)[3]), "Naira\n")

# Pour les graphiques, on utilise le log_dep avec pondération
mediane_pond <- coef(svyquantile(~dep_totale_sante, design_dep, quantiles=0.5))

p_hist <- df_pond |> filter(!is.na(log_dep)) |>
  ggplot(aes(x=log_dep)) +
  geom_histogram(aes(weight=wt_wave4), bins=40,
                 fill="#2E75B6", colour="white", alpha=0.9) +
  geom_vline(xintercept=log(mediane_pond),
             colour="red", linetype="dashed", linewidth=0.8) +
  annotate("text", x=log(mediane_pond)+0.3,
           y=Inf, vjust=1.5, label="Mediane pond.", colour="red", size=3.5) +
  labs(title="Figure 4a. Distribution pondérée des dépenses de santé (log)",
       subtitle="Histogramme pondéré par wt_wave4",
       x="log(dépenses totales en Naira)", y="Effectif pondéré")

p_box <- df_pond |>
  filter(!is.na(dep_consultation), !is.na(prestataire1), dep_consultation > 0) |>
  mutate(presta = fct_lump_n(factor(prestataire1), n=6)) |>
  ggplot(aes(x=fct_reorder(presta, dep_consultation, .fun=median),
             y=dep_consultation+1, fill=presta, weight=wt_wave4)) +
  geom_boxplot(outlier.shape=21, outlier.size=1.5, outlier.alpha=0.4) +
  scale_y_log10(labels=label_number(big.mark=",")) +
  coord_flip() +
  labs(title="Figure 4b. Dépenses de consultation par prestataire (log)",
       x=NULL, y="Dépense consultation (Naira, log)") +
  theme(legend.position="none")

fig4 <- p_hist / p_box
print(fig4)
ggsave(paste0(path_out,"fig4_depenses_sante.png"),
       plot=fig4, width=10, height=10, dpi=300)
message("OK : fig4_depenses_sante.png (pondéré)")


# ══════════════════════════════════════════════════════════════
# TACHE 18 — Dépenses médianes rural vs urbain (pondéré)
# ══════════════════════════════════════════════════════════════

design_dep_zone <- subset(svy_design,
  !is.na(dep_totale_sante) & dep_totale_sante > 0 & !is.na(zone))

cat("\nDepenses de sante PONDÉRÉES par zone :\n")
dep_par_zone <- svyby(~dep_totale_sante, ~zone, design_dep_zone,
                      svymean, na.rm=TRUE, vartype="ci")
print(dep_par_zone)

dep_med_zone <- svyby(~dep_totale_sante, ~zone, design_dep_zone,
                      svyquantile, quantiles=0.5, na.rm=TRUE)
cat("\nMédiane pondérée par zone :\n") ; print(dep_med_zone)

# Test de Rao-Scott (adapté aux données d'enquête) sur variables continues
# On utilise svyranktest (Wilcoxon adapté au sondage complexe)
wilcox_pond <- svyranktest(dep_totale_sante ~ zone, design_dep_zone)
cat("\nTest de Wilcoxon pondéré (svyranktest) :\n") ; print(wilcox_pond)

# Obtention des médianes pondérées pour annoter
med_urban <- coef(svyquantile(~dep_totale_sante,
  subset(design_dep_zone, zone=="Urban"), quantiles=0.5))
med_rural <- coef(svyquantile(~dep_totale_sante,
  subset(design_dep_zone, zone=="Rural"), quantiles=0.5))
cat(sprintf("\nMédiane pondérée Urban: %s N | Rural: %s N\n",
    format(round(med_urban), big.mark=","),
    format(round(med_rural), big.mark=",")))

fig5 <- df_pond |>
  filter(!is.na(dep_totale_sante), !is.na(zone), dep_totale_sante>0) |>
  ggplot(aes(x=zone, y=dep_totale_sante+1, fill=zone)) +
  geom_violin(aes(weight=wt_wave4), alpha=0.6, trim=TRUE, scale="width") +
  geom_boxplot(width=0.12, outlier.shape=NA, colour="grey20", alpha=0.8) +
  stat_summary(fun=median, geom="crossbar", width=0.1,
               colour="white", linewidth=0.8) +
  scale_y_log10(labels=label_number(big.mark=",", suffix=" N")) +
  scale_fill_manual(values=palette_zone) +
  annotate("text", x=1.5,
           y=max(df_pond$dep_totale_sante, na.rm=TRUE)*0.5,
           label=sprintf("Wilcoxon pond. p = %.4f\nMed. Urban: %s N | Rural: %s N",
                         wilcox_pond$p.value,
                         format(round(med_urban), big.mark=","),
                         format(round(med_rural), big.mark=",")),
           size=3.5, colour="grey30") +
  labs(title="Figure 5. Dépenses de santé selon la zone (PONDÉRÉ)",
       subtitle="Violin plot + boxplot — échelle log (Naira) | GHS W4 2018 | wt_wave4",
       x=NULL, y="Dépenses totales de santé (Naira, log)") +
  theme(legend.position="none")
print(fig5)
ggsave(paste0(path_out,"fig5_depenses_rural_urbain.png"),
       plot=fig5, width=8, height=7, dpi=300)
message("OK : fig5_depenses_rural_urbain.png (pondéré)")


# ══════════════════════════════════════════════════════════════
# TABLEAU RÉCAPITULATIF gtsummary (pondéré via survey)
# ══════════════════════════════════════════════════════════════

# gtsummary avec tbl_svysummary() pour respecter le design d'enquête
tbl_pond <- tbl_svysummary(
  data      = subset(svy_design, !is.na(zone)),
  include   = c(sexe, groupe_age, malade, consulte, dep_totale_sante),
  by        = zone,
  label     = list(
    sexe             ~ "Sexe",
    groupe_age       ~ "Groupe d'age",
    malade           ~ "Maladie/blessure (4 sem.)",
    consulte         ~ "A consulte un prestataire",
    dep_totale_sante ~ "Depenses de sante (Naira)"
  ),
  statistic = list(
    all_continuous()  ~ "{median} [{p25}-{p75}]",
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  digits       = list(all_continuous() ~ 0),
  missing_text = "(Manquant)"
) |>
  add_p(test = list(
    all_continuous()  ~ "svyttest",
    all_categorical() ~ "svychisq"
  )) |>
  add_overall() |>
  bold_labels() |>
  modify_header(label = "**Variable**") |>
  modify_caption("**Tableau 1.** Caracteristiques sanitaires par zone — GHS W4 2018 (PONDÉRÉ | wt_wave4)")

tbl_pond |> as_gt() |>
  gt::gtsave(filename=paste0(path_out,"tableau1_recapitulatif.html"))
message("OK : tableau1_recapitulatif.html (pondéré)")
tbl_pond

# ── Récapitulatif ─────────────────────────────────────────────
cat("\nFichiers exportes dans :", normalizePath(path_out), "\n")
cat(paste(list.files(path_out), collapse="\n"), "\n")
cat("\n=== RAPPEL : toutes les estimations utilisent les poids wt_wave4 ===\n")
