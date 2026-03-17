# ============================================================
# TP3 — Accès aux services de santé et chocs sanitaires
# Nigeria GHS Panel Wave 4 (2018)
# ISE 1 — ENSAE Pierre NDIAYE | Projet Statistique R 2025-2026
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
#   secta_harvestw4.dta       (géographie)
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
geo   <- read_dta(paste0(path_raw, "secta_harvestw4.dta"))

# Wave 4 sans cons_agg : on utilise Wave 3 (approche standard LSMS-ISA)
conso <- read_dta(paste0(path_raw, "cons_agg_wave3_visit2.dta"))

# Diagnostic appariement hhid W3 <-> W4
n_commun <- length(intersect(unique(sante$hhid), unique(conso$hhid)))
cat(sprintf("Menages W4: %d | W3: %d | Commun: %d (%.1f%%)\n",
    length(unique(sante$hhid)), length(unique(conso$hhid)),
    n_commun, n_commun / length(unique(sante$hhid)) * 100))

# ── 3. Exploration initiale ───────────────────────────────────
glimpse(sante)

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
  # sector est deja dans sect4a_harvestw4 — pas besoin de jointure avec geo
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
    # Labels lisibles pour les maladies et prestataires
    type_maladie1 = labels_maladie[as.character(s4aq3b_1)],
    type_maladie2 = labels_maladie[as.character(s4aq3b_2)],
    prestataire1  = labels_presta[as.character(s4aq6a)],
    prestataire2  = labels_presta[as.character(s4aq6b)],
    # Dépenses de santé
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
cat("Taux morbidite      :", round(mean(df$malade,   na.rm=TRUE)*100, 1), "%\n")
cat("Taux consultation   :", round(mean(df$consulte, na.rm=TRUE)*100, 1), "%\n")
cat("Zone - Urban/Rural  :") ; print(table(df$zone, useNA="always"))
cat("Sexe                :") ; print(table(df$sexe, useNA="always"))


# ══════════════════════════════════════════════════════════════
# TACHE 13 — Taux de morbidité par sexe et groupe d'âge
# ══════════════════════════════════════════════════════════════

ic_morbidite <- function(data, group_var) {
  data |>
    filter(!is.na(malade), !is.na({{ group_var }})) |>
    group_by({{ group_var }}) |>
    summarise(n = n(), n_malade = sum(malade),
              taux = mean(malade), .groups = "drop") |>
    mutate(
      ic_bas  = mapply(function(x,n) prop.test(x,n)$conf.int[1], n_malade, n),
      ic_haut = mapply(function(x,n) prop.test(x,n)$conf.int[2], n_malade, n)
    )
}

morb_sexe <- ic_morbidite(df, sexe)
morb_age  <- ic_morbidite(df, groupe_age)
morb_zone <- ic_morbidite(df, zone)

cat("\nMorbidite par sexe :\n")      ; print(morb_sexe)
cat("\nMorbidite par groupe d'age :\n") ; print(morb_age)
cat("\nMorbidite par zone :\n")      ; print(morb_zone)

p1 <- morb_sexe |>
  ggplot(aes(x = sexe, y = taux, fill = sexe)) +
  geom_col(width = 0.5, colour = "white") +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux*100,1),"%")),
            vjust = -0.8, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(morb_sexe$ic_haut) + 0.04)) +
  scale_fill_manual(values = c("Homme"="#2E75B6","Femme"="#C00000")) +
  labs(title="Taux de morbidite par sexe",
       subtitle="GHS Panel Wave 4 (2018) — IC 95%",
       x=NULL, y="Part des individus malades/blesses") +
  theme(legend.position = "none")

p2 <- morb_age |>
  ggplot(aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_col(width = 0.6, colour = "white") +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux*100,1),"%")),
            vjust = -0.8, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, max(morb_age$ic_haut) + 0.04)) +
  scale_fill_brewer(palette = "Blues") +
  labs(title="Taux de morbidite par groupe d'age", x=NULL,
       y="Part des individus malades/blesses") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=20, hjust=1))

fig1 <- p1 + p2 + plot_annotation(
  title = "Figure 1. Morbidite declaree dans les 4 semaines precedant l'enquete",
  theme = theme(plot.title = element_text(face="bold", size=14))
)
print(fig1)
ggsave(paste0(path_out,"fig1_morbidite_sexe_age.png"),
       plot=fig1, width=12, height=6, dpi=300)
message("OK : fig1_morbidite_sexe_age.png")


# ══════════════════════════════════════════════════════════════
# TACHE 14 — Types de maladies déclarées
# ══════════════════════════════════════════════════════════════

maladies_long <- df |>
  filter(malade == 1) |>
  select(hhid, indiv, type_maladie1, type_maladie2) |>
  pivot_longer(cols = c(type_maladie1, type_maladie2),
               names_to = "rang", values_to = "type_maladie") |>
  filter(!is.na(type_maladie))

top10 <- maladies_long |>
  count(type_maladie, sort = TRUE) |>
  slice_head(n = 10) |>
  mutate(
    type_maladie = fct_reorder(type_maladie, n),
    categorie = case_when(
      type_maladie %in% c("Malaria","TB","Typhoide","Cholera","Diarrhee",
                          "Pneumonie","Fievre jaune","Hepatite B",
                          "Meningite","Varicelle") ~ "Infectieuse",
      type_maladie %in% c("Blessure")             ~ "Traumatique",
      type_maladie %in% c("Hypertension","Diabete","Ulcere/douleur estomac") ~ "Chronique",
      TRUE ~ "Autre/Symptome"
    )
  )

fig2 <- ggplot(top10, aes(x=type_maladie, y=n, fill=categorie)) +
  geom_col(width=0.7) +
  geom_text(aes(label=n), hjust=-0.2, size=3.5) +
  coord_flip() +
  scale_fill_manual(values=c("Infectieuse"="#C00000","Traumatique"="#ED7D31",
                              "Chronique"="#4472C4","Autre/Symptome"="#A9A9A9")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
  labs(title="Figure 2. Top 10 des affections declarees",
       subtitle="Individus malades/blesses — GHS W4 2018",
       x=NULL, y="Nombre de cas declares", fill="Categorie")
print(fig2)
ggsave(paste0(path_out,"fig2_top10_affections.png"),
       plot=fig2, width=11, height=6, dpi=300)
message("OK : fig2_top10_affections.png")


# ══════════════════════════════════════════════════════════════
# TACHE 15 — Recours aux soins par type de prestataire
# ══════════════════════════════════════════════════════════════

prestataires <- df |>
  filter(!is.na(prestataire1)) |>
  select(hhid, indiv, prestataire1, prestataire2) |>
  pivot_longer(cols = c(prestataire1, prestataire2),
               names_to = "rang", values_to = "prestataire") |>
  filter(!is.na(prestataire))

freq_presta <- prestataires |>
  count(prestataire, sort=TRUE) |>
  mutate(prop = n/sum(n),
         prestataire = fct_reorder(prestataire, n))

fig3 <- ggplot(freq_presta, aes(x=prestataire, y=prop)) +
  geom_col(fill="#2E75B6", width=0.65) +
  geom_text(aes(label=paste0(round(prop*100,1),"%")), hjust=-0.2, size=3.5) +
  coord_flip() +
  scale_y_continuous(labels=percent_format(), expand=expansion(mult=c(0,0.15))) +
  labs(title="Figure 3. Recours aux soins par type de prestataire",
       subtitle="GHS Panel Wave 4 (2018)", x=NULL, y="Part des consultations")
print(fig3)
ggsave(paste0(path_out,"fig3_recours_prestataire.png"),
       plot=fig3, width=11, height=7, dpi=300)
message("OK : fig3_recours_prestataire.png")


# ══════════════════════════════════════════════════════════════
# TACHE 16 — Distribution des dépenses de santé
# ══════════════════════════════════════════════════════════════

cat("\nStatistiques des depenses de sante (Naira) :\n")
df |>
  filter(!is.na(dep_totale_sante)) |>
  summarise(n=n(), Min=min(dep_totale_sante),
            Q1=quantile(dep_totale_sante,0.25),
            Mediane=median(dep_totale_sante),
            Moyenne=mean(dep_totale_sante),
            Q3=quantile(dep_totale_sante,0.75),
            Max=max(dep_totale_sante),
            CV=round(sd(dep_totale_sante)/mean(dep_totale_sante),3)) |> print()

cat("\nDeciles des depenses :\n")
df |> filter(!is.na(dep_totale_sante)) |>
  reframe(decile=paste0("D",1:10),
          valeur=quantile(dep_totale_sante, probs=seq(0.1,1,0.1))) |> print()

p_hist <- df |> filter(!is.na(log_dep)) |>
  ggplot(aes(x=log_dep)) +
  geom_histogram(bins=40, fill="#2E75B6", colour="white", alpha=0.9) +
  geom_vline(xintercept=log(median(df$dep_totale_sante,na.rm=TRUE)),
             colour="red", linetype="dashed", linewidth=0.8) +
  annotate("text", x=log(median(df$dep_totale_sante,na.rm=TRUE))+0.3,
           y=Inf, vjust=1.5, label="Mediane", colour="red", size=3.5) +
  labs(title="Figure 4a. Distribution des depenses de sante (log)",
       x="log(depenses totales en Naira)", y="Effectif")

p_box <- df |>
  filter(!is.na(dep_consultation), !is.na(prestataire1), dep_consultation > 0) |>
  mutate(presta = fct_lump_n(factor(prestataire1), n=6)) |>
  ggplot(aes(x=fct_reorder(presta, dep_consultation, .fun=median),
             y=dep_consultation+1, fill=presta)) +
  geom_boxplot(outlier.shape=21, outlier.size=1.5, outlier.alpha=0.4) +
  scale_y_log10(labels=label_number(big.mark=",")) +
  coord_flip() +
  labs(title="Figure 4b. Depenses de consultation par prestataire (log)",
       x=NULL, y="Depense consultation (Naira, log)") +
  theme(legend.position="none")

fig4 <- p_hist / p_box
print(fig4)
ggsave(paste0(path_out,"fig4_depenses_sante.png"),
       plot=fig4, width=10, height=10, dpi=300)
message("OK : fig4_depenses_sante.png")


# ══════════════════════════════════════════════════════════════
# TACHE 18 — Dépenses médianes rural vs urbain
# ══════════════════════════════════════════════════════════════

cat("\nDepenses de sante par zone :\n")
df |>
  filter(!is.na(dep_totale_sante), !is.na(zone), dep_totale_sante>0) |>
  group_by(zone) |>
  summarise(n=n(), Mediane=median(dep_totale_sante),
            Moyenne=mean(dep_totale_sante),
            Q1=quantile(dep_totale_sante,0.25),
            Q3=quantile(dep_totale_sante,0.75), .groups="drop") |> print()

wilcox_res <- df |>
  filter(!is.na(dep_totale_sante), !is.na(zone), dep_totale_sante>0) |>
  wilcox_test(dep_totale_sante ~ zone)

wilcox_eff <- df |>
  filter(!is.na(dep_totale_sante), !is.na(zone), dep_totale_sante>0) |>
  wilcox_effsize(dep_totale_sante ~ zone)

cat("\nTest de Wilcoxon :\n")       ; print(wilcox_res)
cat("\nTaille d'effet (r) :\n")     ; print(wilcox_eff)

fig5 <- df |>
  filter(!is.na(dep_totale_sante), !is.na(zone), dep_totale_sante>0) |>
  ggplot(aes(x=zone, y=dep_totale_sante+1, fill=zone)) +
  geom_violin(alpha=0.6, trim=TRUE, scale="width") +
  geom_boxplot(width=0.12, outlier.shape=NA, colour="grey20", alpha=0.8) +
  stat_summary(fun=median, geom="crossbar", width=0.1,
               colour="white", linewidth=0.8) +
  scale_y_log10(labels=label_number(big.mark=",", suffix=" N")) +
  scale_fill_manual(values=palette_zone) +
  annotate("text", x=1.5,
           y=max(df$dep_totale_sante,na.rm=TRUE)*0.5,
           label=sprintf("Wilcoxon p = %.4f\nr = %.3f (%s)",
                         wilcox_res$p, wilcox_eff$effsize, wilcox_eff$magnitude),
           size=3.5, colour="grey30") +
  labs(title="Figure 5. Depenses de sante selon la zone de residence",
       subtitle="Violin plot + boxplot — echelle log (Naira) | GHS W4 2018",
       x=NULL, y="Depenses totales de sante (Naira, log)") +
  theme(legend.position="none")
print(fig5)
ggsave(paste0(path_out,"fig5_depenses_rural_urbain.png"),
       plot=fig5, width=8, height=7, dpi=300)
message("OK : fig5_depenses_rural_urbain.png")


# ══════════════════════════════════════════════════════════════
# TABLEAU RÉCAPITULATIF gtsummary
# ══════════════════════════════════════════════════════════════

tbl <- df |>
  filter(!is.na(zone)) |>
  select(zone, sexe, groupe_age, malade, consulte, dep_totale_sante) |>
  mutate(malade   = factor(malade,   labels=c("Non","Oui")),
         consulte = factor(consulte, labels=c("Non","Oui"))) |>
  tbl_summary(
    by        = zone,
    label     = list(sexe~"Sexe", groupe_age~"Groupe d'age",
                     malade~"Maladie/blessure (4 sem.)",
                     consulte~"A consulte un prestataire",
                     dep_totale_sante~"Depenses de sante (Naira)"),
    statistic = list(all_continuous()~"{median} [{p25}-{p75}]",
                     all_categorical()~"{n} ({p}%)"),
    digits       = list(all_continuous()~0),
    missing_text = "(Manquant)"
  ) |>
  add_p(test=list(all_continuous()~"wilcox.test",
                  all_categorical()~"chisq.test")) |>
  add_overall() |> bold_labels() |>
  modify_header(label="**Variable**") |>
  modify_caption("**Tableau 1.** Caracteristiques sanitaires par zone — GHS W4 2018")

tbl |> as_gt() |>
  gt::gtsave(filename=paste0(path_out,"tableau1_recapitulatif.html"))
message("OK : tableau1_recapitulatif.html")
tbl

# ── Récapitulatif ─────────────────────────────────────────────
cat("\nFichiers exportes dans :", normalizePath(path_out), "\n")
cat(paste(list.files(path_out), collapse="\n"), "\n")
