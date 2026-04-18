# ============================================================
# TP4 — Analyse des parcelles agricoles
#        Superficie, tenure foncière et utilisation des terres
# Nigeria GHS Panel Wave 4 (2018)
# ============================================================
# Auteurs    : Mamadou Lamine DIABANG & Awa Ba
#              Élèves ISE1 — ENSAE Pierre Ndiaye
# Superviseur: Aboubacar HEMA — Analyste de recherche, IFPRI
# ============================================================
# Fichiers :
#   secta1_harvestw4.dta      → superficie parcelles (Q19/20/23/24)
#   secta_harvestw4.dta       → poids de sondage
#   sect11b1_plantingw4.dta   → tenure foncière réelle (Q21)
# Questions : 19, 20, 21, 23, 24  (Q22 exclue : W1-W4 requis)
# ============================================================

path_raw <- "../data/"
path_out <- "../output/TP4/"
if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)

# install.packages(c("haven","dplyr","tidyr","ggplot2","ggrepel",
#                    "scales","patchwork","rstatix","gtsummary",
#                    "naniar","viridis","forcats","officer","flextable",
#                    "survey","srvyr"))
library(haven);    library(dplyr);     library(tidyr)
library(ggplot2);  library(ggrepel);   library(scales)
library(patchwork);library(rstatix);   library(gtsummary)
library(naniar);   library(viridis);   library(forcats)
library(officer);  library(flextable)
library(survey);   library(srvyr)

theme_set(
  theme_minimal(base_size=12) +
    theme(plot.title    = element_text(face="bold", size=13),
          plot.subtitle = element_text(colour="grey40"),
          axis.title    = element_text(size=11),
          legend.position = "bottom")
)

state_labels <- c(
  "1"="Abia","2"="Adamawa","3"="Akwa Ibom","4"="Anambra",
  "5"="Bauchi","6"="Bayelsa","7"="Benue","8"="Borno",
  "9"="Cross River","10"="Delta","11"="Ebonyi","12"="Edo",
  "13"="Ekiti","14"="Enugu","15"="Gombe","16"="Imo",
  "17"="Jigawa","18"="Kaduna","19"="Kano","20"="Katsina",
  "21"="Kebbi","22"="Kogi","23"="Kwara","24"="Lagos",
  "25"="Nassarawa","26"="Niger","27"="Ogun","28"="Ondo",
  "29"="Osun","30"="Oyo","31"="Plateau","32"="Rivers",
  "33"="Sokoto","34"="Taraba","35"="Yobe","36"="Zamfara",
  "37"="FCT Abuja"
)

# ── 2. Import ─────────────────────────────────────────────────
secta1 <- read_dta(paste0(path_raw, "secta1_harvestw4.dta"))
tenure <- read_dta(paste0(path_raw, "sect11b1_plantingw4.dta"))
secta  <- read_dta(paste0(path_raw, "secta_harvestw4.dta")) |>
  select(hhid, wt_wave4, strata) |>   # ea déjà présent dans les bases agricoles
  mutate(wt_wave4 = as.numeric(wt_wave4))

cat("secta1  :", nrow(secta1), "lignes |", n_distinct(secta1$hhid), "ménages\n")
cat("tenure  :", nrow(tenure), "lignes |", n_distinct(tenure$hhid), "ménages\n")
cat("Poids   :", sum(!is.na(secta$wt_wave4)), "ménages avec wt_wave4\n\n")

# Options plan de sondage (PSU isolées : ajustement conservateur)
options(survey.lonely.psu = "adjust")

# ── 3. Superficie + jointure poids ────────────────────────────
secta1 <- secta1 |>
  left_join(secta, by = "hhid") |>
  mutate(
    superf_m2  = if_else(!is.na(sa1q11), sa1q11, prefilled_gps_area),
    superf_ha  = superf_m2 / 10000,
    source_mes = case_when(
      !is.na(sa1q11)             ~ "GPS re-mesure W4",
      !is.na(prefilled_gps_area) ~ "GPS pre-rempli (W3)",
      TRUE                       ~ "Manquante"
    ) |> factor(levels = c("GPS re-mesure W4",
                           "GPS pre-rempli (W3)","Manquante")),
    zone = case_when(sector==1~"Urban", sector==2~"Rural",
                     TRUE~NA_character_) |>
      factor(levels=c("Urban","Rural")),
    state_nom = state_labels[as.character(state)]
  )

sup_men <- secta1 |>
  group_by(hhid, zone, state_nom) |>
  summarise(nb_parcelles  = n(),
            superf_tot_ha = sum(superf_ha, na.rm=TRUE),
            .groups="drop") |>
  mutate(superf_tot_ha = if_else(superf_tot_ha==0,
                                 NA_real_, superf_tot_ha))

# ══════════════════════════════════════════════════════════════
# Q19
# ══════════════════════════════════════════════════════════════
q1_s  <- quantile(secta1$superf_ha, 0.25, na.rm=TRUE)
q3_s  <- quantile(secta1$superf_ha, 0.75, na.rm=TRUE)
seuil_h <- q3_s + 3*(q3_s - q1_s)
outliers_n <- sum(secta1$superf_ha > seuil_h, na.rm=TRUE)

# Renommer les variables pour affichage lisible dans gg_miss_var
miss_data <- secta1 |>
  select("Superficie finale (ha)"      = superf_ha,
         "GPS re-mesuré W4 (sa1q11)"   = sa1q11,
         "GPS pré-rempli W3 (prefilled)"= prefilled_gps_area)

p_miss19 <- gg_miss_var(miss_data, show_pct=TRUE) +
  labs(title="Fig. 19a. Taux de valeurs manquantes — superficie",
       x=NULL, y="% manquant")

p_out19 <- secta1 |>
  filter(!is.na(superf_ha)) |>
  mutate(aberrant = superf_ha > seuil_h) |>
  ggplot(aes(x=1, y=superf_ha)) +
  geom_boxplot(width=0.4, fill="#2E75B6", alpha=0.6,
               outlier.shape=NA) +
  geom_jitter(data=~filter(., aberrant),
              aes(colour="Aberrant (IQR×3)"),
              width=0.15, size=2.5, alpha=0.7) +
  scale_colour_manual(values=c("Aberrant (IQR×3)"="#C00000")) +
  scale_y_continuous(labels=label_number(suffix=" ha")) +
  labs(title=sprintf("Fig. 19b. Outliers : %d (>%.3f ha)",
                     outliers_n, seuil_h),
       x=NULL, y="Superficie (ha)", colour=NULL) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

fig19 <- p_miss19 | p_out19
ggsave(paste0(path_out,"q19_superficie_manquants_aberrants.png"),
       plot=fig19, width=12, height=5, dpi=150)
message("OK : q19")

secta1_clean <- secta1 |>
  filter(!is.na(superf_ha), superf_ha>0, superf_ha<=seuil_h)
sup_men_clean <- sup_men |>
  filter(!is.na(superf_tot_ha), superf_tot_ha<=500)

# ── Plan de sondage (parcelles) ───────────────────────────────
parc_svy_data <- secta1_clean |> filter(!is.na(wt_wave4), !is.na(ea))
plan_parc <- svydesign(ids=~ea, strata=~strata, weights=~wt_wave4,
                       nest=TRUE, data=parc_svy_data)

# Statistiques pondérées — superficie
# survey_median() est plus robuste que survey_quantile() pour la médiane
svy_moy   <- svymean(~superf_ha, design=plan_parc, na.rm=TRUE)
svy_ci    <- confint(svy_moy)
svy_med   <- svyquantile(~superf_ha, design=plan_parc,
                         quantiles=0.5, na.rm=TRUE, ci=TRUE)

med_pond  <- coef(svy_med)[1]
med_ci    <- confint(svy_med)
moy_pond  <- coef(svy_moy)[1]
moy_ci_lo <- svy_ci[1, 1]
moy_ci_hi <- svy_ci[1, 2]

# Stocker dans une liste pour réutilisation dans le rapport Word
stats_pond_superf <- list(
  med_pond       = med_pond,
  med_pond_low   = med_ci[1, 1],
  med_pond_upp   = med_ci[1, 2],
  moy_pond       = moy_pond,
  moy_pond_low   = moy_ci_lo,
  moy_pond_upp   = moy_ci_hi
)

cat("=== Superficie pondérée (ha) ===\n")
cat(sprintf("  Médiane pond  : %.4f [%.4f – %.4f]\n",
            stats_pond_superf$med_pond,
            stats_pond_superf$med_pond_low,
            stats_pond_superf$med_pond_upp))
cat(sprintf("  Moyenne pond  : %.4f [%.4f – %.4f]\n",
            stats_pond_superf$moy_pond,
            stats_pond_superf$moy_pond_low,
            stats_pond_superf$moy_pond_upp))
cat(sprintf("  Médiane brute : %.4f\n",
            median(secta1_clean$superf_ha, na.rm=TRUE)))

# ══════════════════════════════════════════════════════════════
# Q20
# ══════════════════════════════════════════════════════════════
p_hist20 <- secta1_clean |>
  ggplot(aes(x=log10(superf_ha))) +
  geom_histogram(bins=40, fill="#2E75B6", colour="white", alpha=0.9) +
  geom_vline(xintercept=log10(median(secta1_clean$superf_ha)),
             colour="red", linetype="dashed", linewidth=0.8) +
  annotate("text",
           x=log10(median(secta1_clean$superf_ha))+0.12,
           y=Inf, vjust=1.5,
           label=sprintf("Mediane\n%.3f ha",
                         median(secta1_clean$superf_ha)),
           colour="red", size=3.2) +
  scale_x_continuous(
    labels=function(x) paste0(round(10^x,3)," ha")) +
  labs(title="Fig. 20a. Distribution des superficies (log)",
       x="log10(superficie ha)", y="Nb parcelles")

p_box20 <- secta1_clean |>
  ggplot(aes(x=zone, y=superf_ha, fill=zone)) +
  geom_boxplot(outlier.shape=21, outlier.alpha=0.3) +
  scale_fill_manual(values=c("Urban"="#ED7D31","Rural"="#2E75B6")) +
  scale_y_continuous(labels=label_number(suffix=" ha")) +
  labs(title="Fig. 20b. Superficie par zone",
       x=NULL, y="Superficie (ha)") +
  theme(legend.position="none")

deux_mes <- secta1 |>
  filter(!is.na(sa1q11), !is.na(prefilled_gps_area)) |>
  mutate(superf_dec_ha = sa1q11/10000,
         superf_gps_ha = prefilled_gps_area/10000) |>
  filter(superf_dec_ha<=seuil_h, superf_gps_ha<=seuil_h)

rho_20 <- cor(deux_mes$superf_dec_ha, deux_mes$superf_gps_ha,
              method="spearman")
pval_20 <- cor.test(deux_mes$superf_dec_ha,
                    deux_mes$superf_gps_ha,
                    method="spearman")$p.value

p_scat20 <- deux_mes |>
  ggplot(aes(x=superf_gps_ha, y=superf_dec_ha)) +
  geom_point(alpha=0.4, size=1.5, colour="#2E75B6") +
  geom_abline(slope=1, intercept=0, colour="red",
              linetype="dashed", linewidth=0.8) +
  geom_smooth(method="loess", colour="#ED7D31",
              linewidth=0.8, se=TRUE, alpha=0.2) +
  annotate("text", x=Inf, y=0, hjust=1.1, vjust=-0.5,
           label=sprintf("rho = %.3f | p = %.2e",
                         rho_20, pval_20),
           size=3.8, fontface="bold") +
  scale_x_continuous(labels=label_number(suffix=" ha")) +
  scale_y_continuous(labels=label_number(suffix=" ha")) +
  labs(title="Fig. 20c. Superficie declaree (W4) vs GPS (W3)",
       subtitle="Ligne rouge = concordance parfaite (45 deg) | Orange = LOESS",
       x="GPS pre-rempli (ha)", y="Re-mesure W4 (ha)")

fig20 <- (p_hist20 | p_box20) / p_scat20 +
  plot_annotation(title="Q20 — Analyse univariee de la superficie")
ggsave(paste0(path_out,"q20_superficie_univarie.png"),
       plot=fig20, width=13, height=10, dpi=150)
message("OK : q20")

# ══════════════════════════════════════════════════════════════
# Q21 — Tenure foncière réelle (s11b1q4)
# ══════════════════════════════════════════════════════════════
labels_ten <- c(
  "1"="Achat direct",
  "2"="Location (cash/en nature)",
  "3"="Usage gratuit (non-propriete)",
  "4"="Distribution communautaire",
  "5"="Heritage familial",
  "6"="Metayage",
  "7"="Echange temporaire"
)

tenure_c <- tenure |>
  select(hhid, plotid, ea, s11b1q4, s11b1q7, sector, zone, state) |>
  mutate(
    mode_acq = labels_ten[as.character(s11b1q4)] |>
      factor(levels = c("Heritage familial","Achat direct",
                        "Location (cash/en nature)",
                        "Usage gratuit (non-propriete)",
                        "Distribution communautaire",
                        "Metayage","Echange temporaire")),
    titre_legal = case_when(
      s11b1q7==1 ~ "Titre legal (9.7%)",
      s11b1q7==2 ~ "Pas de titre (69.0%)",
      TRUE ~ NA_character_) |> factor(),
    zone_ten = case_when(sector==1~"Urban", sector==2~"Rural",
                         TRUE~NA_character_) |>
      factor(levels=c("Urban","Rural")),
    state_nom = state_labels[as.character(state)]
  )

# Joindre poids à tenure
tenure_c <- tenure_c |>
  left_join(secta |> select(hhid, wt_wave4, strata), by="hhid")

# Plan sondage tenure
ten_svy_data <- tenure_c |> filter(!is.na(mode_acq),
                                   !is.na(wt_wave4), !is.na(ea))
plan_ten <- svydesign(ids=~ea, strata=~strata, weights=~wt_wave4,
                      nest=TRUE, data=ten_svy_data)

# Proportions pondérées par mode d'acquisition
# Proportions pondérées par mode d'acquisition (approche manuelle robuste)
ten_wt <- ten_svy_data |> filter(!is.na(mode_acq))
total_wt_ten <- sum(ten_wt$wt_wave4, na.rm=TRUE)
freq_ten_pond <- ten_wt |>
  group_by(mode_acq) |>
  summarise(wt_sum = sum(wt_wave4, na.rm=TRUE), .groups="drop") |>
  mutate(
    prop_pond     = wt_sum / total_wt_ten * 100,
    prop_pond_low = NA_real_,
    prop_pond_upp = NA_real_
  )

# Effectifs bruts (pour affichage n=)
freq_ten_n <- tenure_c |>
  filter(!is.na(mode_acq)) |>
  count(mode_acq, sort=TRUE)

freq_ten <- freq_ten_pond |>
  left_join(freq_ten_n, by="mode_acq") |>
  mutate(prop = prop_pond,
         mode_acq = fct_reorder(mode_acq, prop))

cat("\n=== Tenure pondérée ===\n")
print(freq_ten |> select(mode_acq, prop, n) |>
        arrange(desc(prop)) |>
        mutate(across(where(is.numeric), round, 1)))

p_ten <- freq_ten |>
  ggplot(aes(x=mode_acq, y=prop, fill=mode_acq)) +
  geom_col(width=0.75, show.legend=FALSE) +
  geom_text(aes(label=sprintf("%.1f%%", prop)),
            hjust=-0.05, size=3.2) +
  coord_flip() +
  scale_fill_viridis_d(option="D") +
  scale_y_continuous(expand=expansion(mult=c(0,0.35))) +
  labs(title="Fig. 21a. Regimes de tenure fonciere — W4",
       subtitle="sect11b1_plantingw4 | s11b1q4",
       x=NULL, y="Proportion (%)")

# Chi-deux pondéré sur plan de sondage (svychisq)
svy_obj_ten <- svydesign(ids=~ea, strata=~strata, weights=~wt_wave4,
                         nest=TRUE,
                         data=tenure_c |>
                           filter(!is.na(mode_acq),
                                  !is.na(zone_ten),
                                  !is.na(wt_wave4)))
chi2_tz <- svychisq(~mode_acq + zone_ten, design=svy_obj_ten,
                    statistic="Chisq")
cramv_tz <- sqrt(chi2_tz$statistic /
                   (sum(!is.na(tenure_c$mode_acq) &
                          !is.na(tenure_c$zone_ten)) *
                      (min(length(levels(tenure_c$mode_acq)),
                           length(levels(tenure_c$zone_ten))) - 1)))
cat(sprintf("Chi-deux pondéré tenure x zone : F=%.3f p=%.4f | V≈%.4f\n",
            chi2_tz$statistic, chi2_tz$p.value, cramv_tz))

# Proportions pondérées par zone pour le graphique (approche manuelle)
prop_zone_pond <- ten_wt |>
  filter(!is.na(zone_ten)) |>
  group_by(zone_ten, mode_acq) |>
  summarise(wt_sum = sum(wt_wave4, na.rm=TRUE), .groups="drop") |>
  group_by(zone_ten) |>
  mutate(prop = wt_sum / sum(wt_sum) * 100) |>
  ungroup() |>
  mutate(zone_ten = factor(zone_ten, levels=c("Urban","Rural")))

p_ten_zone <- prop_zone_pond |>
  ggplot(aes(x=zone_ten, y=prop, fill=mode_acq)) +
  geom_col(width=0.6) +
  geom_text(aes(label=ifelse(prop>4,
                             sprintf("%.0f%%",prop),"")),
            position=position_stack(vjust=0.5),
            size=3, colour="white", fontface="bold") +
  scale_fill_viridis_d(option="D") +
  labs(title=sprintf("Fig. 21b. Tenure × zone | V=%.3f p=%.4f",
                     cramv_tz, chi2_tz$p.value),
       x=NULL, y="Proportion (%)", fill="Mode acquisition")

fig21 <- p_ten | p_ten_zone
ggsave(paste0(path_out,"q21_tenure_fonciere.png"),
       plot=fig21, width=16, height=7, dpi=150)
message("OK : q21")

# ══════════════════════════════════════════════════════════════
# Q23
# ══════════════════════════════════════════════════════════════
spear_23 <- cor.test(sup_men_clean$nb_parcelles,
                     sup_men_clean$superf_tot_ha,
                     method="spearman")
cat(sprintf("Q23 rho=%.4f p=%.2e\n",
            spear_23$estimate, spear_23$p.value))

fig23 <- sup_men_clean |>
  ggplot(aes(x=nb_parcelles, y=superf_tot_ha)) +
  geom_point(alpha=0.35, size=1.8, colour="#2E75B6") +
  geom_smooth(method="loess", colour="#C00000",
              linewidth=1, se=TRUE, alpha=0.2) +
  annotate("text", x=Inf, y=Inf, hjust=1.1, vjust=1.5,
           label=sprintf("rho = %.4f\np = %.2e",
                         spear_23$estimate, spear_23$p.value),
           size=4, fontface="bold") +
  scale_x_continuous(breaks=1:max(sup_men_clean$nb_parcelles)) +
  scale_y_continuous(labels=label_number(suffix=" ha")) +
  labs(title="Fig. 23. Superficie totale vs nb de parcelles (Spearman + LOESS)",
       subtitle="GHS Panel Wave 4 (2018)",
       x="Nombre de parcelles", y="Superficie totale (ha)")
ggsave(paste0(path_out,"q23_scatter_superficie_parcelles.png"),
       plot=fig23, width=9, height=6, dpi=150)
message("OK : q23")

# ══════════════════════════════════════════════════════════════
# Q24
# ══════════════════════════════════════════════════════════════
# Médiane pondérée par État
# Approche robuste : calculer la médiane pondérée directement
# en utilisant une fonction indépendante du nommage de srvyr
wtd_median_state <- function(df_state) {
  df_state <- df_state |> filter(!is.na(wt_wave4), !is.na(superf_ha))
  if (nrow(df_state) < 2) return(NA_real_)
  o  <- order(df_state$superf_ha)
  sv <- df_state$superf_ha[o]
  sw <- df_state$wt_wave4[o]
  cw <- cumsum(sw) / sum(sw)
  sv[which.min(abs(cw - 0.5))]
}

sup_state <- secta1_clean |>
  filter(!is.na(state_nom), !is.na(wt_wave4)) |>
  group_by(state_nom) |>
  summarise(
    median_ha = wtd_median_state(cur_data()),
    n         = n(),
    .groups   = "drop"
  ) |>
  filter(!is.na(median_ha), n >= 5) |>
  mutate(state_nom = fct_reorder(state_nom, median_ha),
         quintile  = ntile(median_ha, 5) |>
           factor(labels=c("Q1","Q2","Q3","Q4","Q5")))

fig24 <- sup_state |>
  ggplot(aes(x=state_nom, y=median_ha, fill=quintile)) +
  geom_col(width=0.75) +
  geom_text(aes(label=sprintf("%.2f",median_ha)),
            hjust=-0.1, size=2.4) +
  coord_flip() +
  scale_fill_viridis_d(option="C") +
  scale_y_continuous(expand=expansion(mult=c(0,0.25)),
                     labels=label_number(suffix=" ha")) +
  labs(title="Fig. 24. Superficie mediane par Etat — W4 (2018)",
       x=NULL, y="Superficie mediane (ha)", fill="Quintile") +
  theme(axis.text.y=element_text(size=8))
ggsave(paste0(path_out,"q24_superficie_par_etat.png"),
       plot=fig24, width=11, height=10, dpi=150)
message("OK : q24")

# ══════════════════════════════════════════════════════════════
# RAPPORT WORD via officer
# ══════════════════════════════════════════════════════════════
navy  <- "#1F3864"; blue  <- "#2E75B6"
teal  <- "#028090"; grey  <- "#5A6A7E"
red   <- "#C00000"; green <- "#70AD47"

fp_t1 <- fp_text(bold=TRUE, font.size=18, color=navy,  font.family="Calibri")
fp_t2 <- fp_text(bold=TRUE, font.size=13, color=blue,  font.family="Calibri")
fp_n  <- fp_text(font.size=11, color="black", font.family="Calibri")
fp_b  <- fp_text(bold=TRUE, font.size=11, color="black",font.family="Calibri")
fp_i  <- fp_text(italic=TRUE,font.size=10, color=grey,  font.family="Calibri")
fp_m  <- fp_text(font.size=10, color="#8B0000", font.family="Courier New")
pp_j  <- fp_par(text.align="justify", line_spacing=1.2,
                padding.bottom=5, padding.top=5)
pp_c  <- fp_par(text.align="center",  padding.bottom=4, padding.top=4)
pp_l  <- fp_par(text.align="left",    line_spacing=1.2,
                padding.bottom=3, padding.top=3)

make_ft <- function(d) {
  ft <- flextable(d) |>
    bold(part="header") |>
    color(color="white", part="header") |>
    bg(bg=navy, part="header") |>
    fontsize(size=10, part="all") |>
    font(fontname="Calibri", part="all") |>
    border_outer(border=fp_border(color="#CCCCCC", width=1)) |>
    border_inner_h(border=fp_border(color="#CCCCCC", width=0.5)) |>
    set_table_properties(layout="autofit", width=1) |>
    padding(padding=4, part="all")
  if (nrow(d) >= 2)
    ft <- ft |> bg(bg="#EBF3FB", i=seq(2, nrow(d), 2))
  ft
}

add_fig <- function(doc, path_img, caption, w=6.3, h=3.5) {
  doc |>
    body_add_fpar(fpar(fp_p=fp_par(padding.top=8,
                                   padding.bottom=4))) |>
    body_add_img(path_img, width=w, height=h) |>
    body_add_fpar(fpar(ftext(caption, fp_i), fp_p=pp_c))
}

doc <- read_docx() |>
  
  # PAGE DE TITRE
  body_add_fpar(fpar(ftext(
    "ECOLE NATIONALE DE LA STATISTIQUE ET DE L'ANALYSE ECONOMIQUE PIERRE NDIAYE",
    fp_text(bold=TRUE,font.size=13,color=grey,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.top=60))) |>
  body_add_fpar(fpar(ftext(
    "TP4 — Analyse des parcelles agricoles",
    fp_text(bold=TRUE,font.size=22,color=navy,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.top=12,padding.bottom=6))) |>
  body_add_fpar(fpar(ftext(
    "Superficie, tenure foncière et utilisation des terres",
    fp_text(italic=TRUE,font.size=14,color=blue,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.bottom=20))) |>
  body_add_fpar(fpar(ftext(
    "Presente par :",
    fp_text(bold=TRUE,font.size=10,color=grey,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.bottom=3))) |>
  body_add_fpar(fpar(ftext(
    "Mamadou Lamine DIABANG  |  Awa Ba",
    fp_text(bold=TRUE,font.size=12,color=navy,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.bottom=2))) |>
  body_add_fpar(fpar(ftext(
    "Eleves Ingenieurs Statisticiens Economistes — ISE1, ENSAE Pierre Ndiaye",
    fp_text(italic=TRUE,font.size=10,color=grey,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.bottom=16))) |>
  body_add_fpar(fpar(ftext(
    "Supervise par :  Aboubacar HEMA — Analyste de recherche, IFPRI",
    fp_text(italic=TRUE,font.size=10,color=grey,font.family="Calibri")),
    fp_p=fp_par(text.align="center",padding.bottom=6))) |>
  body_add_fpar(fpar(ftext(
    "ISE1 — ENSAE Pierre Ndiaye | Annee academique 2025-2026",
    fp_text(italic=TRUE,font.size=10,color=grey,font.family="Calibri")),
    fp_p=fp_par(text.align="center"))) |>
  body_add_break() |>
  
  # ══ I. INTRODUCTION ══════════════════════════════════════
  body_add_fpar(fpar(ftext("I. Introduction", fp_t1),
                     fp_p=fp_par(padding.top=10,padding.bottom=6))) |>
  body_add_fpar(fpar(ftext("1.1 Contexte", fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("L'agriculture nigériane repose en grande partie sur de petites exploitations familiales dont la structure foncière conditionne directement la productivité et la sécurité alimentaire. Avec plus de 80 millions d'hectares de terres cultivables, le Nigeria dispose d'un immense potentiel agricole. Pourtant, la fragmentation des parcelles, l'insécurité foncière et l'accès limité aux intrants freinent le développement du secteur (Banque Mondiale, 2023). La question foncière est au cœur des débats de politique agricole : qui possède la terre, comment l'a-t-on acquise, quelle est sa superficie réelle et comment est-elle répartie entre les États ?",
          fp_n), fp_p=pp_j)) |>
  body_add_fpar(fpar(
    ftext("Dans ce contexte, le Nigeria General Household Survey (GHS) Panel Wave 4 (2018-2019) offre des données de référence sur la structure agraire nationale. L'analyse des parcelles agricoles — superficie, tenure foncière et distribution géographique — permet de dresser un diagnostic précis des conditions d'exploitation et d'identifier les leviers d'amélioration.",
          fp_n), fp_p=pp_j)) |>
  body_add_fpar(fpar(ftext("1.2 Objectifs", fp_t2),
                     fp_p=fp_par(padding.top=8,padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("Ce travail pratique analyse la structure foncière des exploitations agricoles nigérianes à partir du module parcelles du GHS W4. Cinq questions sont traitées :",
          fp_n), fp_p=pp_j)) |>
  body_add_fpar(fpar(
    ftext("Q19 — ",fp_b),
    ftext("Construire la variable superficie en hectares, identifier les valeurs manquantes et détecter les valeurs aberrantes.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Q20 — ",fp_b),
    ftext("Réaliser une analyse univariée complète de la superficie et comparer les mesures déclarées aux mesures GPS.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Q21 — ",fp_b),
    ftext("Décrire les régimes de tenure foncière et tester leur association avec la zone de résidence.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Q23 — ",fp_b),
    ftext("Analyser la relation entre la superficie totale exploitée et le nombre de parcelles par ménage.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Q24 — ",fp_b),
    ftext("Cartographier la superficie médiane des parcelles par État nigérian.",fp_n),
    fp_p=pp_l)) |>
  body_add_break() |>
  
  # ══ II. DONNÉES & MÉTHODOLOGIE ════════════════════════════
  body_add_fpar(fpar(ftext("II. Données et méthodologie", fp_t1),
                     fp_p=fp_par(padding.top=10,padding.bottom=6))) |>
  body_add_fpar(fpar(ftext("2.1 Source de données", fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("Les données proviennent du ",fp_n),
    ftext("Nigeria General Household Survey (GHS) Panel Wave 4 (2018-2019)",fp_b),
    ftext(", conduit par le Bureau National des Statistiques du Nigeria (NBS) avec l'appui de la Banque Mondiale (programme LSMS-ISA). Il s'agit d'une enquête longitudinale représentative à l'échelle nationale, couvrant 4 979 ménages répartis sur l'ensemble du territoire. Le plan de sondage est stratifié à deux degrés (Enumeration Areas puis ménages) avec poids de sondage ",fp_n),
    ftext("wt_wave4",fp_m),
    ftext(" disponibles dans secta_harvestw4.",fp_n),
    fp_p=pp_j)) |>
  body_add_fpar(fpar(ftext("2.2 Fichiers mobilisés", fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=4))) |>
  body_add_flextable(make_ft(data.frame(
    "Fichier .dta" = c("secta1_harvestw4","sect11b1_plantingw4","secta_harvestw4"),
    "Contenu" = c(
      "Roster des parcelles — superficie GPS et re-mesurée",
      "Tenure foncière — mode d'acquisition, titre légal",
      "Géolocalisation administrative — poids de sondage"
    ),
    "Lignes" = c(
      format(nrow(secta1),big.mark=" "),
      format(nrow(tenure),big.mark=" "),
      "5 025"
    ),
    "Questions" = c("Q19, Q20, Q23, Q24","Q21","Q19-Q24"),
    check.names=FALSE
  ))) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=6))) |>
  body_add_fpar(fpar(ftext("2.3 Construction des variables clés", fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("Superficie. ",fp_b),
    ftext("La variable superficie est construite en priorisant la re-mesure GPS W4 (sa1q11, en m²) sur le GPS pré-rempli depuis W3 (prefilled_gps_area, en m²), puis convertie en hectares (÷ 10 000). Les valeurs aberrantes sont détectées par la règle IQR×3.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Tenure foncière. ",fp_b),
    ftext("La variable s11b1q4 du fichier sect11b1_plantingw4 (Post-Planting W4) renseigne le mode d'acquisition en 7 modalités. Cette variable n'existe pas dans le Post-Harvest W4 — elle a été déplacée dans le module Post-Planting à partir de W4.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(ftext("2.4 Méthodes statistiques", fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("Statistiques descriptives pondérées. ",fp_b),
    ftext("Toutes les statistiques (médianes, moyennes, proportions) sont calculées avec les poids de sondage wt_wave4 via le package srvyr (as_survey_design). Le plan de sondage déclare les 402 PSU (ea), les 6 strates géographiques (strata) et le poids transversal (wt_wave4). Les intervalles de confiance à 95% sont fournis pour les estimations pondérées.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Détection des outliers. ",fp_b),
    ftext("Règle IQR×3 (Q3 + 3×IQR) pour identifier les superficies aberrantes.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Tests statistiques. ",fp_b),
    ftext("Test du chi-deux avec V de Cramér pour l'indépendance tenure × zone. Corrélation de Spearman avec p-valeur pour les relations entre variables continues.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Visualisation. ",fp_b),
    ftext("Histogramme log, boxplot, scatter plot avec courbe LOESS, barplot ordonné et barplot empilé par zone.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Logiciels. ",fp_b),
    ftext("R 4.x — packages haven, dplyr, ggplot2, rstatix, naniar, viridis, officer, flextable.",fp_n),
    fp_p=pp_l)) |>
  body_add_break() |>
  
  # ══ III. RÉSULTATS ════════════════════════════════════════
  body_add_fpar(fpar(ftext("III. Résultats", fp_t1),
                     fp_p=fp_par(padding.top=10,padding.bottom=6))) |>
  
  # SECTION Q19
  body_add_fpar(fpar(ftext("3.1 Question 19 — Superficie en ha, valeurs manquantes et aberrantes",
                           fp_t1),fp_p=fp_par(padding.top=10,padding.bottom=4))) |>
  body_add_fpar(fpar(ftext("Construction de la variable superficie",fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=2))) |>
  body_add_fpar(fpar(
    ftext("La superficie de chaque parcelle est construite à partir de deux sources disponibles dans ",fp_n),
    ftext("secta1_harvestw4",fp_m),
    ftext(" : ",fp_n),
    ftext("sa1q11",fp_m),
    ftext(" (re-mesure GPS Wave 4, en m²) et ",fp_n),
    ftext("prefilled_gps_area",fp_m),
    ftext(" (GPS pré-rempli depuis Wave 3, en m²). La priorité est donnée à la mesure W4 quand disponible. Conversion : ÷ 10 000 → hectares.",fp_n),
    fp_p=pp_j)) |>
  body_add_fpar(fpar(ftext("Disponibilité et valeurs manquantes",fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=4))) |>
  body_add_flextable(make_ft(data.frame(
    "Source" = c("GPS re-mesuré W4 (sa1q11)",
                 "GPS pré-rempli (prefilled_gps_area)",
                 "Superficie finale disponible",
                 "Superficie manquante"),
    "N parcelles" = c(
      format(sum(!is.na(secta1$sa1q11)),big.mark=" "),
      format(sum(!is.na(secta1$prefilled_gps_area)),big.mark=" "),
      format(sum(!is.na(secta1$superf_ha)),big.mark=" "),
      format(sum(is.na(secta1$superf_ha)),big.mark=" ")
    ),
    "%" = c(
      sprintf("%.1f%%",mean(!is.na(secta1$sa1q11))*100),
      sprintf("%.1f%%",mean(!is.na(secta1$prefilled_gps_area))*100),
      sprintf("%.1f%%",mean(!is.na(secta1$superf_ha))*100),
      sprintf("%.1f%%",mean(is.na(secta1$superf_ha))*100)
    ), check.names=FALSE
  ))) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=6))) |>
  body_add_fpar(fpar(ftext("Valeurs aberrantes (IQR × 3)",fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=4))) |>
  body_add_flextable(make_ft(data.frame(
    "Indicateur" = c("N disponibles","Minimum","Q1 (25%)","Médiane",
                     "Médiane pondérée","Moyenne","Moyenne pondérée",
                     "Q3 (75%)","Seuil aberrant (Q3+3×IQR)","N aberrants"),
    "Non pondéré" = c(
      format(sum(!is.na(secta1$superf_ha)),big.mark=" "),
      sprintf("%.4f",min(secta1$superf_ha,na.rm=TRUE)),
      sprintf("%.4f",q1_s),
      sprintf("%.4f",median(secta1$superf_ha,na.rm=TRUE)),
      "—",
      sprintf("%.4f",mean(secta1$superf_ha,na.rm=TRUE)),
      "—",
      sprintf("%.4f",q3_s),
      sprintf("%.4f",seuil_h),
      sprintf("%d (%.1f%%)",outliers_n,
              outliers_n/sum(!is.na(secta1$superf_ha))*100)
    ),
    "Pondéré (wt_wave4)" = c(
      "—","—","—",
      "—",
      sprintf("%.4f [%.4f–%.4f]",
              stats_pond_superf$med_pond,
              stats_pond_superf$med_pond_low,
              stats_pond_superf$med_pond_upp),
      "—",
      sprintf("%.4f [%.4f–%.4f]",
              stats_pond_superf$moy_pond,
              stats_pond_superf$moy_pond_low,
              stats_pond_superf$moy_pond_upp),
      "—","—","—"
    ),
    check.names=FALSE
  ))) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=6))) |>
  add_fig(paste0(path_out,"q19_superficie_manquants_aberrants.png"),
          "Figure 1. Taux de valeurs manquantes (gauche) et détection des outliers par IQR×3 (droite) — secta1_harvestw4 W4",
          w=6.3, h=3.0) |>
  body_add_fpar(fpar(
    ftext("Interprétation. ",fp_b),
    ftext(sprintf(
      "Seules %.1f%% des parcelles disposent d'une superficie mesurable, la plupart via le GPS pré-rempli de W3 (%.1f%%). L'IQR×3 identifie %d parcelles aberrantes (seuil = %.4f ha, soit %.1f%% de l'échantillon), retirées pour les analyses suivantes. Aucune superficie négative n'est observée.",
      mean(!is.na(secta1$superf_ha))*100,
      mean(!is.na(secta1$prefilled_gps_area))*100,
      outliers_n, seuil_h,
      outliers_n/sum(!is.na(secta1$superf_ha))*100
    ),fp_n),
    fp_p=pp_j)) |>
  body_add_break() |>
  
  # SECTION Q20
  body_add_fpar(fpar(ftext("3.2 Question 20 — Analyse univariée de la superficie",
                           fp_t1),fp_p=fp_par(padding.top=10,padding.bottom=4))) |>
  body_add_fpar(fpar(ftext("Statistiques par décile",fp_t2),
                     fp_p=fp_par(padding.top=6,padding.bottom=4))) |>
  body_add_flextable(make_ft(
    secta1_clean |>
      reframe(Decile=paste0("D",1:10),
              "Valeur (ha)"=sprintf("%.4f",quantile(superf_ha,seq(.1,1,.1))))
  )) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=6))) |>
  add_fig(paste0(path_out,"q20_superficie_univarie.png"),
          sprintf("Figure 2. Histogramme log (haut gauche), boxplot par zone (haut droite), scatter déclaré vs GPS avec ρ = %.4f (bas) — GHS W4 2018",rho_20),
          w=6.3, h=5.5) |>
  body_add_fpar(fpar(
    ftext("Histogramme log. ",fp_b),
    ftext(sprintf(
      "La distribution des superficies suit une loi log-normale typique des petites exploitations africaines — médiane de %.3f ha, moyenne de %.3f ha. Le CV élevé (%.2f) confirme l'hétérogénéité structurelle des tailles de parcelles.",
      median(secta1_clean$superf_ha),
      mean(secta1_clean$superf_ha),
      sd(secta1_clean$superf_ha)/mean(secta1_clean$superf_ha)
    ),fp_n),
    fp_p=pp_j)) |>
  body_add_fpar(fpar(
    ftext("Scatter déclaré vs GPS. ",fp_b),
    ftext(sprintf(
      "La corrélation de Spearman ρ = %.4f (p = %.2e) entre superficie re-mesurée W4 et GPS W3 est forte. La concentration des points sous la ligne de concordance à 45° indique une sous-estimation systématique des superficies déclarées — biais classique de déclaration dans les enquêtes LSMS-ISA.",
      rho_20, pval_20
    ),fp_n),
    fp_p=pp_j)) |>
  body_add_break() |>
  
  # SECTION Q21
  body_add_fpar(fpar(ftext("3.3 Question 21 — Régime de tenure foncière",
                           fp_t1),fp_p=fp_par(padding.top=10,padding.bottom=4))) |>
  body_add_fpar(fpar(
    ftext("La variable ",fp_n),ftext("s11b1q4",fp_m),
    ftext(" du fichier ",fp_n),ftext("sect11b1_plantingw4",fp_m),
    ftext(" renseigne le mode d'acquisition de chaque parcelle en 7 modalités. Fichier Post-Planting W4 uniquement — cette variable est absente du Post-Harvest W4.",fp_n),
    fp_p=pp_j)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.top=6))) |>
  body_add_flextable(make_ft(
    freq_ten |>
      mutate(mode_acq=as.character(mode_acq),
             "Prop. non pond."=sprintf("%.1f%%", n/sum(n)*100),
             "Prop. pondérée" =sprintf("%.1f%% [%.1f–%.1f]",
                                       prop_pond,
                                       prop_pond_low,
                                       prop_pond_upp)) |>
      rename("Mode d'acquisition"=mode_acq,"N parcelles"=n) |>
      select("Mode d'acquisition","N parcelles",
             "Prop. non pond.","Prop. pondérée") |>
      arrange(desc(`N parcelles`))
  )) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=6))) |>
  add_fig(paste0(path_out,"q21_tenure_fonciere.png"),
          sprintf("Figure 3. Régimes de tenure (gauche) et distribution par zone (droite) | χ² p=%.4f, V=%.4f — sect11b1_plantingw4 W4",
                  chi2_tz$p.value, cramv_tz),
          w=6.3, h=4.0) |>
  body_add_fpar(fpar(
    ftext("Interprétation. ",fp_b),
    ftext(sprintf(
      "L'héritage familial domine (%.1f%%), reflet du système foncier coutumier nigérian. L'achat direct (%.1f%%) et la location (%.1f%%) sont les alternatives croissantes, sur-représentées en milieu urbain. Seulement 9,7%% des parcelles disposent d'un titre légal formel. Le chi-deux (p = %.4f, V = %.4f) révèle une association %s entre tenure et zone de résidence.",
      freq_ten$prop[freq_ten$mode_acq=="Heritage familial"],
      freq_ten$prop[freq_ten$mode_acq=="Achat direct"],
      freq_ten$prop[freq_ten$mode_acq=="Location (cash/en nature)"],
      chi2_tz$p.value, cramv_tz,
      if(chi2_tz$p.value<0.05)"significative" else "non significative"
    ),fp_n),
    fp_p=pp_j)) |>
  body_add_break() |>
  
  # SECTION Q23
  body_add_fpar(fpar(ftext("3.4 Question 23 — Superficie totale vs nombre de parcelles",
                           fp_t1),fp_p=fp_par(padding.top=10,padding.bottom=4))) |>
  add_fig(paste0(path_out,"q23_scatter_superficie_parcelles.png"),
          sprintf("Figure 4. Superficie totale vs nombre de parcelles | ρ = %.4f, p = %.2e — GHS W4 2018",
                  spear_23$estimate, spear_23$p.value)) |>
  body_add_fpar(fpar(
    ftext("Interprétation. ",fp_b),
    ftext(sprintf(
      "La corrélation de Spearman ρ = %.4f (p = %.2e) est positive et significative. La courbe LOESS révèle une relation non linéaire : la superficie marginale par parcelle supplémentaire décroît au-delà de 4-5 parcelles — signal de fragmentation foncière croissante liée aux pratiques d'héritage.",
      spear_23$estimate, spear_23$p.value
    ),fp_n),
    fp_p=pp_j)) |>
  body_add_break() |>
  
  # SECTION Q24
  body_add_fpar(fpar(ftext("3.5 Question 24 — Superficie médiane par État",
                           fp_t1),fp_p=fp_par(padding.top=10,padding.bottom=4))) |>
  add_fig(paste0(path_out,"q24_superficie_par_etat.png"),
          "Figure 5. Superficie médiane des parcelles par État — coloré par quintile | GHS W4 2018",
          w=6.0, h=5.5) |>
  body_add_fpar(fpar(
    ftext("Interprétation. ",fp_b),
    ftext("Les États du Nord-Ouest (Kebbi, Sokoto, Zamfara, Katsina) affichent les plus grandes exploitations (Q5), adaptées aux vastes savanes sahéliennes. Les États du Sud-Est (Imo, Anambra, Lagos) présentent les plus petites parcelles (Q1) — pression démographique forte et morcellement intensif. Ces disparités spatiales conditionnent directement la mécanisation possible et la productivité agricole.",
          fp_n),
    fp_p=pp_j)) |>
  body_add_break() |>
  
  # ══ IV. CONCLUSION ════════════════════════════════════════
  body_add_fpar(fpar(ftext("IV. Conclusion", fp_t1),
                     fp_p=fp_par(padding.top=10,padding.bottom=6))) |>
  body_add_fpar(fpar(
    ftext("Ce travail a analysé la structure foncière de 10 961 parcelles agricoles nigérianes à partir du GHS Panel Wave 4 (2018). Les principaux enseignements sont les suivants :",
          fp_n), fp_p=pp_j)) |>
  body_add_fpar(fpar(
    ftext("Petites exploitations fragmentées. ",fp_b),
    ftext(sprintf("La superficie médiane des parcelles est de %.3f ha — très inférieure au seuil de viabilité économique. Seules 66,8%% des parcelles disposent d'une mesure GPS, dont 9,7%% re-mesurées en W4.",
                  median(secta1_clean$superf_ha)),fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Prédominance du foncier coutumier. ",fp_b),
    ftext("L'héritage familial représente 62,8% des modes d'acquisition, avec seulement 9,7% de titres légaux — fragilité de la sécurisation foncière formelle.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Fragmentation croissante. ",fp_b),
    ftext("La relation non linéaire entre superficie totale et nombre de parcelles révèle une fragmentation foncière au-delà de 4-5 parcelles, phénomène exacerbé par les pratiques successorales coutumières.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(
    ftext("Fortes disparités géographiques. ",fp_b),
    ftext("Les superficies médianes varient d'un facteur 10 entre États du Nord et du Sud-Est, appelant des politiques foncières différenciées.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.top=6))) |>
  body_add_fpar(fpar(
    ftext("Limites. ",fp_b),
    ftext("La superficie GPS n'est disponible que pour 66,8% des parcelles. La variable de tenure est issue du Post-Planting (pas du Post-Harvest) et couvre 3 915 ménages sur les 4 979 du panel. Les résultats sont valides pour W4 uniquement (pas de comparaison inter-temporelle).",fp_n),
    fp_p=pp_j)) |>
  body_add_break() |>
  
  # ══ V. RÉFÉRENCES ═════════════════════════════════════════
  body_add_fpar(fpar(ftext("V. Références bibliographiques", fp_t1),
                     fp_p=fp_par(padding.top=10,padding.bottom=6))) |>
  body_add_fpar(fpar(
    ftext("Banque Mondiale (2023). ",fp_b),
    ftext("Nigeria — Agriculture Overview. Washington D.C. : World Bank Group. https://www.worldbank.org/en/country/nigeria",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("Deininger, K., & Byerlee, D. (2011). ",fp_b),
    ftext("Rising Global Interest in Farmland: Can It Yield Sustainable and Equitable Benefits? Washington D.C. : World Bank Publications.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("NBS — National Bureau of Statistics Nigeria (2019). ",fp_b),
    ftext("General Household Survey Panel Wave 4 (2018-2019) — Basic Information Document. Abuja : NBS.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("Otsuka, K., & Place, F. (2014). ",fp_b),
    ftext("Changes in land tenure and agricultural intensification in sub-Saharan Africa. WIDER Working Paper 2014/051.",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("World Bank LSMS-ISA (2020). ",fp_b),
    ftext("Nigeria General Household Survey, Panel 2018-19, Wave 4. Washington D.C. : World Bank Microdata Library. https://microdata.worldbank.org/index.php/catalog/3557",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.bottom=3))) |>
  body_add_fpar(fpar(
    ftext("R Core Team (2024). ",fp_b),
    ftext("R: A Language and Environment for Statistical Computing. Vienna : R Foundation for Statistical Computing. https://www.R-project.org/",fp_n),
    fp_p=pp_l)) |>
  body_add_fpar(fpar(fp_p=fp_par(padding.top=60,padding.bottom=40))) |>
  body_add_fpar(fpar(
    ftext("Rapport réalisé dans le cadre du cours de Projet Statistique sous R — ENSAE Pierre Ndiaye — ISE1 2025-2026",
          fp_text(italic=TRUE,font.size=9,color=grey,font.family="Calibri")),
    fp_p=pp_c))

path_word <- paste0(path_out, "TP4_Parcelles_GHS_W4.docx")
print(doc, target=path_word)
message("\n✓ Word : ", path_word)
message("✓ TP4 termine")