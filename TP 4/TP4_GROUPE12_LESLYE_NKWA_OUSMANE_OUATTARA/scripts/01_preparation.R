# Script de préparation (nettoyage)

# 1. Chargement des données
sect11a1  <- read_dta(here("data", "raw", "sect11a1_plantingw4.dta"))
sect11b1  <- read_dta(here("data", "raw", "sect11b1_plantingw4.dta"))
sect1_pp  <- read_dta(here("data", "raw", "sect1_plantingw4.dta"))
secta1_ph <- read_dta(here("data", "raw", "secta1_harvestw4.dta"))
secta_pp  <- read_dta(here("data", "raw", "secta_plantingw4.dta"))
secta_ph  <- read_dta(here("data", "raw", "secta_harvestw4.dta"))

cat("sect11a1   :", nrow(sect11a1),  "x", ncol(sect11a1),  "\n")
cat("secta1_ph  :", nrow(secta1_ph), "x", ncol(secta1_ph), "\n")

# 2. Vérifications des doublons
doublons_11a1 <- sect11a1 |> group_by(hhid, plotid) |> filter(n() > 1) |> nrow()
cat("Doublons sect11a1 :", doublons_11a1, "\n")

# 3. Ménages sans parcelles
menages_tous   <- unique(secta_pp$hhid)
menages_agri   <- unique(sect11a1$hhid)
menages_sans   <- setdiff(menages_tous, menages_agri)
cat("\n Ménages sans parcelles \n")
cat("Total PP :", length(menages_tous), "; Avec parcelles :", length(menages_agri),
    "; Sans :", length(menages_sans), "(", round(length(menages_sans)/length(menages_tous)*100,1), "%)\n")
cat("Question filtre : ag1a\n")
print(table(secta_pp$ag1a, useNA = "ifany"))

# 4. Poids
poids <- secta_pp |> select(hhid, wt_wave4, strata, cluster, sector, zone, state)

# 5. Sexe du chef de ménage
chefs_menage <- sect1_pp |>
  filter(s1q3 == 1) |>
  select(hhid, indiv_chef = indiv, sexe_chef = s1q2) |>
  mutate(sexe_chef_f = factor(sexe_chef, levels = c(1,2), labels = c("Homme","Femme")))

# 6. Sexe du gestionnaire
sexe_roster <- sect1_pp |> select(hhid, indiv, sexe_indiv = s1q2)
gestionnaires <- sect11a1 |>
  select(hhid, plotid, indiv_gest = s11aq6a) |>
  filter(!is.na(indiv_gest)) |>
  left_join(sexe_roster, by = c("hhid", "indiv_gest" = "indiv"))

# 7. Facteurs de conversion BID Appendix 2
conv_heaps  <- data.frame(zone=1:6, f_heaps =c(0.00012,0.00016,0.00011,0.00019,0.00021,0.00012))
conv_ridges <- data.frame(zone=1:6, f_ridges=c(0.0027,0.004,0.00494,0.0023,0.0023,0.00001))
conv_stands <- data.frame(zone=1:6, f_stands=c(0.00006,0.00016,0.00004,0.00004,0.00013,0.00041))

# 8. Construction superficie 3 niveaux
ph_gps <- secta1_ph |>
  select(hhid, plotid, gps_ph_m2 = sa1q11, encore_detenue = sa1q4)

df_parcelles <- sect11a1 |>
  select(hhid, plotid, zone, state, sector,
         gps_pp_m2 = s11aq4c, sup_declaree = s11aq4aa,
         unite = s11aq4b, cultive = s11b1q27, gestionnaire = s11aq6a) |>
  mutate(zone = as.integer(zone), state = as.integer(state)) |>
  left_join(ph_gps, by = c("hhid","plotid")) |>
  mutate(gps_pp_ha = gps_pp_m2 * 0.0001, gps_ph_ha = gps_ph_m2 * 0.0001) |>
  left_join(conv_heaps,  by = "zone") |>
  left_join(conv_ridges, by = "zone") |>
  left_join(conv_stands, by = "zone") |>
  mutate(
    sup_declaree_ha = case_when(
      unite == 1 ~ sup_declaree * f_heaps,
      unite == 2 ~ sup_declaree * f_ridges,
      unite == 3 ~ sup_declaree * f_stands,
      unite == 5 ~ sup_declaree * 0.4047,
      unite == 6 ~ sup_declaree * 1.0,
      unite == 7 ~ sup_declaree * 0.0001,
      TRUE       ~ NA_real_
    ),
    sup_ha = case_when(
      !is.na(gps_ph_ha) ~ gps_ph_ha,
      !is.na(gps_pp_ha) ~ gps_pp_ha,
      TRUE              ~ sup_declaree_ha
    ),
    source_sup = case_when(
      !is.na(gps_ph_ha) ~ "GPS Post-Harvest",
      !is.na(gps_pp_ha) ~ "GPS Post-Planting",
      !is.na(sup_declaree_ha) ~ "Déclarée convertie",
      TRUE ~ NA_character_
    ),
    parcelle_active = if_else(is.na(encore_detenue) | encore_detenue == 1, TRUE, FALSE)
  ) |>
  select(-f_heaps, -f_ridges, -f_stands)

cat("\n Source de la superficie \n")
print(table(df_parcelles$source_sup, useNA = "ifany"))
cat(sprintf("Couverture GPS totale : %.1f %%\n",
    sum(df_parcelles$source_sup %in% c("GPS Post-Harvest","GPS Post-Planting"), na.rm=TRUE) /
    nrow(df_parcelles) * 100))
cat("Parcelles cédées (sa1q4==2) :", sum(!df_parcelles$parcelle_active), "\n")

# 9. Winsorisation au P99
p99_sup <- quantile(df_parcelles$sup_ha, 0.99, na.rm = TRUE)
n_aberrants <- sum(df_parcelles$sup_ha > p99_sup, na.rm = TRUE)
cat(sprintf("\nP99 = %.2f ha | Aberrants = %d\n", p99_sup, n_aberrants))

aberrants_detail <- df_parcelles |>
  filter(!is.na(sup_ha), sup_ha > p99_sup) |>
  select(hhid, plotid, zone, gps_pp_ha, gps_ph_ha, sup_declaree_ha, sup_ha, source_sup) |>
  arrange(desc(sup_ha))
cat("Détail :\n"); print(aberrants_detail)

df_parcelles <- df_parcelles |>
  mutate(aberrant = !is.na(sup_ha) & sup_ha > p99_sup,
         sup_ha   = if_else(sup_ha > p99_sup, p99_sup, sup_ha))

# 10. Tenure
tenure <- sect11b1 |> select(hhid, plotid, acquisition=s11b1q4, titre_legal=s11b1q7,
                              droit_vente=s11b1q19, droit_collateral=s11b1q20)
df_parcelles <- df_parcelles |>
  left_join(tenure, by = c("hhid","plotid")) |>
  mutate(
    mode_acquisition = case_when(
      acquisition==1~"Achat", acquisition==2~"Location", acquisition==3~"Gratuit",
      acquisition==4~"Distribution communautaire", acquisition==5~"Héritage familial",
      acquisition==6~"Métayage", acquisition==7~"Échange temporaire",
      TRUE ~ NA_character_) |> factor(),
    cultive_f = factor(cultive, levels=c(1,2), labels=c("Oui","Non")),
    milieu = factor(sector, levels=c(1,2), labels=c("Urbain","Rural")),
    zone_f = factor(zone, levels=1:6,
      labels=c("North Central","North East","North West","South East","South South","South West"))
  )

# 11. Sexe
df_parcelles <- df_parcelles |>
  left_join(gestionnaires |> select(hhid,plotid,sexe_gestionnaire=sexe_indiv), by=c("hhid","plotid")) |>
  mutate(sexe_gest_f = factor(sexe_gestionnaire, levels=c(1,2), labels=c("Homme","Femme"))) |>
  left_join(chefs_menage |> select(hhid, sexe_chef, sexe_chef_f), by = "hhid")

# 12. Poids avec les noms des états
df_parcelles <- df_parcelles |>
  left_join(poids |> select(hhid, wt_wave4, strata, cluster), by = "hhid") |>
  mutate(state_name = case_when(
    state==1~"Abia",state==2~"Adamawa",state==3~"Akwa Ibom",state==4~"Anambra",
    state==5~"Bauchi",state==6~"Bayelsa",state==7~"Benue",state==8~"Borno",
    state==9~"Cross River",state==10~"Delta",state==11~"Ebonyi",state==12~"Edo",
    state==13~"Ekiti",state==14~"Enugu",state==15~"Gombe",state==16~"Imo",
    state==17~"Jigawa",state==18~"Kaduna",state==19~"Kano",state==20~"Katsina",
    state==21~"Kebbi",state==22~"Kogi",state==23~"Kwara",state==24~"Lagos",
    state==25~"Nasarawa",state==26~"Niger",state==27~"Ogun",state==28~"Ondo",
    state==29~"Osun",state==30~"Oyo",state==31~"Plateau",state==32~"Rivers",
    state==33~"Sokoto",state==34~"Taraba",state==35~"Yobe",state==36~"Zamfara",
    state==37~"FCT", TRUE~NA_character_))

# 13. Superficie par ménage
sup_menage <- df_parcelles |>
  filter(!is.na(sup_ha), parcelle_active) |>
  group_by(hhid) |>
  summarise(nb_parcelles=n(), nb_cultivees=sum(cultive==1,na.rm=TRUE),
            sup_totale_ha=sum(sup_ha,na.rm=TRUE),
            sup_cultivee_ha=sum(sup_ha[cultive==1],na.rm=TRUE),
            sup_mediane_ha=median(sup_ha,na.rm=TRUE), .groups="drop") |>
  left_join(poids, by="hhid") |>
  left_join(chefs_menage |> select(hhid,sexe_chef,sexe_chef_f), by="hhid") |>
  mutate(milieu=factor(sector,levels=c(1,2),labels=c("Urbain","Rural")),
         zone_f=factor(zone,levels=1:6,labels=c("North Central","North East","North West",
                                                  "South East","South South","South West")))

cat("\nMénages :", nrow(sup_menage), "| Moy parcelles :", round(mean(sup_menage$nb_parcelles),2),
    "| Moy sup :", round(mean(sup_menage$sup_totale_ha),2), "ha\n")

# 14. Graphe NA 
vars_diag <- df_parcelles |>
  select(`GPS PP`=gps_pp_ha, `GPS PH`=gps_ph_ha, Déclarée=sup_declaree_ha,
         Finale=sup_ha, Acquisition=mode_acquisition, `Titre légal`=titre_legal,
         Cultivée=cultive_f, `Gest.(sexe)`=sexe_gest_f, `Chef(sexe)`=sexe_chef_f, Poids=wt_wave4)

fig00 <- vis_miss(vars_diag, sort_miss = TRUE) +
  labs(title="Diagnostic des valeurs manquantes: variables clés uniquement",
       subtitle=paste0("GHS Panel W4: ",format(nrow(df_parcelles),big.mark=" ")," parcelles"),
       caption=source_ghs) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave(here("outputs","figures","fig00_diagnostic_NA.png"), fig00, width=9, height=5, dpi=300)

# 15. Plans de sondage
plan_parcelles <- df_parcelles |>
  filter(!is.na(wt_wave4), !is.na(sup_ha), parcelle_active) |>
  as_survey_design(ids=cluster, strata=strata, weights=wt_wave4, nest=TRUE)

plan_menages <- sup_menage |>
  filter(!is.na(wt_wave4)) |>
  as_survey_design(ids=cluster, strata=strata, weights=wt_wave4, nest=TRUE)

# 16. Sauvegarde
saveRDS(df_parcelles,   here("data","processed","df_parcelles.rds"))
saveRDS(sup_menage,     here("data","processed","sup_menage.rds"))
saveRDS(plan_parcelles, here("data","processed","plan_parcelles.rds"))
saveRDS(plan_menages,   here("data","processed","plan_menages.rds"))
saveRDS(chefs_menage,   here("data","processed","chefs_menage.rds"))

cat(sprintf("\ndf_parcelles : %d obs x %d vars\nsup_menage : %d ménages\n",
    nrow(df_parcelles), ncol(df_parcelles), nrow(sup_menage)))
cat(sprintf("Source : GPS PH=%d, GPS PP=%d, Déclarée=%d\n",
    sum(df_parcelles$source_sup=="GPS Post-Harvest",na.rm=TRUE),
    sum(df_parcelles$source_sup=="GPS Post-Planting",na.rm=TRUE),
    sum(df_parcelles$source_sup=="Déclarée convertie",na.rm=TRUE)))
