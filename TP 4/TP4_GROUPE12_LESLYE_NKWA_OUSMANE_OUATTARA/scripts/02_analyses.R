# Script des analyses 

# Groupe 12 : Nkwa Tsamo Leslye & Ouattara Ousmane


# Palette TP4 (conventions de la FAO pour le foncier)
col_fao <- "#4C9A2A"; col_foret <- "#1B5E20"; col_clair <- "#A5D6A7"
col_gps <- "#F57F17"; col_propriete <- "#2E7D32"; col_location <- "#455A64"
col_metayage <- "#795548"; col_informel <- "#1B5E20"
col_homme <- "#0058AB"; col_femme <- "#E8416F"
col_rural <- "#80BD41"; col_urbain <- "#6C757D"

theme_tp4 <- theme_minimal(base_size = 10) +
  theme(plot.title=element_text(size=11,face="bold",hjust=0),
        plot.subtitle=element_text(size=9,color="grey40"),
        plot.caption=element_text(size=7,color="grey50",hjust=1),
        axis.title=element_text(size=9), legend.position="bottom")
theme_set(theme_tp4)

# Style flextable LSMS vert
style_lsms <- function(ft) {
  vh <- "#2E7D32"; vc <- "#E8F5E9"; bl <- "#FFFFFF"; gb <- "#A0A0A0"
  nr <- nrow(ft$body$dataset)
  ft |> flextable::font(fontname="Garamond",part="all") |>
    flextable::fontsize(size=9,part="body") |> flextable::fontsize(size=9.5,part="header") |>
    flextable::bg(bg=vh,part="header") |> flextable::color(color=bl,part="header") |>
    flextable::bold(part="header") |> flextable::align(align="center",part="header") |>
    flextable::align(j=1,align="left",part="header") |>
    flextable::bg(i=seq(1,nr,2),bg=bl,part="body") |>
    flextable::bg(i=seq(2,nr,2),bg=vc,part="body") |>
    flextable::align(j=1,align="left",part="body") |> flextable::bold(j=1,part="body") |>
    flextable::border_remove() |>
    flextable::border_outer(part="all",border=officer::fp_border(color=vh,width=1.5)) |>
    flextable::border_inner_h(part="body",border=officer::fp_border(color=gb,width=0.4)) |>
    flextable::hline_bottom(part="header",border=officer::fp_border(color=vh,width=2)) |>
    flextable::padding(padding.left=6,padding.right=6,padding.top=3,padding.bottom=3,part="all")
}

# Fonction mode 
# Le calcul se fait en hectares naturels (pas en log) pour que le mode soit comparable à la médiane et à la moyenne, également en hectares.
calc_mode <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) < 10) return(NA_real_)
  d <- density(x, bw = "SJ")
  d$x[which.max(d$y)]
}

# ============================================================
# 19. Statistiques descriptives
# ============================================================
stats_sup <- plan_parcelles |>
  summarise(N=unweighted(n()),
            moyenne=survey_mean(sup_ha,na.rm=TRUE,vartype=NULL),
            mediane=survey_median(sup_ha,na.rm=TRUE,vartype=NULL),
            q1=survey_quantile(sup_ha,quantiles=0.25,na.rm=TRUE,vartype=NULL),
            q3=survey_quantile(sup_ha,quantiles=0.75,na.rm=TRUE,vartype=NULL),
            ecart_type=survey_sd(sup_ha,na.rm=TRUE,vartype=NULL))
cat("Stats pondérées :\n"); print(stats_sup)

stats_sup_milieu <- plan_parcelles |> group_by(milieu) |>
  summarise(N=unweighted(n()), moyenne=survey_mean(sup_ha,na.rm=TRUE,vartype=NULL),
            mediane=survey_median(sup_ha,na.rm=TRUE,vartype=NULL))
cat("Par milieu :\n"); print(stats_sup_milieu)

stats_sup_zone <- plan_parcelles |> group_by(zone_f) |>
  summarise(N=unweighted(n()), moyenne=survey_mean(sup_ha,na.rm=TRUE,vartype=NULL),
            mediane=survey_median(sup_ha,na.rm=TRUE,vartype=NULL))

stats_menage <- plan_menages |>
  summarise(nb_parcelles_moy=survey_mean(nb_parcelles,na.rm=TRUE,vartype=NULL),
            sup_totale_moy=survey_mean(sup_totale_ha,na.rm=TRUE,vartype=NULL),
            sup_cultivee_moy=survey_mean(sup_cultivee_ha,na.rm=TRUE,vartype=NULL))
cat("Stats ménage (réf Table 6.4) :\n"); print(stats_menage)

deciles_sup <- df_parcelles |> filter(!is.na(sup_ha)) |>
  summarise(across(everything(), ~NA_real_)) |> slice(0)  # placeholder
deciles_sup <- df_parcelles |> filter(!is.na(sup_ha)) |>
  reframe(decile = paste0("D",1:10),
          valeur = quantile(sup_ha, probs = seq(0.1, 1, 0.1)))
cat("Déciles :\n"); print(deciles_sup)

# ============================================================
# 20 . Histogramme, boxplot, scatter
# ============================================================

# Fig 1 : Histogramme log avec médiane, moyenne et mode
mode_global <- calc_mode(df_parcelles$sup_ha)
moyenne_global <- mean(df_parcelles$sup_ha, na.rm = TRUE)
mediane_global <- as.numeric(stats_sup$mediane)

# Vérification de l'ordre Mode < Médiane < Moyenne (asymétrie droite)
cat(sprintf("Mode = %.4f ha | Médiane = %.4f ha | Moyenne = %.4f ha\n",
            mode_global, mediane_global, moyenne_global))
cat(sprintf("Mode < Médiane < Moyenne ? %s\n",
            ifelse(mode_global < mediane_global & mediane_global < moyenne_global, "OUI", "NON")))

fig01 <- ggplot(df_parcelles |> filter(!is.na(sup_ha), sup_ha > 0), aes(x=sup_ha)) +
  geom_histogram(bins=50, fill=col_fao, color="white", alpha=0.85) +
  scale_x_log10(labels=label_number(decimal.mark=","), breaks=c(0.001,0.01,0.1,1,10,50)) +
  geom_vline(xintercept=mode_global, linetype="dotted", color="#1565C0", linewidth=0.8) +
  geom_vline(xintercept=mediane_global, linetype="dashed", color=col_foret, linewidth=0.8) +
  geom_vline(xintercept=moyenne_global, linetype="solid", color="#E53935", linewidth=0.7) +
  annotate("text", x=mode_global*1.8, y=Inf, vjust=2.5,
           label=paste0("Mode = ",round(mode_global,2)," ha"),
           color="#1565C0", size=2.8, fontface="bold") +
  annotate("text", x=mediane_global*1.5, y=Inf, vjust=4,
           label=paste0("Médiane = ",round(mediane_global,2)," ha"),
           color=col_foret, size=2.8, fontface="bold") +
  annotate("text", x=moyenne_global*1.5, y=Inf, vjust=5.5,
           label=paste0("Moyenne = ",round(moyenne_global,2)," ha"),
           color="#E53935", size=2.8, fontface="bold") +
  labs(title="Distribution de la superficie des parcelles (échelle log)",
       subtitle=paste0("n = ",format(sum(!is.na(df_parcelles$sup_ha)),big.mark=" "),
                       " | Asymétrie droite (log-normale)"),
       x="Superficie (ha, échelle log)", y="Effectif", caption=source_ghs)
ggsave(here("outputs","figures","fig01_histogramme_superficie.png"), fig01, width=8, height=4.5, dpi=300)


# Fig 2 : Boxplot par milieu
fig02 <- ggplot(df_parcelles |> filter(!is.na(sup_ha), !is.na(milieu)),
                aes(x=milieu, y=sup_ha, fill=milieu)) +
  geom_boxplot(alpha=0.75, outlier.size=0.4, outlier.alpha=0.3, width=0.5) +
  scale_y_log10(labels=label_number(decimal.mark=",")) +
  scale_fill_manual(values=c(Urbain=col_urbain, Rural=col_rural)) +
  labs(title="Superficie des parcelles par milieu de résidence",
       subtitle="Échelle logarithmique",
       x=NULL, y="Superficie (ha, échelle log)", caption=source_ghs) +
  theme(legend.position="none")
ggsave(here("outputs","figures","fig02_boxplot_superficie.png"), fig02, width=5, height=4.5, dpi=300)

# Fig 3 : Scatter GPS vs déclarée
df_comp <- df_parcelles |>
  filter(!is.na(gps_pp_ha), !is.na(sup_declaree_ha),
         gps_pp_ha > 0, sup_declaree_ha > 0, gps_pp_ha < 20, sup_declaree_ha < 20)

spearman_gps <- cor.test(df_comp$sup_declaree_ha, df_comp$gps_pp_ha, method="spearman")
cat(sprintf("Spearman GPS vs déclarée : rho = %.3f\n", spearman_gps$estimate))

fig03 <- ggplot(df_comp, aes(x=sup_declaree_ha, y=gps_pp_ha)) +
  geom_point(alpha=0.25, size=0.8, color=col_fao) +
  geom_abline(slope=1, intercept=0, color=col_gps, linewidth=0.9, linetype="dashed") +
  geom_smooth(method="loess", se=FALSE, color=col_foret, linewidth=0.7) +
  scale_x_log10(labels=label_number(decimal.mark=",")) +
  scale_y_log10(labels=label_number(decimal.mark=",")) +
  annotate("text", x=0.01, y=15,
           label=paste0("rho = ",round(spearman_gps$estimate,3),"\nn = ",
                        format(nrow(df_comp),big.mark=" ")),
           hjust=0, size=3.2, color="grey30") +
  labs(title="Superficie déclarée vs. GPS (Post-Planting)",
       subtitle="Log-log | Orange = concordance parfaite | Vert = loess",
       x="Déclarée (ha)", y="GPS (ha)", caption=source_ghs)
ggsave(here("outputs","figures","fig03_scatter_gps_declaree.png"), fig03, width=7, height=6, dpi=300)

# Combiné 1-3
fig_combined <- (fig01 | fig02) / fig03 +
  plot_annotation(title="Tâches 19-20 : Superficie des parcelles agricoles", caption=source_ghs)
ggsave(here("outputs","figures","fig_01_02_03_combined.png"), fig_combined, width=14, height=10, dpi=200)

# ============================================================
# 21 . Tenure foncière
# ============================================================
tenure_pondere <- plan_parcelles |> filter(!is.na(mode_acquisition)) |>
  group_by(mode_acquisition) |>
  summarise(effectif=survey_total(vartype="ci"), proportion=survey_mean(vartype="ci")) |>
  arrange(desc(proportion))
cat("Tenure pondérée :\n"); print(tenure_pondere)

# Barplot
tenure_plot <- df_parcelles |> filter(!is.na(mode_acquisition)) |>
  count(mode_acquisition, wt=wt_wave4, name="n_pond") |>
  mutate(prop=n_pond/sum(n_pond), pct_lbl=paste0(round(prop*100,1),"%")) |> arrange(desc(prop))

palette_tenure <- c("Héritage familial"=col_informel, "Achat"=col_propriete,
  "Distribution communautaire"=col_fao, "Location"=col_location,
  "Gratuit"=col_clair, "Métayage"=col_metayage, "Échange temporaire"="#1189e0")

fig04 <- ggplot(tenure_plot, aes(x=reorder(mode_acquisition,prop), y=prop, fill=mode_acquisition)) +
  geom_col(alpha=0.9, width=0.65) +
  geom_text(aes(label=pct_lbl), hjust=-0.1, size=3.2) +
  coord_flip() +
  scale_y_continuous(labels=percent_format(accuracy=1), expand=expansion(mult=c(0,0.15))) +
  scale_fill_manual(values=palette_tenure) +
  labs(title="Mode d'acquisition des parcelles (pondéré)",
       subtitle="wt_wave4 | s11b1q4", x=NULL, y="Proportion (%)", caption=source_ghs) +
  theme(legend.position="none")
ggsave(here("outputs","figures","fig04_tenure_barplot.png"), fig04, width=8, height=4.5, dpi=300)

# Chi-deux pondéré
chi2_tenure_svy <- svychisq(~ mode_acquisition + milieu, design = plan_parcelles)
cat("Chi-deux Rao-Scott :\n"); print(chi2_tenure_svy)

# V de Cramér (non pondéré)
tab_tm <- table(df_parcelles$mode_acquisition, df_parcelles$milieu)
chi2_np <- chisq.test(tab_tm)
v_cramer_tenure <- sqrt(chi2_np$statistic / (sum(tab_tm) * (min(dim(tab_tm)) - 1)))
cat(sprintf("V de Cramér : %.3f\n", v_cramer_tenure))

# Tableau croisé tenure x zone 
tab_tenure_zone <- df_parcelles |>
  filter(!is.na(mode_acquisition), !is.na(zone_f)) |>
  count(zone_f, mode_acquisition, wt = wt_wave4, name = "n_pond") |>
  group_by(zone_f) |>
  mutate(pct = n_pond / sum(n_pond)) |>
  ungroup() |>
  select(mode_acquisition, zone_f, pct) |>
  pivot_wider(names_from = zone_f, values_from = pct, values_fill = 0)

tab_tz_ft <- tab_tenure_zone |>
  mutate(across(where(is.numeric), ~ round(. * 100, 1))) |>
  flextable() |> set_caption("Mode d'acquisition par zone (%, pondérées)") |> style_lsms()
saveRDS(tab_tz_ft, here("data","processed","tab_tenure_zone_ft.rds"))

# Tableau gtsummary désagrégé par sexe
tab_tenure_sexe <- plan_parcelles |>
  # On filtre les observations ou le milieu et le sexe du gestionnaire sont renseignés
  srvyr::filter(!is.na(milieu), !is.na(sexe_gest_f)) |>
  
  # tbl_strata : stratifie le tableau par milieu (Urbain / Rural)
  # Ca crée deux sous-tableaux cote à cote, un pour chaque strate
  tbl_strata(
    strata = milieu,
    # Pour chaque strate, on applique tbl_svysummary
    .tbl_fun = ~ tbl_svysummary(
      data = .x,                          # .x = le sous-plan filtre sur une strate
      by = sexe_gest_f,                   # colonnes = Homme / Femme (gestionnaire)
      include = c(sup_ha, mode_acquisition),  # les deux variables à croiser
      label = list(
        sup_ha           ~ "Superficie (ha)",
        mode_acquisition ~ "Mode d'acquisition"
      ),
      # sup_ha (continue) : médiane [Q1 ; Q3]
      # mode_acquisition (catégorielle) : effectif (%)
      statistic = list(
        all_continuous()  ~ "{median} [{p25} ; {p75}]",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(
        all_continuous()  ~ 2,       
        all_categorical() ~ 0       
      ),
      missing = "ifany",             # affiche les NA seulement s'il y en a
      missing_text = "Non renseigné"
    )
  ) |>
  # Reformate les en-tetes de colonnes
  modify_header(
    label ~ "Variable",
    all_stat_cols() ~ "**{level}**\nn = {n}"  
    # {level} = Homme ou Femme, {n} = effectif pondéré de la strate
  )
tab_tenure_sexe |> as_gt() |> gt::gtsave(here("outputs","tables","tab_tenure_sexe_milieu.html"))
tab_tenure_sexe_ft <- tab_tenure_sexe |> as_flex_table() |> style_lsms()
saveRDS(tab_tenure_sexe_ft, here("data","processed","tab_tenure_sexe_ft.rds"))

tab_tenure_chef <- plan_parcelles |>
  srvyr::filter(!is.na(milieu), !is.na(sexe_chef_f)) |>
  tbl_strata(strata=milieu, .tbl_fun = ~ tbl_svysummary(
    data=.x, by=sexe_chef_f, include=c(sup_ha, mode_acquisition),
    label=list(sup_ha~"Superficie (ha)", mode_acquisition~"Mode d'acquisition"),
    statistic=list(all_continuous()~"{median} [{p25};{p75}]", all_categorical()~"{n} ({p}%)"),
    digits=list(all_continuous()~2, all_categorical()~0),
    missing="ifany", missing_text="Non renseigné")) |>
  modify_header(label~"Variable", all_stat_cols()~"**{level}**\nn = {n}")
saveRDS(tab_tenure_chef |> as_flex_table() |> style_lsms(),
        here("data","processed","tab_tenure_chef_ft.rds"))

# ============================================================
# 23 . Nb parcelles vs superficie totale
# ============================================================
fig05 <- ggplot(sup_menage, aes(x=nb_parcelles, y=sup_totale_ha)) +
  geom_jitter(alpha=0.25, size=0.8, color=col_fao, width=0.15) +
  geom_smooth(method="loess", se=TRUE, color=col_foret, fill=col_clair, alpha=0.3) +
  scale_y_log10(labels=label_number(decimal.mark=",")) +
  labs(title="Nombre de parcelles et superficie totale du ménage",
       subtitle="Loess (régression locale) avec IC 95%",
       x="Nombre de parcelles", y="Superficie totale (ha, échelle log)", caption=source_ghs)
ggsave(here("outputs","figures","fig05_scatter_nb_sup.png"), fig05, width=7, height=5.5, dpi=300)

spearman_nb <- cor.test(sup_menage$nb_parcelles, sup_menage$sup_totale_ha, method="spearman")
set.seed(2070)
rho_boot <- replicate(1000, {
  idx <- sample(nrow(sup_menage), replace=TRUE)
  cor(sup_menage$nb_parcelles[idx], sup_menage$sup_totale_ha[idx], method="spearman", use="complete.obs")
})
ic_rho <- quantile(rho_boot, c(0.025, 0.975))
cat(sprintf("Spearman nb vs sup : rho=%.3f, IC[%.3f;%.3f]\n", spearman_nb$estimate, ic_rho[1], ic_rho[2]))

# ============================================================
# 24 . Carte choroplèthe
# ============================================================
sup_etat <- df_parcelles |> filter(!is.na(sup_ha), !is.na(state_name)) |>
  group_by(state_name) |>
  summarise(n_parcelles=n(), sup_mediane=median(sup_ha), sup_moyenne=mean(sup_ha), .groups="drop")

nigeria_sf <- ne_states(country="Nigeria", returnclass="sf") |>
  mutate(state_name=case_when(
    name=="Federal Capital Territory"~"FCT",
    name=="Nassarawa"~"Nasarawa",
    TRUE~name))

carte_data <- nigeria_sf |> left_join(sup_etat, by="state_name")
centroides <- carte_data |> st_centroid() |>
  mutate(lon=st_coordinates(geometry)[,1], lat=st_coordinates(geometry)[,2])

fig06 <- ggplot(carte_data) +
  geom_sf(aes(fill=sup_mediane), color="white", linewidth=0.3) +
  geom_text(data=centroides |> filter(!is.na(sup_mediane)),
            aes(x=lon, y=lat, label=paste0(state_name,"\n",round(sup_mediane,2)," ha")),
            size=3.2, color="white", lineheight=0.99, check_overlap=TRUE) +
  scale_fill_gradient(low=col_clair, high=col_foret, na.value="grey90",
    name="Médiane (ha)", labels=label_number(decimal.mark=",",accuracy=0.01)) +
  labs(title="Superficie médiane par État", subtitle="..",
       caption=source_ghs) +
  theme_void(base_size=10) +
  theme(plot.title=element_text(size=12,face="bold"), legend.position="right")
ggsave(here("outputs","figures","fig06_carte_superficie.png"), fig06, width=12, height=9, dpi=300)

# Carte choroplèthe par région
sup_region <- df_parcelles |>
  filter(!is.na(sup_ha), !is.na(zone_f)) |>
  group_by(zone_f) |>
  summarise(n_parcelles=n(), sup_mediane=median(sup_ha),
            sup_moyenne=mean(sup_ha), .groups="drop")

cat("Superficie médiane par région :\n"); print(sup_region)

# Construire un sf agrégé par zone à partir des états
carte_data <- carte_data |>
  mutate(zone_f = case_when(
    state_name %in% c("Benue","Kogi","Kwara","Nasarawa","Niger","Plateau","FCT") ~ "North Central",
    state_name %in% c("Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe") ~ "North East",
    state_name %in% c("Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara") ~ "North West",
    state_name %in% c("Abia","Anambra","Ebonyi","Enugu","Imo") ~ "South East",
    state_name %in% c("Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers") ~ "South South",
    state_name %in% c("Ekiti","Lagos","Ogun","Ondo","Osun","Oyo") ~ "South West",
    TRUE ~ NA_character_
  ))

region_sf <- carte_data |>
  filter(!is.na(zone_f)) |>
  group_by(zone_f) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  left_join(sup_region, by = "zone_f")

centroides_reg <- region_sf |>
  st_centroid() |>
  mutate(lon=st_coordinates(geometry)[,1], lat=st_coordinates(geometry)[,2])

fig08 <- ggplot(region_sf) +
  geom_sf(aes(fill=sup_mediane), color="white", linewidth=0.5) +
  geom_text(data=centroides_reg,
            aes(x=lon, y=lat,
                label=paste0(zone_f,"\n",round(sup_mediane,2)," ha\n(n=",
                             format(n_parcelles,big.mark=" "),")")),
            size=3, color="white", lineheight=0.85, fontface="bold") +
  scale_fill_gradient(low=col_clair, high=col_foret, na.value="grey90",
                      name="Médiane (ha)", labels=label_number(decimal.mark=",",accuracy=0.01)) +
  labs(title="Superficie médiane des parcelles par zone géopolitique",
       subtitle="6 régions du Nigéria",
       caption=source_ghs) +
  theme_void(base_size=10) +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=9,color="grey40"),
        legend.position="right")
ggsave(here("outputs","figures","fig08_carte_region.png"), fig08, width=10, height=8, dpi=300)
cat("fig08 (carte par région) exportée\n")

saveRDS(sup_region, here("data","processed","sup_region.rds"))

# Boxplots par zone à facettes
fig07 <- ggplot(df_parcelles |> filter(!is.na(sup_ha),!is.na(milieu),!is.na(zone_f)),
                aes(x=milieu, y=sup_ha, fill=milieu)) +
  geom_boxplot(alpha=0.75, outlier.size=0.4, width=0.55) +
  stat_summary(fun=mean, geom="point", shape=17, size=2.5, color="#E53935") +
  scale_y_log10(labels=label_number(decimal.mark=",")) +
  scale_fill_manual(values=c(Urbain=col_urbain, Rural=col_rural)) +
  facet_wrap(~zone_f, nrow=2, ncol=3) +
  labs(title="Superficie par zone et milieu",
       subtitle="Triangle = moyenne | Échelle log",
       x=NULL, y="Superficie (ha)", caption=source_ghs) +
  theme(legend.position="bottom", strip.text=element_text(face="bold",size=9))
ggsave(here("outputs","figures","fig07_boxplot_zone_milieu.png"), fig07, width=10, height=7, dpi=300)

# Tableau gtsummary global
tab_parcelles <- plan_parcelles |> srvyr::filter(!is.na(milieu)) |>
  tbl_svysummary(by=milieu,
    include=c(sup_ha, mode_acquisition, cultive_f, sexe_gest_f, sexe_chef_f, source_sup),
    label=list(sup_ha~"Superficie (ha)", mode_acquisition~"Mode d'acquisition",
               cultive_f~"Cultivée", sexe_gest_f~"Sexe gestionnaire",
               sexe_chef_f~"Sexe chef ménage", source_sup~"Source superficie"),
    statistic=list(all_continuous()~"{median} [{p25};{p75}]", all_categorical()~"{n} ({p}%)"),
    digits=list(all_continuous()~2, all_categorical()~0),
    missing="ifany", missing_text="Non renseigné") |>
  add_overall(last=FALSE) |> add_p() |>
  modify_header(label~"Variable", all_stat_cols()~"**{level}**\nn = {n}")

tab_parcelles |> as_gt() |> gt::gtsave(here("outputs","tables","tab_gtsummary_parcelles.html"))
saveRDS(tab_parcelles |> as_flex_table() |> style_lsms(), here("data","processed","tab_parcelles_word.rds"))
tab_parcelles |> as_tibble() |> write.csv(here("outputs","tables","tab_gtsummary_parcelles.csv"), row.names=FALSE)

# Sauvegarde objets
saveRDS(stats_sup,        here("data","processed","stats_sup.rds"))
saveRDS(stats_sup_milieu, here("data","processed","stats_sup_milieu.rds"))
saveRDS(stats_sup_zone,   here("data","processed","stats_sup_zone.rds"))
saveRDS(stats_menage,     here("data","processed","stats_menage.rds"))
saveRDS(deciles_sup,      here("data","processed","deciles_sup.rds"))
saveRDS(tenure_pondere,   here("data","processed","tenure_pondere.rds"))
saveRDS(sup_etat,         here("data","processed","sup_etat.rds"))
saveRDS(list(chi2_svy=chi2_tenure_svy, v_cramer=v_cramer_tenure),
        here("data","processed","tests_tenure.rds"))
saveRDS(list(spearman=spearman_nb, ic_boot=ic_rho),
        here("data","processed","spearman_nb_sup.rds"))
saveRDS(list(spearman_gps=spearman_gps), here("data","processed","spearman_gps.rds"))
saveRDS(list(mode=mode_global, moyenne=moyenne_global, mediane=mediane_global),
        here("data","processed","stats_histo.rds"))

cat("\nScript analyses terminé\n")
