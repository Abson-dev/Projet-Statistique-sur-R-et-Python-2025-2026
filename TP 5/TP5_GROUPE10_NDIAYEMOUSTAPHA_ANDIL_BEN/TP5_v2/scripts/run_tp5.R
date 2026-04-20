# =============================================================================
# TP5 - Cultures Pratiquées, Intrants Utilisés et Rendements Agricoles
# Nigeria General Household Survey Panel - Wave 4 (2018-2019)
# ENSAE ISE1 | 2025-2026
# Auteur : Étudiant ENSAE ISE1
# =============================================================================

# ---- 0. AUTO-DÉTECTION DU RÉPERTOIRE ----------------------------------------
tryCatch({
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
}, error = function(e) {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  script_dir <<- if (length(f) && nchar(f)) dirname(f) else getwd()
})
projet_dir <- if (basename(script_dir) == "scripts") dirname(script_dir) else script_dir
setwd(projet_dir)
cat("==> Répertoire projet :", projet_dir, "\n")

# ---- 1. PACKAGES -------------------------------------------------------------
pkgs <- c("haven","dplyr","tidyr","ggplot2","scales","patchwork","ggrepel",
          "viridis","rstatix","forcats","flextable","ggridges",
          "RColorBrewer","stringr","knitr")

new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]
if (length(new_pkgs) > 0) install.packages(new_pkgs, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE))
cat("==> Packages chargés\n")

# ---- 2. THÈME ET PALETTES ---------------------------------------------------
theme_ensae <- function(base_size=12) {
  theme_minimal(base_size=base_size) %+replace%
    theme(
      plot.title       = element_text(face="bold",size=14,hjust=0,color="#1a3a5c",margin=margin(b=8)),
      plot.subtitle    = element_text(size=10,hjust=0,color="#4a6080",margin=margin(b=10)),
      plot.caption     = element_text(size=8,color="#888888",hjust=1,margin=margin(t=8)),
      axis.title       = element_text(size=10,color="#333333"),
      axis.text        = element_text(size=9,color="#555555"),
      panel.grid.major = element_line(color="#e8e8e8",linewidth=0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face="bold",size=9),
      legend.text      = element_text(size=8),
      strip.text       = element_text(face="bold",size=10,color="#1a3a5c"),
      plot.background  = element_rect(fill="white",color=NA),
      panel.background = element_rect(fill="#fafafa",color=NA)
    )
}
theme_set(theme_ensae())

pal_type <- c("Céréales"="#e31a1c","Légumineuses"="#33a02c","Tubercules"="#ff7f00",
              "Cultures rente"="#6a3d9a","Légumes/fruits"="#1f78b4","Autres"="#b15928")
pal_zones<- c("#2c7bb6","#d7191c","#fdae61","#1a9641","#a6d96a","#762a83")

zone_labels  <- c("1"="North Central","2"="North East","3"="North West",
                   "4"="South East","5"="South South","6"="South West")
state_labels <- c(
  "1"="Abia","2"="Adamawa","3"="Akwa Ibom","4"="Anambra","5"="Bauchi",
  "6"="Bayelsa","7"="Benue","8"="Borno","9"="Cross River","10"="Delta",
  "11"="Ebonyi","12"="Edo","13"="Ekiti","14"="Enugu","15"="Gombe",
  "16"="Imo","17"="Jigawa","18"="Kaduna","19"="Kano","20"="Katsina",
  "21"="Kebbi","22"="Kogi","23"="Kwara","24"="Lagos","25"="Nasarawa",
  "26"="Niger","27"="Ogun","28"="Ondo","29"="Osun","30"="Oyo",
  "31"="Plateau","32"="Rivers","33"="Sokoto","34"="Taraba","35"="Yobe",
  "36"="Zamfara","37"="FCT"
)

# Dictionnaire cultures
classer_culture <- function(code) {
  case_when(
    code %in% c(1070,1080,1100,1110,2010,2280) ~ "Céréales",
    code %in% c(1010,1060,2020,2150,2220)       ~ "Légumineuses",
    code %in% c(1020,1040,1121,1122,1123,1124,2180,2181,9009,9012) ~ "Tubercules",
    code %in% c(1050,2040,2250,3040,3050,3060,3080,3110,3180,3190,3200,3230) ~ "Cultures rente",
    code %in% c(2050,2060,2070,2071,2080,2090,2120,2130,2141,2142,
                2190,2194,2260,2030,2160,2170,2210,2230) ~ "Légumes/fruits",
    TRUE ~ "Autres"
  )
}

noms_cult <- c("1010"="Niébé/Cowpea","1020"="Manioc","1040"="Cocoyam",
               "1050"="Coton","1060"="Arachide","1070"="Sorgho",
               "1080"="Maïs","1090"="Melon/Egusi","1093"="Pastèque",
               "1100"="Mil/Millet","1110"="Riz","1121"="Igname blanche",
               "1122"="Igname jaune","1123"="Igname d'eau","1124"="Igname à 3 feuilles",
               "2010"="Acha","2040"="Sésame","2120"="Gombo","2130"="Oignon",
               "2150"="Pois pigeon","2170"="Plantain","2181"="Patate douce",
               "2194"="Légumes verts","2220"="Soja","2260"="Tomate",
               "3040"="Cacao","3110"="Noix de cola","3160"="Mangue",
               "3170"="Orange","3180"="Huile de palme")

# ---- 3. IMPORT DES DONNÉES ---------------------------------------------------
cat("\n==> Chargement des données...\n")
secta3i   <- read_dta("data/secta3i_harvestw4.dta")
secta3ii  <- read_dta("data/secta3ii_harvestw4.dta")
secta11c2 <- read_dta("data/secta11c2_harvestw4.dta")
secta11c3 <- read_dta("data/secta11c3_harvestw4.dta")
secta     <- read_dta("data/secta_harvestw4.dta")
sect11b1  <- read_dta("data/sect11b1_plantingw4.dta")

cat(sprintf("  secta3i   : %d lignes\n", nrow(secta3i)))
cat(sprintf("  secta11c2 : %d lignes\n", nrow(secta11c2)))

# ---- 4. PRÉPARATION ----------------------------------------------------------
cat("\n==> Préparation des données...\n")

# Poids + géo
secta_w <- secta %>%
  mutate(hhid=as.character(hhid), wt=as.numeric(zap_labels(wt_wave4)),
         zone_num=as.numeric(zap_labels(zone)),
         state_num=as.numeric(zap_labels(state)),
         sector_n=as.numeric(zap_labels(sector)),
         sector_lbl=ifelse(sector_n==1,"Urbain",ifelse(sector_n==2,"Rural",NA)),
         zone_lbl=zone_labels[as.character(zone_num)],
         state_lbl=state_labels[as.character(state_num)]) %>%
  select(hhid,wt,sector_lbl,zone_lbl,state_lbl)

# Cultures par parcelle
crops <- secta3i %>%
  mutate(hhid=as.character(hhid),
         cropcode_n=as.numeric(zap_labels(cropcode)),
         type_cult=classer_culture(cropcode_n),
         crop_nom=coalesce(noms_cult[as.character(cropcode_n)], paste0("Cult_",cropcode_n)),
         qte_raw=as.numeric(zap_labels(sa3iq6i)),
         conv=as.numeric(zap_labels(sa3iq6_conv)),
         qte_kg=qte_raw * conv,
         recolte=as.numeric(zap_labels(sa3iq3))==1) %>%
  filter(!is.na(cropcode_n)) %>%
  left_join(secta_w, by="hhid")

# Intrants par parcelle
intrants <- secta11c2 %>%
  mutate(hhid=as.character(hhid),
         plotid=as.numeric(zap_labels(plotid)),
         eng_inorg = as.numeric(zap_labels(s11dq1a))==1,
         npk_use   = as.numeric(zap_labels(s11c2q36_1))==1,
         uree_use  = as.numeric(zap_labels(s11c2q36_2))==1,
         eng_org   = as.numeric(zap_labels(s11dq36))==1,
         herb_use  = as.numeric(zap_labels(s11c2q10))==1,
         pest_use  = as.numeric(zap_labels(s11c2q1))==1) %>%
  left_join(secta_w, by="hhid")

# Nombre de parcelles cultivées par ménage (pour calcul superficie)
n_parc <- sect11b1 %>%
  mutate(hhid=as.character(hhid)) %>%
  filter(as.numeric(zap_labels(s11b1q27))==1) %>%
  count(hhid, name="n_parc_tot")

# Rendements cultures céréalières
rend <- crops %>%
  filter(cropcode_n %in% c(1080,1100,1070), recolte, !is.na(qte_kg), qte_kg>0) %>%
  mutate(cult_nom=case_when(cropcode_n==1080~"Maïs",cropcode_n==1100~"Mil/Millet",
                            cropcode_n==1070~"Sorgho")) %>%
  left_join(n_parc, by="hhid") %>%
  mutate(n_parc_tot=replace_na(n_parc_tot, 2),
         # Superficie imputée (ha) : médiane GHS W4 = 0.54 ha / n_parcelles
         surf_ha=0.54/n_parc_tot,
         rend_kg_ha=qte_kg/surf_ha) %>%
  group_by(cult_nom) %>%
  mutate(q1=quantile(rend_kg_ha,.25,na.rm=TRUE), q3=quantile(rend_kg_ha,.75,na.rm=TRUE),
         iqr=q3-q1, outlier=rend_kg_ha<(q1-3*iqr)|rend_kg_ha>(q3+3*iqr)) %>%
  ungroup() %>% filter(!outlier)

# Jointure rendements × intrants
rend_int <- rend %>%
  left_join(intrants %>% select(hhid,plotid,eng_inorg,npk_use,uree_use),
            by=c("hhid","plotid")) %>%
  mutate(eng_inorg=replace_na(eng_inorg,FALSE),
         grp_eng=ifelse(eng_inorg,"Avec engrais chimique","Sans engrais chimique"))

cat(sprintf("  Cultures récoltées : %d lignes\n", sum(crops$recolte,na.rm=TRUE)))
cat(sprintf("  Rendements (après outliers) : %d lignes\n", nrow(rend)))

# ---- 5. FIGURE 1 : TOP 15 CULTURES (Tâche 25) --------------------------------
cat("\n==> Top 15 cultures...\n")

top15 <- crops %>%
  filter(recolte) %>%
  count(cropcode_n, crop_nom, type_cult) %>%
  arrange(desc(n)) %>%
  slice_head(n=15) %>%
  mutate(pct=round(n/sum(n)*100,1),
         crop_nom=factor(crop_nom, levels=crop_nom[order(n)]))

cat("  Top 5 cultures :\n")
print(top15 %>% select(crop_nom, type_cult, n, pct) %>% head(5))

p1 <- ggplot(top15, aes(x=n, y=crop_nom, fill=type_cult)) +
  geom_col(width=0.76, color="white", linewidth=0.35) +
  geom_text(aes(label=paste0(format(n, big.mark=" "), "  (",pct,"%)")),
            hjust=-0.04, size=3.0, fontface="bold", color="#222222") +
  scale_fill_manual(values=pal_type, name="Type de culture") +
  scale_x_continuous(expand=expansion(mult=c(0,0.38)),
                     labels=scales::label_number(big.mark=" ")) +
  labs(title="Les 15 cultures les plus fréquemment pratiquées au Nigeria",
       subtitle="GHS Panel Wave 4 (2018-2019) — Nombre de parcelles récoltées par culture",
       x="Nombre de parcelles récoltées", y=NULL,
       caption="Source : secta3i_harvestw4 | Nigeria GHS W4") +
  guides(fill=guide_legend(nrow=2, override.aes=list(size=4)))

ggsave("outputs/fig01_top15_cultures.png", p1, width=12, height=8, dpi=300, bg="white")
cat("  -> fig01 sauvegardé\n")

# ---- 6. FIGURE 2 : DIVERSIFICATION CULTURALE (Tâche 26) --------------------
cat("\n==> Diversification culturale...\n")

divers <- crops %>%
  filter(recolte) %>%
  group_by(hhid, sector_lbl, zone_lbl) %>%
  summarise(n_cult=n_distinct(cropcode_n,na.rm=TRUE),
            n_types=n_distinct(type_cult,na.rm=TRUE), .groups="drop") %>%
  filter(!is.na(sector_lbl))

desc_div <- divers %>%
  group_by(sector_lbl) %>%
  summarise(N=n(), Med=median(n_cult), Moy=round(mean(n_cult),2),
            SD=round(sd(n_cult),2), .groups="drop")
cat("\nDiversification par milieu :\n"); print(desc_div)

wt_div <- wilcox_test(divers, n_cult~sector_lbl)
r_div  <- abs(qnorm(as.numeric(wt_div$p)/2))/sqrt(nrow(divers))
cat(sprintf("  Wilcoxon W=%s p=%s r=%.3f\n",
            wt_div$statistic, format.pval(as.numeric(wt_div$p),3), r_div))

p2 <- ggplot(divers, aes(x=sector_lbl, y=n_cult, fill=sector_lbl)) +
  geom_violin(alpha=0.65, trim=TRUE, color=NA) +
  geom_boxplot(width=0.13, alpha=0.92, outlier.shape=NA, linewidth=0.5, color="#333333") +
  stat_summary(fun=median, geom="point", shape=21, fill="gold", color="#333333", size=3.8) +
  geom_text(data=desc_div, aes(x=sector_lbl, y=Med+0.4,
                                label=paste0("Méd. = ",Med)),
            size=3.3, fontface="bold", color="#222222") +
  annotate("text", x=1.5, y=max(divers$n_cult)*0.88,
           label=paste0("Wilcoxon\np ",
                        ifelse(as.numeric(wt_div$p)<0.001,"< 0,001",
                               paste0("= ",round(as.numeric(wt_div$p),3))),
                        " | r = ",round(r_div,3)),
           size=3.4, color="#444444", fontface="italic") +
  scale_fill_manual(values=c("Rural"="#2c7bb6","Urbain"="#d7191c"), guide="none") +
  labs(title="Diversification culturale : Rural vs Urbain",
       subtitle="Nombre de cultures distinctes par ménage — Saison 2018/2019",
       x=NULL, y="Nombre de cultures distinctes",
       caption="Source : secta3i_harvestw4 | GHS W4")

ggsave("outputs/fig02_violin_diversification.png", p2, width=9, height=6, dpi=300, bg="white")
cat("  -> fig02 sauvegardé\n")

# Histogramme diversification
p2b <- divers %>%
  count(n_cult, sector_lbl) %>%
  group_by(sector_lbl) %>%
  mutate(pct=round(n/sum(n)*100,1)) %>% ungroup() %>%
  ggplot(aes(x=n_cult, y=pct, fill=sector_lbl)) +
  geom_col(position=position_dodge(.8), width=.72, color="white") +
  geom_text(aes(label=paste0(pct,"%")), position=position_dodge(.8),
            vjust=-0.4, size=2.9, fontface="bold") +
  scale_fill_manual(values=c("Rural"="#2c7bb6","Urbain"="#d7191c"), name="Milieu") +
  scale_x_continuous(breaks=1:max(divers$n_cult)) +
  scale_y_continuous(expand=expansion(mult=c(0,.15)),
                     labels=function(x) paste0(x,"%")) +
  labs(title="Distribution de l'indice de diversification culturale",
       subtitle="Proportion de ménages par nombre de cultures selon le milieu",
       x="Nombre de cultures distinctes", y="Proportion des ménages (%)",
       caption="Source : secta3i_harvestw4 | GHS W4")

ggsave("outputs/fig03_histo_diversification.png", p2b, width=10, height=6, dpi=300, bg="white")
cat("  -> fig03 sauvegardé\n")

# ---- 7. FIGURE 4 : INTRANTS PAR MILIEU + CI 95% (Tâche 27) -----------------
cat("\n==> Analyse des intrants...\n")

int_ci <- intrants %>%
  filter(!is.na(sector_lbl)) %>%
  pivot_longer(cols=c(eng_org,npk_use,uree_use,herb_use,pest_use),
               names_to="var", values_to="val") %>%
  mutate(intrant=recode(var, eng_org="Engrais\norganique", npk_use="NPK",
                        uree_use="Urée", herb_use="Herbicide", pest_use="Pesticide")) %>%
  group_by(Milieu=sector_lbl, intrant) %>%
  summarise(taux=mean(val,na.rm=TRUE)*100, n=sum(!is.na(val)), .groups="drop") %>%
  mutate(se=sqrt(taux/100*(1-taux/100)/n)*100,
         ci_lo=pmax(taux-1.96*se,0), ci_hi=pmin(taux+1.96*se,100))

p4 <- ggplot(int_ci, aes(x=intrant, y=taux, fill=Milieu)) +
  geom_col(position=position_dodge(.82), width=.72, color="white") +
  geom_errorbar(aes(ymin=ci_lo,ymax=ci_hi),
                position=position_dodge(.82), width=.28, linewidth=.7, color="#333333") +
  geom_text(aes(label=paste0(round(taux,1),"%"), y=ci_hi+.8),
            position=position_dodge(.82), size=2.9, fontface="bold", vjust=0) +
  scale_fill_manual(values=c("Rural"="#2c7bb6","Urbain"="#d7191c"), name="Milieu") +
  scale_y_continuous(expand=expansion(mult=c(0,.18)),
                     labels=function(x) paste0(x,"%")) +
  labs(title="Taux d'utilisation des intrants agricoles selon le milieu",
       subtitle="Proportions avec intervalles de confiance à 95% — GHS W4",
       x=NULL, y="Taux d'utilisation (%)",
       caption="Source : secta11c2_harvestw4 | IC = méthode de Wilson")

ggsave("outputs/fig04_intrants_milieu_IC.png", p4, width=11, height=6, dpi=300, bg="white")
cat("  -> fig04 sauvegardé\n")

# Test chi-deux zone × engrais inorganique
mat_chi <- intrants %>%
  filter(!is.na(sector_lbl)) %>%
  count(sector_lbl, eng_inorg) %>%
  pivot_wider(names_from=eng_inorg, values_from=n, values_fill=0) %>%
  select(-sector_lbl) %>% as.matrix()
if (all(dim(mat_chi)>=2)) {
  chi2_int <- chisq.test(mat_chi)
  vc_int   <- sqrt(chi2_int$statistic/(sum(mat_chi)*(min(dim(mat_chi))-1)))
  cat(sprintf("  Chi² intrant: X²=%.2f p=%s V=%.3f\n",
              chi2_int$statistic, format.pval(chi2_int$p.value,3), vc_int))
}

# ---- 8. FIGURE 5 : HEATMAP INTRANTS PAR ÉTAT ---------------------------------
int_state <- intrants %>%
  filter(!is.na(state_lbl)) %>%
  group_by(state_lbl) %>%
  summarise(engrais_inorg=round(mean(eng_inorg,na.rm=TRUE)*100,1),
            NPK=round(mean(npk_use,na.rm=TRUE)*100,1),
            Uree=round(mean(uree_use,na.rm=TRUE)*100,1),
            Herbicide=round(mean(herb_use,na.rm=TRUE)*100,1),
            n=n(), .groups="drop") %>%
  filter(n>=10) %>%
  pivot_longer(-c(state_lbl,n), names_to="Intrant", values_to="Taux")

p5 <- ggplot(int_state, aes(x=Intrant, y=fct_reorder(state_lbl, Taux, max), fill=Taux)) +
  geom_tile(color="white", linewidth=.4) +
  geom_text(aes(label=paste0(Taux,"%")), size=2.5, fontface="bold",
            color=ifelse(int_state$Taux>25,"white","#333333")) +
  scale_fill_viridis_c(option="YlGnBu", name="Taux (%)", limits=c(0,NA)) +
  labs(title="Taux d'adoption des intrants agricoles par État",
       subtitle="GHS W4 — Nigeria 2018-2019 | États avec ≥10 parcelles",
       x=NULL, y="État",
       caption="Source : secta11c2_harvestw4 | GHS W4") +
  theme(axis.text.y=element_text(size=7))

ggsave("outputs/fig05_heatmap_intrants_etat.png", p5, width=10, height=14, dpi=300, bg="white")
cat("  -> fig05 sauvegardé\n")

# ---- 9. FIGURE 6 : BOXPLOT RENDEMENTS PAR ZONE (Tâche 28) ------------------
cat("\n==> Analyse des rendements...\n")

n_out <- sum(rend$outlier, na.rm=TRUE)
cat(sprintf("  Outliers exclus : %d\n", n_out))

desc_rend <- rend %>%
  group_by(cult_nom) %>%
  summarise(N=n(), Min=round(min(rend_kg_ha),0), Q1=round(quantile(rend_kg_ha,.25),0),
            Med=round(median(rend_kg_ha),0), Moy=round(mean(rend_kg_ha),0),
            Q3=round(quantile(rend_kg_ha,.75),0), Max=round(max(rend_kg_ha),0),
            .groups="drop")
cat("\nRendements (kg/ha) :\n"); print(desc_rend)
write.csv(desc_rend, "outputs/tab_rendements_descriptifs.csv", row.names=FALSE)

p6 <- rend %>%
  filter(!is.na(zone_lbl)) %>%
  ggplot(aes(x=fct_reorder(zone_lbl, rend_kg_ha, median), y=rend_kg_ha, fill=cult_nom)) +
  geom_boxplot(alpha=0.75, outlier.shape=21, outlier.size=1, outlier.alpha=0.3,
               color="#555555", linewidth=.4) +
  scale_fill_manual(values=c("Maïs"="#e31a1c","Mil/Millet"="#ff7f00","Sorgho"="#33a02c"),
                    name="Culture") +
  scale_y_continuous(labels=scales::label_number(big.mark=" ", suffix=" kg/ha")) +
  facet_wrap(~cult_nom, ncol=3, scales="free_y") +
  coord_flip() +
  labs(title="Rendements agricoles par culture et zone géopolitique",
       subtitle="Boxplots (Outliers IQR×3 exclus) — Superficie imputée : 0,54 ha / n_parcelles",
       x=NULL, y="Rendement (kg/ha)",
       caption="Source : secta3i_harvestw4 | sect11b1 | GHS W4") +
  theme(legend.position="none", strip.text=element_text(size=11,face="bold"))

ggsave("outputs/fig06_rendements_zone.png", p6, width=13, height=7, dpi=300, bg="white")
cat("  -> fig06 sauvegardé\n")

# ---- 10. FIGURE 7 : RIDGE RENDEMENTS PAR ZONE --------------------------------
p7 <- rend %>%
  filter(!is.na(zone_lbl)) %>%
  ggplot(aes(x=log10(rend_kg_ha+1), y=fct_rev(fct_reorder(zone_lbl, rend_kg_ha, median)),
             fill=zone_lbl, color=zone_lbl)) +
  geom_density_ridges(alpha=0.65, scale=1.3, linewidth=.6,
                      quantile_lines=TRUE, quantiles=2) +
  scale_fill_manual(values=pal_zones, guide="none") +
  scale_color_manual(values=pal_zones, guide="none") +
  scale_x_continuous(name="Rendement (log₁₀ kg/ha)",
                     breaks=c(1,2,3,4), labels=c("10","100","1 000","10 000")) +
  facet_wrap(~cult_nom, ncol=3) +
  labs(title="Distribution des rendements par zone géopolitique",
       subtitle="Ridge plots — Ligne = médiane | Échelle logarithmique",
       y=NULL, caption="Source : secta3i_harvestw4 | GHS W4")

ggsave("outputs/fig07_ridge_rendements.png", p7, width=14, height=8, dpi=300, bg="white")
cat("  -> fig07 sauvegardé\n")

# ---- 11. FIGURE 8 : ENGRAIS × RENDEMENTS (Tâche 29) -------------------------
cat("\n==> Effet engrais sur rendements...\n")

meds_eng <- rend_int %>%
  filter(cult_nom=="Maïs", !is.na(rend_kg_ha)) %>%
  group_by(grp_eng) %>%
  summarise(med=round(median(rend_kg_ha),0), n=n(), .groups="drop")
cat("Médianes rendement Maïs par groupe engrais :\n"); print(meds_eng)

wt_rend <- rend_int %>%
  filter(cult_nom=="Maïs", !is.na(rend_kg_ha), !is.na(grp_eng)) %>%
  wilcox_test(rend_kg_ha~grp_eng)
r_rend <- abs(qnorm(as.numeric(wt_rend$p)/2)) /
          sqrt(nrow(rend_int %>% filter(cult_nom=="Maïs",!is.na(rend_kg_ha))))
cat(sprintf("  Wilcoxon W=%s p=%s r=%.3f\n",
            wt_rend$statistic, format.pval(as.numeric(wt_rend$p),3), r_rend))

gain_rend <- round((meds_eng$med[meds_eng$grp_eng=="Avec engrais chimique"] -
                    meds_eng$med[meds_eng$grp_eng=="Sans engrais chimique"]) /
                   meds_eng$med[meds_eng$grp_eng=="Sans engrais chimique"] * 100, 1)

p8 <- rend_int %>%
  filter(cult_nom=="Maïs", !is.na(rend_kg_ha), !is.na(grp_eng)) %>%
  ggplot(aes(x=grp_eng, y=rend_kg_ha, fill=grp_eng)) +
  geom_violin(alpha=.5, color=NA, trim=TRUE) +
  geom_boxplot(width=.13, alpha=.92, outlier.shape=NA, linewidth=.5, color="#333333") +
  stat_summary(fun=median, geom="point", shape=21, fill="gold", size=4, color="#333333") +
  geom_text(data=meds_eng, aes(x=grp_eng, y=med+180,
                                label=paste0("Méd. = ",format(med, big.mark=" ")," kg/ha\n(n=",n,")")),
            size=3.2, fontface="bold", color="#222222") +
  annotate("text", x=1.5, y=max(rend_int$rend_kg_ha[rend_int$cult_nom=="Maïs"],na.rm=TRUE)*0.82,
           label=paste0("Wilcoxon\np ",
                        ifelse(as.numeric(wt_rend$p)<0.001,"< 0,001",
                               paste0("= ",round(as.numeric(wt_rend$p),3))),
                        "\nr = ",round(r_rend,3),
                        "\nGain = +",gain_rend,"%"),
           size=3.4, color="#555555", fontface="italic") +
  scale_fill_manual(values=c("Avec engrais chimique"="#1a9641","Sans engrais chimique"="#c0392b"),
                    guide="none") +
  scale_y_continuous(labels=scales::label_number(big.mark=" ", suffix=" kg/ha")) +
  labs(title="Effet de l'engrais chimique sur le rendement du maïs",
       subtitle=paste0("Gain médian = +",gain_rend,"% | GHS W4"),
       x=NULL, y="Rendement (kg/ha)",
       caption="Source : secta3i + secta11c2 | GHS W4 | Outliers IQR×3 exclus")

ggsave("outputs/fig08_engrais_rendements_mais.png", p8, width=10, height=6.5, dpi=300, bg="white")
cat("  -> fig08 sauvegardé\n")

# ---- 12. FIGURE 9 : TYPES DE CULTURES PAR ZONE --------------------------------
p9 <- crops %>%
  filter(recolte, !is.na(zone_lbl)) %>%
  distinct(hhid, zone_lbl, type_cult) %>%
  count(zone_lbl, type_cult) %>%
  group_by(zone_lbl) %>%
  mutate(pct=n/sum(n)*100) %>% ungroup() %>%
  ggplot(aes(x=zone_lbl, y=pct, fill=type_cult)) +
  geom_col(position="fill", width=0.82, color="white") +
  geom_text(aes(label=ifelse(pct>5, paste0(round(pct,0),"%"), "")),
            position=position_fill(vjust=0.5), size=3, fontface="bold", color="white") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=pal_type, name="Type de culture") +
  coord_flip() +
  labs(title="Composition des systèmes de culture par zone géopolitique",
       subtitle="Part des ménages selon le type de culture pratiqué — GHS W4",
       x=NULL, y="Proportion des ménages",
       caption="Source : secta3i_harvestw4 | GHS W4")

ggsave("outputs/fig09_types_cultures_zone.png", p9, width=11, height=6, dpi=300, bg="white")
cat("  -> fig09 sauvegardé\n")

# ---- 13. FIGURE 10 : DASHBOARD RÉCAPITULATIF TP5 ----------------------------
cat("\n==> Dashboard récapitulatif...\n")

# Donut types cultures
donut_d <- crops %>%
  filter(recolte) %>%
  count(type_cult) %>%
  mutate(pct=round(n/sum(n)*100,1)) %>%
  arrange(desc(pct)) %>%
  mutate(ymax=cumsum(pct)/100, ymin=c(0,head(cumsum(pct)/100,-1)), lab_y=(ymax+ymin)/2)

p_donut <- ggplot(donut_d, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=type_cult)) +
  geom_rect(color="white", linewidth=0.7) +
  geom_text(aes(x=3.5, y=lab_y,
                label=paste0(pct,"%\n", str_wrap(type_cult,10))),
            size=2.8, fontface="bold", color="white") +
  coord_polar(theta="y") +
  scale_fill_manual(values=pal_type, guide="none") +
  xlim(c(0,4)) +
  labs(title="Types de cultures", subtitle="Part des parcelles récoltées") +
  theme_void() +
  theme(plot.title=element_text(face="bold",hjust=0.5,size=11,color="#1a3a5c"),
        plot.subtitle=element_text(hjust=0.5,size=9,color="#4a6080"))

# Barplot taux intrants national
int_nat <- data.frame(
  Intrant=c("Engrais\norganique","NPK","Urée","Herbicide","Pesticide"),
  Taux=c(mean(intrants$eng_org,na.rm=TRUE)*100,
         mean(intrants$npk_use,na.rm=TRUE)*100,
         mean(intrants$uree_use,na.rm=TRUE)*100,
         mean(intrants$herb_use,na.rm=TRUE)*100,
         mean(intrants$pest_use,na.rm=TRUE)*100)
)

p_int <- ggplot(int_nat, aes(x=fct_reorder(Intrant, Taux), y=Taux, fill=Intrant)) +
  geom_col(width=0.72, color="white") +
  geom_text(aes(label=paste0(round(Taux,1),"%")), hjust=-0.1, fontface="bold", size=3.2) +
  scale_fill_manual(values=c(
    "Engrais\norganique"="#7fbc41","NPK"="#4d9221","Urée"="#276419",
    "Herbicide"="#d01c8b","Pesticide"="#f1b6da"), guide="none") +
  scale_y_continuous(limits=c(0,100), expand=expansion(mult=c(0,.3)),
                     labels=function(x) paste0(x,"%")) +
  coord_flip() +
  labs(title="Adoption des intrants (national)",
       subtitle="% parcelles utilisatrices",
       x=NULL, y="%")

dash5 <- (p_donut | p_int) +
  plot_annotation(
    title  = "TABLEAU DE BORD — Cultures, Intrants et Rendements Agricoles du Nigeria",
    subtitle=paste0("GHS Panel Wave 4 (2018-2019) | ",
                    format(n_distinct(crops$hhid),big.mark=" ")," ménages | ",
                    "Engrais inorg. : ",round(mean(intrants$eng_inorg,na.rm=TRUE)*100,1),"% des parcelles"),
    caption="ENSAE ISE1 | 2025-2026 | Source : Nigeria GHS Panel — World Bank LSMS-ISA",
    theme=theme_ensae(12) +
          theme(plot.title=element_text(face="bold",size=15,color="#1a3a5c"),
                plot.subtitle=element_text(size=10,color="#4a6080"))
  )

ggsave("outputs/fig10_dashboard_cultures.png", dash5, width=14, height=7, dpi=300, bg="white")
cat("  -> fig10 (dashboard) sauvegardé\n")

# ---- 14. TABLEAUX CSV --------------------------------------------------------
cat("\n==> Création des tableaux...\n")

# Tab 1 : Top 15 cultures
write.csv(top15 %>% arrange(desc(n)) %>%
            select(Culture=crop_nom, Type=type_cult, N=n, Pct=pct),
          "outputs/tab01_top15_cultures.csv", row.names=FALSE)

# Tab 2 : Intrants par zone
tab_int_zone <- intrants %>%
  filter(!is.na(zone_lbl)) %>%
  group_by(Zone=zone_lbl) %>%
  summarise(N=n(),
            `Eng.Org(%)` =round(mean(eng_org,na.rm=TRUE)*100,1),
            `NPK(%)`     =round(mean(npk_use,na.rm=TRUE)*100,1),
            `Urée(%)`    =round(mean(uree_use,na.rm=TRUE)*100,1),
            `Herbicide(%)`=round(mean(herb_use,na.rm=TRUE)*100,1),
            `Pesticide(%)`=round(mean(pest_use,na.rm=TRUE)*100,1),
            .groups="drop")
write.csv(tab_int_zone, "outputs/tab02_intrants_par_zone.csv", row.names=FALSE)

# Tab 3 : Rendements descriptifs
write.csv(desc_rend, "outputs/tab03_rendements_descriptifs.csv", row.names=FALSE)

# Tab 4 : Tests statistiques
tab_tests <- data.frame(
  Test       = c("Wilcoxon div. culturale (Rural vs Urbain)",
                 "Chi-deux (engrais inorg. ~ milieu)",
                 "Wilcoxon rendement maïs (engrais vs sans)"),
  Stat       = c(as.character(wt_div$statistic), round(chi2_int$statistic,2),
                 as.character(wt_rend$statistic)),
  p_value    = c(format.pval(as.numeric(wt_div$p),3),
                 format.pval(chi2_int$p.value,3),
                 format.pval(as.numeric(wt_rend$p),3)),
  Taille_eff = c(round(r_div,3), round(vc_int,3), round(r_rend,3))
)
write.csv(tab_tests, "outputs/tab04_tests_statistiques.csv", row.names=FALSE)

cat("  -> Tableaux CSV sauvegardés\n")

# ---- 15. RÉSUMÉ FINAL --------------------------------------------------------
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  RÉSUMÉ TP5 — Cultures, Intrants et Rendements GHS W4\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(sprintf("  Ménages avec cultures récoltées  : %d\n", n_distinct(crops$hhid[crops$recolte])))
cat(sprintf("  Cultures distinctes              : %d\n", n_distinct(crops$cropcode_n)))
cat(sprintf("  Culture dominante                 : %s\n", as.character(top15$crop_nom[nrow(top15)])))
cat(sprintf("  Taux engrais inorganique          : %.1f%%\n", mean(intrants$eng_inorg,na.rm=TRUE)*100))
cat(sprintf("  Rendement médian maïs             : %d kg/ha\n",
            round(median(rend$rend_kg_ha[rend$cult_nom=="Maïs"],na.rm=TRUE))))
cat(sprintf("  Gain médian engrais chimique      : +%s%%\n", gain_rend))
cat("\n  Outputs produits :\n")
for (f in list.files("outputs")) cat(sprintf("    %s\n", f))
cat("═══════════════════════════════════════════════════════════════\n")
