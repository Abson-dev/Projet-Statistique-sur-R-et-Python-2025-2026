# =============================================================================
# TP4 - Analyse des Parcelles Agricoles
# Nigeria General Household Survey Panel - Wave 4 (2018-2019)
# ENSAE ISE1 | 2025-2026
# Auteur : Étudiant ENSAE ISE1
# =============================================================================

# ---- 0. AUTO-DÉTECTION DU RÉPERTOIRE (reproductible sur toute machine) ------
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

# ---- 1. PACKAGES --------------------------------------------------------------
pkgs <- c("haven","dplyr","tidyr","ggplot2","scales","patchwork","ggrepel",
          "viridis","rstatix","forcats","flextable","ggridges",
          "RColorBrewer","stringr","knitr")

new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installation : ", paste(new_pkgs, collapse=", "))
  install.packages(new_pkgs, repos="https://cloud.r-project.org", dependencies=TRUE)
}
invisible(lapply(pkgs, library, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE))
cat("==> Packages chargés\n")

# ---- 2. THÈME GRAPHIQUE GLOBAL -----------------------------------------------
theme_ensae <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(face="bold", size=14, hjust=0,
                                      color="#1a3a5c", margin=margin(b=8)),
      plot.subtitle    = element_text(size=10, hjust=0, color="#4a6080",
                                      margin=margin(b=10)),
      plot.caption     = element_text(size=8, color="#888888", hjust=1,
                                      margin=margin(t=8)),
      axis.title       = element_text(size=10, color="#333333"),
      axis.text        = element_text(size=9, color="#555555"),
      panel.grid.major = element_line(color="#e8e8e8", linewidth=0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face="bold", size=9),
      legend.text      = element_text(size=8),
      strip.text       = element_text(face="bold", size=10, color="#1a3a5c"),
      plot.background  = element_rect(fill="white", color=NA),
      panel.background = element_rect(fill="#fafafa", color=NA)
    )
}
theme_set(theme_ensae())

pal_zones  <- c("#2c7bb6","#d7191c","#fdae61","#1a9641","#a6d96a","#762a83")
pal_tenure <- c(
  "Héritage familial"      = "#1b7837",
  "Achat direct"           = "#2166ac",
  "Location (cash)"        = "#d6604d",
  "Prêt/Gratuit"           = "#f4a582",
  "Distribution communale" = "#92c5de",
  "Métayage"               = "#b2abd2",
  "Échange temporaire"     = "#fdbf6f"
)

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

# Fonction nettoyage tenure
clean_tenure <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    grepl("OUTRIGHT|PURCHASE",  x, ignore.case=TRUE) ~ "Achat direct",
    grepl("RENT|CASH",          x, ignore.case=TRUE) ~ "Location (cash)",
    grepl("FREE OF CHARGE",     x, ignore.case=TRUE) ~ "Prêt/Gratuit",
    grepl("COMMUN|DISTRIBUTED", x, ignore.case=TRUE) ~ "Distribution communale",
    grepl("INHERIT|FAMILY",     x, ignore.case=TRUE) ~ "Héritage familial",
    grepl("SHARE",              x, ignore.case=TRUE) ~ "Métayage",
    grepl("EXCHANGE|TEMPORARY", x, ignore.case=TRUE) ~ "Échange temporaire",
    grepl("^1$",                x)                   ~ "Achat direct",
    grepl("^2$",                x)                   ~ "Location (cash)",
    grepl("^3$",                x)                   ~ "Prêt/Gratuit",
    grepl("^4$",                x)                   ~ "Distribution communale",
    grepl("^5$",                x)                   ~ "Héritage familial",
    grepl("^6$",                x)                   ~ "Métayage",
    grepl("^7$",                x)                   ~ "Échange temporaire",
    TRUE ~ NA_character_
  )
}

# ---- 3. IMPORT DES DONNÉES ---------------------------------------------------
cat("\n==> Chargement des données...\n")

sect11b1 <- read_dta("data/sect11b1_plantingw4.dta")
secta    <- read_dta("data/secta_harvestw4.dta")
geo_plot <- read_dta("data/nga_plotgeovariables_y4.dta")

cat(sprintf("  sect11b1 : %d lignes, %d cols\n", nrow(sect11b1), ncol(sect11b1)))
cat(sprintf("  secta    : %d lignes, %d cols\n", nrow(secta),    ncol(secta)))
cat(sprintf("  geo_plot : %d lignes, %d cols\n", nrow(geo_plot), ncol(geo_plot)))

# ---- 4. PRÉPARATION -----------------------------------------------------------
cat("\n==> Nettoyage et préparation...\n")

# Poids et géographie
secta_w <- secta %>%
  mutate(
    hhid      = as.character(hhid),
    wt        = as.numeric(zap_labels(wt_wave4)),
    zone_num  = as.numeric(zap_labels(zone)),
    state_num = as.numeric(zap_labels(state)),
    sector_n  = as.numeric(zap_labels(sector)),
    sector_lbl= ifelse(sector_n == 1, "Urbain", ifelse(sector_n == 2, "Rural", NA)),
    zone_lbl  = zone_labels[as.character(zone_num)],
    state_lbl = state_labels[as.character(state_num)]
  ) %>%
  select(hhid, wt, zone_num, state_num, sector_lbl, zone_lbl, state_lbl)

# GPS distances
geo_c <- geo_plot %>%
  mutate(hhid=as.character(hhid), plotid=as.numeric(plotid))

# Parcelles
parc <- sect11b1 %>%
  mutate(
    hhid      = as.character(hhid),
    plotid    = as.numeric(zap_labels(plotid)),
    tenure_lbl= clean_tenure(as_factor(s11b1q4)),
    cultive   = as.numeric(zap_labels(s11b1q27)),
    irrigue_n = as.numeric(zap_labels(s11b1q39)),
    sol_q     = as.numeric(zap_labels(s11b1q45)),
    pente_q   = as.numeric(zap_labels(s11b1q46)),
    sector_n  = as.numeric(zap_labels(sector)),
    zone_num  = as.numeric(zap_labels(zone)),
    state_num = as.numeric(zap_labels(state)),
    sector_lbl= ifelse(sector_n==1,"Urbain", ifelse(sector_n==2,"Rural", NA)),
    zone_lbl  = zone_labels[as.character(zone_num)],
    state_lbl = state_labels[as.character(state_num)]
  ) %>%
  left_join(geo_c %>% select(hhid,plotid,dist_km=dist_household,srtm=srtm_nga,slope=srtmslp_nga),
            by=c("hhid","plotid")) %>%
  left_join(secta_w %>% select(hhid, wt), by="hhid")

# Ménages avec parcelles cultivées
menage <- parc %>%
  filter(cultive == 1) %>%
  group_by(hhid, zone_lbl, state_lbl, sector_lbl, wt) %>%
  summarise(n_parc = n(), dist_moy = mean(dist_km, na.rm=TRUE), .groups="drop")

cat(sprintf("  Parcelles totales        : %d\n", nrow(parc)))
cat(sprintf("  Ménages (parc. cultivées): %d\n", nrow(menage)))

# ---- 5. STATISTIQUES DESCRIPTIVES GÉNÉRALES (Tâche 19) ----------------------
cat("\n==> Statistiques descriptives...\n")

na_tab <- data.frame(
  Variable = c("Tenure foncière","Secteur","Cultivée","Irriguée","Qualité sol","Pente","Distance GPS"),
  N_obs    = nrow(parc),
  N_NA     = c(sum(is.na(parc$tenure_lbl)), sum(is.na(parc$sector_lbl)),
               sum(is.na(parc$cultive)), sum(is.na(parc$irrigue_n)),
               sum(is.na(parc$sol_q)), sum(is.na(parc$pente_q)),
               sum(is.na(parc$dist_km))),
  Pct_NA   = round(c(mean(is.na(parc$tenure_lbl)), mean(is.na(parc$sector_lbl)),
                     mean(is.na(parc$cultive)), mean(is.na(parc$irrigue_n)),
                     mean(is.na(parc$sol_q)), mean(is.na(parc$pente_q)),
                     mean(is.na(parc$dist_km)))*100, 1)
)
cat("\nAnalyse des valeurs manquantes :\n")
print(na_tab)
write.csv(na_tab, "outputs/tab_NA_parcelles.csv", row.names=FALSE)

# ---- 6. FIGURE 1 : Carte des NA (heatmap manuelle) --------------------------
na_plot_data <- data.frame(
  Variable = factor(na_tab$Variable, levels=rev(na_tab$Variable)),
  Pct_NA   = na_tab$Pct_NA
)

p1 <- ggplot(na_plot_data, aes(x=1, y=Variable, fill=Pct_NA)) +
  geom_tile(color="white", linewidth=0.8, width=0.6) +
  geom_text(aes(label=paste0(Pct_NA,"%")), size=3.5, fontface="bold",
            color=ifelse(na_plot_data$Pct_NA>20,"white","#333333")) +
  scale_fill_gradient2(low="#27ae60", mid="#f39c12", high="#e74c3c",
                       midpoint=30, name="% NA", limits=c(0,100)) +
  scale_x_continuous(breaks=NULL) +
  labs(title="Valeurs manquantes par variable — Module Parcelles GHS W4",
       subtitle="Les NA de tenure sont structurels (module CAPI non activé pour parcelles héritées)",
       x=NULL, y=NULL,
       caption="Source : sect11b1_plantingw4 | Nigeria GHS Panel Wave 4") +
  theme(legend.position="right", axis.text.y=element_text(size=10))

ggsave("outputs/fig01_carte_NA.png", p1, width=9, height=5, dpi=300, bg="white")
cat("  -> fig01 sauvegardé\n")

# ---- 7. FIGURE 2 : Tenure foncière - barplot horizontal (Tâche 21) ----------
tenure_freq <- parc %>%
  filter(!is.na(tenure_lbl)) %>%
  count(tenure_lbl) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  arrange(pct) %>%
  mutate(tenure_lbl = factor(tenure_lbl, levels=tenure_lbl))

p2 <- ggplot(tenure_freq, aes(x=pct, y=tenure_lbl, fill=tenure_lbl)) +
  geom_bar(stat="identity", width=0.72, color="white", linewidth=0.4) +
  geom_text(aes(label=paste0(pct,"% — n=",format(n, big.mark=" "))),
            hjust=-0.05, size=3.3, fontface="bold", color="#222222") +
  scale_fill_manual(values=pal_tenure, guide="none") +
  scale_x_continuous(expand=expansion(mult=c(0,0.45)),
                     labels=function(x) paste0(x,"%")) +
  labs(title="Régime de tenure foncière des parcelles agricoles au Nigeria",
       subtitle="GHS Panel Wave 4 (2018-2019) — Parcelles avec tenure renseignée",
       x="Proportion des parcelles (%)", y=NULL,
       caption=paste0("Source : sect11b1_plantingw4 | N=",sum(tenure_freq$n)," parcelles"))

ggsave("outputs/fig02_tenure_barplot.png", p2, width=11, height=6, dpi=300, bg="white")
cat("  -> fig02 sauvegardé\n")

# ---- 8. TEST CHI-DEUX : Tenure × Secteur (Tâche 21) -------------------------
cat("\n==> Test chi-deux Tenure × Secteur...\n")

ts_cross <- parc %>%
  filter(!is.na(tenure_lbl), !is.na(sector_lbl)) %>%
  count(tenure_lbl, sector_lbl) %>%
  pivot_wider(names_from=sector_lbl, values_from=n, values_fill=0)

mat <- as.matrix(ts_cross[,-1])
if (all(dim(mat) >= 2) && sum(mat) > 0) {
  chi2 <- chisq.test(mat)
  vc   <- sqrt(chi2$statistic / (sum(mat)*(min(dim(mat))-1)))
  cat(sprintf("  X² = %.3f | ddl = %d | p = %s | V Cramér = %.3f\n",
              chi2$statistic, chi2$parameter,
              format.pval(chi2$p.value, digits=3), vc))
}

# Heatmap Tenure × Secteur
ts_pct <- parc %>%
  filter(!is.na(tenure_lbl), !is.na(sector_lbl)) %>%
  count(tenure_lbl, sector_lbl) %>%
  group_by(sector_lbl) %>%
  mutate(pct=round(n/sum(n)*100,1)) %>% ungroup()

p3 <- ggplot(ts_pct, aes(x=sector_lbl, y=fct_reorder(tenure_lbl, pct, .fun=max), fill=pct)) +
  geom_tile(color="white", linewidth=0.9) +
  geom_text(aes(label=paste0(pct,"%")), size=4, fontface="bold",
            color=ifelse(ts_pct$pct>25,"white","#333333")) +
  scale_fill_gradient(low="#d4e6f1", high="#1a5276", name="% dans\nle milieu") +
  labs(title="Répartition de la tenure foncière selon le milieu de résidence",
       subtitle=paste0("Chi² = ",round(chi2$statistic,1),
                       " | V de Cramér = ",round(vc,3)," | p ",
                       ifelse(chi2$p.value<0.001,"< 0,001",
                              paste0("= ",round(chi2$p.value,3)))),
       x="Milieu", y=NULL,
       caption="Source : sect11b1_plantingw4 | GHS W4")

ggsave("outputs/fig03_tenure_milieu_heatmap.png", p3, width=9, height=7, dpi=300, bg="white")
cat("  -> fig03 sauvegardé\n")

# ---- 9. FIGURE 4 : Distribution des parcelles (Tâche 20) --------------------
n_max  <- quantile(menage$n_parc, 0.99)
desc_p <- menage %>%
  summarise(N=n(), Min=min(n_parc), Q1=quantile(n_parc,.25),
            Med=median(n_parc), Moy=round(mean(n_parc),2),
            Q3=quantile(n_parc,.75), Max=max(n_parc),
            SD=round(sd(n_parc),2), CV=round(sd(n_parc)/mean(n_parc)*100,1))
cat("\nStatistiques n_parcelles :\n"); print(desc_p)

cnt_parc <- menage %>%
  filter(n_parc <= n_max) %>%
  count(n_parc) %>%
  mutate(pct=round(n/nrow(menage)*100,1))

p4 <- ggplot(cnt_parc, aes(x=factor(n_parc), y=pct, fill=factor(n_parc))) +
  geom_col(width=0.78, color="white", linewidth=0.3) +
  geom_text(aes(label=paste0(pct,"%")), vjust=-0.4, size=3.2, fontface="bold", color="#333333") +
  scale_fill_viridis_d(option="plasma", guide="none") +
  scale_y_continuous(expand=expansion(mult=c(0,0.15)),
                     labels=function(x) paste0(x,"%")) +
  labs(title="Distribution du nombre de parcelles cultivées par ménage",
       subtitle=paste0("Médiane = ",desc_p$Med," | Moyenne = ",desc_p$Moy,
                       " | CV = ",desc_p$CV,"% — GHS W4"),
       x="Nombre de parcelles cultivées", y="Proportion des ménages (%)",
       caption=paste0("N = ",format(nrow(menage), big.mark=" ")," ménages"))

ggsave("outputs/fig04_distrib_parcelles.png", p4, width=10, height=6, dpi=300, bg="white")
cat("  -> fig04 sauvegardé\n")

# ---- 10. FIGURE 5 : Boxplot par zone + Kruskal-Wallis (Tâche 22) ------------
menage_z <- menage %>% filter(!is.na(zone_lbl))

kw <- kruskal.test(n_parc ~ zone_lbl, data=menage_z)
cat(sprintf("\n  Kruskal-Wallis H=%.2f ddl=%d p=%s\n",
            kw$statistic, kw$parameter, format.pval(kw$p.value,digits=3)))

p5 <- ggplot(menage_z, aes(x=fct_reorder(zone_lbl, n_parc, median), y=n_parc, fill=zone_lbl)) +
  geom_boxplot(alpha=0.82, outlier.shape=21, outlier.size=1.2, outlier.alpha=0.35,
               color="#333333", linewidth=0.45) +
  geom_jitter(aes(color=zone_lbl), width=0.18, alpha=0.10, size=0.7) +
  scale_fill_manual(values=pal_zones, guide="none") +
  scale_color_manual(values=pal_zones, guide="none") +
  scale_y_log10(breaks=c(1,2,3,5,10,20), labels=as.character) +
  coord_flip() +
  annotate("text", x=0.7, y=15,
           label=paste0("Kruskal-Wallis\nH=",round(kw$statistic,1),
                        " | p",ifelse(kw$p.value<0.001,"<0.001",
                                      paste0("=",round(kw$p.value,3)))),
           size=3, color="#555555", fontface="italic") +
  labs(title="Nombre de parcelles cultivées par zone géopolitique",
       subtitle="Échelle log₁₀ | Trait central = médiane — GHS W4",
       x=NULL, y="Nombre de parcelles (échelle log₁₀)",
       caption="Source : sect11b1_plantingw4 | Nigeria GHS Panel Wave 4")

ggsave("outputs/fig05_boxplot_zones.png", p5, width=10, height=6, dpi=300, bg="white")
cat("  -> fig05 sauvegardé\n")

# ---- 11. FIGURE 6 : Violin Rural vs Urbain + Wilcoxon (Tâche 26 adaptée) ---
ru <- menage %>% filter(!is.na(sector_lbl))
wt <- wilcox_test(ru, n_parc ~ sector_lbl)
r_eff <- abs(qnorm(as.numeric(wt$p)/2))/sqrt(nrow(ru))

meds_ru <- ru %>% group_by(sector_lbl) %>%
  summarise(med=median(n_parc), .groups="drop")

p6 <- ggplot(ru, aes(x=sector_lbl, y=n_parc, fill=sector_lbl)) +
  geom_violin(alpha=0.65, trim=TRUE, color=NA) +
  geom_boxplot(width=0.12, alpha=0.92, outlier.shape=NA, linewidth=0.5, color="#333333") +
  stat_summary(fun=median, geom="point", shape=21, fill="gold", color="#333333", size=3.5) +
  geom_text(data=meds_ru, aes(x=sector_lbl, y=med+0.45,
                               label=paste0("Méd. = ",med)),
            size=3.3, fontface="bold", color="#222222") +
  annotate("text", x=1.5, y=max(ru$n_parc,na.rm=TRUE)*0.87,
           label=paste0("Wilcoxon\np ",
                        ifelse(as.numeric(wt$p)<0.001,"< 0,001",
                               paste0("= ",round(as.numeric(wt$p),3))),
                        " | r = ",round(r_eff,3)),
           size=3.4, color="#444444", fontface="italic") +
  scale_fill_manual(values=c("Rural"="#2c7bb6","Urbain"="#d7191c"), guide="none") +
  labs(title="Nombre de parcelles cultivées : Rural vs Urbain",
       subtitle="Les ménages ruraux exploitent significativement plus de parcelles",
       x=NULL, y="Nombre de parcelles cultivées",
       caption="Source : sect11b1_plantingw4 | GHS W4")

ggsave("outputs/fig06_violin_rural_urbain.png", p6, width=9, height=6, dpi=300, bg="white")
cat("  -> fig06 sauvegardé\n")

# ---- 12. FIGURE 7 : Heatmap État × Zone (Tâche 24) -------------------------
state_stats <- menage %>%
  filter(!is.na(state_lbl), !is.na(zone_lbl)) %>%
  group_by(state_lbl, zone_lbl) %>%
  summarise(med=median(n_parc), moy=round(mean(n_parc),1), n=n(), .groups="drop") %>%
  filter(n >= 5)

p7 <- ggplot(state_stats, aes(x=zone_lbl, y=fct_reorder(state_lbl, med), fill=med)) +
  geom_tile(color="white", linewidth=0.5) +
  geom_text(aes(label=sprintf("%.1f", med)), size=2.7, fontface="bold",
            color=ifelse(state_stats$med >= 3,"white","#222222")) +
  scale_fill_viridis_c(option="plasma", name="Médiane\nparcelles",
                       breaks=pretty(state_stats$med, 5)) +
  labs(title="Intensité foncière par État et Zone géopolitique",
       subtitle="Médiane du nombre de parcelles cultivées par ménage — GHS W4",
       x="Zone géopolitique", y="État",
       caption="Source : sect11b1_plantingw4 | États avec N ≥ 5 ménages") +
  theme(axis.text.x=element_text(angle=30, hjust=1, size=8),
        axis.text.y=element_text(size=7.5))

ggsave("outputs/fig07_heatmap_etat_zone.png", p7, width=12, height=15, dpi=300, bg="white")
cat("  -> fig07 sauvegardé\n")

# ---- 13. FIGURE 8 : Distance des parcelles au domicile ----------------------
parc_dist <- parc %>%
  filter(!is.na(dist_km), dist_km < 50, !is.na(sector_lbl))

meds_dist <- parc_dist %>%
  group_by(sector_lbl) %>%
  summarise(med=median(dist_km), .groups="drop")

p8 <- ggplot(parc_dist, aes(x=dist_km, fill=sector_lbl, color=sector_lbl)) +
  geom_density(alpha=0.38, linewidth=0.9) +
  geom_vline(data=meds_dist, aes(xintercept=med, color=sector_lbl),
             linetype="dashed", linewidth=0.9) +
  geom_text(data=meds_dist,
            aes(x=med+1.2, y=0.25, label=paste0("Méd.=",round(med,1),"km"),
                color=sector_lbl),
            size=3.2, fontface="bold", hjust=0) +
  scale_fill_manual(values=c("Rural"="#2c7bb6","Urbain"="#d7191c"), name="Milieu") +
  scale_color_manual(values=c("Rural"="#2c7bb6","Urbain"="#d7191c"), name="Milieu") +
  scale_x_continuous(labels=function(x) paste0(x," km")) +
  labs(title="Distance des parcelles au domicile selon le milieu",
       subtitle="Densités de probabilité | Lignes pointillées = médianes | Parcelles < 50 km",
       x="Distance au domicile (km)", y="Densité",
       caption="Source : nga_plotgeovariables_y4 | GHS W4")

ggsave("outputs/fig08_distance_parcelles.png", p8, width=10, height=6, dpi=300, bg="white")
cat("  -> fig08 sauvegardé\n")

# ---- 14. FIGURE 9 : Ridge plot parcelles par zone ---------------------------
p9 <- ggplot(menage_z, aes(x=n_parc, y=fct_rev(fct_reorder(zone_lbl, n_parc, median)),
                            fill=zone_lbl, color=zone_lbl)) +
  geom_density_ridges(alpha=0.65, scale=1.3, linewidth=0.6,
                      quantile_lines=TRUE, quantiles=2) +
  scale_fill_manual(values=pal_zones, guide="none") +
  scale_color_manual(values=pal_zones, guide="none") +
  scale_x_continuous(breaks=1:15, limits=c(0,15)) +
  labs(title="Distribution du nombre de parcelles par zone géopolitique",
       subtitle="Ridge plots | Ligne verticale = médiane | GHS W4",
       x="Nombre de parcelles cultivées", y=NULL,
       caption="Source : sect11b1_plantingw4 | Nigeria GHS Panel Wave 4")

ggsave("outputs/fig09_ridge_zones.png", p9, width=10, height=6, dpi=300, bg="white")
cat("  -> fig09 sauvegardé\n")

# ---- 15. FIGURE 10 : Dashboard récapitulatif --------------------------------
# Donut tenure
donut_data <- tenure_freq %>%
  arrange(desc(pct)) %>% head(5) %>%
  mutate(ymax=cumsum(pct)/100, ymin=c(0,head(cumsum(pct)/100,-1)),
         lab_y=(ymax+ymin)/2)

p_donut <- ggplot(donut_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=tenure_lbl)) +
  geom_rect(color="white", linewidth=0.7) +
  geom_text(aes(x=3.5, y=lab_y, label=paste0(pct,"%\n",str_wrap(tenure_lbl,12))),
            size=2.8, fontface="bold", color="white") +
  coord_polar(theta="y") +
  scale_fill_manual(values=pal_tenure, guide="none") +
  xlim(c(0,4)) +
  labs(title="Structure de la tenure", subtitle="Top 5 modalités") +
  theme_void() +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=11, color="#1a3a5c"),
        plot.subtitle=element_text(hjust=0.5, size=9, color="#4a6080"))

# Barplot zone
zone_med <- menage_z %>%
  group_by(zone_lbl) %>%
  summarise(med=median(n_parc), n=n(), .groups="drop")

p_zone <- ggplot(zone_med, aes(x=fct_reorder(zone_lbl, med), y=med, fill=zone_lbl)) +
  geom_col(width=0.75, color="white") +
  geom_text(aes(label=sprintf("%.1f",med)), hjust=-0.15, fontface="bold", size=3.3) +
  geom_text(aes(label=paste0("n=",format(n, big.mark=" ")), y=0.05),
            hjust=0, size=2.8, color="white") +
  scale_fill_manual(values=pal_zones, guide="none") +
  scale_y_continuous(expand=expansion(mult=c(0,0.3))) +
  coord_flip() +
  labs(title="Médiane parcelles/ménage par zone",
       subtitle="Disparités géopolitiques", x=NULL, y="Médiane")

dashboard <- (p_zone | p_donut) +
  plot_annotation(
    title   = "TABLEAU DE BORD — Structure Foncière des Exploitations Agricoles Nigérianes",
    subtitle= paste0("GHS Panel Wave 4 (2018-2019) | ",
                     format(nrow(menage), big.mark=" ")," ménages | ",
                     format(nrow(parc), big.mark=" ")," parcelles"),
    caption = "ENSAE ISE1 | 2025-2026 | Source : Nigeria GHS Panel — World Bank LSMS-ISA",
    theme   = theme_ensae(12) +
              theme(plot.title=element_text(face="bold", size=15, color="#1a3a5c"),
                    plot.subtitle=element_text(size=10, color="#4a6080"))
  )

ggsave("outputs/fig10_dashboard_foncier.png", dashboard, width=14, height=7, dpi=300, bg="white")
cat("  -> fig10 (dashboard) sauvegardé\n")

# ---- 16. TABLEAUX CSV --------------------------------------------------------
cat("\n==> Création des tableaux...\n")

# Tableau 1 : Statistiques par zone
tab_zone <- menage_z %>%
  group_by(Zone = zone_lbl) %>%
  summarise(
    N_menages          = n(),
    Mediane_parcelles  = median(n_parc),
    Moyenne_parcelles  = round(mean(n_parc), 2),
    Ecart_type         = round(sd(n_parc), 2),
    CV_pct             = round(sd(n_parc)/mean(n_parc)*100, 1),
    Pct_plus_3_parc    = round(mean(n_parc > 3)*100, 1),
    .groups = "drop"
  )
write.csv(tab_zone, "outputs/tab01_structure_par_zone.csv", row.names=FALSE)

# Tableau 2 : Tenure par milieu
tab_tenure_milieu <- parc %>%
  filter(!is.na(tenure_lbl), !is.na(sector_lbl)) %>%
  count(Tenure=tenure_lbl, Milieu=sector_lbl) %>%
  group_by(Milieu) %>%
  mutate(Pct_dans_milieu=round(n/sum(n)*100,1)) %>%
  ungroup() %>%
  arrange(Tenure, Milieu)
write.csv(tab_tenure_milieu, "outputs/tab02_tenure_par_milieu.csv", row.names=FALSE)

# Tableau 3 : Résultats tests statistiques
tab_tests <- data.frame(
  Test         = c("Kruskal-Wallis (parcelles ~ zone)",
                   "Wilcoxon (parcelles ~ milieu)",
                   "Chi-deux (tenure ~ milieu)"),
  Statistique  = c(round(kw$statistic,3), as.numeric(wt$statistic), round(chi2$statistic,3)),
  ddl          = c(kw$parameter, NA, chi2$parameter),
  p_value      = c(format.pval(kw$p.value,3),
                   format.pval(as.numeric(wt$p),3),
                   format.pval(chi2$p.value,3)),
  Taille_effet = c(NA, round(r_eff,3), round(vc,3))
)
write.csv(tab_tests, "outputs/tab03_tests_statistiques.csv", row.names=FALSE)

cat("  -> Tableaux CSV sauvegardés\n")

# ---- 17. RÉSUMÉ FINAL --------------------------------------------------------
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  RÉSUMÉ TP4 — Analyse des Parcelles Agricoles GHS W4\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(sprintf("  Parcelles analysées              : %d\n", nrow(parc)))
cat(sprintf("  Ménages (parc. cultivées)        : %d\n", nrow(menage)))
cat(sprintf("  Médiane parcelles/ménage          : %d\n", median(menage$n_parc)))
cat(sprintf("  %% ménages en milieu rural         : %.1f%%\n",
            mean(menage$sector_lbl=="Rural", na.rm=TRUE)*100))
cat(sprintf("  Tenure dominante                  : %s (%.1f%%)\n",
            tenure_freq$tenure_lbl[which.max(tenure_freq$pct)],
            max(tenure_freq$pct)))
cat(sprintf("  Kruskal-Wallis p                  : %s\n", format.pval(kw$p.value,3)))
cat("\n  Outputs produits :\n")
for (f in list.files("outputs")) cat(sprintf("    %s\n", f))
cat("═══════════════════════════════════════════════════════════════\n")
