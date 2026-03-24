# =============================================================================
# Script 04 : Distribution des dépenses de santé (Tâche 16) — pondéré
#
# PONDÉRATIONS : la médiane est calculée via svyquantile().
# L'histogramme utilise aes(weight=wt_wave4) pour représenter la
# distribution dans la population et non dans l'échantillon.
#
# Auteurs : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(dplyr); library(ggplot2); library(scales)
library(patchwork); library(survey); library(srvyr)

df_health <- readRDS("data/processed/df_health_base.rds")

# Construction des dépenses totales
df_depenses <- df_health %>%
  mutate(
    dep_consultation = if_else(is.na(s4aq9),  0, as.numeric(s4aq9)),
    dep_medicaments  = if_else(is.na(s4aq14), 0, as.numeric(s4aq14)),
    dep_hopital      = if_else(is.na(s4aq17), 0, as.numeric(s4aq17)),
    dep_totale       = dep_consultation + dep_medicaments + dep_hopital
  ) %>%
  filter(dep_totale > 0, !is.na(wt_wave4))

cat("Individus avec dépenses > 0 :", nrow(df_depenses), "\n")

# Médiane 
plan_dep <- svydesign(ids=~1, weights=~wt_wave4, data=df_depenses)
med_pond <- coef(svyquantile(~dep_totale, plan_dep, quantiles=0.5, na.rm=TRUE))
q1_pond  <- coef(svyquantile(~dep_totale, plan_dep, quantiles=0.25, na.rm=TRUE))
q3_pond  <- coef(svyquantile(~dep_totale, plan_dep, quantiles=0.75, na.rm=TRUE))

cat("Médiane :", round(med_pond), "Naira\n")
cat("Q1       :", round(q1_pond),  "Naira\n")
cat("Q3       :", round(q3_pond),  "Naira\n")

# Outliers (règle Tukey étendue)
iqr_p       <- q3_pond - q1_pond
seuil_haut  <- q3_pond + 3 * iqr_p
n_outliers  <- sum(df_depenses$dep_totale > seuil_haut)
cat("Seuil Tukey × 3  :", round(seuil_haut), "Naira |",
    n_outliers, "outliers\n")

# Statistiques par décile
df_depenses <- df_depenses %>% mutate(decile = ntile(dep_totale, 10))
stats_dec <- df_depenses %>%
  group_by(decile) %>%
  summarise(n=n(), min=min(dep_totale), mediane=median(dep_totale),
            moyenne=round(mean(dep_totale)), max=max(dep_totale), .groups="drop")
# Export en Excel
library(openxlsx)
stats_dec_export <- stats_dec %>%
  mutate(across(c(min, mediane, moyenne, max), ~round(.x, 2)))
wb04 <- createWorkbook()
addWorksheet(wb04, "Depenses_Decile")
titre04 <- "Statistiques des dépenses de santé par décile"
writeData(wb04, "Depenses_Decile", x = titre04, startRow = 1, startCol = 1)
mergeCells(wb04, "Depenses_Decile", cols = 1:6, rows = 1)
addStyle(wb04, "Depenses_Decile",
         createStyle(fontSize = 13, fontColour = "#1D3557", textDecoration = "bold",
                     halign = "center", border = "Bottom", borderColour = "#1D3557",
                     borderStyle = "medium"),
         rows = 1, cols = 1:6, gridExpand = TRUE)
writeData(wb04, "Depenses_Decile", stats_dec_export, startRow = 3, startCol = 1,
          headerStyle = createStyle(fontSize = 11, fontColour = "white",
                                     fgFill = "#1D3557", textDecoration = "bold",
                                     halign = "center", wrapText = TRUE,
                                     border = "TopBottomLeftRight"))
for (i in seq_len(nrow(stats_dec_export))) {
  s <- if (i %% 2 == 0)
    createStyle(fgFill = "#EBF0F9", border = "TopBottomLeftRight", numFmt = "#,##0.00")
  else
    createStyle(fgFill = "white", border = "TopBottomLeftRight", numFmt = "#,##0.00")
  addStyle(wb04, "Depenses_Decile", s, rows = i + 3, cols = seq_len(ncol(stats_dec_export)),
           gridExpand = TRUE)
}
setColWidths(wb04, "Depenses_Decile", cols = 1:6, widths = 14)
saveWorkbook(wb04, "outputs/tables/04_depenses_decile.xlsx", overwrite = TRUE)

# Histogramme (weight = wt_wave4)
p_histo <- ggplot(df_depenses, aes(x=dep_totale, weight=wt_wave4)) +
  geom_histogram(bins=40, fill="#3A86FF", color="white", alpha=0.85) +
  geom_vline(xintercept=med_pond, linetype="dashed",
             color="#E63946", linewidth=1) +
  annotate("label", x=min(df_depenses$dep_totale, na.rm=TRUE)*1.2, y=Inf,
           label=paste0("Médiane : ", format(round(med_pond), big.mark=" ")),
           hjust=0, vjust=1.2, color="#E63946", size=4,
           fill="white", label.size=0.3, fontface="bold") +
  scale_x_log10(labels=label_comma(big.mark=" ")) +
  scale_y_continuous(labels=label_comma(scale=1/1e6, suffix=" M")) +
  labs(title="Distribution des dépenses de santé (échelle log)",
       subtitle="Effectifs pondérés (wt_wave4) — Wave 4 (2018) | Ligne rouge = médiane",
       x="Dépense totale (Naira, échelle log)", y="Effectif",
       caption="Source : NBS Nigeria, GHS-Panel W4 | wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40"))

ggsave("outputs/figures/04a_depenses_histogramme.png", p_histo,
       width=10, height=5.5, dpi=300)
cat("-> 04a_depenses_histogramme.png\n")

# Boxplot par prestataire
groupe_praticien <- c(
  "0"="Aucun recours","1"="Tradipraticien","2"="Hôpital / Clinique",
  "3"="Hôpital / Clinique","4"="Hôpital / Clinique",
  "5"="Hôpital / Clinique","6"="Hôpital / Clinique",
  "7"="Pharmacie","8"="Pharmacie","9"="Tradipraticien",
  "10"="Tradipraticien","11"="Pharmacie","13"="Autre",
  "14"="Agent de santé comm.","15"="Agent de santé comm."
)

df_box <- df_depenses %>%
  mutate(
    code_prat        = as.character(if_else(is.na(s4aq6a)|s4aq6a==0, 0, as.integer(s4aq6a))),
    praticien_groupe = groupe_praticien[code_prat]
  ) %>%
  filter(!is.na(praticien_groupe),
         praticien_groupe != "Aucun recours",
         dep_totale <= seuil_haut)

p_box <- ggplot(df_box,
                aes(x=reorder(praticien_groupe, dep_totale, FUN=median),
                    y=dep_totale, fill=praticien_groupe)) +
  geom_boxplot(outlier.shape=21, outlier.alpha=0.3,
               outlier.size=1.2, show.legend=FALSE, width=0.6) +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="#E63946") +
  scale_y_log10(labels=label_comma(big.mark=" ")) +
  scale_fill_brewer(palette="Set2") +
  coord_flip() +
  labs(title="Dépenses de santé par type de prestataire (pondéré)",
       subtitle="Échelle log — outliers extrêmes exclus (> Q3+3×IQR) | losange = médiane",
       x=NULL, y="Dépense totale (Naira, échelle log)",
       caption="Source : NBS Nigeria, GHS-Panel W4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40",size=10),
        panel.grid.major.y=element_blank())

ggsave("outputs/figures/04b_depenses_boxplot_prestataire.png", p_box,
       width=10, height=5.5, dpi=300)
cat("-> 04b_depenses_boxplot_prestataire.png\n")

saveRDS(list(seuil_haut=seuil_haut, groupe_praticien=groupe_praticien,
             med_pond=med_pond, q1_pond=q1_pond, q3_pond=q3_pond),
        "data/processed/params_depenses.rds")

cat("=== Script 04 terminé ===\n")
