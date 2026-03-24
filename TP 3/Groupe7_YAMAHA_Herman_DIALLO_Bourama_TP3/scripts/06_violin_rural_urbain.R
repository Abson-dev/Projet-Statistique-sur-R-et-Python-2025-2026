# =============================================================================
# Script 06 : Dépenses de santé — rural vs urbain (Tâche 18) — pondéré
#
# PONDÉRATIONS : les statistiques descriptives utilisent svyquantile()
# et svymean(). Le test de Wilcoxon est appliqué sur les données brutes
# (aucun équivalent pondéré standard en R), avec une note méthodologique.
# Le tableau gtsummary pondéré est exporté en Excel.
#
# Auteurs : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(dplyr); library(ggplot2); library(scales)
library(patchwork); library(survey); library(srvyr)
library(gtsummary); library(openxlsx)

df_health <- readRDS("data/processed/df_health_base.rds")
params    <- readRDS("data/processed/params_depenses.rds")
cons      <- haven::read_dta("data/raw/totcons_final.dta")

cons_clean <- cons %>%
  select(hhid, totcons_adj) %>%
  mutate(
    quintile       = ntile(totcons_adj, 5),
    quintile_label = factor(quintile, levels=1:5,
                            labels=c("Q1 (plus pauvres)","Q2","Q3",
                                     "Q4","Q5 (plus riches)"))
  )

df_depenses <- df_health %>%
  mutate(
    dep_consultation = if_else(is.na(s4aq9),  0, as.numeric(s4aq9)),
    dep_medicaments  = if_else(is.na(s4aq14), 0, as.numeric(s4aq14)),
    dep_hopital      = if_else(is.na(s4aq17), 0, as.numeric(s4aq17)),
    dep_totale       = dep_consultation + dep_medicaments + dep_hopital
  ) %>%
  filter(dep_totale > 0, !is.na(milieu_label), !is.na(wt_wave4)) %>%
  left_join(cons_clean, by="hhid")

# Plan de sondage sur les dépensiers
plan_dep <- svydesign(ids=~1, weights=~wt_wave4, data=df_depenses)
svy_dep  <- as_survey_design(plan_dep)

# Médianes pondérées par milieu
med_milieu <- svy_dep %>%
  group_by(milieu_label) %>%
  summarise(
    med_pond = survey_quantile(dep_totale, quantiles=0.5, na.rm=TRUE, vartype=NULL),
    moy_pond = survey_mean(dep_totale, na.rm=TRUE, vartype=NULL),
    n        = n()
  )

cat("=== Dépenses médianes par milieu ===\n"); print(med_milieu)

# Test Wilcoxon (non pondéré — aucun équivalent pondéré standard)
wilcox_res <- wilcox.test(dep_totale ~ milieu_label, data=df_depenses,
                          exact=FALSE, conf.int=TRUE)
n_tot <- nrow(df_depenses)
r_eff <- abs(qnorm(wilcox_res$p.value/2)) / sqrt(n_tot)

cat("Wilcoxon W =", wilcox_res$statistic, "| p =",
    format.pval(wilcox_res$p.value, digits=4),
    "| r =", round(r_eff,4), "\n")

# Export Wilcoxon en Excel
library(openxlsx)
df_wilcox <- data.frame(Test = "Wilcoxon-Mann-Whitney",
                        W = round(wilcox_res$statistic, 2),
                        p_value = round(wilcox_res$p.value, 4),
                        r_effet = round(r_eff, 4))
wb06 <- createWorkbook()
addWorksheet(wb06, "Wilcoxon")
titre06 <- "Test de Wilcoxon — Dépenses rural vs urbain"
writeData(wb06, "Wilcoxon", x = titre06, startRow = 1, startCol = 1)
mergeCells(wb06, "Wilcoxon", cols = 1:4, rows = 1)
addStyle(wb06, "Wilcoxon",
         createStyle(fontSize = 13, fontColour = "#1D3557", textDecoration = "bold",
                     halign = "center", border = "Bottom", borderColour = "#1D3557",
                     borderStyle = "medium"),
         rows = 1, cols = 1:4, gridExpand = TRUE)
writeData(wb06, "Wilcoxon", df_wilcox, startRow = 3, startCol = 1,
          headerStyle = createStyle(fontSize = 11, fontColour = "white",
                                     fgFill = "#1D3557", textDecoration = "bold",
                                     halign = "center", border = "TopBottomLeftRight"))
addStyle(wb06, "Wilcoxon",
         createStyle(fgFill = "white", border = "TopBottomLeftRight", numFmt = "0.0000"),
         rows = 4, cols = 1:4, gridExpand = TRUE)
setColWidths(wb06, "Wilcoxon", cols = 1, widths = 25)
setColWidths(wb06, "Wilcoxon", cols = 2:4, widths = 16)
saveWorkbook(wb06, "outputs/tables/06_wilcoxon_rural_urbain.xlsx", overwrite = TRUE)

# Annotation
p_label <- ifelse(wilcox_res$p.value < 0.001, "p < 0,001",
                  paste0("p = ", round(wilcox_res$p.value,3)))

# Violin 1 — rural vs urbain
p_violin <- ggplot(df_depenses,
                   aes(x=milieu_label, y=dep_totale, fill=milieu_label)) +
  geom_violin(trim=TRUE, alpha=0.55, color=NA) +
  geom_boxplot(width=0.12, fill="white", color="grey30",
               outlier.shape=NA, linewidth=0.7) +
  stat_summary(fun=median, geom="point", shape=18, size=4, color="#E63946") +
  annotate("label", x=1.5, y=Inf,
           label=paste0("Wilcoxon\n", p_label, " | r = ", round(r_eff,3)),
           vjust=1.2, size=3.8, fill="white", label.size=0.3,
           fontface="italic", color="grey20") +
  scale_y_log10(labels=label_comma(big.mark=" "),
                breaks=c(10,100,500,1000,5000,10000,50000)) +
  scale_fill_manual(values=c("Urbain"="#3A86FF","Rural"="#FFBE0B"), name="Milieu") +
  labs(title="Distribution pondérée des dépenses — rural vs urbain",
       subtitle="Échelle log — Wave 4 (2018) | losange rouge = médiane | wt_wave4",
       x=NULL, y="Dépense totale (Naira, échelle log)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40"),
        legend.position="none",
        axis.text.x=element_text(size=13,face="bold"))

ggsave("outputs/figures/06a_violin_rural_urbain.png", p_violin,
       width=8, height=6, dpi=300)
cat("-> 06a_violin_rural_urbain.png\n")

# Violin 2 — par quintile
df_q <- df_depenses %>% filter(!is.na(quintile_label))

p_vq <- ggplot(df_q, aes(x=quintile_label, y=dep_totale, fill=quintile_label)) +
  geom_violin(trim=TRUE, alpha=0.6, color=NA) +
  geom_boxplot(width=0.1, fill="white", color="grey30",
               outlier.shape=NA, linewidth=0.6) +
  stat_summary(fun=median, geom="point", shape=18, size=3.5, color="#E63946") +
  scale_y_log10(labels=label_comma(big.mark=" "),
                breaks=c(10,100,500,1000,5000,10000,50000)) +
  scale_fill_brewer(palette="RdYlBu", direction=-1) +
  labs(title="Dépenses pondérées par quintile de consommation",
       subtitle="Échelle log — Wave 4 (2018) | losange rouge = médiane | wt_wave4",
       x="Quintile de consommation", y="Dépense totale (Naira, échelle log)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40"),
        legend.position="none")

ggsave("outputs/figures/06b_violin_quintiles.png", p_vq,
       width=10, height=6, dpi=300)
cat("-> 06b_violin_quintiles.png\n")

# Combiné
p_combined <- p_violin / p_vq +
  plot_annotation(
    title="Dépenses de santé — disparités par milieu et richesse",
    subtitle="Nigeria GHS Panel Wave 4 (2018) | wt_wave4",
    theme=theme(plot.title=element_text(face="bold",size=15),
                plot.subtitle=element_text(color="grey40",size=12))
  )
ggsave("outputs/figures/06c_violin_combined.png", p_combined,
       width=10, height=12, dpi=300)
cat("-> 06c_violin_combined.png\n")

# ---- Tableau gtsummary → Excel ----
tbl_milieu <- tbl_svysummary(
  data      = plan_dep,
  by        = milieu_label,
  include   = c(dep_totale, dep_consultation, dep_medicaments, dep_hopital),
  label     = list(dep_totale       ~ "Depense totale (Naira)",
                   dep_consultation ~ "  Consultation",
                   dep_medicaments  ~ "  Medicaments",
                   dep_hopital      ~ "  Hospitalisation"),
  statistic = list(all_continuous() ~ "{median} [{p25} - {p75}]"),
  digits    = list(all_continuous() ~ 0)
) %>%
  add_p(test=list(all_continuous() ~ "svy.wilcox.test")) %>%
  modify_caption("Depenses de sante medianes par milieu (wt_wave4)")

df_excel <- as_tibble(tbl_milieu)

wb2 <- createWorkbook()
addWorksheet(wb2, "Depenses_Milieu")

titre2 <- "Dépenses de santé médianes par milieu de résidence"
writeData(wb2, "Depenses_Milieu", x=titre2, startRow=1, startCol=1)
mergeCells(wb2, "Depenses_Milieu", cols=1:5, rows=1)
addStyle(wb2, "Depenses_Milieu",
         createStyle(fontSize=13, fontColour="#1D3557", textDecoration="bold",
                     halign="center", border="Bottom", borderColour="#1D3557",
                     borderStyle="medium"),
         rows=1, cols=1:5, gridExpand=TRUE)

writeData(wb2, "Depenses_Milieu", df_excel, startRow=3, startCol=1,
          headerStyle=createStyle(fontSize=11, fontColour="white",
                                   fgFill="#1D3557", textDecoration="bold",
                                   halign="center", wrapText=TRUE))

for (i in seq_len(nrow(df_excel))) {
  s <- if (i%%2==0) createStyle(fgFill="#EBF0F9",border="TopBottomLeftRight")
       else         createStyle(fgFill="white",  border="TopBottomLeftRight")
  addStyle(wb2,"Depenses_Milieu",s,rows=i+3,cols=seq_len(ncol(df_excel)),gridExpand=TRUE)
}

note_row2 <- nrow(df_excel) + 5
writeData(wb2,"Depenses_Milieu",
          x=paste0("Note : statistiques calculées avec les poids wt_wave4. ",
                   "Format : Médiane [Q1 - Q3]. ",
                   "Test : Wilcoxon pondéré (svy.wilcox.test)."),
          startRow=note_row2, startCol=1)
addStyle(wb2,"Depenses_Milieu",
         createStyle(fontSize=9,fontColour="#555555",textDecoration="italic"),
         rows=note_row2,cols=1)
setColWidths(wb2,"Depenses_Milieu",cols=1,widths=28)
setColWidths(wb2,"Depenses_Milieu",cols=2:5,widths=16)

saveWorkbook(wb2,"outputs/tables/tableau_depenses_milieu.xlsx",overwrite=TRUE)
cat("-> outputs/tables/tableau_depenses_milieu.xlsx\n")
cat("=== Script 06 terminé ===\n")
