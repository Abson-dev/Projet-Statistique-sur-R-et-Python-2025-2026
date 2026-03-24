# =============================================================================
# Script 05 : Test d'indépendance recours × quintile (Tâche 17) — pondéré
#
# PONDÉRATIONS : le test du chi-deux est réalisé via svychisq()
# du package survey. Le tableau récapitulatif gtsummary est exporté en
# Excel (openxlsx) — pas de sortie HTML.
#
# Auteurs : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(dplyr); library(ggplot2); library(scales)
library(gtsummary); library(survey); library(srvyr)
library(openxlsx); library(flextable)

df_health <- readRDS("data/processed/df_health_base.rds")
cons      <- haven::read_dta("data/raw/totcons_final.dta")

# Quintiles de consommation
cons_clean <- cons %>%
  select(hhid, totcons_adj) %>%
  mutate(
    quintile       = ntile(totcons_adj, 5),
    quintile_label = factor(quintile, levels=1:5,
                            labels=c("Q1 (plus pauvres)","Q2","Q3",
                                     "Q4","Q5 (plus riches)"))
  )

df_merge <- df_health %>%
  filter(!is.na(s4aq3), s4aq3==1, !is.na(wt_wave4)) %>%
  mutate(
    consulte = factor(if_else(s4aq1==1,"Consulte","Non consulte",
                              missing="Non consulte"),
                      levels=c("Consulte","Non consulte"))
  ) %>%
  left_join(cons_clean, by="hhid") %>%
  filter(!is.na(quintile_label), !is.na(consulte))

cat("Individus malades dans l'analyse :", nrow(df_merge), "\n")

# Plan de sondage sur les malades
plan_merge <- svydesign(ids=~1, weights=~wt_wave4, data=df_merge)

# Chi-deux pondéré via svychisq (méthode de Rao-Scott)
test_pond <- svychisq(~consulte + quintile_label, plan_merge)
cat("\n=== Chi-deux pondéré (Rao-Scott) ===\n")
print(test_pond)

# Table de contingence
tab_cont <- table(Recours=df_merge$consulte, Quintile=df_merge$quintile_label)
chi2_val <- suppressWarnings(chisq.test(tab_cont)$statistic)
n_total  <- sum(tab_cont)
v_cramer <- sqrt(chi2_val / (n_total * (min(nrow(tab_cont), ncol(tab_cont))-1)))
cat("V de Cramer :", round(v_cramer, 4), "\n")

# Export contingence en Excel
tab_df <- as.data.frame.matrix(tab_cont)
tab_df$Recours <- rownames(tab_df)
wb05c <- createWorkbook()
addWorksheet(wb05c, "Contingence")
titre05c <- "Table de contingence : Recours x Quintile de consommation"
writeData(wb05c, "Contingence", x = titre05c, startRow = 1, startCol = 1)
mergeCells(wb05c, "Contingence", cols = 1:6, rows = 1)
addStyle(wb05c, "Contingence",
         createStyle(fontSize = 13, fontColour = "#1D3557", textDecoration = "bold",
                     halign = "center", border = "Bottom", borderColour = "#1D3557",
                     borderStyle = "medium"),
         rows = 1, cols = 1:6, gridExpand = TRUE)
writeData(wb05c, "Contingence", tab_df, startRow = 3, startCol = 1,
          headerStyle = createStyle(fontSize = 11, fontColour = "white",
                                     fgFill = "#1D3557", textDecoration = "bold",
                                     halign = "center", border = "TopBottomLeftRight"))
for (i in seq_len(nrow(tab_df))) {
  s <- if (i %% 2 == 0)
    createStyle(fgFill = "#EBF0F9", border = "TopBottomLeftRight")
  else
    createStyle(fgFill = "white", border = "TopBottomLeftRight")
  addStyle(wb05c, "Contingence", s, rows = i + 3, cols = seq_len(ncol(tab_df)),
           gridExpand = TRUE)
}
setColWidths(wb05c, "Contingence", cols = 1:6, widths = 18)
saveWorkbook(wb05c, "outputs/tables/05_contingence_recours_quintile.xlsx", overwrite = TRUE)

# Barplot 100% empilé
df_plot <- df_merge %>%
  group_by(quintile_label) %>%
  mutate(pond_total = sum(wt_wave4)) %>%
  group_by(quintile_label, consulte) %>%
  summarise(pond_groupe = sum(wt_wave4), pond_total=first(pond_total),
            .groups="drop") %>%
  mutate(pct = pond_groupe / pond_total * 100)

p_stacked <- ggplot(df_plot,
                    aes(x=quintile_label, y=pct, fill=consulte)) +
  geom_col(position="stack", width=0.65) +
  geom_text(aes(label=paste0(round(pct,1),"%")),
            position=position_stack(vjust=0.5),
            size=3.8, color="white", fontface="bold") +
  scale_y_continuous(labels=label_percent(scale=1)) +
  scale_fill_manual(values=c("Consulte"="#1D3557","Non consulte"="#E63946"),
                    name="Recours aux soins") +
  labs(title="Recours selon le quintile de consommation",
       subtitle=paste0("Chi² pondéré (Rao-Scott) — V de Cramer = ",
                       round(v_cramer,3)," — Wave 4 (2018)"),
       x="Quintile de consommation", y="Part pondérée (%)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40",size=10),
        legend.position="top",
        panel.grid.major.x=element_blank())

ggsave("outputs/figures/05_recours_quintile.png", p_stacked,
       width=10, height=6, dpi=300)
cat("-> 05_recours_quintile.png\n")

# ---- Tableau gtsummary pondéré → Excel -----
tbl_pond <- tbl_svysummary(
  data      = plan_merge,
  by        = quintile_label,
  include   = consulte,
  label     = list(consulte ~ "Recours aux soins"),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
) %>%
  add_p(test=list(all_categorical() ~ "svy.chisq.test")) %>%
  modify_caption("Tableau — Recours aux soins x Quintile de consommation")

df_excel <- as_tibble(tbl_pond)

wb <- createWorkbook()
addWorksheet(wb, "Recours_Quintile")

titre <- "Tableau - Recours aux soins selon le quintile de consommation"
writeData(wb, "Recours_Quintile", x=titre, startRow=1, startCol=1)
mergeCells(wb, "Recours_Quintile", cols=1:7, rows=1)
addStyle(wb, "Recours_Quintile",
         createStyle(fontSize=13, fontColour="#1D3557", textDecoration="bold",
                     halign="center", border="Bottom", borderColour="#1D3557",
                     borderStyle="medium"),
         rows=1, cols=1:7, gridExpand=TRUE)

writeData(wb, "Recours_Quintile", df_excel, startRow=3, startCol=1,
          headerStyle=createStyle(fontSize=11, fontColour="white",
                                   fgFill="#1D3557", textDecoration="bold",
                                   halign="center", wrapText=TRUE,
                                   border="TopBottomLeftRight"))

for (i in seq_len(nrow(df_excel))) {
  s <- if (i %% 2 == 0)
    createStyle(fgFill="#EBF0F9", border="TopBottomLeftRight")
  else
    createStyle(fgFill="white",   border="TopBottomLeftRight")
  addStyle(wb, "Recours_Quintile", s, rows=i+3, cols=seq_len(ncol(df_excel)),
           gridExpand=TRUE)
}

note_row <- nrow(df_excel) + 5
writeData(wb, "Recours_Quintile",
          x=paste0("Note : estimations pondérées (wt_wave4). ",
                   "Test chi-deux pondéré Rao-Scott. ",
                   "V de Cramer = ", round(v_cramer,3), "."),
          startRow=note_row, startCol=1)
addStyle(wb, "Recours_Quintile",
         createStyle(fontSize=9, fontColour="#555555", textDecoration="italic"),
         rows=note_row, cols=1)

setColWidths(wb, "Recours_Quintile", cols=1, widths=25)
setColWidths(wb, "Recours_Quintile", cols=2:7, widths=16)

saveWorkbook(wb, "outputs/tables/tableau_recours_quintile.xlsx", overwrite=TRUE)
cat("-> outputs/tables/tableau_recours_quintile.xlsx\n")
cat("=== Script 05 terminé ===\n")
