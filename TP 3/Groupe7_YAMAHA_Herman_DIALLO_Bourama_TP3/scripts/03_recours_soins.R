# =============================================================================
# Script 03 : Recours aux soins selon le prestataire (Tâche 15) — pondéré
#
# PONDÉRATIONS : les proportions de recours sont des parts pondérées
# calculées sur la somme des poids wt_wave4.
#
# Auteurs : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(dplyr); library(ggplot2); library(forcats); library(scales)

df_health <- readRDS("data/processed/df_health_base.rds")

groupe_praticien <- c(
  "0"="Aucun recours","1"="Tradipraticien","2"="Hôpital/Clinique (médecin)",
  "3"="Hôpital/Clinique (médecin)","4"="Hôpital/Clinique (infirmier)",
  "5"="Hôpital/Clinique (infirmier)","6"="Hôpital/Clinique (infirmier)",
  "7"="Pharmacie","8"="Pharmacie","9"="Tradipraticien",
  "10"="Tradipraticien","11"="Pharmacie","13"="Autre",
  "14"="Agent de santé comm.","15"="Agent de santé comm."
)

# Proportions du recours
df_recours <- df_health %>%
  filter(!is.na(s4aq3), s4aq3==1, !is.na(wt_wave4)) %>%
  mutate(
    code_prat        = as.character(if_else(is.na(s4aq6a)|s4aq6a==0, 0, as.integer(s4aq6a))),
    praticien_groupe = groupe_praticien[code_prat]
  ) %>%
  filter(!is.na(praticien_groupe))

taux_prat <- df_recours %>%
  group_by(praticien_groupe) %>%
  summarise(n_pond = sum(wt_wave4, na.rm=TRUE), .groups="drop") %>%
  mutate(
    pct             = n_pond / sum(n_pond) * 100,
    praticien_ordre = fct_reorder(praticien_groupe, n_pond)
  )

cat("=== Recours par prestataire ===\n"); print(taux_prat)

# Export en Excel
library(openxlsx)
taux_prat_export <- taux_prat %>%
  mutate(pct = round(pct, 2)) %>%
  select(Prestataire = praticien_groupe, `Part (%)` = pct)
wb03 <- createWorkbook()
addWorksheet(wb03, "Recours_Prestataires")
titre03 <- "Recours aux soins par type de prestataire (pondéré)"
writeData(wb03, "Recours_Prestataires", x = titre03, startRow = 1, startCol = 1)
mergeCells(wb03, "Recours_Prestataires", cols = 1:2, rows = 1)
addStyle(wb03, "Recours_Prestataires",
         createStyle(fontSize = 13, fontColour = "#1D3557", textDecoration = "bold",
                     halign = "center", border = "Bottom", borderColour = "#1D3557",
                     borderStyle = "medium"),
         rows = 1, cols = 1:2, gridExpand = TRUE)
writeData(wb03, "Recours_Prestataires", taux_prat_export, startRow = 3, startCol = 1,
          headerStyle = createStyle(fontSize = 11, fontColour = "white",
                                     fgFill = "#1D3557", textDecoration = "bold",
                                     halign = "center", wrapText = TRUE,
                                     border = "TopBottomLeftRight"))
for (i in seq_len(nrow(taux_prat_export))) {
  s <- if (i %% 2 == 0)
    createStyle(fgFill = "#EBF0F9", border = "TopBottomLeftRight", numFmt = "0.00")
  else
    createStyle(fgFill = "white", border = "TopBottomLeftRight", numFmt = "0.00")
  addStyle(wb03, "Recours_Prestataires", s, rows = i + 3, cols = seq_len(ncol(taux_prat_export)),
           gridExpand = TRUE)
}
setColWidths(wb03, "Recours_Prestataires", cols = 1, widths = 30)
setColWidths(wb03, "Recours_Prestataires", cols = 2, widths = 14)
saveWorkbook(wb03, "outputs/tables/03_recours_prestataires.xlsx", overwrite = TRUE)

couleurs_prat <- c(
  "Aucun recours"                ="# E63946",
  "Hôpital/Clinique (médecin)"  ="#1D3557",
  "Hôpital/Clinique (infirmier)"="#457B9D",
  "Pharmacie"                    ="#2A9D8F",
  "Tradipraticien"               ="#E9C46A",
  "Agent de santé comm."         ="#F4A261",
  "Autre"                        ="#999999"
)
couleurs_prat["Aucun recours"] <- "#E63946"

p_recours <- ggplot(taux_prat,
                    aes(x=praticien_ordre, y=pct, fill=praticien_groupe)) +
  geom_col(width=0.7, show.legend=FALSE) +
  geom_text(aes(label=paste0(round(pct,1),"%")),
            hjust=-0.15, size=4, fontface="bold") +
  coord_flip() +
  scale_fill_manual(values=couleurs_prat) +
  scale_y_continuous(expand=expansion(mult=c(0,0.20))) +
  labs(title="Recours selon le type de prestataire",
       subtitle="Part pondérée (wt_wave4) parmi les individus malades — Wave 4 (2018)",
       x=NULL, y="Part des individus malades (%)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40"),
        panel.grid.major.y=element_blank())

ggsave("outputs/figures/03_recours_prestataires.png", p_recours,
       width=10, height=6, dpi=300)
cat("-> 03_recours_prestataires.png\n=== Script 03 terminé ===\n")
