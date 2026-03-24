# =============================================================================
# Script 02 : Types de maladies déclarées (Tâche 14) 
#
# PONDÉRATIONS : les fréquences sont calculées avec des effectifs pondérés
# (sum(wt_wave4)) pour représenter la population nigériane.
#
# Auteurs : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(dplyr); library(ggplot2); library(forcats); library(scales)

df_health <- readRDS("data/processed/df_health_base.rds")

# Dictionnaire des maladies
maladie_labels <- c(
  "1"="Paludisme","2"="Tuberculose","3"="Fièvre jaune","4"="Typhoïde",
  "5"="Choléra","6"="Diarrhée","7"="Méningite","8"="Varicelle",
  "9"="Pneumonie","10"="Rhume commun","11"="Blessure/Traumatisme",
  "12"="Autre","13"="Hypertension","14"="Grippe","15"="Rhinite/Catarrhe",
  "16"="Toux","17"="Céphalée","18"="Diabète","19"="Ver de Guinée",
  "20"="Dysenterie","21"="Infection cutanée","22"="Rougeole",
  "23"="Infection urinaire","24"="Douleurs articulaires",
  "25"="Fièvre non spécifiée","26"="Trouble digestif","27"="Faiblesse/Fatigue"
)

categorie_maladie <- c(
  "1"="Infectieuse","2"="Infectieuse","3"="Infectieuse","4"="Infectieuse",
  "5"="Infectieuse","6"="Infectieuse","7"="Infectieuse","8"="Infectieuse",
  "9"="Infectieuse","10"="Infectieuse","11"="Traumatique","12"="Autre",
  "13"="Chronique","14"="Infectieuse","15"="Infectieuse","16"="Infectieuse",
  "17"="Autre","18"="Chronique","19"="Infectieuse","20"="Infectieuse",
  "21"="Infectieuse","22"="Infectieuse","23"="Infectieuse","24"="Chronique",
  "25"="Infectieuse","26"="Autre","27"="Autre"
)

# Préparation avec poids
df_maladies <- df_health %>%
  filter(!is.na(s4aq3b_1), s4aq3==1, !is.na(wt_wave4)) %>%
  mutate(
    code_str  = as.character(as.integer(s4aq3b_1)),
    maladie   = maladie_labels[code_str],
    categorie = categorie_maladie[code_str]
  ) %>%
  filter(!is.na(maladie))

# Top 10 par effectif
top10 <- df_maladies %>%
  group_by(maladie, categorie) %>%
  summarise(n_pond = sum(wt_wave4, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(n_pond)) %>%
  slice_head(n=10) %>%
  mutate(
    pct     = n_pond / sum(n_pond) * 100,
    maladie = fct_reorder(maladie, n_pond)
  )

cat("=== Top 10 maladies ===\n"); print(top10)

# Export en Excel avec décimales formatées
library(openxlsx)
top10_export <- top10 %>%
  mutate(pct = round(pct, 2)) %>%
  select(Maladie = maladie, Catégorie = categorie, `Part (%)` = pct)
wb02 <- createWorkbook()
addWorksheet(wb02, "Top10_Maladies")
titre02 <- "Top 10 des affections les plus fréquentes (effectifs pondérés)"
writeData(wb02, "Top10_Maladies", x = titre02, startRow = 1, startCol = 1)
mergeCells(wb02, "Top10_Maladies", cols = 1:3, rows = 1)
addStyle(wb02, "Top10_Maladies",
         createStyle(fontSize = 13, fontColour = "#1D3557", textDecoration = "bold",
                     halign = "center", border = "Bottom", borderColour = "#1D3557",
                     borderStyle = "medium"),
         rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb02, "Top10_Maladies", top10_export, startRow = 3, startCol = 1,
          headerStyle = createStyle(fontSize = 11, fontColour = "white",
                                     fgFill = "#1D3557", textDecoration = "bold",
                                     halign = "center", wrapText = TRUE,
                                     border = "TopBottomLeftRight"))
for (i in seq_len(nrow(top10_export))) {
  s <- if (i %% 2 == 0)
    createStyle(fgFill = "#EBF0F9", border = "TopBottomLeftRight", numFmt = "0.00")
  else
    createStyle(fgFill = "white", border = "TopBottomLeftRight", numFmt = "0.00")
  addStyle(wb02, "Top10_Maladies", s, rows = i + 3, cols = seq_len(ncol(top10_export)),
           gridExpand = TRUE)
}
setColWidths(wb02, "Top10_Maladies", cols = 1, widths = 25)
setColWidths(wb02, "Top10_Maladies", cols = 2:3, widths = 16)
saveWorkbook(wb02, "outputs/tables/02_top10_maladies.xlsx", overwrite = TRUE)

couleurs_cat <- c("Infectieuse"="#3A86FF","Chronique"="#FF6B6B",
                  "Traumatique"="#FFBE0B","Autre"="#8338EC")

p_maladies <- ggplot(top10, aes(x=maladie, y=pct, fill=categorie)) +
  geom_col(width=0.7) +
  geom_text(aes(label=paste0(round(pct,1),"%")),
            hjust=-0.15, size=3.8, fontface="bold") +
  coord_flip() +
  scale_fill_manual(values=couleurs_cat, name="Catégorie") +
  scale_y_continuous(expand=expansion(mult=c(0,0.18))) +
  labs(title="Dix affections les plus fréquentes",
       subtitle="Effectifs pondérés (wt_wave4) — Wave 4 (2018)",
       x=NULL, y="Part (%)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40",size=11),
        legend.position="top",
        panel.grid.major.y=element_blank())

ggsave("outputs/figures/02_types_maladies.png", p_maladies,
       width=10, height=6, dpi=300)
cat("-> 02_types_maladies.png\n=== Script 02 terminé ===\n")
