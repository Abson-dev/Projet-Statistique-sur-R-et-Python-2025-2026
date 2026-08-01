rm(list=ls())
# Importation des packages
{
  library(gglorenz)
  library(ineq)
  library(gtsummary)
  library(dplyr)
  library(ggridges) 
  library(srvyr)
  library(tmap)
  library(gt)
  library(sf)
}

data <- read_dta("data/processed/data_processed.dta")
View(data)
limit <- st_read("data/raw/tcd_admin1.shp")


#7. Déclarons le plan de sondage
plan_sondage <- data |>
  as_survey_design(
    ids = grappe,
    strata =  strate,
    weights = poids_final,
    nest = TRUE
    
  )


#8. Calcul des scores de diversité

menages <- plan_sondage |> mutate(
    # Score HDDS:somme des 12 groupes alimentaires
    hdds = rowSums(across(starts_with("cons_")), na.rm = TRUE),
  
    # Classification FAO
    securite_alim = case_when(
      hdds <= 3 ~"Insécurité alimentaire sévère",
      hdds <= 6 ~"Insécurité alimentaire modérée",
      hdds <= 9 ~"Sécurité alimentaire acceptable",
      TRUE ~ "Bonne diversité alimentaire"
    ) |> factor(levels = c("Insécurité alimentaire sévère",
                           "Insécurité alimentaire modérée",
                           "Sécurité alimentaire acceptable",
                           "Bonne diversité alimentaire"), ordered = TRUE)
)


#9. Construire un indice composite de vulnérabilité aux chocs

# Tableau descriptif complet
tableau_1 <- plan_sondage |>
  select(revenu_pc, taille_menage, sexe, age, milieu,
         niveau_educ) |>
  tbl_svysummary(
    by = milieu,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", # Moyenne (écart-type) pour numériques
      all_categorical() ~"{n} ({p}%)"
    ),
    digits = all_continuous()~1,
    label = list(
      revenu_pc ~ "Revenu par tête",
      taille_menage ~ "Taille du ménage",
      sexe ~ "Sexe du chef de ménage",
      age ~ "Âge du chef de ménage"),
      
      missing = "ifany") |> 
      
      add_p() |>
        add_overall() |>
        add_n() |>
        bold_labels() |>
        modify_header(label = " ** Caractéristique ** ") |>
        modify_caption(" ** Tableau 1. Caractéristiques des ménages par milieu de
résidence ** ")

tableau_1 |> as_gt() |> gtsave("output/tables/01_table.docx")

tableau_2 <- plan_sondage |>
  select(revenu_pc, taille_menage, sexe, age, milieu,
         niveau_educ) |>
  tbl_svysummary(
    by = milieu,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", # Moyenne (écart-type) pour numériques
      all_categorical() ~"{n} ({p}%)"
    ),
    digits = all_continuous()~1,
    label = list(
      revenu_pc ~ "Revenu par tête",
      taille_menage ~ "Taille du ménage",
      sexe ~ "Sexe du chef de ménage",
      age ~ "Âge du chef de ménage"),
    
    missing = "ifany") |> 
  
  add_p() |>
  add_overall() |>
  add_n() |>
  bold_labels() |>
  modify_header(label = " ** Caractéristique ** ") |>
  modify_caption(" ** Tableau 1. Caractéristiques des ménages par région ** ")

tableau_2 |> as_gt() |> gtsave("output/tables/02_table.pdf")



# Coefficient de Gini
gini_national <- menages |>  summarise(gini_nation = ineq(revenu_pc, type="Gini", na.rm=TRUE))


# Gini par région
menages |>
  group_by(region) |>
  summarise(gini = ineq(revenu_pc, type = "Gini", na.rm = TRUE)) |>
  arrange(desc(gini))

# Courbe de Lorenz
lorenz <- ggplot(menages, aes(revenu_pc)) +
  stat_lorenz(color ="#2E75B6", size = 1.2) +
  geom_abline(linetype = "dashed", color ="gray50") +
  annotate("text", x =0.25, y = 0.75,
           label = paste0("Gini =", round(gini_national, 3)),
           size = 4, color = "#1F4E79") +
  labs(title = "Courbe de Lorenz - Revenu par tête",
       x = "Part cumulée de la population",
       y = "Part cumulée du revenu") +
  theme_minimal(base_size = 12)
ggsave("output/figures/01_fig.png", lorenz)


#12. Représentation du revenu par tête par région



# Graphique de densités par région (ridgeline plot) 
graph2 <- ggplot(menages, aes(x = revenu_pc, y = fct_reorder(region, revenu_pc, median), 
                    fill = after_stat(x))) + 
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) + 
  scale_x_log10() + 
  scale_fill_viridis_c(option = "plasma", name = "Revenu") + 
  labs(title = "Distribution du revenu par tête selon la région", 
       x = "Revenu par tête (log)", y = NULL) + 
  theme_ridges()

ggsave("output/figures/02_fig.png", graph2)


# Résumé par région du score
resume_regional <- menages |> group_by(region) |> summary(score = mean(hdds))


# Charger le shapefile
limit <- st_read("data/raw/tcd_admin1.shp") 

# Joindre les données statistiques 
carte_data <- limit |> 
  left_join(resume_regional |> select(region, score), 
            by = c("region" = "region")) 

