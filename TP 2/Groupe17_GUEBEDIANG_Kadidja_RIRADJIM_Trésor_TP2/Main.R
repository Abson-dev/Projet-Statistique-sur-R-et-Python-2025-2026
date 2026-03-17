############################################################
# TP2 - Analyse Education et Alphabétisation
# Nigeria General Household Survey
############################################################

############################################################
# QUESTION 1 : Préparer l’environnement de travail
############################################################

packages <- c(
  "haven",
  "dplyr",
  "forcats",
  "ggplot2",
  "rstatix",
  "ggpubr",
  "gtsummary",
  "viridis",
  "patchwork",
  "DescTools",
  "openxlsx"   
)

for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

############################################################
# QUESTION 2 : Importer et fusionner les données
############################################################

data_raw <- "data/raw/"
data_processed <- "data/processed/"
output_fig <- "output/figures/"
output_tab <- "output/tables/"

sect2b_w4 <- read_dta(paste0(data_raw,"sect2_harvestw4.dta"))
sect1_w4  <- read_dta(paste0(data_raw,"sect1_harvestw4.dta"))

data_w4 <- sect2b_w4 %>%
  left_join(sect1_w4, by = c("hhid","indiv"))

############################################################
# QUESTION 3 : Examiner les valeurs manquantes
############################################################

summary(data_w4$s2aq13a)

data_w4 %>%
  summarise(
    total = n(),
    missing = sum(is.na(s2aq13a)),
    pct_missing = mean(is.na(s2aq13a))*100
  )

############################################################
# QUESTION 4 : Construire la variable niveau d’éducation
############################################################

data_w4 <- data_w4 %>%
  mutate(
    niveau_educ = case_when(
      s2aq6 == 2 ~ "Aucun",
      s2aq9 %in% 11:16 ~ "Primaire",
      s2aq9 %in% c(21,22,23,24,25,26,27,28,321) ~ "Secondaire",
      s2aq9 %in% c(31,33,34,35,41,43,322,411,412,421,422,423,424) ~ "Tertiaire",
      s2aq9 %in% c(51,52) ~ "Religieux",
      s2aq9 == 61 ~ "Education adulte",
      TRUE ~ NA_character_
    )
  )

############################################################
# QUESTION 5 : Distribution du niveau d’éducation
############################################################

freq_educ <- data_w4 %>%
  count(niveau_educ) %>%
  mutate(prop = n/sum(n))

freq_educ

############################################################
# QUESTION 6 : Visualisation de la distribution
############################################################

plot_educ <- ggplot(freq_educ,
                    aes(x = niveau_educ,
                        y = prop,
                        fill = niveau_educ)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution du niveau d'éducation",
    x = "Niveau",
    y = "Proportion"
  )

plot_educ

ggsave(paste0(output_fig,"barplot_education.png"),
       plot_educ,
       width=8,
       height=6)

############################################################
# QUESTION 7 : Comparaison selon le sexe
############################################################

data_w4 <- data_w4 %>%
  mutate(age = 2019 - s1q6_year)

data_adult <- data_w4 %>%
  filter(age >= 18)

tab_sexe <- table(data_adult$s1q2,
                  data_adult$niveau_educ)

tab_sexe

############################################################
# QUESTION 8 : Test du Chi2
############################################################

test_chi <- chisq.test(tab_sexe)
test_chi

############################################################
# QUESTION 9 : V de Cramer
############################################################

cramer <- CramerV(tab_sexe)
cramer

############################################################
# QUESTION 10 : Visualisation sexe-éducation
############################################################

plot_sex <- ggplot(data_adult,
                   aes(x = s1q2,
                       fill = niveau_educ)) +
  geom_bar(position="fill") +
  scale_fill_viridis_d() +
  labs(
    title = "Niveau d'éducation par sexe",
    y = "Proportion",
    x = "Sexe"
  )

plot_sex

ggsave(paste0(output_fig,"education_sexe.png"),
       plot_sex,
       width=8,
       height=6)

############################################################
# QUESTION 11 : Construction groupes d’âge
############################################################

data_w4 <- data_w4 %>%
  mutate(
    age_group = case_when(
      s1q4 >= 18 & s1q4 <= 30 ~ "18-30",
      s1q4 >= 31 & s1q4 <= 45 ~ "31-45",
      s1q4 >= 46 & s1q4 <= 60 ~ "46-60",
      s1q4 > 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

data_adult <- data_w4 %>%
  filter(s1q4 >= 18)

############################################################
# QUESTION 12 : Visualisation âge-éducation
############################################################

plot_age <- ggplot(data_adult,
                   aes(x = age_group,
                       fill = niveau_educ)) +
  geom_bar(position = "fill") +
  labs(
    title = "Répartition du niveau d'éducation par groupe d'âge",
    x = "Groupe d'âge",
    y = "Proportion",
    fill = "Niveau d'éducation"
  )

plot_age

ggsave("output/figures/plot_age_education.png",
       plot = plot_age)

############################################################
# QUESTION 13 : Test Kruskal-Wallis
############################################################

data_adult <- data_adult %>%
  mutate(
    niveau_educ_num = case_when(
      niveau_educ == "Aucun" ~ 0,
      niveau_educ == "Primaire" ~ 1,
      niveau_educ == "Secondaire" ~ 2,
      niveau_educ == "Tertiaire" ~ 3,
      TRUE ~ NA_real_
    )
  )

kruskal <- kruskal_test(
  data_adult,
  niveau_educ_num ~ age_group
)

kruskal

############################################################
# QUESTION 14 : Test post-hoc (Dunn)
############################################################

dunn_test(
  data_adult,
  niveau_educ_num ~ age_group,
  p.adjust.method="bonferroni"
)

############################################################
# QUESTION 15 : Scolarisation enfants (6–17 ans)
############################################################

data_child <- data_w4 %>%
  filter(s1q4 >=6 & s1q4 <=17)

tab_school <- table(data_child$zone.x,
                    data_child$niveau_educ)

chisq.test(tab_school)

############################################################
# QUESTION 16 : Visualisation par zone
############################################################

plot_zone <- ggplot(data_child,
                    aes(x=zone.x,
                        fill=as.factor(niveau_educ)))+
  geom_bar(position="dodge")

plot_zone

ggsave("output/figures/Répartition_du_niveau_d_éducation_des_enfants_selon_la_zone_de_résidence.png",
       plot = plot_zone)

############################################################
# QUESTION 17 : Heatmap analphabétisme
############################################################

heat_data <- data_adult %>%
  group_by(state.x) %>%
  summarise(
    taux_aucun = mean(niveau_educ=="Aucun",
                      na.rm=TRUE)
  )

heatmap <- ggplot(heat_data,
                  aes(x=state.x,
                      y=1,
                      fill=taux_aucun))+
  geom_tile() +
  scale_fill_viridis()

heatmap

ggsave("output/figures/Aucune_scolarisation.png",
       plot = heatmap)

############################################################
# QUESTION 18 : Export des résultats Excel
############################################################

library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, "Frequences")
writeData(wb, "Frequences", freq_educ)

addWorksheet(wb, "Sexe_Education")
writeData(wb, "Sexe_Education", as.data.frame.matrix(tab_sexe))

addWorksheet(wb, "Chi2")
chi_result <- data.frame(
  Statistique = test_chi$statistic,
  ddl = test_chi$parameter,
  p_value = test_chi$p.value
)
writeData(wb, "Chi2", chi_result)

addWorksheet(wb, "CramerV")
writeData(wb, "CramerV", data.frame(CramerV = cramer))

addWorksheet(wb, "Kruskal")
writeData(wb, "Kruskal", kruskal)

dunn <- dunn_test(
  data_adult,
  niveau_educ_num ~ age_group,
  p.adjust.method="bonferroni"
)

addWorksheet(wb, "Dunn_test")
writeData(wb, "Dunn_test", dunn)

saveWorkbook(wb,
             file = paste0(output_tab, "resultats_analyse.xlsx"),
             overwrite = TRUE)
