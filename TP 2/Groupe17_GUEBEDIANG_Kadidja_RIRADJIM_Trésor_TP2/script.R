############################################################
# TP2 - Analyse Education et Alphabétisation
# Nigeria General Household Survey avec ponderation
############################################################

############################################################
# QUESTION 1 : Préparer l’environnement de travail
############################################################

packages <- c(
  "haven","dplyr","forcats","ggplot2","rstatix",
  "ggpubr","gtsummary","viridis","patchwork",
  "DescTools","openxlsx","survey"   
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
secta_w4  <- read_dta(paste0(data_raw,"secta_harvestw4.dta"))

############################################################
# NETTOYAGE AVANT FUSION
############################################################

secta_w4_incomplet <- secta_w4 %>%
  filter(is.na(wt_wave4) | wt_wave4 == 0)

nrow(secta_w4_incomplet) 

secta_valide <- secta_w4 %>%
  filter(!is.na(wt_wave4) & wt_wave4 > 0)

############################################################
# FUSION
############################################################

data_w4 <- sect2b_w4 %>%
  inner_join(sect1_w4, by = c("hhid","indiv")) %>%
  inner_join(secta_valide %>% select(hhid, wt_wave4), by = "hhid")

names(data_w4) <- gsub("\\.", "_", names(data_w4))

############################################################
# Vérifications
############################################################

sum(is.na(data_w4$wt_wave4))
sum(data_w4$wt_wave4 == 0)

############################################################
# QUESTION 3 : Valeurs manquantes
############################################################

summary(data_w4$s2aq13a)

data_w4 %>%
  summarise(
    total = n(),
    missing = sum(is.na(s2aq13a)),
    pct_missing = mean(is.na(s2aq13a))*100
  )

############################################################
# QUESTION 4 : Niveau d’éducation
############################################################

data_w4 <- data_w4 %>%
  mutate(
    niveau_educ = case_when(
      s2aq6 == 2 ~ "Aucun",
      s2aq9 %in% 11:16 ~ "Primaire",
      s2aq9 %in% c(21:28,321) ~ "Secondaire",
      s2aq9 %in% c(31,33,34,35,41,43,322,411:424) ~ "Tertiaire",
      s2aq9 %in% c(51,52) ~ "Religieux",
      s2aq9 == 61 ~ "Education adulte",
      TRUE ~ NA_character_
    )
  )

############################################################
# AJOUT DESIGN (APRÈS VARIABLES)
############################################################

design <- svydesign(
  ids = ~1,
  weights = ~wt_wave4,
  data = data_w4
)

############################################################
# QUESTION 5 : Distribution (pondérée)
############################################################

freq_educ <- data_w4 %>%
  group_by(niveau_educ) %>%
  summarise(poids = sum(wt_wave4, na.rm=TRUE)) %>%
  mutate(prop = poids/sum(poids))

freq_educ

############################################################
# QUESTION 6 : Graphique (pondéré)
############################################################
tab_sexe <- svytable(~s1q2 + niveau_educ, design_adult)

tab_df <- as.data.frame(tab_sexe)

tab_df <- tab_df %>%
  group_by(s1q2) %>%
  mutate(prop = Freq / sum(Freq))

plot_sex <- ggplot(tab_df,
                   aes(x = s1q2,
                       y = prop,
                       fill = niveau_educ)) +
  geom_bar(stat="identity", position="fill") +
  scale_fill_viridis_d() +
  labs(
    title = "Niveau d'éducation par sexe (pondéré)",
    x = "Sexe",
    y = "Proportion"
  )

ggsave(paste0(output_fig,"education_sexe.png"),
       plot_sex,
       width=8,
       height=6)
############################################################
# QUESTION 7 : Sexe
############################################################



data_w4 <- data_w4 %>%
  mutate(age = 2019 - s1q6_year)

design <- svydesign(
  ids = ~1,
  weights = ~wt_wave4,
  data = data_w4
)

data_adult <- data_w4 %>%
  filter(age >= 18)

design_adult <- subset(design, age >= 18)

tab_sexe <- svytable(~s1q2 + niveau_educ, design_adult)

tab_sexe

############################################################
# QUESTION 8 : Chi2 pondéré
############################################################

test_chi <- svychisq(~s1q2 + niveau_educ, design_adult)

test_chi

############################################################
# QUESTION 9 : V de Cramer
############################################################

cramer <- CramerV(tab_sexe)
cramer

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

############################################################
# QUESTION 10 : Graphique pondéré
############################################################

design <- svydesign(
  ids = ~1,
  weights = ~wt_wave4,
  data = data_w4
)

design_adult <- subset(design, age_group %in% c("18-30","31-45","46-60","60+"))

tab_age <- svytable(~age_group + niveau_educ, design_adult)

tab_age_df <- as.data.frame(tab_age)

tab_age_df <- tab_age_df %>%
  group_by(age_group) %>%
  mutate(prop = Freq / sum(Freq))


plot_sex <- ggplot(data_adult,
                   aes(x = s1q2,
                       weight = wt_wave4,
                       fill = niveau_educ)) +
  geom_bar(position="fill") +
  scale_fill_viridis_d()

plot_sex <- ggplot(tab_df,
                   aes(x = s1q2,
                       y = prop,
                       fill = niveau_educ)) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 3) +
  scale_fill_viridis_d() +
  labs(
    title = "Niveau d'éducation par sexe (pondéré)",
    x = "Sexe",
    y = "Proportion"
  )
ggsave("output/figures/plot_age_education.png",
       plot_age,
       width = 8,
       height = 6)
############################################################
# QUESTION 11 : Groupes d’âge
############################################################


data_adult <- data_w4 %>%
  filter(s1q4 >= 18)

############################################################
# QUESTION 12 : Graphique âge pondéré
############################################################

tab_zone <- svytable(~zone_x + niveau_educ, design_child)

tab_zone_df <- as.data.frame(tab_zone)


tab_zone_df <- tab_zone_df %>%
  group_by(zone_x) %>%
  mutate(prop = Freq / sum(Freq))


plot_zone <- ggplot(tab_zone_df,
                    aes(x = zone_x,
                        y = prop,
                        fill = niveau_educ)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 3) +
  labs(
    title = "Répartition du niveau d'éducation selon le milieu",
    x = "Zone",
    y = "Proportion"
  )
ggsave("output/figures/zone.png",
       plot_zone,
       width = 8,
       height = 6)
############################################################
# QUESTION 13 : Kruskal-Wallis (non pondéré ⚠️)
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

############################################################
# QUESTION 15 : Enfants
############################################################

data_child <- data_w4 %>%
  filter(s1q4 >=6 & s1q4 <=17)

design_child <- subset(design, s1q4 >=6 & s1q4 <=17)

tab_school <- xtabs(wt_wave4 ~ zone_x + niveau_educ, data = data_child)

svychisq(~zone_x + niveau_educ, design_child)

############################################################
# QUESTION 16 : Graphique pondéré
############################################################

tab_zone <- svytable(~zone_x + niveau_educ, design_child)

tab_zone_df <- as.data.frame(tab_zone)


tab_zone_df <- tab_zone_df %>%
  group_by(zone_x) %>%
  mutate(prop = Freq / sum(Freq))

plot_zone <- ggplot(tab_zone_df,
                    aes(x = zone_x,
                        y = prop,
                        fill = niveau_educ)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Répartition du niveau d'éducation selon le milieu (pondéré)",
    x = "Zone",
    y = "Proportion"
  )

ggsave("output/figures/zone.png",
       plot_zone,
       width = 8,
       height = 6)
############################################################
# QUESTION 17 : Heatmap pondérée
############################################################

heat_data <- data_adult %>%
  group_by(state_x) %>%
  summarise(
    taux_aucun = weighted.mean(niveau_educ=="Aucun",
                               wt_wave4,
                               na.rm=TRUE)
  )

heat_data <- svyby(
  ~I(niveau_educ == "Aucun"),
  ~state_x,
  design_adult,
  svymean,
  na.rm = TRUE
)

heat_data <- as.data.frame(heat_data)
names(heat_data)[2] <- "taux_aucun"


heatmap <- ggplot(heat_data,
                  aes(x = state_x,
                      y = 1,
                      fill = taux_aucun)) +
  geom_tile(color = "white") +
  scale_fill_viridis(
    name = "Taux",
    labels = scales::percent
  ) +
  labs(
    title = "Taux d'absence de scolarisation par État (pondéré)",
    x = "État",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

ggsave("output/figures/Aucune_scolarisation.png",
       heatmap,
       width = 10,
       height = 4)
############################################################
# QUESTION 18 : Export Excel
############################################################

wb <- createWorkbook()

addWorksheet(wb, "Frequences")
writeData(wb, "Frequences", freq_educ)

addWorksheet(wb, "Sexe_Education")
writeData(wb, "Sexe_Education", as.data.frame.matrix(tab_sexe))

addWorksheet(wb, "Chi2")
writeData(wb, "Chi2", data.frame(p_value = test_chi$p.value))

addWorksheet(wb, "CramerV")
writeData(wb, "CramerV", data.frame(CramerV = cramer))

addWorksheet(wb, "Kruskal")
writeData(wb, "Kruskal", kruskal)

saveWorkbook(wb,
             file = paste0(output_tab, "resultats_analyse.xlsx"),
             overwrite = TRUE)