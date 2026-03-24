# Nettoyage de l'environnement
rm(list=ls())
# importation des packages
{
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(apyramid)
  library(naniar)
  library(gtsummary)
  library(rstatix)
  library(PropCIs)
  library(patchwork)
}
# Chargement de sect1_harvestw4 avec haven
data <- read_stata("data/raw/sect1_harvestw4.dta")

# 1.
# Exploration de la structure
str(data)
glimpse(data)
summary(data)

# Vérification des doublons
data %>% count(hhid, indiv) %>% filter(n > 1) %>% count()

# Vérification des valeurs manquantes
vis_miss(data, warn_large_data = F)

# 2. Analyse univariée de l'âge des membres

# Histogramme avec des effectifs de 5 par classe
ggplot(data, aes(x = s1q4)) +
  geom_histogram(binwidth = 5, fill="darkgreen")

# Boîte à moustaches
ggplot(data, aes(x = s1q4)) +
  geom_boxplot()

# Statistiques descriptives pour la variable s1q4
stat_desc_age <- data %>% 
                  select(s1q4) %>%
                      tbl_summary(
                        type = s1q4~"continuous2",
                        statistic = all_continuous() ~ c("{mean}","{median}","{p25}","{p75}"),
                        label = s1q4~"Âge")
stat_desc_age

# Test de normalité de SHapiro wilk (inadapté car la taille de l'échantillon dépasse 5000 individus)
shapiro.test(data$s1q4)

# 3. Pyramide des âges
data_pyramid <- data %>% 
  select(age=s1q4, sexe=s1q2) %>% 
  filter(!is.na(age), !is.na(sexe)) %>% 
  mutate(age_group = cut(age, breaks = seq(0,100, by=5)))
age_pyramid(data_pyramid, age_group = age_group, split_by = sexe)

# Création de la pyramide
pyramide <- age_pyramid(
  data_pyramid, 
  age_group = age_group, 
  split_by = sexe
) +
  labs(
    title = "Pyramide des âges - Vague 4",
    subtitle = paste0("Enquête Harvest W4 (n = ", nrow(data_pyramid), " individus)"),
    caption = "Source: sect1_harvestw4.dta",
    x = "Effectif",
    y = "Groupe d'âge"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )
# Export de la pyramide
ggsave(
  filename = "output/figures/pyramide_ages_vague4.png",
  plot = pyramide,
  width = 10,
  height = 8,
  dpi = 300
)

# 4. Fréquence des liens de parenté
# Diagramme en barres

data %>% 
  count(s1q3) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x=n, y=reorder(s1q3, n)))+
  geom_col(fill="darkblue")+
  labs(x="fréquence", y='')
# Calcul des proportions avec intervalle de confiance à 95%

total <- nrow(data)

data %>%
  filter(!is.na(s1q3)) %>%
  count(s1q3, name = "effectif") %>%
  mutate(
    proportion = effectif / total,
    IC_lower = sapply(effectif, function(x) scoreci(x, total, conf.level=0.95)$conf.int[1]),
    IC_upper = sapply(effectif, function(x) scoreci(x, total, conf.level=0.95)$conf.int[2])
  )


# 5. Comparaison de la taille moyenne des ménages entre zones rurales et urbaines
# Boxplot
data_2 <- read_stata("data/raw/secta_harvestw4.dta")


data_group <- data %>% 
                select(hhid) %>% 
                group_by(hhid) %>% 
                summarise(taille_men = n())

data_group <- inner_join(data_group, data_2, by="hhid")

ggplot(data_group, aes(x = as_factor(sector), y = taille_men)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Zone", y = "Taille ménage")

# Test de Wilcoxox-Man-Whitney
wilcox.test(taille_men ~ as_factor(sector), data = data_group)

# 6. Tableau récapitulatif gtsummary stratifié par zone
data_2 <- data_2 %>% select(hhid, sector)
data <- inner_join(data, data_2, by="hhid")

data <- data %>% group_by(hhid) %>% mutate(taille_men = n()) %>% ungroup()

data_tab <- data %>%
  select(hhid, s1q4, s1q2, taille_men, sector.y) %>%
  inner_join(data_2 %>% select(hhid, sector), by = "hhid") %>%
  mutate(
    sector = as_factor(sector),
    s1q2 = as_factor(s1q2)
  )

# Tableau récapitulatif stratifié par zone


tab_resume <- data_tab %>%
  mutate(
    sector.y = haven::as_factor(sector.y),
    s1q2 = haven::as_factor(s1q2)
  ) %>%
  select(sector.y, s1q4, s1q2, taille_men) %>%
  tbl_summary(
    by = sector.y,
    label = list(
      s1q4 ~ "Âge",
      s1q2 ~ "Sexe",
      taille_men ~ "Taille du ménage"
    ),
    type = list(
      s1q4 ~ "continuous2",
      taille_men ~ "continuous2"
    ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{median} [{p25} ; {p75}]"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      s1q4 ~ "wilcox.test",
      s1q2 ~ "chisq.test",
      taille_men ~ "wilcox.test"
    )
  ) %>%
  bold_labels()
# Export en HTML (interactif)
tab_resume %>%
  as_gt() %>%
  gt::gtsave(filename = "output/tables/statistiques_demo_par_zone.html")

tab_resume
