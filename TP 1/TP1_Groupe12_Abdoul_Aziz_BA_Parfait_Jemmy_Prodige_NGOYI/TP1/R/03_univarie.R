# =====================================================
# SCRIPT : 03_univarie.R
# OBJECTIF : Analyse univariée de l'âge
# =====================================================

# =========================
# 1. Chargement des packages
# =========================
library(tidyverse)

# =========================
# 2. Chargement des données propres
# =========================
data_clean <- readRDS("data/processed/data_clean.rds")

# =========================
# 3. Statistiques descriptives
# =========================
stats_age <- data_clean %>%
  summarise(
    n = n(),
    moyenne = mean(s1q4, na.rm = TRUE),
    mediane = median(s1q4, na.rm = TRUE),
    ecart_type = sd(s1q4, na.rm = TRUE),
    min = min(s1q4, na.rm = TRUE),
    Q1 = quantile(s1q4, 0.25, na.rm = TRUE),
    Q3 = quantile(s1q4, 0.75, na.rm = TRUE),
    max = max(s1q4, na.rm = TRUE),
    CV = (sd(s1q4, na.rm = TRUE) / mean(s1q4, na.rm = TRUE)) * 100
  )

print(stats_age)

# =========================
# 4. Histogramme
# =========================
ggplot(data_clean, aes(x = s1q4)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution de l'âge",
    x = "Âge",
    y = "Effectif"
  ) +
  theme_minimal()

# =========================
# 5. Boxplot
# =========================
ggplot(data_clean, aes(y = s1q4)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Boxplot de l'âge",
    y = "Âge"
  ) +
  theme_minimal()

# =========================
# 6. Test de normalité (Shapiro-Wilk)
# =========================

set.seed(123)
sample_data <- sample(data_clean$s1q4, 5000)
shapiro.test(sample_data)

View(data_clean)

# =========================
# 7. Construction de la pyramide
# =========================

# Pyramide des âges (groupes de 5 ans) par sexe pour la vague 4
pyramid_data <- data_clean %>%
  count(age_group, sexe)
# mettre les les hommes en négatif
pyramid_data <- pyramid_data %>%
  mutate(effectif = ifelse(sexe == "Homme",-n, n))

#Construction dela pyramide
ggplot(pyramid_data, aes(x=age_group,y=effectif,fill=sexe))+
  geom_bar(stat="identity")+
  coord_flip() +
  labs(
    title = "Pyramidedesâges-Vague4",
    x="Groupesd'âge",
    y="Effectif"
  )+
  theme_minimal()

# =========================
# 8.  Fréquences du lien de parenté (chef de ménage, conjoint, enfant, autre)
# =========================

#Calculer les fréquences et proportions de chaque modalité

freq_data<-data_clean%>%
  count(lien_parente)%>%
  mutate(
    proportion=n/sum(n)
  )
freq_data

#ordonner les frequences
freq_data<-freq_data %>%
  arrange(proportion)

#calcul des intervalles de confiance à 95%,avec binom.test
freq_data<-freq_data %>%
  rowwise()%>%
  mutate(
    ic=list(binom.test(n,sum(freq_data$n))$conf.int),
    ic_inf=ic[1],
    ic_sup=ic[2]
  ) %>%
  ungroup()
View(freq_data)

#barplot
ggplot(freq_data, aes(x= reorder(lien_parente,proportion),y=proportion)) +
  geom_bar(stat="identity",fill = "steelblue") +
  geom_errorbar(aes(ymin=ic_inf,ymax=ic_sup),width=0.2) +
  geom_text(aes(label=scales::percent(proportion,accuracy=1)),
            hjust=-0.1,size=3)+ 
  coord_flip()+
  labs(
    title="Lien de parenté avec le chef de ménage",
    x="Modalités",
    y="Proportion"
  ) +
  theme_minimal()

# =========================
# 9.  Comparer la taille moyenne des ménages entre zones rurales et urbaines
# =========================

#Calculer taille des menages
data_clean<-data_clean %>%
  group_by(hhid)%>%
  mutate(taille_men=n())%>%
  ungroup()

#traitement de la variable sector
#Transformer en facteur avec labels
data_clean$sector<-factor(data_clean$sector,
                           levels= c(1,2), #les valeurs numériques actuelles
                           labels= c("Urbain","Rural")) #nouveaux labels


#Boxplot des tailles de ménages par sector
ggplot(data_clean,aes(x=sector,y=taille_men,fill=sector))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun=mean,geom="point",color="red",size=3) + #moyenne
  labs(
    title = "Distribution de la taille des ménages par zone",
    x="Zone",
    y="Taille du ménage"
  )+
  theme_minimal()+
  scale_fill_manual(values= c("Urbain"="steelblue","Rural"="orange"))

# =========================
# test de Wilcoxon-Mann-Whitney
# =========================

wilcox_test <- wilcox.test(taille_men ~ sector,
                           data = data_clean,
                           conf.int = TRUE,
                           conf.level = 0.95)
print(wilcox_test)

# la taille d'effet

effect_size <- data_clean %>%
  wilcox_effsize(taille_men ~ sector)
effect_size

# =========================
# Tableau récapitulatif gtsummary stratifié par zone (rural/urbain)
# =========================

# =============================================================================
# TACHE 6 — Tableau gtsummary optimisé
# =============================================================================

library(dplyr)
library(gtsummary)
library(gt)
library(here)

# Préparer les données
donnees_tableau <- data_clean |>
  filter(!is.na(sector)) |>
  select(
    sector,        # variable de stratification
    s1q4,          # âge en années complètes
    sexe,          # sexe recodé (Homme / Femme)
    taille_men # taille du ménage (niveau ménage répété par individu)
  )

# Tableau descriptif gtsummary
tableau <- donnees_tableau |>
  tbl_summary(
    by = sector,
    label = list(
      s1q4          ~ "Age (années)",
      sexe          ~ "Sexe",
      taille_men ~ "Taille du ménage"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25} - {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing      = "ifany",
    missing_text = "Valeurs manquantes"
  ) |>
  add_overall(last = FALSE) |>
  add_p() |>
  modify_header(
    label         = "**Variable**",
    all_stat_cols() ~ "**{level}**\nN = {n}"
  ) |>
  bold_labels()

View(tableau_csv)

saveRDS(tableau, "data/processed/tableau_csv.rds")