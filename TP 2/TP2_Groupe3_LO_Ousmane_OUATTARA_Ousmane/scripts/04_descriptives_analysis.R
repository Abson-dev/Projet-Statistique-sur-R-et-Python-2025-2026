
library(rlang)
library(dplyr)
library(here)
library(haven)
library(ggplot2)

# Charger les données brutes

data_var <- readRDS(here("data", "processed", "data_var.rds"))


# Calculer les fréquences et proportions

freq_table <- data_var %>%
  group_by(niveau_educ) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    prop = round(count / sum(count) * 100, 2)  # proportion en %
  )


# Ordonner les catégories par proportion décroissante

freq_table <- freq_table %>%
  arrange(desc(prop)) %>%
  mutate(niveau_educ = factor(niveau_educ, levels = niveau_educ))  # niveaux dans l'ordre du plus grand au plus petit


# visualiser avec un barplot horizontal coloré par catégorie

library(ggplot2)

barplot = ggplot(freq_table, aes(x = niveau_educ, y = prop, fill = niveau_educ)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(prop, "%")),    # ajouter le pourcentage
            hjust = -0.1,                     # légèrement à droite de la barre
            size = 4) +
  coord_flip() +
  labs(
    title = "Proportion des niveaux d'éducation (%)",
    x = "Niveau d'éducation",
    y = "Proportion (%)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(freq_table$prop) * 1.15)  # pour que le texte ne dépasse pas la limite


# Sauvegarder le graphique dans outputs/figures

ggsave(here("outputs", "figures", "barplot.png"),
       plot = barplot,
       width = 8, height = 5, dpi = 300)



# Comparer la distribution du niveau d'éducation entre hommes et femmes (adultes 18+): 
# graphique en barres 100% empilées côte à côte, test du chi-deux sur le tableau de contingence, 
# V de Cramér.

educ_sexe <- data_var %>%
  filter(s1q4 >= 18) %>%
  mutate( sexe = ifelse(s1q2 == 1, "Homme", "Femmme"))

dist_educ_sexe = table(educ_sexe$sexe, educ_sexe$niveau_educ)

# Sauvegarder la table

saveRDS(dist_educ_sexe, here("outputs", "tables", "dist_educ_sexe.rds"))


chi2 <- chisq.test(dist_educ_sexe)

chi2

# p-value < 0.05 → association significative entre les variables


library(lsr)

vcramer = cramersV(dist_educ_sexe)

# 0 – 0.1   lien très faible

khi2 = cbind(p_value = chi2$p.value, valeur = chi2$statistic, v = vcramer)

# Sauvegarder des résulats des tests

saveRDS(khi2, here("outputs", "tables", "chi2.rds"))





df_plot <- as.data.frame(dist_educ_sexe)

emplile = ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    x = "Sexe",
    y = "Proportion",
    fill = "Niveau d'éducation",
    title = "Répartition du niveau d'éducation selon le sexe"
  ) +
  geom_text(
    aes(label = scales::percent(Freq / ave(Freq, Var1, FUN = sum))),
    position = position_fill(vjust = 0.5),
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


# Sauvegarder le graphique dans outputs/figures

ggsave(here("outputs", "figures", "emplile.png"),
       plot = emplile,
       width = 8, height = 5, dpi = 300)





# Analyser la relation entre âge et niveau d'éducation: boxplot du niveau d'éducation (axe y 
# numérique ou ordonné) par groupe d'âge (18-30, 31-45, 46-60, 60+). Kruskal-Wallis + post-hoc Dunn. 


# unique(data_var$s1q2) # Sexe

# table(data_var$s1q2, data_var$s1q4)

# max(data_var$s1q4) # Age



data_18 <- data_var %>%
  filter(s1q4 >= 18) %>%
  mutate(
    groupe_age = case_when(
      s1q4 <= 30 ~ "18-30",
      s1q4 <= 45 ~ "31-45",
      s1q4 <= 60 ~ "46-60",
      s1q4 > 60 ~ "60 +"
    ))

# Ordre correct pour l'éducation

data_18 <- data_18 %>%
  mutate(niveau_educ = factor(niveau_educ,
                              levels = c("Aucun", "Primaire", "Junior Secondary",
                                         "Senior Secondary", "Tertiaire"),
                              ordered = TRUE))


box = ggplot(data_18, aes(x = niveau_educ, y = s1q4)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_continuous(breaks = 1:5,
                     labels = levels(data_18$niveau_educ)) +
  labs(
    x = "Niveau d'éducation",
    y = "L'âge",
    title = "Distribution de l'âge en par niveau d'éducation"
  ) +
  theme_minimal()


# Sauvegarder le graphique dans outputs/figures

ggsave(here("outputs", "figures", "box.png"),
       plot = box,
       width = 8, height = 5, dpi = 300)


kruskal_test <- kruskal.test(s1q4 ~ niveau_educ, data = data_18)
kruskal_test



# Installer si nécessaire
# install.packages("FSA")

# library(FSA)

# dunn_result <- dunnTest(s1q4 ~ niveau_educ, data = data_18,
                       # method = "bonferroni")  # correction des comparaisons multiples
# dunn_result



# 11. Comparer le taux de scolarisation des 6-17 ans entre zones rurales et urbaines (sect2a + 
# secta): tableau de contingence, test du chi-deux, graphique en barres groupées avec IC à 95%. 


# unique(data_var$s2aq6) # Aller à l'école ou pas ?

# unique(data_var$sector.x) # Milieu de résidence



sco_resi <- data_final %>%
  filter(s1q4 >= 6 & s1q4 <= 17) %>%
  mutate( statut_ecole = ifelse(s2aq6 == 1, "Scolarisé", "Non scolarisé"),
          residence = ifelse(sector.x == 1, "Urban", "Rural"))

dist_sco_resi = table(sco_resi$statut_ecole, sco_resi$residence)

# Sauvegarder la table

saveRDS(dist_sco_resi, here("outputs", "tables", "dist_sco_resi.rds"))


chi_deux <- chisq.test(dist_sco_resi)

v_cramer = cramersV(dist_sco_resi)


chi_deux = cbind(p_value = chi_deux$p.value, valeur = chi_deux$statistic, v = v_cramer)



# Sauvegarder la table

saveRDS(chi_deux, here("outputs", "tables", "chi_deux.rds"))



df <- as.data.frame(dist_sco_resi)

df



library(dplyr)

df_plot1 <- df %>%
  group_by(Var1) %>%
  mutate(
    total = sum(Freq),
    prop = Freq / total,
    se = sqrt(prop * (1 - prop) / total),
    lower = prop - 1.96 * se,
    upper = prop + 1.96 * se
  )




plot_group <- ggplot(df_plot1, aes(x = Var2, y = prop, fill = Var1)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Milieu de résidence",
    y = "Proportion",
    fill = "Statut scolaire",
    title = "Taux de scolarisation des 6–17 ans selon la zone (IC 95%)"
  ) +
  theme_minimal()

plot_group



# Sauvegarder le graphique dans outputs/figures

ggsave(here("outputs", "figures", "taux_sco_resi.png"),
       plot = plot_group,
       width = 8, height = 5, dpi = 300)






# 12. Heatmap (geom_tile) croisant État nigérian (state) et niveau d'éducation: part d'adultes sans 
# instruction par État, colorée par quintile.

library(dplyr)

data_adult <- data_var %>%
  filter(s1q4 >= 18)


state_educ <- data_adult %>%
  group_by(state.x) %>%
  summarise(
    total = n(),
    aucun = sum(niveau_educ == "Aucun", na.rm = TRUE),
    part_aucun = aucun / total
  )

state_educ <- state_educ %>%
  mutate(
    quintile = factor(ntile(part_aucun, 5))
  )



heat_data <- state_educ %>%
  mutate(niveau = "Sans instruction")


heat_data <- heat_data %>%
  arrange(part_aucun)

library(ggplot2)

heatmap_plot <- ggplot(heat_data, aes(x = niveau, y = state.x, fill = quintile)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Reds") +
  labs(
    x = "Niveau d'éducation",
    y = "État",
    fill = "Quintile",
    title = "Part d'adultes sans instruction par État nigérian"
  ) +
  theme_minimal()

heatmap_plot



# Sauvegarder le graphique dans outputs/figures

ggsave(here("outputs", "figures", "heatmap_plot.png"),
       plot = heatmap_plot,
       width = 8, height = 5, dpi = 300)


