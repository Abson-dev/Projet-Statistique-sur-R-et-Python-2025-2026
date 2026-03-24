source("R/fonctions.R")
sect2_harvestw4 <- read_dta("data/raw/NGA-GHSP-W4/sect2_harvestw4.dta")
sect1_harvestw4 <- read_dta("data/raw/NGA-GHSP-W4/sect1_harvestw4.dta")
View(sect1_harvestw4)
View(sect2_harvestw4)
join <- sect1_harvestw4 %>%
  full_join(sect2_harvestw4, by = c("hhid", "indiv"))
view(join)
colnames(join)
sum(is.na(join$s2aq10))
sum(is.na(sect2_harvestw4$s2aq10))

attr(join$s2aq10, "labels")

join <- join %>%
  mutate(niveau_educ = case_when(
    s2aq10 == 1 ~ "Aucun",
    s2aq10 == 2 ~ "Primaire",
    s2aq10 == 5 ~ "Junior Secondary",
    s2aq10 == 6 ~ "Senior Secondary",
    !s2aq10 %in% c(1, 2, 5, 6, NA) ~ "Tertiaire",
    TRUE ~ NA_character_  
  ))


freq_educ <- join %>%
  filter(!is.na(niveau_educ)) %>%
  count(niveau_educ) %>%
  mutate(proportion = n / sum(n) * 100) %>%
  arrange(desc(proportion))
print(freq_educ)
write.csv(freq_educ, "output/tables/freq_educ.csv", row.names = FALSE)
prop_educ <- ggplot(freq_educ, aes(x = proportion, 
                      y = reorder(niveau_educ, proportion), 
                      fill = niveau_educ)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.2, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Répartition par niveau d'éducation",
    x = "Proportion (%)",
    y = "Niveau d'éducation"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  xlim(0, max(freq_educ$proportion) * 1.15)  

ggsave("output/figures/barplot_niveau_educ.png", 
       plot = prop_educ,
       width = 8, height = 5, dpi = 300)

adultes <- join %>%
  filter(s1q4 >= 18, !is.na(niveau_educ), !is.na(s1q2))
tab <- table(adultes$s1q2, adultes$niveau_educ)
print(tab)
adultes_clean <- adultes %>%
  filter(!is.na(s1q2), !is.na(niveau_educ))

tab <- table(adultes_clean$s1q2, adultes_clean$niveau_educ)
print(tab)
write.csv(tab, "output/tables/tab.csv", row.names = FALSE)

# Proportions par sexe (en ligne)
prop.table(tab, margin = 1) * 100

freq_sexe_educ <- adultes_clean %>%
  count(s1q2, niveau_educ) %>%
  group_by(s1q2) %>%
  mutate(proportion = n / sum(n) * 100,
         niveau_educ = factor(niveau_educ, 
                              levels = c("Aucun", "Primaire", "Junior Secondary",
                                         "Senior Secondary", "Tertiaire")))

educ_sexe <- ggplot(freq_sexe_educ, aes(x = s1q2, y = proportion, fill = niveau_educ)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Niveau d'éducation par sexe (adultes 18+)",
    x = "Sexe", y = "Proportion (%)",
    fill = "Niveau d'éducation"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("output/figures/distr_educ_sex.png" , plot=educ_sexe,width = 8, height = 5, dpi = 300)

chi2 <- chisq.test(tab)
print(chi2)

n <- sum(tab)
k <- min(nrow(tab), ncol(tab)) - 1
v_cramer <- sqrt(chi2$statistic / (n * k))
cat("V de Cramér :", round(v_cramer, 3))


adultes_clean <- adultes_clean %>%
  mutate(
    # Groupes d'âge
    groupe_age = case_when(
      s1q4 >= 18 & s1q4 <= 30 ~ "18-30",
      s1q4 >= 31 & s1q4 <= 45 ~ "31-45",
      s1q4 >= 46 & s1q4 <= 60 ~ "46-60",
      s1q4 >  60             ~ "60+",
      TRUE ~ NA_character_
    ),
    groupe_age = factor(groupe_age, 
                        levels = c("18-30", "31-45", "46-60", "60+")),
    
    # Niveau d'éducation ordonné numérique
    niveau_educ_num = as.numeric(factor(niveau_educ,
                                        levels = c("Aucun", "Primaire", 
                                                   "Junior Secondary",
                                                   "Senior Secondary", 
                                                   "Tertiaire"),
                                        ordered = TRUE))
  ) %>%
  filter(!is.na(groupe_age), !is.na(niveau_educ))

box_age <- ggplot(adultes_clean, aes(x = groupe_age, y = niveau_educ_num, fill = groupe_age)) +
  geom_boxplot(width = 0.5, outlier.colour = "red", outlier.size = 1.5) +
  scale_y_continuous(
    breaks = 1:5,
    labels = c("Aucun", "Primaire", "Junior Secondary", "Senior Secondary", "Tertiaire")
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribution du niveau d'éducation par groupe d'âge",
    x = "Groupe d'âge", y = "Niveau d'éducation"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("output/figures/box_age.png", plot= box_age,width = 8, height = 5, dpi = 300)
kruskal.test(niveau_educ_num ~ groupe_age, data = adultes_clean)
write.csv(adultes_clean, "output/tables/adultes_clean.csv", row.names = FALSE)


dunn.test(adultes_clean$niveau_educ_num, 
          adultes_clean$groupe_age,
          method = "bonferroni",   # correction : "bonferroni", "holm", "bh"
          kw = TRUE, 
          label = TRUE)

scolar <- join %>%
  filter(s1q4 >= 6 & s1q4 <= 17, !is.na(sector.x), !is.na(s2aq13a)) %>%
  mutate(
    milieu    = factor(sector.x,    labels = c("Urbain", "Rural")), 
    scolarise = factor(s2aq13a, labels = c("Scolarisé", "Non scolarisé"))
  )

tab_scol <- table(scolar$sector.x, scolar$s2aq13a)
print(tab_scol)

# Proportions par milieu
prop.table(tab_scol, margin = 1) * 100

# Chi-deux
chi2_scol <- chisq.test(tab_scol)
print(chi2_scol)


freq_scol <- scolar %>%
  group_by(sector.x, s2aq13a) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sector.x) %>%
  mutate(
    total = sum(n),
    prop  = n / total,
    se    = sqrt(prop * (1 - prop) / total),
    ic_low  = pmax(0, prop - 1.96 * se),
    ic_high = pmin(1, prop + 1.96 * se)
  ) %>%
  filter(s2aq13a == 1)
freq_scol <- scolar %>%
  mutate(sector.x = factor(sector.x, 
                           levels = c(1, 2),
                           labels = c("Urbain", "Rural"))) %>%  
  group_by(sector.x, scolarise) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sector.x) %>%
  mutate(
    total   = sum(n),
    prop    = n / total,
    se      = sqrt(prop * (1 - prop) / total),
    ic_low  = pmax(0, prop - 1.96 * se),
    ic_high = pmin(1, prop + 1.96 * se)
  ) %>%
  filter(scolarise == "Scolarisé")  

sect_educ <- ggplot(freq_scol, aes(x = sector.x, y = prop, fill = sector.x)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            vjust = -0.8, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title    = "Taux de scolarisation (6-17 ans) par milieu",
    subtitle = paste0("Chi² = ", round(chi2_scol$statistic, 2),
                      ", p = ", format.pval(chi2_scol$p.value, digits = 3)),
    x = "Milieu", y = "Taux de scolarisation"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("output/figures/sect_educ.png", plot= sect_educ,width = 8, height = 5, dpi = 300)

heatmap_data <- join %>%
  filter(s1q4 >= 18, !is.na(state.x), !is.na(niveau_educ)) %>%
  group_by(state.x) %>%
  summarise(
    total      = n(),
    prop_aucun = mean(niveau_educ == "Aucun") * 100,
    .groups    = "drop"
  ) %>%
  mutate(
    quintile = cut(prop_aucun,
                   breaks = unique(quantile(prop_aucun, probs = seq(0, 1, 0.2), na.rm = TRUE)),
                   labels = c("Q1 (plus instruit)", "Q2", "Q3", "Q4", "Q5 (moins instruit)"),
                   include.lowest = TRUE),
    quintile = factor(quintile)  )      

part_adult_inst <- ggplot(heatmap_data, aes(x = 1, y = reorder(state.x, prop_aucun), fill = quintile)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(prop_aucun, 1), "%")), size = 3) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +  # ✅ fonctionne avec facteur
  labs(
    title = "Part d'adultes sans instruction par État",
    y = "État", x = NULL, fill = "Quintile"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title   = element_text(face = "bold", hjust = 0.5)
  )
write.csv(heatmap_data, "output/tables/heatmap_data.csv", row.names = FALSE)

ggsave("output/figures/part_adult_inst.png", plot= part_adult_inst,width = 8, height = 5, dpi = 300)