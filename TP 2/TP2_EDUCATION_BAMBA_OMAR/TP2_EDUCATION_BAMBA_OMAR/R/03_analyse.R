# =============================================================
# 03_analyse.R — Analyses éducation
# Questions 8 à 12
# =============================================================
# -- Charger les données jointes
df_education <- readRDS(here("data", "processed", "df_education_w4.rds"))

# ── QUESTION 8 ────────────────────────────────────────────────
df_education <- df_education %>%
  mutate(
    niveau_educ = case_when(
      s2aq9 == 0                        ~ "Aucun",
      s2aq9 %in% c(1, 2, 3, 11:16)     ~ "Primaire",
      s2aq9 %in% c(21:28)               ~ "Junior Secondary",
      s2aq9 %in% c(31:35)               ~ "Senior Secondary",
      s2aq9 %in% c(41, 43, 51, 52, 61,
                   321, 322, 411, 412,
                   421, 422, 423, 424)  ~ "Tertiaire",
      TRUE                              ~ NA_character_
    ),
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun", "Primaire", "Junior Secondary",
                                    "Senior Secondary", "Tertiaire"),
                         ordered = TRUE)
  )

freq_educ <- df_education %>%
  filter(!is.na(niveau_educ)) %>%
  count(niveau_educ) %>%
  mutate(proportion = n / sum(n) * 100)

sauvegarder_tab(freq_educ, "Q8_frequences_niveau_educ.csv")

p_educ <- ggplot(freq_educ, aes(x = proportion, y = niveau_educ,
                                fill = niveau_educ)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.2, size = 3.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title   = "Niveau d'éducation des membres – W4",
       x       = "Proportion (%)", y = NULL,
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

sauvegarder_fig(p_educ, "Q8_barplot_niveau_educ.png")

#9
df_adultes <- df_education %>%
  filter(!is.na(niveau_educ), !is.na(s1q2), s1q4 >= 18) %>%
  mutate(
    sexe = case_when(
      as.numeric(s1q2) == 1 ~ "Masculin",
      as.numeric(s1q2) == 2 ~ "Féminin"
    ),
    groupe_age = case_when(
      s1q4 >= 18 & s1q4 <= 30 ~ "18-30",
      s1q4 >= 31 & s1q4 <= 45 ~ "31-45",
      s1q4 >= 46 & s1q4 <= 60 ~ "46-60",
      s1q4 >  60               ~ "60+"
    ),
    groupe_age = factor(groupe_age,
                        levels = c("18-30", "31-45", "46-60", "60+")),
    niveau_num = as.numeric(niveau_educ)
  )
# ── QUESTION 10 ───────────────────────────────────────────────
# -- Boxplot
p_age_educ <- ggplot(df_adultes %>% filter(!is.na(groupe_age)),
                     aes(x = groupe_age, y = niveau_num, fill = groupe_age)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(breaks = 1:5,
                     labels = levels(df_adultes$niveau_educ)) +
  labs(title   = "Niveau d'éducation par groupe d'âge – W4",
       x = "Groupe d'âge", y = "Niveau d'éducation",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

sauvegarder_fig(p_age_educ, "Q10_boxplot_age_educ.png")

# -- Kruskal-Wallis
kruskal_res <- kruskal.test(niveau_num ~ groupe_age,
                            data = df_adultes %>% filter(!is.na(groupe_age)))
print(kruskal_res)

# -- Post-hoc Dunn
dunn_res <- dunn_test(df_adultes %>% filter(!is.na(groupe_age)),
                      niveau_num ~ groupe_age,
                      p.adjust.method = "bonferroni")
print(dunn_res)
sauvegarder_tab(as.data.frame(dunn_res), "Q10_dunn_test.csv")


# ── QUESTION 11 ───────────────────────────────────────────────
# -- Charger secta pour avoir la zone
secta_w4 <- charger_dta("secta_harvest", 4)

df_scol <- df_education %>%
  filter(s1q4 >= 6, s1q4 <= 17) %>%
  left_join(secta_w4 %>% select(hhid, zone_secta = zone), by = "hhid") %>%
  mutate(
    zone_label = case_when(
      as.numeric(zone_secta) == 1 ~ "Urbain",
      as.numeric(zone_secta) == 2 ~ "Rural"
    ),
    scolarise = case_when(
      as.numeric(s2aq13) == 1 ~ "Scolarisé",
      as.numeric(s2aq13) == 2 ~ "Non scolarisé",
      TRUE                    ~ NA_character_
    )
  ) %>%
  filter(!is.na(zone_label), !is.na(scolarise))

# -- Tableau de contingence + chi-deux
tab_scol <- table(df_scol$zone_label, df_scol$scolarise)
print(tab_scol)
print(chisq.test(tab_scol))

# -- Barplot groupé avec IC 95%
freq_scol <- df_scol %>%
  count(zone_label, scolarise) %>%
  group_by(zone_label) %>%
  mutate(
    prop    = n / sum(n),
    ic_bas  = mapply(function(x) binom.test(x, sum(n))$conf.int[1], n),
    ic_haut = mapply(function(x) binom.test(x, sum(n))$conf.int[2], n)
  )

p_scol <- ggplot(freq_scol, aes(x = zone_label, y = prop, fill = scolarise)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                position = position_dodge(0.9), width = 0.25) +
  scale_y_continuous(labels = percent_format()) +
  labs(title   = "Taux de scolarisation (6-17 ans) par zone – W4",
       x = NULL, y = "Proportion (%)", fill = NULL,
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_scol, "Q11_scolarisation_zone.png")


# ── QUESTION 12 ───────────────────────────────────────────────
df_heatmap <- df_education %>%
  filter(!is.na(niveau_educ), s1q4 >= 18) %>%
  left_join(secta_w4 %>% select(hhid, state_secta = state), by = "hhid") %>%
  group_by(state_secta) %>%
  summarise(
    pct_aucun = mean(niveau_educ == "Aucun", na.rm = TRUE) * 100,
    .groups   = "drop"
  ) %>%
  mutate(quintile = ntile(pct_aucun, 5))

sauvegarder_tab(df_heatmap, "Q12_heatmap_data.csv")

p_heatmap <- ggplot(df_heatmap,
                    aes(x = 1,
                        y = reorder(as.factor(state_secta), pct_aucun),
                        fill = pct_aucun)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "% sans instruction") +
  labs(title   = "Part d'adultes sans instruction par État – W4",
       x = NULL, y = "État",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_blank())

sauvegarder_fig(p_heatmap, "Q12_heatmap_etat_educ.png",
                largeur = 8, hauteur = 12)