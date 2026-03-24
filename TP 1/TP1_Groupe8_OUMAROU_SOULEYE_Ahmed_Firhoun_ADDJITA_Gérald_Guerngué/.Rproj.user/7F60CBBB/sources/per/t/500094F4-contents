library(haven)
library(dplyr)
library(visdat)
library(ggplot2)
library(PropCIs)
library(forcats)
library(rcompanion)
library(gtsummary)

waves <- 1:4

# ─────────────────────────────────────────────
# CHARGEMENT DES DONNÉES
# ─────────────────────────────────────────────
sect1_harvestw4 <- read_dta("data/raw/sect1_harvestw4.dta") %>%
  select(hhid, indiv, s1q2, s1q3, s1q4) %>%
  mutate(across(-c(hhid, indiv, s1q4), ~ as_factor(.x)))

for (w in waves) {
  assign(
    paste0("processed_w", w),
    read_dta(paste0("data/processed/processed_w", w, ".dta"))
  )
}

processed_w4 <- processed_w4 %>%
  mutate(
    harvest_s1q3 = as_factor(harvest_s1q3),
    sector       = as_factor(sector)
  )

# ─────────────────────────────────────────────
# 1. VIS_MISS
# ─────────────────────────────────────────────
png("output/figures/01_vis_miss.png", width = 1200, height = 700, res = 150)
visdat::vis_miss(sect1_harvestw4)
dev.off()

# ─────────────────────────────────────────────
# 2. HISTOGRAMME DE L'ÂGE
# ─────────────────────────────────────────────
p_hist <- ggplot(sect1_harvestw4, aes(x = s1q4)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Histogramme de l'âge des membres des ménages",
    x     = "Âge",
    y     = "Fréquence"
  ) +
  theme_minimal()

ggsave("output/figures/02_histogramme_age.png",
       plot = p_hist, width = 8, height = 5, dpi = 150)

# ─────────────────────────────────────────────
# 3. BOXPLOT DE L'ÂGE
# ─────────────────────────────────────────────
p_box_age <- ggplot(sect1_harvestw4, aes(y = s1q4)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Boxplot de l'âge des membres des ménages",
    y     = "Âge"
  ) +
  theme_minimal()

ggsave("output/figures/03_boxplot_age.png",
       plot = p_box_age, width = 6, height = 5, dpi = 150)

# ─────────────────────────────────────────────
# 4. STATISTIQUES DESCRIPTIVES (toutes vagues)
# ─────────────────────────────────────────────
resultats <- data.frame()
vars <- c("harvest_s1q4")

for (w in waves) {
  df <- get(paste0("processed_w", w))
  for (v in vars) {
    x    <- df[[v]]
    m    <- mean(x, na.rm = TRUE)
    s    <- sd(x,   na.rm = TRUE)
    skew <- mean((x - m)^3, na.rm = TRUE) / (s^3)
    kurt <- mean((x - m)^4, na.rm = TRUE) / (s^4)
    temp <- data.frame(
      Vague      = w,
      Variable   = v,
      N          = sum(!is.na(x)),
      Moyenne    = round(m, 2),
      Médiane    = round(median(x, na.rm = TRUE), 2),
      Ecart_type = round(s, 2),
      Q1         = round(quantile(x, 0.25, na.rm = TRUE), 2),
      Q3         = round(quantile(x, 0.75, na.rm = TRUE), 2),
      IQR        = round(IQR(x, na.rm = TRUE), 2),
      Min        = min(x, na.rm = TRUE),
      Max        = max(x, na.rm = TRUE),
      CV_pct     = round(s / m * 100, 2),
      Asymetrie  = round(skew, 3),
      Kurtosis   = round(kurt, 3)
    )
    resultats <- rbind(resultats, temp)
  }
}

write.csv(resultats,
          "output/tables/04_stats_descriptives_age.csv",
          row.names = FALSE)

# ─────────────────────────────────────────────
# 5. TEST DE SHAPIRO-WILK
# ─────────────────────────────────────────────
resultats_shapiro <- data.frame()

for (w in waves) {
  df <- get(paste0("processed_w", w))
  for (v in vars) {
    x    <- as.numeric(df[[v]])
    x    <- x[!is.na(x)]
    if (length(x) > 5000) x <- sample(x, 5000)
    test <- shapiro.test(x)
    resultats_shapiro <- rbind(resultats_shapiro, data.frame(
      Vague            = w,
      Variable         = v,
      N                = length(x),
      Statistique_test = round(test$statistic, 4),
      p_value          = round(test$p.value, 4)
    ))
  }
}

write.csv(resultats_shapiro,
          "output/tables/05_shapiro_wilk.csv",
          row.names = FALSE)

# ─────────────────────────────────────────────
# 6. PYRAMIDE DES ÂGES
# ─────────────────────────────────────────────
pyramid_data <- processed_w4 %>%
  filter(!is.na(harvest_s1q4), !is.na(harvest_s1q2),
         harvest_s1q4 >= 0, harvest_s1q4 <= 110) %>%
  mutate(
    groupe_age = cut(harvest_s1q4,
                     breaks = c(0,5,10,15,20,25,30,35,40,45,
                                50,55,60,65,70,75,80,Inf),
                     labels = c("0-4","5-9","10-14","15-19","20-24",
                                "25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74",
                                "75-79","80+"),
                     right = FALSE),
    sexe_label = factor(harvest_s1q2,
                        levels = c(1, 2),
                        labels = c("Hommes", "Femmes"))
  ) %>%
  filter(!is.na(groupe_age))

n_total_pyr <- nrow(pyramid_data)

pyr_count <- pyramid_data %>%
  count(groupe_age, sexe_label) %>%
  mutate(
    pct_plot  = ifelse(sexe_label == "Hommes",
                       -n / n_total_pyr * 100,
                       n / n_total_pyr * 100),
    label_pct = paste0(abs(round(pct_plot, 1)), "%")
  )

y_lim <- max(abs(pyr_count$pct_plot)) * 1.28

p_pyramid <- ggplot(pyr_count,
                    aes(x = groupe_age, y = pct_plot, fill = sexe_label)) +
  geom_col(width = 0.88, alpha = 0.92) +
  geom_text(
    aes(label = label_pct),
    hjust     = ifelse(pyr_count$sexe_label == "Hommes", 1.18, -0.18),
    size      = 3, color = "#2D3436", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Hommes" = "#4ECDC4", "Femmes" = "#FF6B6B"),
    name   = "Sexe"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),
    limits = c(-y_lim, y_lim),
    breaks = seq(-8, 8, 2)
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "#2D3436", linewidth = 0.9) +
  annotate("text", x = 17.5, y = -y_lim * 0.55,
           label = "◄  HOMMES", fontface = "bold",
           color = "#4ECDC4", size = 4.5) +
  annotate("text", x = 17.5, y = y_lim * 0.55,
           label = "FEMMES  ►", fontface = "bold",
           color = "#FF6B6B", size = 4.5) +
  labs(
    title    = "Pyramide des âges des membres des ménages",
    subtitle = paste0("Nigeria GHS Panel — Wave 4 (2018) | N = ",
                      format(n_total_pyr, big.mark = " "), " individus"),
    x        = "Groupe d'âge (années)",
    y        = "Part de la population (%)",
    caption  = paste0("Source : World Bank LSMS-ISA | sect1_harvestw4\n",
                      "Note : Structure pyramidale typique d'un pays en développement")
  ) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.5)
  )

ggsave("output/figures/06_pyramide_ages.png",
       plot = p_pyramid, width = 10, height = 7, dpi = 150)

# ─────────────────────────────────────────────
# 7. DISTRIBUTION DU LIEN DE PARENTÉ
# ─────────────────────────────────────────────
df_lien <- processed_w4 %>%
  filter(!is.na(harvest_s1q3))

freq_table <- df_lien %>%
  group_by(harvest_s1q3) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(total = sum(n), prop = n / total) %>%
  mutate(
    prop_lower = mapply(function(x, n) exactci(x, n, conf.level = 0.95)$conf.int[1], n, total),
    prop_upper = mapply(function(x, n) exactci(x, n, conf.level = 0.95)$conf.int[2], n, total)
  )

write.csv(freq_table,
          "output/tables/07_freq_lien_parente.csv",
          row.names = FALSE)

p_lien <- ggplot(freq_table,
                 aes(x = reorder(harvest_s1q3, n), y = prop)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_lower, ymax = prop_upper), width = 0.2) +
  geom_text(
    aes(label = paste0(round(prop * 100, 1), "%")),
    hjust = -0.15, size = 3, color = "#2D3436"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(freq_table$prop) * 1.15)
  ) +
  labs(
    title = "Distribution du lien de parenté (Vague 4)",
    x     = "Lien de parenté",
    y     = "Proportion (IC 95%)"
  ) +
  theme_minimal()

ggsave("output/figures/07_lien_parente.png",
       plot = p_lien, width = 9, height = 6, dpi = 150)

# ─────────────────────────────────────────────
# 8. BOXPLOT TAILLE MÉNAGE PAR ZONE + WILCOXON
# ─────────────────────────────────────────────
menages <- processed_w4 %>%
  filter(!is.na(hhid), !is.na(sector)) %>%
  group_by(hhid, sector) %>%
  summarise(taille_menage = n(), .groups = "drop") %>%
  mutate(sector = as.factor(sector))

p_box_zone <- ggplot(menages,
                     aes(x = sector, y = taille_menage, fill = sector)) +
  geom_boxplot() +
  labs(
    title = "Taille des ménages selon la zone",
    x     = "Zone",
    y     = "Taille du ménage"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/figures/08_boxplot_taille_menage_zone.png",
       plot = p_box_zone, width = 7, height = 5, dpi = 150)

wilcox_res     <- wilcox.test(taille_menage ~ sector, data = menages, exact = FALSE)
wilcox_effsize <- wilcoxonR(x = menages$taille_menage, g = menages$sector)

wilcox_table <- data.frame(
  Statistique_W  = wilcox_res$statistic,
  p_value        = wilcox_res$p.value,
  r_rang         = round(wilcox_effsize, 3)
)

write.csv(wilcox_table,
          "output/tables/08_wilcoxon_taille_menage.csv",
          row.names = FALSE)

# ─────────────────────────────────────────────
# 9. TABLEAU GTSUMMARY STRATIFIÉ PAR ZONE
# ─────────────────────────────────────────────
tableau_recap <- processed_w4 %>%
  mutate(
    zone = factor(sector,
                  levels = c("Urban", "Rural"),
                  labels = c("Urbain", "Rural")),
    sexe = factor(harvest_s1q2,
                  levels = c(1, 2),
                  labels = c("Homme", "Femme")),
    age  = harvest_s1q4
  ) %>%
  group_by(hhid) %>%
  mutate(taille_menage = n()) %>%
  ungroup() %>%
  filter(!is.na(zone)) %>%
  select(zone, age, sexe, taille_menage) %>%
  tbl_summary(
    by        = zone,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      age           ~ "Âge (années)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du ménage"
    ),
    missing = "no"
  ) %>%
  add_p(test = list(
    age           ~ "wilcox.test",
    sexe          ~ "chisq.test",
    taille_menage ~ "wilcox.test"
  )) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Tableau 1. Caractéristiques sociodémographiques selon la zone**")

# Export en HTML
tableau_recap %>%
  as_gt() %>%
  gt::gtsave("output/tables/09_tableau_gtsummary_zone.html")