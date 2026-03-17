# =============================================================================
# Analyse 3 - Script 05 : Test d'indépendance recours × quintile de richesse
# Tâche 17 : Croiser le recours aux soins (consulté/non consulté) avec le
#            quintile de consommation (5 groupes). Chi-deux, test exact de
#            Fisher si effectifs < 5, V de Cramér. Barplot 100% empilé.
# Auteurs   : Groupe 7 - Herman YAMAHA | Bourama DIALLO
# Données   : Nigeria GHS Panel - Wave 4 (2018), Post-Harvest
# =============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(rstatix)
library(gtsummary)

# --- 1. Chargement -----------------------------------------------------------

df_health   <- readRDS("data/df_health_base.rds")
cons        <- read_dta("data/raw/totcons_final.dta")

# --- 2. Préparation de la consommation et quintile ---------------------------

cons_clean <- cons %>%
  select(hhid, totcons_adj, hhsize) %>%
  mutate(
    quintile = ntile(totcons_adj, 5),
    quintile_label = factor(quintile,
                            levels = 1:5,
                            labels = c("Q1 (plus pauvres)",
                                       "Q2", "Q3", "Q4",
                                       "Q5 (plus riches)"))
  )

# --- 3. Fusion avec df_health ------------------------------------------------

df_merge <- df_health %>%
  filter(!is.na(s4aq3), s4aq3 == 1) %>%    # individus malades seulement
  mutate(
    consulte = if_else(s4aq1 == 1, "Consulté", "Non consulté",
                       missing = "Non consulté"),
    consulte = factor(consulte,
                      levels = c("Consulté", "Non consulté"))
  ) %>%
  left_join(cons_clean, by = "hhid") %>%
  filter(!is.na(quintile_label), !is.na(consulte))

cat("=== Jeu fusionné : ", nrow(df_merge), "individus malades ===\n")

# --- 4. Tableau de contingence -----------------------------------------------

tab_contingence <- table(
  Recours   = df_merge$consulte,
  Quintile  = df_merge$quintile_label
)

cat("\n=== Tableau de contingence : recours × quintile ===\n")
print(tab_contingence)

# Vérification des effectifs < 5
min_eff <- min(tab_contingence)
cat("\nEffectif minimum dans le tableau :", min_eff, "\n")

# --- 5. Tests statistiques ---------------------------------------------------

if (min_eff < 5) {
  cat("Effectifs < 5 → Test exact de Fisher\n")
  test_result <- fisher.test(tab_contingence, simulate.p.value = TRUE,
                             B = 10000)
  cat("Test exact de Fisher (simulation Monte-Carlo) :\n")
  cat("  p-value =", round(test_result$p.value, 4), "\n")
} else {
  cat("Effectifs >= 5 → Test du chi-deux de Pearson\n")
  test_result <- chisq.test(tab_contingence)
  cat("Chi-deux de Pearson :\n")
  cat("  X² =", round(test_result$statistic, 3),
      "| ddl =", test_result$parameter,
      "| p-value =", format.pval(test_result$p.value, digits = 3), "\n")
}

# V de Cramer
n_total  <- sum(tab_contingence)
chi2_val <- suppressWarnings(chisq.test(tab_contingence)$statistic)
v_cramer <- sqrt(chi2_val / (n_total * (min(nrow(tab_contingence),
                                            ncol(tab_contingence)) - 1)))
cat("\nV de Cramér =", round(v_cramer, 4), "\n")
cat("Interprétation :",
    ifelse(v_cramer < 0.1, "association très faible",
    ifelse(v_cramer < 0.3, "association faible",
    ifelse(v_cramer < 0.5, "association modérée",
                           "association forte"))), "\n")

# Sauvegarde tableau
tab_df <- as.data.frame.matrix(tab_contingence)
tab_df$Recours <- rownames(tab_df)
write.csv(tab_df, "outputs/tables/05_contingence_recours_quintile.csv",
          row.names = FALSE)

# --- 6. Barplot 100% empilé --------------------------------------------------

df_plot <- df_merge %>%
  count(quintile_label, consulte) %>%
  group_by(quintile_label) %>%
  mutate(
    total = sum(n),
    pct   = n / total * 100
  ) %>%
  ungroup()

p_stacked <- ggplot(df_plot,
                    aes(x = quintile_label, y = pct,
                        fill = consulte)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.8, color = "white", fontface = "bold") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_fill_manual(
    values = c("Consulté"     = "#1D3557",
               "Non consulté" = "#E63946"),
    name   = "Recours aux soins"
  ) +
  labs(
    title    = "Recours aux soins selon le quintile de consommation",
    subtitle = paste0("Parmi les individus malades — Wave 4 (2018)\n",
                      "Chi² = ", round(chi2_val, 2),
                      " | V de Cramér = ", round(v_cramer, 3),
                      " | p ", ifelse(test_result$p.value < 0.001,
                                      "< 0.001",
                                      paste("=", round(test_result$p.value, 3)))),
    x        = "Quintile de consommation",
    y        = "Part des individus malades (%)",
    caption  = "Source : Nigeria GHS Panel W4, sect4a + totcons_final"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/05_recours_quintile.png", p_stacked,
       width = 10, height = 6, dpi = 300)
cat("Figure sauvegardée : outputs/figures/05_recours_quintile.png\n")

# --- 7. Tableau gtsummary ----------------------------------------------------

tbl_recap <- df_merge %>%
  select(consulte, quintile_label) %>%
  tbl_summary(
    by         = quintile_label,
    label      = list(consulte ~ "Recours aux soins"),
    statistic  = list(all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_p(test = list(all_categorical() ~ "chisq.test")) %>%
  modify_caption("**Recours aux soins × Quintile de consommation**")

cat("\n=== Table gtsummary ===\n")
print(tbl_recap)

cat("\n=== Script 05 terminé ===\n")
