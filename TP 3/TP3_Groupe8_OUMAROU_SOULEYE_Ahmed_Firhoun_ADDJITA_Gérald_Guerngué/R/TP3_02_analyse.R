library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(scales)
library(viridis)
library(gtsummary)


processed_sante_w4 <- read_dta("data/processed/processed_sante_w4.dta")

processed_sante_w4 <- processed_sante_w4 %>%
  mutate(
    zone_label = factor(sector, levels = c(1, 2),
                        labels = c("Rural", "Urbain")),
    sexe_label = factor(s1q2,   levels = c(1, 2),
                        labels = c("Homme", "Femme"))
  )

# ─────────────────────────────────────────────
# 13. EXPLORATION ET TAUX DE MORBIDITÉ
# ─────────────────────────────────────────────

## Résumé des dimensions
dim_table <- data.frame(
  Base        = "processed_sante_w4",
  N_individus = nrow(processed_sante_w4),
  N_variables = ncol(processed_sante_w4)
)
write.csv(dim_table,
          "output/tables/13_dimensions_sante.csv",
          row.names = FALSE)

## Valeurs manquantes sur les variables clés
vars_sante_cles <- c("sante_s4aq3","sante_s4aq1","sante_s4aq6a",
                     "sante_s4aq9","sante_s4aq14","sante_s4aq15")
miss_sante <- processed_sante_w4 %>%
  select(any_of(vars_sante_cles)) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "Variable",
               values_to = "N_manquants") %>%
  mutate(Pct = round(N_manquants / nrow(processed_sante_w4) * 100, 1))

write.csv(miss_sante,
          "output/tables/13_valeurs_manquantes_sante.csv",
          row.names = FALSE)

png("output/figures/13_vis_miss_sante.png",
    width = 1200, height = 700, res = 150)
visdat::vis_miss(
  select(processed_sante_w4, any_of(vars_sante_cles))
)
dev.off()

## Taux de morbidité global
morb_d <- processed_sante_w4 %>%
  filter(!is.na(sante_s4aq3), !is.na(s1q4), !is.na(s1q2)) %>%
  mutate(
    gr_age = cut(s1q4,
                 breaks = c(0, 5, 15, 30, 45, 60, Inf),
                 labels = c("0-4 ans","5-14 ans","15-29 ans",
                            "30-44 ans","45-59 ans","60+ ans"),
                 right  = FALSE)
  ) %>%
  filter(!is.na(gr_age))

taux_g <- morb_d %>%
  summarise(
    N      = n(),
    n_mal  = sum(sante_s4aq3 == 1, na.rm = TRUE),
    taux   = round(n_mal / N * 100, 1)
  )

taux_morb <- morb_d %>%
  group_by(sexe_label, gr_age) %>%
  summarise(
    N      = n(),
    n_mal  = sum(sante_s4aq3 == 1, na.rm = TRUE),
    Taux   = round(n_mal / N * 100, 1),
    IC_inf = round(binom.test(n_mal, N)$conf.int[1] * 100, 1),
    IC_sup = round(binom.test(n_mal, N)$conf.int[2] * 100, 1),
    .groups = "drop"
  )

write.csv(taux_morb,
          "output/tables/13_taux_morbidite.csv",
          row.names = FALSE)

p_morb <- ggplot(taux_morb,
                 aes(x = gr_age, y = Taux,
                     fill = sexe_label, group = sexe_label)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.68, alpha = 0.92) +
  geom_errorbar(
    aes(ymin = IC_inf, ymax = IC_sup),
    position = position_dodge(width = 0.75),
    width = 0.25, linewidth = 0.9, color = "#2D3436"
  ) +
  geom_text(
    aes(label = paste0(Taux, "%")),
    position = position_dodge(width = 0.75),
    vjust = -1.5, size = 2.8,
    fontface = "bold", color = "#2D3436"
  ) +
  scale_fill_manual(
    values = c("Homme" = "#45B7D1", "Femme" = "#FF6B6B"),
    name   = "Sexe"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 40), expand = c(0, 0)
  ) +
  labs(
    title    = "Taux de morbidite par sexe et groupe d'age",
    subtitle = paste0(
      "Part ayant declare une maladie/blessure (4 dernieres semaines)\n",
      "Nigeria GHS Panel Wave 4 (2018) | Barres d'erreur = IC 95%"),
    x       = "Groupe d'age",
    y       = "Taux de morbidite (%)",
    caption = "Source : World Bank LSMS-ISA | sect4a_harvestw4"
  ) +
  theme_minimal()

ggsave("output/figures/13_taux_morbidite.png",
       plot = p_morb, width = 13, height = 7, dpi = 150)