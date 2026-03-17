source("R/01_packages.R")
source("R/02_paths.R")

# =========================================================
# FIGURES - ANALYSE 1
# =========================================================

# ---------------------------------------------------------
# 1. Charger la base
# ---------------------------------------------------------
demo_w4_fr <- readRDS(
  here::here("data", "processed", "clean", "demo_w4_fr.rds")
)

# ---------------------------------------------------------
# 2. Supprimer anciennes figures
# ---------------------------------------------------------
unlink(here::here("output", "figures", "analyse1"), recursive = TRUE)

dir.create(
  here::here("output", "figures", "analyse1"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ---------------------------------------------------------
# 3. Thème
# ---------------------------------------------------------
theme_rapport <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 9, hjust = 1)
    )
}

print(table(demo_w4_fr$sexe, useNA = "ifany"))

# ---------------------------------------------------------
# 4. Figure 1 : Histogramme âge
# ---------------------------------------------------------
p1 <- ggplot2::ggplot(
  demo_w4_fr,
  ggplot2::aes(x = age)
) +
  ggplot2::geom_histogram(binwidth = 5, boundary = 0) +
  ggplot2::labs(
    title = "Figure 1. Distribution de l'âge des individus",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = "Âge (années)",
    y = "Effectif",
    caption = "Source : calculs de l'auteur à partir des données GHS-Panel."
  ) +
  theme_rapport()

ggplot2::ggsave(
  here::here("output", "figures", "analyse1", "figure1_hist_age_w4.png"),
  p1, width = 8, height = 5, dpi = 300
)

# ---------------------------------------------------------
# 5. Figure 2 : Boxplot âge sexe
# ---------------------------------------------------------
p2 <- ggplot2::ggplot(
  demo_w4_fr,
  ggplot2::aes(x = sexe, y = age)
) +
  ggplot2::geom_boxplot(outlier.alpha = 0.25) +
  ggplot2::labs(
    title = "Figure 2. Distribution de l'âge selon le sexe",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = "Sexe",
    y = "Âge (années)",
    caption = "Source : calculs de l'auteur."
  ) +
  theme_rapport()

ggplot2::ggsave(
  here::here("output", "figures", "analyse1", "figure2_boxplot_age_sex_w4.png"),
  p2, width = 8, height = 5, dpi = 300
)

# ---------------------------------------------------------
# 6. Figure 3 : Pyramide des âges
# ---------------------------------------------------------
pyramid <- demo_w4_fr |>
  dplyr::count(age_group, sexe) |>
  dplyr::mutate(
    n_plot = dplyr::if_else(sexe == "Hommes", -n, n)
  )

max_effectif <- max(abs(pyramid$n_plot), na.rm = TRUE)

p3 <- ggplot2::ggplot(
  pyramid,
  ggplot2::aes(x = age_group, y = n_plot, fill = sexe)
) +
  ggplot2::geom_col(width = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    labels = function(x) format(abs(x), big.mark = " ", scientific = FALSE),
    limits = c(-max_effectif * 1.1, max_effectif * 1.1)
  ) +
  ggplot2::labs(
    title = "Figure 3. Pyramide des âges",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = "Groupe d'âge",
    y = "Effectif",
    fill = "Sexe",
    caption = "Source : calculs de l'auteur."
  ) +
  theme_rapport()

ggplot2::ggsave(
  here::here("output", "figures", "analyse1", "figure3_pyramide_ages_w4.png"),
  p3, width = 9, height = 6, dpi = 300
)

# ---------------------------------------------------------
# 7. Figure 4 : Relation au chef
# ---------------------------------------------------------
tab_relation <- demo_w4_fr |>
  dplyr::count(lien_chef, sort = TRUE)

p4 <- ggplot2::ggplot(
  tab_relation,
  ggplot2::aes(
    x = forcats::fct_reorder(lien_chef, n),
    y = n
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Figure 4. Lien avec le chef de ménage",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = NULL,
    y = "Effectif",
    caption = "Source : calculs de l'auteur."
  ) +
  theme_rapport()

ggplot2::ggsave(
  here::here("output", "figures", "analyse1", "figure4_lien_chef_w4.png"),
  p4, width = 8, height = 6, dpi = 300
)

# ---------------------------------------------------------
# 8. Figure 5 : Secteur
# ---------------------------------------------------------
tab_sector <- demo_w4_fr |>
  dplyr::count(secteur_residence) |>
  dplyr::mutate(pct = 100 * n / sum(n))

p5 <- ggplot2::ggplot(
  tab_sector,
  ggplot2::aes(x = secteur_residence, y = pct)
) +
  ggplot2::geom_col(width = 0.6) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(pct, 1), " %")),
    vjust = -0.5
  ) +
  ggplot2::labs(
    title = "Figure 5. Répartition des individus selon le secteur de résidence",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = "Secteur de résidence",
    y = "Pourcentage (%)",
    caption = "Source : calculs de l'auteur."
  ) +
  theme_rapport() +
  ggplot2::ylim(0, max(tab_sector$pct) + 8)

ggplot2::ggsave(
  here::here("output", "figures", "analyse1", "figure5_secteur_residence_w4.png"),
  p5, width = 8, height = 5, dpi = 300
)

# ---------------------------------------------------------
# 9. Figure 6 : Taille ménage par zone
# ---------------------------------------------------------
hh_size <- demo_w4_fr |>
  dplyr::count(hhid, secteur_residence, name = "taille_menage")

p6 <- ggplot2::ggplot(
  hh_size,
  ggplot2::aes(x = secteur_residence, y = taille_menage)
) +
  ggplot2::geom_boxplot(outlier.alpha = 0.25) +
  ggplot2::labs(
    title = "Figure 6. Taille des ménages selon le secteur de résidence",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = "Secteur de résidence",
    y = "Taille du ménage",
    caption = "Source : calculs de l'auteur."
  ) +
  theme_rapport()

ggplot2::ggsave(
  here::here("output", "figures", "analyse1", "figure6_boxplot_taille_menage_zone_w4.png"),
  p6, width = 8, height = 5, dpi = 300
)

cat("\nFigures Analyse 1 générées avec succès.\n")