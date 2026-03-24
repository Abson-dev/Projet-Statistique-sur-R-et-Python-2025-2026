# =============================================================================
# Fonctions personnalisées réutilisables
# TP1 : Profil démographique des ménages nigérians
# ENSAE ISE 1 — 2025-2026
# =============================================================================

library(moments)

# -----------------------------------------------------------------------------
#' Calcule les statistiques descriptives complètes d'une variable numérique
#'
#' @param x   Vecteur numérique
#' @param var_name Nom affiché dans le tableau résultat
#' @return data.frame une ligne avec toutes les stats
# -----------------------------------------------------------------------------
stats_descriptives <- function(x, poids, var_name = "variable") {
  
  data <- data.frame(x = x, w = poids) |>
    dplyr::filter(!is.na(x), !is.na(w))
  
  m <- weighted.mean(data$x, data$w)
  sd_w <- sqrt(weighted.mean((data$x - m)^2, data$w))
  
  data.frame(
    variable   = var_name,
    n          = sum(data$w),  # effectif pondéré
    moyenne    = round(m, 2),
    mediane    = round(Hmisc::wtd.quantile(data$x, weights = data$w, probs = 0.5), 2),
    Q1         = round(Hmisc::wtd.quantile(data$x, weights = data$w, probs = 0.25), 2),
    Q3         = round(Hmisc::wtd.quantile(data$x, weights = data$w, probs = 0.75), 2),
    ecart_type = round(sd_w, 2),
    CV_pct     = round(sd_w / m * 100, 2),
    asymetrie  = round(moments::skewness(data$x), 3), # ⚠️ non pondéré (voir note)
    min        = round(min(data$x), 2),
    max        = round(max(data$x), 2)
  )
}

# -----------------------------------------------------------------------------
#' Calcule et interprète la taille d'effet r du test de Wilcoxon
#'
#' @param wilcox_result  Objet retourné par wilcox.test()
#' @param n1  Taille du groupe 1
#' @param n2  Taille du groupe 2
#' @return Valeur de r (invisible)
# -----------------------------------------------------------------------------
effet_wilcoxon_r <- function(wilcox_result, n1, n2) {
  W <- as.numeric(wilcox_result$statistic)
  
  mu <- n1 * n2 / 2
  sigma <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
  
  Z <- (W - mu) / sigma
  
  # Prendre la valeur absolue pour r
  r <- abs(Z) / sqrt(n1 + n2)   # ← CORRECTION : r toujours positif
  
  interpretation <- dplyr::case_when(
    r < 0.1 ~ "négligeable",
    r < 0.3 ~ "petit",
    r < 0.5 ~ "moyen",
    TRUE    ~ "grand"
  )
  cat("Taille d'effet r =", round(r, 3), "→ effet", interpretation, "\n")
  invisible(r)
}

# -----------------------------------------------------------------------------
#' IC 95% de proportion par groupe (binom.test)
calc_ic_proportion <- function(data, group_var, success_var) {
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::summarise(
      n_total  = dplyr::n(),
      n_succes = sum(.data[[success_var]], na.rm = TRUE),
      taux     = n_succes / n_total * 100,
      ic_lo    = binom.test(n_succes, n_total)$conf.int[1] * 100,
      ic_hi    = binom.test(n_succes, n_total)$conf.int[2] * 100,
      .groups  = "drop"
    )
}


# -----------------------------------------------------------------------------
#' Sauvegarde un graphique ggplot dans output/figures/
#'
#' @param plot      Objet ggplot
#' @param filename  Nom du fichier sans extension
#' @param width     Largeur en pouces (défaut 10)
#' @param height    Hauteur en pouces (défaut 7)
# -----------------------------------------------------------------------------
sauvegarder_figure <- function(plot, filename, width = 10, height = 7) {
  path <- paste0("output/figures/", filename, ".png")
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300)
  cat("[OK] Figure sauvegardée :", path, "\n")
}

# -----------------------------------------------------------------------------
#' Sauvegarde un tableau gtsummary en HTML dans output/tables/
#'
#' @param tableau   Objet gtsummary
#' @param filename  Nom du fichier sans extension
# -----------------------------------------------------------------------------
sauvegarder_tableau <- function(tableau, filename) {
  path <- paste0("output/tables/", filename, ".html")
  tableau |> gtsummary::as_gt() |> gt::gtsave(path)
  cat("[OK] Tableau sauvegardé :", path, "\n")
}
