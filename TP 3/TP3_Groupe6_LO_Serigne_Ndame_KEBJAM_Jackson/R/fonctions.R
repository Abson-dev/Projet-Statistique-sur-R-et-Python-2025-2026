# =============================================================================
# fonctions.R — Fonctions personnalisées réutilisables
# TP3 : Accès aux services de santé et chocs sanitaires
# ENSAE ISE 1 — 2025-2026
# =============================================================================

library(moments)

# -----------------------------------------------------------------------------
#' Calcule statistiques descriptives complètes d'une variable numérique
stats_descriptives <- function(x, var_name = "variable") {
  x <- x[!is.na(x) & is.finite(x)]
  data.frame(
    variable   = var_name,
    n          = length(x),
    moyenne    = round(mean(x), 2),
    mediane    = round(median(x), 2),
    Q1         = round(quantile(x, 0.25), 2),
    Q3         = round(quantile(x, 0.75), 2),
    ecart_type = round(sd(x), 2),
    CV_pct     = round(sd(x) / mean(x) * 100, 2),
    asymetrie  = round(skewness(x), 3),
    min        = round(min(x), 2),
    max        = round(max(x), 2)
  )
}

# -----------------------------------------------------------------------------
#' Statistiques par décile d'une variable numérique
stats_par_decile <- function(x, var_name = "variable") {
  x <- x[!is.na(x) & is.finite(x) & x > 0]
  deciles <- quantile(x, probs = seq(0.1, 1, by = 0.1), na.rm = TRUE)
  data.frame(
    decile = paste0("D", 1:10),
    seuil  = round(deciles, 0)
  )
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
#' Calcule le V de Cramér à partir d'un test chi-deux
v_cramer <- function(chi2_result, n_total, n_lignes, n_colonnes) {
  v <- sqrt(chi2_result$statistic / (n_total * (min(n_lignes, n_colonnes) - 1)))
  interpretation <- dplyr::case_when(
    v < 0.1 ~ "négligeable",
    v < 0.3 ~ "faible",
    v < 0.5 ~ "modéré",
    TRUE    ~ "fort"
  )
  cat("V de Cramér =", round(v, 3), "→ association", interpretation, "\n")
  invisible(v)
}

# -----------------------------------------------------------------------------
#' Calcule la taille d'effet r du test de Wilcoxon
effet_wilcoxon_r <- function(wilcox_result, n1, n2) {
  r <- as.numeric(wilcox_result$statistic) / sqrt(n1 * n2)
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
#' Sauvegarde un graphique ggplot dans output/figures/
sauvegarder_figure <- function(plot, filename, width = 10, height = 7) {
  path <- paste0("output/figures/", filename, ".png")
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300)
  cat("[OK] Figure :", path, "\n")
}

# -----------------------------------------------------------------------------
#' Sauvegarde un tableau gtsummary en HTML dans output/tables/
sauvegarder_tableau <- function(tableau, filename) {
  path <- paste0("output/tables/", filename, ".html")
  tableau |> gtsummary::as_gt() |> gt::gtsave(path)
  cat("[OK] Tableau :", path, "\n")
}