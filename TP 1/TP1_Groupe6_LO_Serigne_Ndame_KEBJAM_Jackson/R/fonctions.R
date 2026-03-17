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
stats_descriptives <- function(x, var_name = "variable") {
  x <- x[!is.na(x)]
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
  
  r <- Z / sqrt(n1 + n2)
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
