# =============================================================================
# fonctions.R — Fonctions utilitaires réutilisables
# Projet  : Analyse parcelles agricoles — Nigeria GHS Panel (W1–W4)
# Auteur  : ENSAE ISE 1 | 2025–2026
# Convention : aucun opérateur :: dans le corps des fonctions
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(tibble)

# =============================================================================
# 1. STATISTIQUES PONDÉRÉES
# =============================================================================

# Médiane pondérée ---------------------------------------------------------
w_median <- function(x, w) {
  ok    <- !is.na(x) & !is.na(w)
  x     <- x[ok];  w <- w[ok]
  ord   <- order(x)
  x     <- x[ord]; w <- w[ord]
  wcum  <- cumsum(w) / sum(w)
  x[which.max(wcum >= 0.5)]
}

# Quantiles pondérés -------------------------------------------------------
w_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  ok    <- !is.na(x) & !is.na(w)
  x     <- x[ok];  w <- w[ok]
  ord   <- order(x)
  x     <- x[ord]; w <- w[ord]
  wcum  <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which.max(wcum >= p)])
}

# Moyenne pondérée + IC (Kish n_eff) ---------------------------------------
w_mean_ci <- function(x, w, level = 0.95) {
  ok     <- !is.na(x) & !is.na(w)
  x      <- x[ok]; w <- w[ok]
  mu     <- weighted.mean(x, w)
  n_eff  <- sum(w)^2 / sum(w^2)
  vw     <- sum(w * (x - mu)^2) / sum(w)
  se     <- sqrt(vw / n_eff)
  z      <- qnorm((1 + level) / 2)
  tibble(mean = mu, se = se,
         ci_lower = mu - z * se, ci_upper = mu + z * se,
         n_eff = n_eff)
}

# Statistiques pondérées complètes ----------------------------------------
desc_w <- function(x, w, nom = "variable") {
  ok  <- !is.na(x) & !is.na(w)
  xc  <- x[ok]; wc <- w[ok]
  mu  <- weighted.mean(xc, wc)
  n_e <- sum(wc)^2 / sum(wc^2)
  sdw <- sqrt(sum(wc * (xc - mu)^2) / sum(wc))
  q   <- w_quantile(xc, wc, probs = c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1))
  tibble(variable = nom, n_obs = sum(ok), n_miss = sum(!ok),
         n_eff = round(n_e), mean = mu, sd = sdw,
         min = q[1], p10 = q[2], p25 = q[3],
         median = q[4], p75 = q[5], p90 = q[6], max = q[7],
         cv = sdw / mu)
}

# Statistiques par décile pondéré -----------------------------------------
decile_w <- function(df, var, wt) {
  x <- df[[var]]; w <- df[[wt]]
  ok <- !is.na(x) & !is.na(w) & x > 0
  xc <- x[ok]; wc <- w[ok]
  breaks <- c(-Inf, w_quantile(xc, wc, seq(0.1, 0.9, 0.1)), Inf)
  df[ok, ] %>%
    mutate(decile = cut(.data[[var]], breaks = breaks,
                        labels = paste0("D", 1:10),
                        include.lowest = TRUE)) %>%
    group_by(decile) %>%
    summarise(
      n_eff     = round(sum(.data[[wt]])),
      min_ha    = round(min(.data[[var]]),  4),
      mean_ha   = round(weighted.mean(.data[[var]], .data[[wt]]), 4),
      median_ha = round(w_median(.data[[var]], .data[[wt]]), 4),
      max_ha    = round(max(.data[[var]]),  4),
      .groups   = "drop"
    )
}

# Corrélation de Spearman + IC bootstrap -----------------------------------
spearman_ci <- function(x, y, B = 2000, alpha = 0.05) {
  ok   <- !is.na(x) & !is.na(y)
  x    <- x[ok]; y <- y[ok]
  rho  <- cor(x, y, method = "spearman")
  pval <- cor.test(x, y, method = "spearman")$p.value
  set.seed(42)
  boot <- replicate(B, {
    i <- sample(length(x), replace = TRUE)
    cor(x[i], y[i], method = "spearman")
  })
  ci <- quantile(boot, c(alpha / 2, 1 - alpha / 2))
  tibble(rho = rho, ci_lower = ci[1], ci_upper = ci[2],
         p_value = pval, n = sum(ok))
}

# Chi-deux pondéré (approximation) ----------------------------------------
# Limite : effectifs pondérés arrondis → chisq.test()
# Méthode exacte recommandée : survey::svychisq (Rao-Scott)
chi2_w <- function(df, var1, var2, wt) {
  mat <- df %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]])) %>%
    group_by(.data[[var1]], .data[[var2]]) %>%
    summarise(wn = sum(.data[[wt]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from  = all_of(var2),
                       values_from = wn, values_fill = 0) %>%
    tibble::column_to_rownames(var1) %>%
    as.matrix()
  test <- suppressWarnings(chisq.test(round(mat), correct = TRUE))
  n    <- sum(mat)
  k    <- min(nrow(mat), ncol(mat))
  v    <- sqrt(test$statistic / (n * (k - 1)))
  list(table = mat, test = test, cramer_v = unname(v))
}

# =============================================================================
# 2. THÈME GRAPHIQUE
# =============================================================================

palette_ensae <- c(
  bleu        = "#1D3557",
  bleu_clair  = "#457B9D",
  rouge       = "#E63946",
  vert        = "#2D6A4F",
  orange      = "#F4A261",
  gris        = "#6C757D",
  jaune       = "#E9C46A"
)

theme_ensae <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold",   size = base_size + 3,
                                       colour = "#1D3557", hjust = 0,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size - 0.5,
                                       colour = "grey40", hjust = 0,
                                       margin = margin(b = 8)),
      plot.caption      = element_text(size = base_size - 2,
                                       colour = "grey55", hjust = 1,
                                       margin = margin(t = 6)),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major  = element_line(colour = "grey90", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(size = base_size, face = "bold",
                                       colour = "#1D3557"),
      axis.text         = element_text(size = base_size - 1, colour = "grey30"),
      axis.ticks        = element_line(colour = "grey70"),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = base_size - 1),
      legend.background = element_rect(fill = "white", colour = NA),
      strip.text        = element_text(face = "bold", colour = "#1D3557"),
      strip.background  = element_rect(fill = "#EBF2FA", colour = NA)
    )
}

# =============================================================================
# 3. SAUVEGARDE
# =============================================================================

save_fig <- function(p, nom, w = 9, h = 6, dpi = 300) {
  dir.create(figure, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(figure, paste0(nom, ".png")),
         plot = p, width = w, height = h, dpi = dpi, bg = "white")
  invisible(nom)
}

save_tab <- function(df, nom) {
  dir.create(table, showWarnings = FALSE, recursive = TRUE)
  write.csv(df, file.path(table, paste0(nom, ".csv")), row.names = FALSE)
  invisible(nom)
}

message("✔  fonctions.R chargé — ", length(ls()), " objets.")