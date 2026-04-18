# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# FICHIER  : scripts/functions.R
# OBJET    : Fonctions utilitaires réutilisables dans tous les scripts
# USAGE    : source("scripts/functions.R") en tête de chaque script
# ================================================================


# ----------------------------------------------------------------
# 1. EXPLORER UNE BASE
# Affiche dimensions, aperçu, valeurs manquantes et doublons
# ----------------------------------------------------------------
explorer_base <- function(df, nom) {
  cat(sprintf("\n%s\nBASE : %s\n%s\n", strrep("=", 60), nom, strrep("=", 60)))
  cat("Dimensions :", nrow(df), "lignes x", ncol(df), "colonnes\n")
  
  cat("\nAperçu (glimpse) :\n")
  dplyr::glimpse(df)
  
  # Valeurs manquantes — afficher uniquement les variables concernées
  vm <- sort(colSums(is.na(df)), decreasing = TRUE)
  vm <- vm[vm > 0]
  if (length(vm) > 0) {
    cat("\nValeurs manquantes (variables concernées) :\n")
    print(vm)
  } else {
    cat("\nAucune valeur manquante.\n")
  }
  
  cat("\nDoublons exacts :", nrow(df) - nrow(dplyr::distinct(df)), "\n")
}


# ----------------------------------------------------------------
# 2. DÉTECTER LES CODES SPÉCIAUX
# Repère les valeurs 999 / 9999 / -99 / -999 qui encodent
# "ne sait pas", "refus" ou "non applicable" dans les enquêtes
# Banque Mondiale. Ces valeurs doivent être remplacées par NA
# avant toute analyse.
# ----------------------------------------------------------------


detect_special_codes_clean <- function(df, special_codes = c(999, 9999, -99, -999)) {
  results <- data.frame(variable = character(), codes_detectes = character(),
                        stringsAsFactors = FALSE)
  for (v in names(df)) {
    if (is.numeric(df[[v]])) {
      detected <- special_codes[special_codes %in% unique(df[[v]])]
      if (length(detected) > 0) {
        results <- rbind(results,
                         data.frame(variable = v,
                                    codes_detectes = paste(detected, collapse = ", ")))
      }
    }
  }
  if (nrow(results) == 0) cat("Aucun code spécial détecté.\n") else print(results)
}


# ----------------------------------------------------------------
# 3. MÉDIANE PONDÉRÉE
# Méthode : tri croissant + cumul des poids normalisés.
# On prend le premier point où le cumul atteint 50%.
# Plus robuste que la moyenne pour les distributions foncières
# très asymétriques à droite.
# ----------------------------------------------------------------
mediane_ponderee <- function(x, poids) {
  # Supprimer les NA et les poids nuls ou négatifs
  ok    <- !is.na(x) & !is.na(poids) & poids > 0
  x     <- x[ok]
  poids <- poids[ok]
  
  # Trier par valeur croissante
  ord   <- order(x)
  x     <- x[ord]
  poids <- poids[ord]
  
  # Cumul des poids normalisés → point à 50 %
  cum_poids <- cumsum(poids) / sum(poids)
  x[which(cum_poids >= 0.5)[1]]
}


# ----------------------------------------------------------------
# 4. INTERVALLE DE CONFIANCE DU RHO DE SPEARMAN
# Technique : transformation de Fisher
#   z = atanh(rho)  →  IC sur z  →  retransformer avec tanh
# Valide pour n > 10 même en présence d'ex-aequo.
# ----------------------------------------------------------------
ic_spearman <- function(x, y, niveau = 0.95) {
  test  <- cor.test(x, y, method = "spearman", exact = FALSE)
  rho   <- as.numeric(test$estimate)
  n     <- sum(!is.na(x) & !is.na(y))
  
  # Transformation de Fisher
  z     <- atanh(rho)
  marge <- qnorm(1 - (1 - niveau) / 2) / sqrt(n - 3)
  
  list(
    rho    = rho,
    ic_inf = tanh(z - marge),
    ic_sup = tanh(z + marge),
    p_val  = test$p.value,
    n      = n
  )
}


# ----------------------------------------------------------------
# 5. THÈME GGPLOT2 UNIFORME
# Thème épuré et lisible pour tous les graphiques du TP.
# ----------------------------------------------------------------
theme_tp4 <- function(taille_base = 12) {
  ggplot2::theme_minimal(base_size = taille_base) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold",
                                              size = taille_base + 2),
      plot.subtitle   = ggplot2::element_text(color = "grey45",
                                              size = taille_base - 1),
      plot.caption    = ggplot2::element_text(color = "grey55",
                                              size = taille_base - 2,
                                              hjust = 0),
      axis.title      = ggplot2::element_text(size = taille_base - 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}


# ----------------------------------------------------------------
# 6. SAUVEGARDER UNE FIGURE (PNG)
# ----------------------------------------------------------------
sauvegarder_figure <- function(nom_fichier,
                               largeur = 10,
                               hauteur = 6,
                               dpi     = 300) {
  chemin <- file.path("outputs", "figures", nom_fichier)
  ggplot2::ggsave(chemin, width = largeur, height = hauteur, dpi = dpi)
  cat(sprintf("  -> Figure : %s\n", chemin))
}


# ----------------------------------------------------------------
# 7. SAUVEGARDER UN TABLEAU (CSV)
# ----------------------------------------------------------------
sauvegarder_tableau <- function(df, nom_fichier) {
  chemin <- file.path("outputs", "tables", nom_fichier)
  readr::write_csv(df, chemin)
  cat(sprintf("  -> Tableau : %s\n", chemin))
}










































