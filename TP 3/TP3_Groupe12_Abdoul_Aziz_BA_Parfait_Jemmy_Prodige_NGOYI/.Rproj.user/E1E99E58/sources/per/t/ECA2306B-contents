
# ============================================
# FONCTIONS PERSONNALISÉES
# ============================================

# Fonction pour calculer le taux avec IC 95%
calc_taux <- function(data, var_group, var_malade) {
  data %>%
    filter(!is.na({{var_group}})) %>%
    group_by({{var_group}}) %>%
    summarise(
      n         = n(),
      n_malades = sum({{var_malade}}, na.rm = TRUE),
      taux      = mean({{var_malade}}, na.rm = TRUE) * 100,
      ic_bas    = taux - 1.96 * sqrt(taux * (100 - taux) / n),
      ic_haut   = taux + 1.96 * sqrt(taux * (100 - taux) / n)
    )
}