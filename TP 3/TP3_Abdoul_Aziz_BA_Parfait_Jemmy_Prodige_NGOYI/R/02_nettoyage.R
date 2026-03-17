# ============================================
# NETTOYAGE DES DONNÉES
# ============================================
source("R/01_import.R")

# ============================================
# 1. NETTOYER L'ÂGE (999 = manquant)
# ============================================
demo <- demo %>%
  mutate(
    age = ifelse(s1q4 == 999, NA, s1q4),
    sexe = factor(s1q2, levels = c(1, 2),
                  labels = c("Homme", "Femme"))
  )

# ============================================
# 2. CRÉER LES GROUPES D'ÂGE
# ============================================
demo <- demo %>%
  mutate(
    groupe_age = cut(age,
                     breaks = c(0, 14, 24, 34, 49, 64, Inf),
                     labels = c("0-14", "15-24", "25-34",
                                "35-49", "50-64", "65+"),
                     right  = TRUE)
  )

# ============================================
# 3. FUSIONNER AVEC LES DONNÉES DE MORBIDITÉ
# ============================================
donnees_merge <- donnees %>%
  left_join(demo %>% select(hhid, indiv, sexe, age, groupe_age),
            by = c("hhid", "indiv"))

# ============================================
# 4. VARIABLE MORBIDITÉ
# ============================================
donnees_merge <- donnees_merge %>%
  mutate(
    malade = ifelse(s3q1 == 1, 1, 0)
  )

# ============================================
# 5. VÉRIFICATION
# ============================================
dim(donnees_merge)
colSums(is.na(donnees_merge))
summary(donnees_merge$age)
table(donnees_merge$sexe)

# ============================================
# 6. SAUVEGARDE
# ============================================
saveRDS(donnees_merge, "data/processed/donnees_propres.rds")

