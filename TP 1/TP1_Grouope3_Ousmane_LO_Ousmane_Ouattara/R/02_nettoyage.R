###############  DONNEES SELECTIONNES POUR REPONDRE AUX QUESTIONS ###############
donnees_propres <- sect1_w4 |>
  # On filtre les données manquantes pour la variable age et s'assure que c'est bien numérique!
  # On ne supprime pas tous les NA sur les variables au risque de perdre beaucoup trop d'informations.
  # La gestion se fera au fur et à mesure
  mutate(s1q4 = as.numeric(s1q4)) |>
  filter(!is.na(s1q4), s1q4 >= 0 & s1q4 <= 120) |> # On suppose que tous les indiviudus ont moins de 120 ans
  
  select(s1q2, s1q3, s1q4) |>
  zap_labels() |> 
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    
    lien_parente = factor(s1q3, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15), 
                          labels = c("Chef", "Conjoint", "Enfant", "Enfant (conj)", 
                                     "Enfant adopté", "Petit-enfant", "Frère/Sœur", 
                                     "Neveu/Nièce", "Beau-frère/sœur", "Parent", 
                                     "Beau-parent", "Domestique", "Autre parent", "Non parent"))
  )
save(donnees_propres , file = "data/processed/donnees_propres.RDATA")
summary(donnees_propres$s1q4)

## Pour repondre à la question 5 , on fusionne la variable zone à notre donées initiales
## en faisant correspondre les id des ménage (hhid)
## 1. on calcul la taille des ménage en groupant par hhid
taille_menage <- sect1_w4 |>
  group_by(hhid) |>
  summarise(taille  = n()) |>
  ungroup()
taille_menage
## 2. On recupère l'id et la zone de la nouvelle base secta_w4
donnees_secteur <- secta_w4 |>
  select(hhid , sector) |>
   mutate(secteur = as_factor(sector))

## 3. On fusionne les deux bases
menage_secteur <- taille_menage|>
  inner_join(donnees_secteur, by = "hhid")

menage_secteur

#############
