###############  DONNEES SELECTIONNES POUR REPONDRE AUX QUESTIONS ###############
### On récupère la variable poids qui se trouve dans secta_harvestw4 pour l'intégrer dans nos analyse

w4_poids <- secta_w4 |>
  select(hhid, wt_wave4 , sector) |>
  # on s'assure qu'il n'y a qu'une ligne par ménage
  distinct(hhid, .keep_all = TRUE) |>
  mutate(secteur = as_factor(sector))
### On récupère les données utiles à notre analyse sur les individus
donnees_individus <- sect1_w4 |>
  select(hhid, indiv, s1q2, s1q3, s1q4) |>
  mutate(s1q4 = as.numeric(s1q4)) |>
  filter(!is.na(s1q4), s1q4 >= 0 & s1q4 <= 120)


donnees_propres <- donnees_individus |>
  left_join(w4_poids, by = "hhid") |>
  filter(!is.na(wt_wave4)) |> # On s'assure que tous les ménage présente une pondération
  #zap_labels() |> 
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    
    lien_parente = factor(s1q3, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15), 
                          labels = c("Chef", "Conjoint", "Enfant", "Enfant (conj)", 
                                     "Enfant adopté", "Petit-enfant", "Frère/Sœur", 
                                     "Neveu/Nièce", "Beau-frère/sœur", "Parent", 
                                     "Beau-parent", "Domestique", "Autre parent", "Non parent"))
  )
### On enregistre les données dans data/porcessed
save(donnees_propres , file = "data/processed/donnees_propres.RDATA")

## Pour repondre à la question 5 , on fusionne la variable zone à notre donées initiales
## en faisant correspondre les id des ménage (hhid)
## 1. on calcul la taille des ménage en groupant par hhid
taille_menage <- sect1_w4 |>
  group_by(hhid) |>
  summarise(taille  = n()) |>
  ungroup()
taille_menage
## 2. On recupère l'id et la zone de la nouvelle base secta_w4 avec les pondération
donnees_secteur_poids <- secta_w4 |>
  select(hhid, sector, wt_wave4) |>
  distinct(hhid, .keep_all = TRUE) |>
  mutate(secteur = as_factor(sector))

## 3. On fusionne les deux bases
menage_secteur <- taille_menage|>
  inner_join(donnees_secteur_poids, by = "hhid")

menage_secteur

#############
