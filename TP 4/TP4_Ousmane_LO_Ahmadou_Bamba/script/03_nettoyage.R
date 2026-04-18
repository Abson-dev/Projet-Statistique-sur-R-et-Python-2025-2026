## Nettoyage de certains variables de la base 
#========== pour la base sect11a1_p4
sect11a1_p4 <- sect11a1_p4 |>
    distinct(hhid, plotid, .keep_all = TRUE) |> # supprimer les doublonns sur hhid, plot_id
    filter(superficie_ha >= 0 & superficie_ha <= 500) |> # Éliminer les superficies aberrantes (< 0 ou >500 ha)
    filter(!is.na(hhid)) |> # 3. Supprimer les hhid manquants (ménages non identifiés)
    filter(!is.na(superficie_ha)) # 4. Supprimer les parcelles sans superficie renseigné
#========== pour la base sect11b1_p4

sect11b1_p4 <- sect11b1_p4 |>
    distinct(hhid, plotid , wt_wave4 , .keep_all = TRUE) |> # supprimer les doublonns sur hhid, plot_id
    filter(!is.na(hhid))  # 3. Supprimer les hhid manquants (ménages non identifiés)




