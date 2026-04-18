######### Recodage des différentes variables utilisé dans le TP ##############"

## Transformation des noms de certains variables et leurs modalités 
#============= pour la base sect11a1_plantingw4 (sect11a1_p4)
sect11a1_p4 <- sect11a1_p4 |>
    mutate(
        zone = as_factor (zone) , 
        unite_mesure = as_factor(s11aq4b) , 
        superficie_agricole = s11aq4aa, 
        superficie_gps_m2 = s11aq4c ,
        secteur = as_factor(sector)
        
    ) 
#============ pour la base sect11b1_plantingw4 (sect11b1_p4)
sect11b1_p4 <- sect11b1_p4 |>
    mutate(
        tenure = factor(s11b1q4 , 
                        levels = 1:7,
                        labels = c("Proprieté pleine (achatd direct)",
                                   "Location (espèces/nature)",
                                   "Usage graduit",
                                   "Distribution par la communautée",
                                   "héritage familialle",
                                   "Métayage",
                                   "Echange temporaire de terres")
                        ) , 
        secteur = factor(sector , levels=1:2 , labels =c("urbain","rural")) 
        
        )

#============================= Question 19. ====================================
# Construction de la variable superficie en hectare 
#===============================================================================
##  L'unité de mesure de la surface agricole dépend de la de la zone. les facteurs de conversion ont été trouvé dans
## un rapport du Nigéria sur le site de la banque mondiale.

sect11a1_p4 <- sect11a1_p4 |> 
    mutate(
        superficie_ha = case_when(
            unite_mesure == "4. Plots"          ~ superficie_agricole * 0.0667,
            unite_mesure == "5. ACRES"          ~ superficie_agricole * 0.4,
            unite_mesure == "6. HECTARES"       ~ superficie_agricole * 1,
            unite_mesure == "7. SQUARE METERS"  ~ superficie_agricole * 0.0001,
            
            # Stands
            unite_mesure == "3. STANDS" & zone == "1. North Central" ~ superficie_agricole * 0.00006,
            unite_mesure == "3. STANDS" & zone == "2. North East"    ~ superficie_agricole * 0.00016,
            unite_mesure == "3. STANDS" & zone == "3. North West"    ~ superficie_agricole * 0.00004,
            unite_mesure == "3. STANDS" & zone == "4. South East"    ~ superficie_agricole * 0.00004,
            unite_mesure == "3. STANDS" & zone == "5. South South"   ~ superficie_agricole * 0.00013,
            unite_mesure == "3. STANDS" & zone == "6. South West"    ~ superficie_agricole * 0.00041,
            
            # Ridges
            unite_mesure == "2. RIDGES" & zone == "1. North Central" ~ superficie_agricole * 0.0027,
            unite_mesure == "2. RIDGES" & zone == "2. North East"    ~ superficie_agricole * 0.004,
            unite_mesure == "2. RIDGES" & zone == "3. North West"    ~ superficie_agricole * 0.00494,
            unite_mesure == "2. RIDGES" & zone == "4. South East"    ~ superficie_agricole * 0.0023,
            unite_mesure == "2. RIDGES" & zone == "5. South South"   ~ superficie_agricole * 0.0023,
            unite_mesure == "2. RIDGES" & zone == "6. South West"    ~ superficie_agricole * 0.00001,
            
            # Heaps
            unite_mesure == "1. HEAPS" & zone == "1. North Central" ~ superficie_agricole * 0.00012,
            unite_mesure == "1. HEAPS" & zone == "2. North East"    ~ superficie_agricole * 0.00016,
            unite_mesure == "1. HEAPS" & zone == "3. North West"    ~ superficie_agricole * 0.00011,
            unite_mesure == "1. HEAPS" & zone == "4. South East"    ~ superficie_agricole * 0.00019,
            unite_mesure == "1. HEAPS" & zone == "5. South South"   ~ superficie_agricole * 0.00021,
            unite_mesure == "1. HEAPS" & zone == "6. South West"    ~ superficie_agricole * 0.00012,
            
            TRUE ~ NA_real_
        )
    )

## Intégration de la variable "poids" dans la base.
## La variable poids se trouve dans la base "secta_harvestw4 ( secta_w4)
##  dans la base sect11a1_p4
sect11a1_p4 <- sect11a1_p4 |>
    left_join(
        secta_w4 |> select(hhid, wt_wave4) ,
        by =  "hhid"
        
    )
sect11b1_p4 <- sect11b1_p4 |>
    left_join(
        secta_w4 |> select(hhid ,  wt_wave4) ,
        by = "hhid"
    )

