source("R/fonctions.R")

##25/

# Nombre total de ménages uniques
# ── Étape 1 : Joindre les poids à secta3i ────────────────────────────────────
sa3i <- secta3i %>%
  left_join(
    secta_p %>% select(hhid, wt_wave4),
    by = "hhid"
  )

# ── Étape 2 : Dédupliquer à niveau ménage-culture ────────────────────────────
# Garder 1 ligne par ménage-culture avec son poids
sa3i_distinct <- sa3i %>%
  distinct(hhid, cropcode, .keep_all = TRUE)

# ── Vérifier les NA dans les poids
sum(is.na(sa3i_distinct$wt_wave4))   # combien de NA ?

# ── Solution : filtrer les NA avant de déclarer le plan ──────────────────────
sa3i_distinct <- sa3i %>%
  distinct(hhid, cropcode, .keep_all = TRUE) %>%
  filter(!is.na(wt_wave4))            # supprimer les lignes sans poids

# ── Étape 3 : Déclarer le plan de sondage ────────────────────────────────────
plan_sondage <- svydesign(
  ids     = ~hhid,          # identifiant ménage
  weights = ~wt_wave4,      # poids d'enquête
  data    = sa3i_distinct
)

# ── Étape 4 : Nombre total pondéré de ménages ────────────────────────────────
n_menages_total_pond <- sa3i %>%
  distinct(hhid, .keep_all = TRUE) %>%
  filter(!is.na(wt_wave4)) %>%         # ← même filtre pour cohérence
  summarise(total = sum(wt_wave4)) %>%
  pull(total)

# ── Étape 5 : Calculer les fréquences pondérées par culture ──────────────────
top15_pondere <- sa3i_distinct %>%
  group_by(cropcode) %>%
  summarise(
    n_menages      = n(),                          # effectif non pondéré
    n_menages_pond = sum(wt_wave4, na.rm = TRUE),  # effectif pondéré
    .groups = "drop"
  ) %>%
  arrange(desc(n_menages_pond)) %>%
  slice(1:15) %>%
  mutate(
    frequence      = n_menages      / n_distinct(sa3i$hhid)      * 100,
    frequence_pond = n_menages_pond / n_menages_total_pond * 100
  )

top15_pondere


#Barplot coloré par type de culture

# Données directement intégrées avec les vrais labels
# Dictionnaire des noms de cultures depuis le questionnaire
crop_names <- c(
  "1010" = "Cowpea/Beans",
  "1020" = "Cassava",
  "1040" = "Cocoyam",
  "1050" = "Cotton",
  "1060" = "Groundnut",
  "1070" = "Sorghum",
  "1080" = "Maize",
  "1090" = "Melon",
  "1100" = "Millet",
  "1110" = "Rice",
  "1121" = "White Yam",
  "1122" = "Yellow Yam",
  "1123" = "Water Yam",
  "1124" = "Three Leave Yam",
  "2010" = "Acha",
  "2020" = "Bambara Nut",
  "2030" = "Banana",
  "2040" = "Sesame",
  "2050" = "Carrot",
  "2060" = "Cucumber",
  "2070" = "Cabbage",
  "2080" = "Garden Egg",
  "2090" = "Garlic",
  "2100" = "Ginger",
  "2120" = "Okro",
  "2130" = "Onion",
  "2141" = "Sweet Pepper",
  "2142" = "Small Pepper",
  "2150" = "Pigeon Pea",
  "2160" = "Pineapple",
  "2170" = "Plantain",
  "2180" = "Irish Potato",
  "2181" = "Sweet Potato",
  "2190" = "Pumpkin",
  "2194" = "Green Vegetable",
  "2220" = "Soya Beans",
  "2230" = "Sugar Cane",
  "2240" = "Tea",
  "2250" = "Tobacco",
  "2260" = "Tomato",
  "2270" = "Walnut",
  "2280" = "Wheat",
  "2290" = "Zobo",
  "3010" = "Apple",
  "3020" = "Cashew",
  "3040" = "Cocoa",
  "3050" = "Coconut",
  "3060" = "Coffee",
  "3080" = "Grape Fruit",
  "3090" = "Guava",
  "3110" = "Kolanut",
  "3120" = "Lemon",
  "3130" = "Lime",
  "3150" = "Mandarin",
  "3160" = "Mango",
  "3170" = "Orange",
  "3180" = "Oil Palm",
  "3190" = "Agbono",
  "3200" = "Oil Bean",
  "3210" = "Pawpaw",
  "3220" = "Pear",
  "3221" = "Avocado Pear",
  "3230" = "Rubber",
  "9999" = "Other"
)

# ── Ajouter la colonne crop_name au top15_pondere ────────────────────────────
top15_pondere <- top15_pondere %>%
  mutate(crop_name = crop_names[as.character(cropcode)])

top15_pondere

# Assignation des types de cultures
top15_pondere <- top15_pondere %>%
  mutate(type = case_when(
    cropcode %in% c(1080, 1070, 1100, 1110)        ~ "Céréale",
    cropcode %in% c(1010, 2220, 1060, 2040)        ~ "Légumineuse/Oléagineux",
    cropcode %in% c(1020, 1121, 1040, 1123)        ~ "Tubercule/Racine",
    cropcode %in% c(2120, 2190, 2142)              ~ "Légume/Culture maraîchère"
  ))

# Barplot
ggplot(top15_pondere, aes(x = reorder(crop_name, frequence_pond),
                  y = frequence_pond,
                  fill = type)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(frequence_pond, 1), "%")),
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Céréale"                  = "#E07B39",
    "Légumineuse/Oléagineux"   = "#3A7D44",
    "Tubercule/Racine"         = "#8B5E3C",
    "Légume/Culture maraîchère"= "#4A90D9"
  )) +
  labs(
    title    = "Top 15 des cultures les plus fréquentes au Nigeria",
    subtitle = "% de ménages cultivant chaque culture — GHSP Wave 4 (2018/19)",
    x        = NULL,
    y        = "Fréquence (% des ménages)",
    fill     = "Type de culture"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 62), breaks = seq(0, 60, 10))

  
  ##26/
  # Nombre de cultures différentes par ménage (pondéré)
  diversification_index <- sa3i %>%
    filter(!is.na(wt_wave4)) %>%                    # supprimer les NA de poids
    distinct(hhid, cropcode, .keep_all = TRUE) %>%
    group_by(hhid, wt_wave4) %>%                    # garder le poids avec hhid
    summarise(nb_cultures = n_distinct(cropcode),
              .groups = "drop")
  
  #Histogramme
  counts <- diversification_index %>%
    count(nb_cultures, name = "n_menages")
  
  ggplot(counts, aes(x = nb_cultures, y = n_menages)) +
    geom_bar(stat = "identity", fill = "orange",
             color = "black", alpha = 0.85) +
    geom_text(aes(label = n_menages),
              vjust = -0.5, size = 3.5,
              fontface = "bold", color = "black") +
    geom_vline(xintercept = weighted.mean(diversification_index$nb_cultures,
                                          diversification_index$wt_wave4), # ← pondéré
               color = "blue", linetype = "dashed", linewidth = 1) +
    annotate("text",
             x     = weighted.mean(diversification_index$nb_cultures,
                                   diversification_index$wt_wave4) + 0.3,
             y     = Inf,
             label = paste0("Moyenne = ",
                            round(weighted.mean(diversification_index$nb_cultures,
                                                diversification_index$wt_wave4), 1)),
             vjust = 2, hjust = 0, color = "blue", size = 4) +
    scale_x_continuous(breaks = 1:max(diversification_index$nb_cultures)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title    = "Distribution du nombre de cultures par ménage",
      subtitle = "Nigeria GHSP Wave 4 (2018/19)",
      x        = "Nombre de cultures",
      y        = "Nombre de ménages"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = "grey40"),
      panel.grid.minor = element_blank()
    )
  
  #Test de Wilcoxon
  
  # Récupérer le secteur (urban/rural) pour chaque ménage
  sector_hhid <- sa3i %>%
    distinct(hhid, sector) %>%
    mutate(secteur = case_when(
      sector == 1 ~ "Urbain",
      sector == 2 ~ "Rural"
    ))
  
  # Fusionner avec la table de diversification
  diversification_index <- diversification_index %>%
    left_join(sector_hhid, by = "hhid")
  
  # Vérifier la normalité (justifie l'usage de Wilcoxon)
  Normality_test <- shapiro.test(diversification_index$nb_cultures)
  
  # Test de Wilcoxon : urbain vs rural (pondéré via coin)
  library(coin)
  wilcox_test <- wilcox.test(nb_cultures ~ factor(secteur),
                             data    = diversification_index,
                             weights = ~wt_wave4,
                             distribution = "asymptotic")
  print(wilcox_test)
  
  # Statistiques descriptives par groupe (pondérées)
  Stat_sector <- diversification_index %>%
    group_by(secteur) %>%
    summarise(
      n          = n(),
      moyenne    = round(weighted.mean(nb_cultures, wt_wave4), 2),   # ← pondéré
      mediane    = median(nb_cultures),
      ecart_type = round(sqrt(sum(wt_wave4 * (nb_cultures -          # ← pondéré
                                                weighted.mean(nb_cultures, wt_wave4))^2) /
                                sum(wt_wave4)), 2)
    )
  
  #Violin Plot
  
  # Préparer le label de p-value pour l'afficher sur le graphique
  p_value <- round(wilcox_test$p.value, 4)
  p_label <- ifelse(p_value < 0.001, "p < 0.001",
                    paste0("p = ", p_value))
  
  # Précalculer les moyennes pondérées par secteur
  moyennes_pond <- diversification_index %>%
    group_by(secteur) %>%
    summarise(moyenne_pond = weighted.mean(nb_cultures, wt_wave4),
              .groups = "drop")
  
  ggplot(diversification_index, aes(x = secteur, y = nb_cultures, fill = secteur)) +
    
    geom_violin(alpha = 0.7, trim = FALSE) +
    
    # Boxplot à l'intérieur du violin
    geom_boxplot(width = 0.15, fill = "white",
                 outlier.shape = NA, alpha = 0.8) +
    
    # Moyenne pondérée par groupe
    geom_point(data  = moyennes_pond,
               aes(x = secteur, y = moyenne_pond),
               shape = 18, size = 4,
               color = "red", inherit.aes = FALSE) +
    
    # Afficher la p-value
    annotate("text",
             x     = 1.5,
             y     = max(diversification_index$nb_cultures) + 0.5,
             label = paste0("Wilcoxon\n", p_label),
             size  = 4, fontface = "bold", color = "black") +
    
    # Ligne de significativité
    annotate("segment",
             x = 1, xend = 2,
             y = max(diversification_index$nb_cultures) + 0.2,
             yend = max(diversification_index$nb_cultures) + 0.2) +
    
    scale_fill_manual(values = c("Urbain" = "#4A90D9",
                                 "Rural"  = "#3A7D44")) +
    scale_y_continuous(breaks = 1:max(diversification_index$nb_cultures)) +
    labs(
      title    = "Diversification culturale : Urbain vs Rural",
      subtitle = "Nigeria GHSP Wave 4 (2018/19)",
      x        = NULL,
      y        = "Nombre de cultures par ménage",
      fill     = "Zone"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = "grey40"),
      legend.position  = "none",
      panel.grid.minor = element_blank()
    )
  
 
  
  ##27/
  # Joindre les poids a secta11c
  secta11c <- secta11c2 %>%
    left_join(
      secta_p %>% select(hhid, wt_wave4),
      by = "hhid"
    )
  
  # Nombre total pondere de menages
  n_total_pond <- secta11c %>%
    distinct(hhid, wt_wave4) %>%
    filter(!is.na(wt_wave4)) %>%
    summarise(total = sum(wt_wave4)) %>%
    pull(total)
  
  taux_engrais <- secta11c %>%
    filter(!is.na(wt_wave4)) %>%
    group_by(hhid, sector, state, wt_wave4) %>%
    summarise(
      chimique  = as.integer(any(s11c2q10 == 1 | s11c2q1  == 1, na.rm = TRUE)),
      npk       = as.integer(any(s11c2q36_1 == 1,               na.rm = TRUE)),
      uree      = as.integer(any(s11c2q36_2 == 1,               na.rm = TRUE)),
      organique = as.integer(any(s11dq36   == 1,                na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Par milieu de residence
  taux_sector <- taux_engrais %>%
    group_by(sector) %>%
    summarise(
      n_chimique  = sum(chimique  * wt_wave4),
      n_npk       = sum(npk       * wt_wave4),
      n_uree      = sum(uree      * wt_wave4),
      n_organique = sum(organique * wt_wave4),
      .groups = "drop"
    ) %>%
    mutate(
      freq_chimique  = round(n_chimique  / n_total_pond * 100, 2),
      freq_npk       = round(n_npk       / n_total_pond * 100, 2),
      freq_uree      = round(n_uree      / n_total_pond * 100, 2),
      freq_organique = round(n_organique / n_total_pond * 100, 2),
      secteur_label  = case_when(
        sector == 1 ~ "Urbain",
        sector == 2 ~ "Rural"
      )
    ) %>%
    select(secteur_label, everything(), -sector)
  
  # Taille d'echantillon pour les IC
  n_ech <- nrow(taux_engrais)
  
  # Ajouter les IC
  taux_sector <- taux_sector %>%
    mutate(
      ic_chimique  = 1.96 * sqrt((freq_chimique/100)  * (1 - freq_chimique/100)  / n_ech) * 100,
      ic_npk       = 1.96 * sqrt((freq_npk/100)       * (1 - freq_npk/100)       / n_ech) * 100,
      ic_uree      = 1.96 * sqrt((freq_uree/100)      * (1 - freq_uree/100)      / n_ech) * 100,
      ic_organique = 1.96 * sqrt((freq_organique/100) * (1 - freq_organique/100) / n_ech) * 100
    )
  
  # Format long pour ggplot
  taux_long <- taux_sector %>%
    select(secteur_label,
           freq_chimique, freq_npk, freq_uree, freq_organique,
           ic_chimique,   ic_npk,   ic_uree,   ic_organique) %>%
    pivot_longer(
      cols          = -secteur_label,
      names_to      = c(".value", "type_engrais"),
      names_pattern = "(.+)_(.+)"
    ) %>%
    mutate(type_engrais = case_when(
      type_engrais == "chimique"  ~ "Chimique",
      type_engrais == "npk"       ~ "NPK",
      type_engrais == "uree"      ~ "Uree",
      type_engrais == "organique" ~ "Organique"
    ))
  
  # Barplot groupe avec IC
  ggplot(taux_long, aes(x = secteur_label, y = freq, fill = type_engrais)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(
      aes(ymin = freq - ic, ymax = freq + ic),
      position  = position_dodge(width = 0.8),
      width     = 0.25, color = "black", linewidth = 0.6
    ) +
    geom_text(
      aes(label = paste0(round(freq, 1), "%"), y = freq + ic + 0.5),
      position = position_dodge(width = 0.8), size = 3, fontface = "bold"
    ) +
    scale_fill_manual(values = c(
      "Chimique"  = "#E07B39", "NPK" = "#3A7D44",
      "Uree"      = "#4A90D9", "Organique" = "#8B5E3C"
    )) +
    scale_y_continuous(
      limits = c(0, max(taux_long$freq + taux_long$ic) + 5),
      breaks = seq(0, 100, 10)
    ) +
    labs(
      title    = "Taux d'utilisation des engrais par milieu de residence",
      subtitle = "Avec intervalles de confiance a 95% - Nigeria GHSP Wave 4 (2018/19)",
      x        = NULL, y = "% de menages", fill = "Type d'engrais"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "grey40"),
      legend.position    = "bottom",
      panel.grid.major.x = element_blank()
    )
  
  ##28/
  
      #Superficie calculée par gps ou estimée 
      s11a1<- sect11a1
      superficie_recalc <- s11a1 %>%
      mutate(superficie_ha = case_when(
      
      # Cas 1 : mesure GPS disponible (s11aq4a = 1)
      s11aq4a == 1 ~ s11aq4c / 10000,
      
      # Cas 2 : pas de GPS → utiliser la mesure déclarée selon l'unité
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 5 ~ s11aq4aa * 0.4047,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 6 ~ s11aq4aa,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 7 ~ s11aq4aa / 10000,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 1 & (zone == 1| zone == 6) ~ s11aq4aa *0.00012,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 1 & zone == 2 ~ s11aq4aa *0.00016,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 1 & zone == 3 ~ s11aq4aa *0.00011,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 1 & zone == 4 ~ s11aq4aa *0.00019,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 1 & zone == 5 ~ s11aq4aa *0.00021,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 2 & zone == 1 ~ s11aq4aa *0.0027,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 2 & zone == 2 ~ s11aq4aa *0.004,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 2 & zone == 3 ~ s11aq4aa *0.00494,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 2 & (zone == 4| zone == 5) ~ s11aq4aa *0.0023,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 2 & zone == 6 ~ s11aq4aa *0.00001,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 3 & zone == 1 ~ s11aq4aa *0.00006,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 3 & zone == 2 ~ s11aq4aa *0.00016,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 3 & (zone == 3| zone == 4) ~ s11aq4aa *0.00004,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 3 & zone == 5 ~ s11aq4aa *0.00013,
      (is.na(s11aq4a) | s11aq4a == 2) & s11aq4b == 3 & zone == 6 ~ s11aq4aa *0.00041,
      
      
      # Sinon : NA
      TRUE ~ NA_real_
    )) %>%
    
    # On sélectionne les 3 colonnes APRÈS le mutate
     select(hhid, plotid, superficie_ha)
  
    #Jointure entre la base sect11f (remfermant les pourcentages de plot cultivés) et superficie_recalc
        sect11f_suprecalc<- sect11f %>%
        inner_join(superficie_recalc, by= c("hhid","plotid"))
        
    #Data frame avec superficie cultivée
        dataframe_supcultivée <- sect11f_suprecalc %>%
          mutate(superficie_cultivee = case_when(
            !is.na(s11fq1) ~ (superficie_ha * s11fq1) / 100,
            TRUE           ~ NA_real_
          )) %>%
          select(hhid, plotid, cropcode, s11fq1, superficie_ha, superficie_cultivee)
    
  
  
        # ══════════════════════════════════════════════════════════════════════════════
        # DICTIONNAIRE : Codes → Noms des États du Nigeria (Nigeria GHSP / NBS)
        # ══════════════════════════════════════════════════════════════════════════════
        state_labels <- tibble::tibble(
          state = c(1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
                    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                    21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                    31, 32, 33, 34, 35, 36, 37),
          state_name = c(
            "Abia",         "Adamawa",      "Akwa Ibom",    "Anambra",
            "Bauchi",       "Bayelsa",      "Benue",        "Borno",
            "Cross River",  "Delta",        "Ebonyi",       "Edo",
            "Ekiti",        "Enugu",        "Gombe",        "Imo",
            "Jigawa",       "Kaduna",       "Kano",         "Katsina",
            "Kebbi",        "Kogi",         "Kwara",        "Lagos",
            "Nasarawa",     "Niger",        "Ogun",         "Ondo",
            "Osun",         "Oyo",          "Plateau",      "Rivers",
            "Sokoto",       "Taraba",       "Yobe",         "Zamfara",
            "FCT Abuja"
          )
        )
        
        # ══════════════════════════════════════════════════════════════════════════════
        # ÉTAPE 1 : Filtrer maïs et millet uniquement
        # ══════════════════════════════════════════════════════════════════════════════
        rendement_data <- prod_kg %>%
          filter(cropcode %in% c(1080, 1100)) %>%
          filter(!is.na(rendement) & !is.na(superficie_cultivee)) %>%
          filter(superficie_cultivee > 0 & production > 0) %>%
          mutate(crop_name = case_when(
            cropcode == 1080 ~ "Maïs",
            cropcode == 1100 ~ "Millet"
          )) %>%
          # ── Jointure pour ajouter les noms d'États dès le départ ──────────────────
          left_join(state_labels, by = "state") %>%
          mutate(
            # Si le nom est manquant (code non répertorié), conserver le code en texte
            state_name = if_else(is.na(state_name), paste0("État ", state), state_name)
          )
        
        # ══════════════════════════════════════════════════════════════════════════════
        # ÉTAPE 2 : Supprimer les outliers (IQR × 3)
        # ══════════════════════════════════════════════════════════════════════════════
        rendement_data <- rendement_data %>%
          group_by(cropcode) %>%
          mutate(
            Q1        = quantile(rendement, 0.25, na.rm = TRUE),
            Q3        = quantile(rendement, 0.75, na.rm = TRUE),
            IQR_val   = Q3 - Q1,
            borne_inf = Q1 - 3 * IQR_val,
            borne_sup = Q3 + 3 * IQR_val
          ) %>%
          filter(rendement >= borne_inf & rendement <= borne_sup) %>%
          select(-Q1, -Q3, -IQR_val, -borne_inf, -borne_sup) %>%
          ungroup()
        
        cat("Observations après suppression outliers :\n")
        print(rendement_data %>% count(crop_name))
        
        # ══════════════════════════════════════════════════════════════════════════════
        # ÉTAPE 3 : Identifier le Top 12 des États (par rendement médian)
        # ══════════════════════════════════════════════════════════════════════════════
        top_states <- rendement_data %>%
          group_by(state_name) %>%                        # <-- grouper par NOM
          summarise(
            rendement_median = median(rendement, na.rm = TRUE),
            n_menages        = n(),
            .groups          = "drop"
          ) %>%
          filter(n_menages >= 5) %>%
          arrange(desc(rendement_median)) %>%
          slice(1:12) %>%
          pull(state_name)
        
        cat("\nTop 12 États sélectionnés :\n")
        print(top_states)
        
        # Filtrer sur les noms d'États
        rendement_top <- rendement_data %>%
          filter(state_name %in% top_states) %>%
          mutate(state_name = factor(state_name))         # <-- facteur sur le NOM
        
        # ══════════════════════════════════════════════════════════════════════════════
        # ÉTAPE 4 : Calcul de la moyenne pondérée ROBUSTE (pré-calculée)
        # ══════════════════════════════════════════════════════════════════════════════
        # Calculer hors de ggplot pour éviter les problèmes d'alignement x / w
        wmean_df <- rendement_top %>%
          group_by(state_name, crop_name) %>%
          summarise(
            wmean_rendement = weighted.mean(rendement, w = wt_wave4, na.rm = TRUE),
            .groups = "drop"
          )
        
        # ══════════════════════════════════════════════════════════════════════════════
        # ÉTAPE 5 : Graphique
        # ══════════════════════════════════════════════════════════════════════════════
        ggplot(rendement_top,
               aes(x    = reorder(state_name, rendement, FUN = median),  # <-- state_NAME
                   y    = rendement,
                   fill = crop_name)) +
          
          # ── Boxplot ────────────────────────────────────────────────────────────────
          geom_boxplot(
            outlier.shape = NA,
            alpha         = 0.7,
            width         = 0.6
          ) +
          
          # ── Moyenne pondérée (source externe pré-calculée = 100% fiable) ───────────
          geom_point(
            data     = wmean_df,
            aes(x    = reorder(state_name, wmean_rendement),
                y    = wmean_rendement,
                group = crop_name),
            shape    = 18,
            size     = 3,
            color    = "red",
            position = position_dodge(width = 0.6)
          ) +
          
          coord_flip() +
          
          scale_fill_manual(values = c(
            "Maïs"   = "#3B82F6",   # bleu vif
            "Millet" = "#22C55E"    # vert vif
          )) +
          
          scale_y_continuous(
            labels = scales::comma,
            breaks = scales::pretty_breaks(n = 6)
          ) +
          
          labs(
            title    = "Distribution des rendements par État",
            subtitle = "Top 12 États — Maïs et Millet — Nigeria GHSP Wave 4 (2018/19)",
            x        = "État",
            y        = "Rendement (kg/ha)",
            fill     = "Culture",
            caption  = "Outliers écartés via la règle IQR × 3. Losange rouge = moyenne pondérée (wt_wave4)."
          ) +
          
          theme_minimal(base_size = 12) +
          theme(
            plot.title       = element_text(face = "bold", size = 14),
            plot.subtitle    = element_text(color = "grey40"),
            legend.position  = "bottom",
            panel.grid.minor = element_blank(),
            axis.text.y      = element_text(size = 11)   # lisibilité des noms d'États
          )
        
        
        ##29/
        
        library(coin)
        
        # ── Étape 1 : Joindre l'utilisation d'engrais chimique à rendement_data ───────
        # taux_engrais contient déjà chimique (0/1) par hhid
        rendement_engrais <- rendement_data %>%
          left_join(
            taux_engrais %>% select(hhid, chimique),
            by = "hhid"
          ) %>%
          filter(!is.na(chimique)) %>%
          mutate(
            engrais_chimique = factor(chimique,
                                      levels = c(0, 1),
                                      labels = c("Sans engrais", "Avec engrais"))
          )
        
        # ── Étape 2 : Statistiques descriptives par groupe ────────────────────────────
        stats_engrais <- rendement_engrais %>%
          group_by(crop_name, engrais_chimique) %>%
          summarise(
            n         = n(),
            mediane   = round(median(rendement), 1),
            moyenne   = round(weighted.mean(rendement, wt_wave4), 1),
            ecart_type = round(sd(rendement), 1),
            .groups   = "drop"
          )
        print(stats_engrais)
        
        # ── Étape 3 : Test de Wilcoxon par culture ────────────────────────────────────
        # Maïs
        wilcox_mais <- wilcox.test(
          rendement ~ engrais_chimique,
          data  = rendement_engrais %>% filter(crop_name == "Maïs"),
          exact = FALSE
        )
        
        # Millet
        wilcox_millet <- wilcox.test(
          rendement ~ engrais_chimique,
          data  = rendement_engrais %>% filter(crop_name == "Millet"),
          exact = FALSE
        )
        
        cat("=== Test de Wilcoxon — Maïs ===\n")
        print(wilcox_mais)
        cat("\n=== Test de Wilcoxon — Millet ===\n")
        print(wilcox_millet)
        
        # ── Étape 4 : Taille d'effet (r de Wilcoxon) ─────────────────────────────────
        # r = Z / sqrt(N)
        n_mais   <- nrow(rendement_engrais %>% filter(crop_name == "Maïs"))
        n_millet <- nrow(rendement_engrais %>% filter(crop_name == "Millet"))
        
        r_mais   <- qnorm(wilcox_mais$p.value   / 2) / sqrt(n_mais)
        r_millet <- qnorm(wilcox_millet$p.value / 2) / sqrt(n_millet)
        
        taille_effet <- data.frame(
          Culture       = c("Maïs", "Millet"),
          W             = round(c(wilcox_mais$statistic,
                                  wilcox_millet$statistic), 0),
          p_value       = round(c(wilcox_mais$p.value,
                                  wilcox_millet$p.value), 4),
          r_effet       = round(abs(c(r_mais, r_millet)), 3),
          interpretation = case_when(
            abs(c(r_mais, r_millet)) < 0.1 ~ "Effet négligeable",
            abs(c(r_mais, r_millet)) < 0.3 ~ "Effet faible",
            abs(c(r_mais, r_millet)) < 0.5 ~ "Effet modéré",
            TRUE                           ~ "Effet fort"
          )
        )
        print(taille_effet)
        
        # ── Étape 5 : Boxplot groupé engrais × rendement ─────────────────────────────
        # Labels p-value pour annotation
        p_mais_label   <- ifelse(wilcox_mais$p.value   < 0.001,
                                 "p < 0.001",
                                 paste0("p = ", round(wilcox_mais$p.value, 3)))
        p_millet_label <- ifelse(wilcox_millet$p.value < 0.001,
                                 "p < 0.001",
                                 paste0("p = ", round(wilcox_millet$p.value, 3)))
        
        # ── Créer un dataframe d'annotations ─────────────────────────────────────────
        annotations <- data.frame(
          crop_name = c("Maïs", "Millet"),
          label     = c(
            paste0("Wilcoxon\n", p_mais_label),
            paste0("Wilcoxon\n", p_millet_label)   # ← p-value correcte pour chaque culture
          ),
          x         = c(1.5, 1.5),
          y         = c(Inf, Inf)
        )
        
        
        ggplot(rendement_engrais,
               aes(x = engrais_chimique, y = rendement, fill = engrais_chimique)) +
          geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
          stat_summary(fun = mean, geom = "point",
                       shape = 18, size = 3, color = "red") +
          
          # ← Remplace annotate() par geom_text() avec le dataframe annotations
          geom_text(data     = annotations,
                    aes(x = x, y = y, label = label),
                    vjust    = 2,
                    size     = 3.5,
                    fontface = "bold",
                    inherit.aes = FALSE) +
          
          facet_wrap(~crop_name, scales = "free_y") +
          scale_fill_manual(values = c(
            "Sans engrais" = "#95a5a6",
            "Avec engrais" = "#E07B39"
          )) +
          labs(
            title    = "Rendement selon l'utilisation d'engrais chimique",
            subtitle = "Maïs et Millet — Nigeria GHSP Wave 4 (2018/19)",
            x        = NULL,
            y        = "Rendement (kg/ha)",
            fill     = "Utilisation engrais",
            caption  = "Losange rouge = moyenne. Outliers écartés via IQR × 3."
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title       = element_text(face = "bold", size = 14),
            plot.subtitle    = element_text(color = "grey40"),
            legend.position  = "bottom",
            panel.grid.minor = element_blank(),
            strip.text       = element_text(face = "bold", size = 12)
          )
        
        
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  