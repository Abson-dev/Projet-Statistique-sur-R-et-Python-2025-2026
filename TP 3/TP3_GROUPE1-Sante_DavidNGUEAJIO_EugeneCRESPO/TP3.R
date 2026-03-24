source("R/fonctions.R")
df <- read_dta("data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect4a_harvestw4.dta")
#names(df)
#sapply(df, function(x) attr(x, "label")) # les labels des variables
#attr(df$s3q2,   "labels") #pour les labels des modalités
#table(df$s3q2, useNA = "always") # Vérifier les modalités
secta  <- read_dta("data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/secta_harvestw4.dta")
#View(secta)
# =========================================================================
# 2. FUSION SUR sect3a_harvestW4 + secta_harvestw4(ajout des pondérations)
# =========================================================================
menage <- secta %>%
  select(hhid, wt_wave4, strata,
         psu     = ea,       # Unité primaire de sondage (zone de dénombrement)
         sector_hh = sector, # Milieu urbain/rural
         state_hh  = state,  # État nigérian
         zone_hh   = zone    # Zone géographique
  ) %>%
  mutate(
    milieu   = factor(sector_hh, levels = 1:2, labels = c("Urbain", "Rural")),
    state_hh = factor(state_hh, levels = 1:37,
                      labels = c("Abia","Adamawa","Akwa Ibom","Anambra",
                                 "Bauchi","Bayelsa","Benue","Borno",
                                 "Cross River","Delta","Ebonyi","Edo",
                                 "Ekiti","Enugu","Gombe","Imo","Jigawa",
                                 "Kaduna","Kano","Katsina","Kebbi","Kogi",
                                 "Kwara","Lagos","Nasarawa","Niger","Ogun",
                                 "Ondo","Osun","Oyo","Plateau","Rivers",
                                 "Sokoto","Taraba","Yobe","Zamfara","FCT")),
    zone_hh  = factor(zone_hh, levels = 1:6,
                      labels = c("North Central","North East","North West",
                                 "South East","South South","South West"))
  )
DF <-df  %>%
  left_join(menage, by = "hhid") %>%
  filter(!is.na(wt_wave4)   # exclure ménages sans poids (N=49)
         )      # garder individus ≥ 3 ans

#View(DF)
#View(df)
#names(DF)
# ============================================
# 1. IMPORTATION
# ============================================
chemin <- "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/"

df_roster <- read_dta(paste0(chemin, "sect1_harvestw4.dta"))
df_roster <- df_roster %>% 
  mutate(sexe = s1q2) %>%
  select(!s1q2)

df_roster <- df_roster %>% 
  mutate(age = s1q4) %>%
  select(!s1q4)
# ============================================
# 2. FUSION SUR hhid + indiv
# ============================================
dg <- DF %>%
  select(hhid, indiv,wt_wave4, s4aq3, s4aq3b_1) %>%   # <- s3q3 ajouté
  left_join(df_roster, by = c("hhid", "indiv")) %>%
  filter(!is.na(s4aq3), !is.na(sexe), !is.na(age)) %>%
  mutate(
    malade        = as.integer(s4aq3 == 1),
    sexe_label    = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
    groupe_age    = cut(age,
                        breaks = c(0, 5, 15, 25, 35, 50, 65, Inf),
                        labels = c("0-4", "5-14", "15-24", "25-34", "35-49", "50-64", "65+"),
                        right  = FALSE),
    maladie_label = as_factor(s4aq3b_1)   # <- ajouté
  ) %>%
  filter(!is.na(groupe_age))

# ============================================
# 3. CALCUL TAUX + IC 95% GLOBAL
# ============================================
ic <- dg %>%
  summarise(
    n       = sum(wt_wave4),
    malades = sum(malade*wt_wave4),
    taux    = malades / n,
    se      = sqrt(taux * (1 - taux) / n),
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = pmin(1, taux + 1.96 * se)
  ) %>%
  mutate(across(c(taux, ic_low, ic_high), ~ . * 100))
# ============================================
# 3. CALCUL TAUX + IC 95% PAR SEXE
# ============================================
ic_sexe <- dg %>%
  group_by(sexe_label) %>%
  summarise(
    n       = sum(wt_wave4),
    malades = sum(malade*wt_wave4),
    taux    = malades / n,
    se      = sqrt(taux * (1 - taux) / n),
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = pmin(1, taux + 1.96 * se)
  ) %>%
  mutate(across(c(taux, ic_low, ic_high), ~ . * 100))

# ============================================
# 4. CALCUL TAUX + IC 95% PAR GROUPE D'ÂGE
# ============================================
ic_age <- dg %>%
  group_by(groupe_age) %>%
  summarise(
    n       = sum(wt_wave4),
    malades = sum(malade*wt_wave4),
    taux    = malades / n,
    se      = sqrt(taux * (1 - taux) / n),
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = pmin(1, taux + 1.96 * se)
  ) %>%
  mutate(across(c(taux, ic_low, ic_high), ~ . * 100))

# ============================================
# 5. CALCUL TAUX + IC 95% PAR SEXE ET GROUPE D'ÂGE
# ============================================
ic_sexe_age <- dg %>%
  group_by(groupe_age, sexe_label) %>%
  summarise(
    n       = sum(wt_wave4),
    malades = sum(malade*wt_wave4),
    taux    = malades / n,
    se      = sqrt(taux * (1 - taux) / n),
    ic_low  = pmax(0, taux - 1.96 * se),
    ic_high = pmin(1, taux + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(across(c(taux, ic_low, ic_high), ~ . * 100))

# ============================================
# 6. BARPLOT PAR SEXE
# ============================================
p1 <- ggplot(ic_sexe, aes(x = sexe_label, y = taux, fill = sexe_label)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux, 1), "%")), vjust = -0.8, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Homme" = "#2196F3", "Femme" = "#E91E63")) +
  scale_y_continuous(limits = c(0, 10), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux de morbidité par sexe(IC à 95%)",
    x        = "Sexe",
    y        = "Taux de morbidité (%)",
    fill     = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    legend.position = "none"
  )

print(p1)
ggsave("output/figures/Taux_morbidité_par_sexe.png", 
       plot = p1,
       width = 8, height = 5, dpi = 300)

# ============================================
# 7. BARPLOT PAR GROUPE D'ÂGE
# ============================================
p2 <- ggplot(ic_age, aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high), width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux, 1), "%")), vjust = -0.8, size = 3.8, fontface = "bold") +
  scale_fill_brewer(palette = "Reds", direction = 1) +
  scale_y_continuous(limits = c(0, 30), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux de morbidité par groupe d'âge(IC à 95%)",
    x        = "Groupe d'âge",
    y        = "Taux de morbidité (%)",
    fill     = "Groupe d'âge"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray50"),
    legend.position = "none"
  )

print(p2)
ggsave("output/figures/Taux_morbidité_par_groupe_age.png", 
       plot = p2,
       width = 8, height = 5, dpi = 300)

# ============================================
# 8. BARPLOT CROISÉ SEXE × GROUPE D'ÂGE
# ============================================

# Forcer l'ordre des groupes d'âge
ic_sexe_age <- ic_sexe_age %>%
  mutate(groupe_age = factor(groupe_age,
                             levels = c("5-14","15-24","25-34","35-49","50-64","65+")))

p3 <- ggplot(ic_sexe_age,
             aes(x = groupe_age, y = taux, fill = sexe_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                position = position_dodge(width = 0.7), width = 0.2, linewidth = 0.7) +
  scale_fill_manual(values = c("Homme" = "#2196F3", "Femme" = "#E91E63")) +
  scale_y_continuous(limits = c(0, 30), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux de morbidité par groupe d'âge et par sexe(IC à 95%)",
    x        = "Groupe d'âge", y = "Taux de morbidité (%)", fill = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray50"),
    legend.position = "bottom"
  )

print(p3)
ggsave("output/figures/Taux_morbidité_croisé.png", 
       plot = p3,
       width = 8, height = 5, dpi = 300)







###QUESTION 2######

 # ============================================
 # 3. DICTIONNAIRE DES MALADIES
 # ============================================
 dict_maladies <- c(
   "1"  = "Paludisme",          "2"  = "Tuberculose",
   "3"  = "Fièvre jaune",       "4"  = "Typhoïde",
   "5"  = "Choléra",            "6"  = "Diarrhée",
   "7"  = "Méningite",          "8"  = "Varicelle",
   "9"  = "Pneumonie",          "10" = "Rhume commun",
   "11" = "Blessure/Trauma",    "12" = "Autre",
   "13" = "Hypertension",       "14" = "Grippe",
   "15" = "Catarrhe",           "16" = "Toux",
   "17" = "Céphalées",          "18" = "Diabète",
   "19" = "Ver de Guinée",      "20" = "Dysenterie",
   "21" = "Gale",               "22" = "Teigne",
   "23" = "Hépatite B",         "24" = "Ulcère/Douleur gastrique",
   "25" = "Problème oculaire",  "26" = "Problème dentaire",
   "27" = "Douleurs corporelles"
 )
 
 dict_categories <- c(
   "1"="Infectieuse",  "2"="Infectieuse",  "3"="Infectieuse",
   "4"="Infectieuse",  "5"="Infectieuse",  "6"="Infectieuse",
   "7"="Infectieuse",  "8"="Infectieuse",  "9"="Infectieuse",
   "10"="Infectieuse", "11"="Traumatique", "12"="Autre",
   "13"="Chronique",   "14"="Infectieuse", "15"="Infectieuse",
   "16"="Infectieuse", "17"="Chronique",   "18"="Chronique",
   "19"="Infectieuse", "20"="Infectieuse", "21"="Infectieuse",
   "22"="Infectieuse", "23"="Infectieuse", "24"="Chronique",
   "25"="Chronique",   "26"="Chronique",   "27"="Chronique"
 )
 
 # ============================================
 # 3. FRÉQUENCES PONDÉRÉES — TOP 10
 # ============================================
 freq_maladies <- DF %>%
   filter(!is.na(s4aq3b_1), !is.na(wt_wave4)) %>%
   mutate(code = as.character(s4aq3b_1)) %>%
   group_by(code) %>%
   summarise(
     effectif      = n(),
     effectif_pond = sum(wt_wave4),
     .groups = "drop"
   ) %>%
   mutate(
     label     = recode(code, !!!dict_maladies,   .default = "Autre"),
     categorie = recode(code, !!!dict_categories, .default = "Autre"),
     pct_pond  = effectif_pond / sum(effectif_pond) * 100
   ) %>%
   arrange(desc(effectif_pond)) %>%
   slice_head(n = 10)
 
 print(freq_maladies)
 
 # ============================================
 # 4. BARPLOT HORIZONTAL PONDÉRÉ
 # ============================================
 couleurs_cat <- c(
   "Infectieuse" = "#E53935",
   "Traumatique" = "#FB8C00",
   "Chronique"   = "#1E88E5",
   "Autre"       = "#757575"
 )
 
 p4<-ggplot(freq_maladies,
        aes(x = pct_pond,
            y = fct_reorder(label, pct_pond),
            fill = categorie)) +
   geom_bar(stat = "identity", width = 0.7) +
   geom_text(aes(label = round(pct_pond, 1)),
             hjust = -0.08, size = 3.5, fontface = "bold") +
   scale_fill_manual(values = couleurs_cat) +
   scale_x_continuous(
     limits = c(0, max(freq_maladies$pct_pond) * 1.3),
     labels = function(x) paste0(x, "%")
   ) +
   labs(
     title    = "Top 10 des affections les plus déclarées (pondéré)",
     x        = "Fréquence relative pondérée (%)",
     y        = NULL,
     fill     = "Catégorie",
     caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
   ) +
   theme_minimal(base_size = 13) +
   theme(
     plot.title         = element_text(face = "bold", hjust = 0.5),
     plot.subtitle      = element_text(hjust = 0.5, color = "gray50"),
     legend.position    = "bottom",
     panel.grid.major.y = element_blank(),
     panel.grid.minor   = element_blank(),
     axis.text.y        = element_text(size = 11)
   )
 
 print(p4)
 ggsave("output/figures/TOP10.png", 
        plot = p4,
        width = 8, height = 5, dpi = 300)
 

 
 ###Question 3###
 library(dplyr)
 library(ggplot2)
 library(scales)
 library(forcats)
 library(survey)
 library(Hmisc)
 
 # ============================================
 # IMPORTER ET PRÉPARER
 # ============================================
 df <- DF %>%
   mutate(
     # Dépense totale de santé
     depense = ifelse(!is.na(s4aq14), s4aq14, 0) +
       ifelse(!is.na(s4aq10), s4aq10, 0),
     
     # Recours aux soins (1=Consulté, 0=Non consulté)
     consulte = ifelse(s4aq4 == 1, "Consulté", "Non consulté"),
     
     # Type de prestataire
     prestataire = recode(as.character(s4aq8),
                          "1" = "Hôpital public",
                          "2" = "Hôpital privé",
                          "3" = "Clinique privée",
                          "4" = "Dispensaire",
                          "5" = "Maternité",
                          "6" = "MCH Post",
                          "7" = "Pharmacie/Chimiste",
                          "8" = "Autre",
                          .default = NA_character_
     ),
     
     # Milieu
     milieu = factor(milieu, levels = c("Urbain", "Rural"))
   )
 
 # Objet sondage pour pondérations
 design <- svydesign(
   ids     = ~psu,
   strata  = ~strata,
   weights = ~wt_wave4,
   data    = df,
   nest    = TRUE
 )
 
 # ============================================
 # A. DÉPENSES DE SANTÉ
 # ============================================
 
 # --- Données dépenses non nulles ---
 df_dep <- df %>%
   filter(!is.na(s4aq14), s4aq14 > 0)
 
 # --- Statistiques par décile ---
 deciles <- wtd.quantile(df_dep$s4aq14,
                         weights = df_dep$wt_wave4,
                         probs   = seq(0, 1, 0.1))
 cat("=== DÉCILES DES DÉPENSES (pondérés) ===\n")
 print(round(deciles, 0))
 
 # --- Valeurs aberrantes (méthode IQR) ---
 Q1  <- wtd.quantile(df_dep$s4aq14, df_dep$wt_wave4, 0.25)
 Q3  <- wtd.quantile(df_dep$s4aq14, df_dep$wt_wave4, 0.75)
 IQR <- Q3 - Q1
 seuil_haut <- Q3 + 1.5 * IQR
 seuil_bas  <- max(0, Q1 - 1.5 * IQR)
 outliers   <- df_dep %>% filter(s4aq14 > seuil_haut)
 
 cat("\n=== VALEURS ABERRANTES ===\n")
 cat("Seuil haut :", round(seuil_haut, 0), "Naira\n")
 cat("Nombre     :", nrow(outliers), "(", round(nrow(outliers)/nrow(df_dep)*100, 1), "%)\n")
 cat("Maximum    :", max(df_dep$s4aq14), "Naira\n")
 
 # --- Histogramme échelle log ---
 med_pond <- wtd.quantile(df_dep$s4aq14, df_dep$wt_wave4, 0.5)
 moy_pond <- sum(df_dep$s4aq14 * df_dep$wt_wave4) / sum(df_dep$wt_wave4)
 
 p_hist <- ggplot(df_dep, aes(x = s4aq14, weight = wt_wave4)) +
   geom_histogram(aes(y = after_stat(density)),
                  bins = 50, fill = "#1E88E5",
                  color = "white", alpha = 0.8) +
   geom_density(aes(weight = wt_wave4),
                color = "#E53935", linewidth = 1) +
   scale_x_log10(labels = label_comma(big.mark = " ")) +
   geom_vline(xintercept = med_pond, linetype = "dashed",
              color = "#FB8C00", linewidth = 1) +
   geom_vline(xintercept = moy_pond, linetype = "dotted",
              color = "#43A047", linewidth = 1) +
   annotate("text", x = med_pond * 0.3, y = Inf, vjust = 2,
            label = paste0("Médiane\n", format(round(med_pond), big.mark=" "), " N"),
            color = "#FB8C00", size = 3.5, fontface = "bold") +
   annotate("text", x = moy_pond * 4, y = Inf, vjust = 2,
            label = paste0("Moyenne\n", format(round(moy_pond), big.mark=" "), " N"),
            color = "#43A047", size = 3.5, fontface = "bold") +
   labs(
     title    = "Distribution des dépenses de santé (échelle log, pondérée)",
     subtitle = "Nigeria GHS-Panel 2018 | Variable : s4aq14 (médicaments)",
     x        = "Dépenses en Naira (log10)",
     y        = "Densité"
   ) +
   theme_minimal(base_size = 13) +
   theme(plot.title    = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5, color = "gray50"))
 
 print(p_hist)
 ggsave("output/figures/Dep_sante_distr.png", 
        plot = p_hist,
        width = 8, height = 5, dpi = 300)
 # --- Boxplot par type de prestataire ---
 df_box <- df %>%
   filter(!is.na(s4aq14), s4aq14 > 0, !is.na(prestataire)) %>%
   group_by(prestataire) %>%
   filter(n() >= 10) %>%
   ungroup() %>%
   mutate(prestataire = fct_reorder(prestataire, s4aq14, .fun = median))
 
 p_box <- ggplot(df_box,
                 aes(x = s4aq14, y = prestataire,
                     fill = prestataire, weight = wt_wave4)) +
   geom_boxplot(outlier.shape = 21, outlier.size  = 1.5,
                outlier.fill  = "#E53935", outlier.color = "#E53935",
                alpha = 0.7) +
   scale_x_log10(labels = label_comma(big.mark = " ")) +
   scale_fill_brewer(palette = "Set2") +
   labs(
     title    = "Dépenses de santé par type de prestataire (pondéré)",
     x        = "Dépenses en Naira (log10)",
     y        = NULL,
     caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
   ) +
   theme_minimal(base_size = 13) +
   theme(plot.title      = element_text(face = "bold", hjust = 0.5),
         plot.subtitle   = element_text(hjust = 0.5, color = "gray50"),
         legend.position = "none",
         panel.grid.major.y = element_blank())
 
 print(p_box)
 ggsave("output/figures/Dep_sante_barplot.png", 
        plot = p_box,
        width = 8, height = 5, dpi = 300)
 
 
 
 
 # B. RECOURS AUX SOINS × QUINTILE

 totcons   <- read_dta("data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/totcons_final.dta")  # consommation ménage
 sapply(totcons, function(x) attr(x, "label"))
 # Joindre totcons au DF (nécessaire pour T17 et T18)
 # totcons apporte : totcons_adj, health31, health32
 DF <- DF %>%
   left_join(
     totcons %>% select(hhid, totcons_adj, totcons_pc, health31, health32, hhsize),
     by = "hhid"
   )
 
 # -----------------------------------------------------------------------------
 # 2. RECODAGES
 # -----------------------------------------------------------------------------
 
 # 2.1 Variable prestataire (5 catégories) à partir de s4aq6a
 #     Logique :
 #       - Aucun         : s4aq1 == 2 (n'a pas consulté)
 #       - Tradipraticien: s4aq6a ∈ {1=Traditional Healer, 9=TBA, 10=Spiritualist}
 #       - Pharmacie/PMV : s4aq6a ∈ {7=Pharmacist, 8=Chemist, 11=PMV}
 #       - Hôpital public: codes médicaux (2,4,5,6,14,15) ET s4aq8 ∈ {1,2,3,4}
 #       - Clinique privée: codes médicaux (2,4,5,6,14,15) ET s4aq8 == 7
 DF <- DF %>%
   mutate(
     prestataire = case_when(
       s4aq1 == 2                                             ~ "Aucun",
       s4aq6a %in% c(1, 9, 10)                               ~ "Tradipraticien",
       s4aq6a %in% c(7, 8, 11)                               ~ "Pharmacie / PMV",
       s4aq6a %in% c(2, 3, 4, 5, 6, 14, 15) &
         s4aq8 %in% c(1, 2, 3, 4, 5, 6)                     ~ "Hôpital public",
       s4aq6a %in% c(2, 3, 4, 5, 6, 14, 15) &
         s4aq8 == 7                                           ~ "Clinique privée",
       # Cas résiduels avec prestataire médical et gestionnaire inconnu
       s4aq6a %in% c(2, 3, 4, 5, 6, 14, 15)                 ~ "Hôpital public",
       TRUE                                                   ~ NA_character_
     ),
     prestataire = factor(prestataire,
                          levels = c("Aucun", "Pharmacie / PMV",
                                     "Hôpital public", "Clinique privée",
                                     "Tradipraticien"))
   )
 
 # 2.2 Dépense de santé totale (4 dernières semaines, individu)
 #     s4aq9  = coût consultation
 #     s4aq14 = médicaments sans ordonnance
 #     s4aq17 = hospitalisation
 #     NA → 0 (pas de dépense pour ce poste)
 DF <- DF %>%
   mutate(
     dep_sante = replace_na(s4aq9,  0) +
       replace_na(s4aq14, 0) +
       replace_na(s4aq17, 0)
   )
 
 # 2.3 Variable binaire consultation
 DF <- DF %>%
   mutate(consulte = as.integer(s4aq1 == 1))
 
 # 2.4 Quintile pondéré de consommation (totcons_adj)
 #     On calcule les seuils avec les poids ménage, puis on affecte chaque ménage
 seuils_q <- wtd.quantile(
   x       = DF$totcons_adj,
   weights = DF$wt_wave4,
   probs   = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm   = TRUE
 )
 
 DF <- DF %>%
   mutate(
     quintile = cut(totcons_adj,
                    breaks         = seuils_q,
                    labels         = c("Q1 (plus pauvres)", "Q2", "Q3",
                                       "Q4", "Q5 (plus riches)"),
                    include.lowest = TRUE),
     quintile = factor(quintile,
                       levels = c("Q1 (plus pauvres)", "Q2", "Q3",
                                  "Q4", "Q5 (plus riches)"))
   )
 
 cat("\nprestataire :\n"); print(table(DF$prestataire, useNA = "ifany"))
 cat("\nquintile :\n");    print(table(DF$quintile,    useNA = "ifany"))
 
 # 3. PLAN D'ENQUÊTE
 design <- DF %>%
   filter(!is.na(wt_wave4)) %>%
   as_survey_design(
     ids     = psu,
     strata  = strata,
     weights = wt_wave4,
     nest    = TRUE
   )
 
 # TÂCHE 16 — Distribution des dépenses de santé
 #            Histogramme log + déciles + boxplot par prestataire + outliers

 # Sous-ensemble : individus ayant effectivement dépensé
 df_dep <- DF %>% filter(dep_sante > 0, !is.na(wt_wave4))
 
 # Statistiques pondérées par décile
 deciles_pond <- wtd.quantile(
   x       = df_dep$dep_sante,
   weights = df_dep$wt_wave4,
   probs   = seq(0.1, 0.9, by = 0.1),
   na.rm   = TRUE
 )
 cat("\nDéciles pondérés des dépenses de santé (₦) :\n")
 print(round(deciles_pond))
 
 # Statistiques résumées pondérées
 cat("\nStatistiques pondérées :\n")
 cat(sprintf("  Médiane    : %s ₦\n", format(round(deciles_pond["50%"]), big.mark=" ")))
 cat(sprintf("  Moyenne pondérée : %s ₦\n",
             format(round(wtd.mean(df_dep$dep_sante, df_dep$wt_wave4)), big.mark=" ")))
 
 # A — Histogramme en échelle log
 p16a <- df_dep %>%
   ggplot(aes(x = dep_sante, weight = wt_wave4)) +
   geom_histogram(bins = 40, fill = "#2196F3", color = "white",
                  linewidth = 0.2, alpha = 0.85) +
   geom_vline(xintercept = deciles_pond["50%"],
              color = "red", linetype = "dashed", linewidth = 0.9) +
   annotate("text",
            x = deciles_pond["50%"] * 1.15,
            y = Inf, vjust = 1.5,
            label = paste0("Médiane = ", format(round(deciles_pond["50%"]), big.mark=" "), " ₦"),
            color = "red", size = 3.5) +
   scale_x_log10(labels = label_comma(suffix = " ₦")) +
   scale_y_continuous(labels = label_comma()) +
   labs(
     title    = "Distribution des dépenses de santé (individus ayant dépensé)",
     subtitle = "Échelle logarithmique — Effectifs pondérés",
     x = "Dépenses de santé (₦, log)",
     y = "Effectif pondéré",
     caption  = "Inclut : consultation + médicaments OTC + hospitalisation. Source : GHS-Panel W4."
   ) +
   theme_minimal(base_size = 12) +
   theme(plot.title = element_text(face = "bold"))
 
 print(p16a)
 ggsave("output/figures/histo_dep_sante.png", p16a, width = 9, height = 5, dpi = 300)
 
 # B — Tableau déciles
 tbl_deciles <- data.frame(
   Décile = paste0(seq(10, 90, 10), "%"),
   `Dépense (₦)` = format(round(deciles_pond), big.mark = " ")
 )
 cat("\nTableau des déciles pondérés :\n")
 print(tbl_deciles, row.names = FALSE)
 
 # C — Boxplot par prestataire (échelle log)
 df_dep_prest <- df_dep %>%
   filter(!is.na(prestataire), prestataire != "Aucun", dep_sante > 0)
 
 p16b <- df_dep_prest %>%
   mutate(prestataire = fct_reorder(prestataire,
                                    dep_sante,
                                    .fun = function(x) median(x, na.rm = TRUE))) %>%
   ggplot(aes(x = prestataire, y = dep_sante, fill = prestataire,
              weight = wt_wave4)) +
   geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1.2,
                outlier.alpha = 0.3) +
   scale_y_log10(labels = label_comma(suffix = " ₦")) +
   scale_fill_viridis_d(option = "C", guide = "none") +
   coord_flip() +
   labs(
     title    = "Dépenses de santé par type de prestataire",
     subtitle = "Échelle logarithmique — Pondération wt_wave4",
     x = NULL,
     y = "Dépenses de santé (₦, log)",
     caption  = "Source : GHS-Panel W4 Nigeria."
   ) +
   theme_minimal(base_size = 12) +
   theme(
     plot.title         = element_text(face = "bold"),
     panel.grid.major.y = element_blank()
   )
 
 print(p16b)
 ggsave("fig_t16b_boxplot_dep_prestataire.png", p16b, width = 9, height = 5, dpi = 300)
 
 # D — Identification des valeurs aberrantes (Q3 + 3×IQR)
 q1_dep  <- quantile(df_dep$dep_sante, 0.25, na.rm = TRUE)
 q3_dep  <- quantile(df_dep$dep_sante, 0.75, na.rm = TRUE)
 iqr_dep <- q3_dep - q1_dep
 seuil_outlier <- q3_dep + 3 * iqr_dep
 
 outliers <- df_dep %>%
   filter(dep_sante > seuil_outlier) %>%
   select(hhid, indiv, dep_sante, s4aq9, s4aq14, s4aq17,
          prestataire, milieu, wt_wave4) %>%
   arrange(desc(dep_sante))
 
 cat(sprintf("\nSeuil outliers (Q3 + 3×IQR) : %s ₦\n",
             format(round(seuil_outlier), big.mark = " ")))
 cat(sprintf("Nombre de valeurs aberrantes : %d\n", nrow(outliers)))
 cat("\n10 observations les plus extrêmes :\n")
 print(head(outliers, 10))
 
 # TÂCHE 17 — Quintile de consommation × recours aux soins
 #            Chi-deux Rao-Scott + Fisher si effectif < 5 + V Cramér + barplot

 df_t17 <- DF %>%
   filter(!is.na(wt_wave4), !is.na(quintile), !is.na(consulte)) %>%
   mutate(
     consulte_fct = factor(consulte,
                           levels = c(1, 0),
                           labels = c("Consulté", "Non consulté"))
   )
 
 # Un seul design, utilisé partout
 # (srvyr pour survey_mean/survey_total, converti en svydesign pour svychisq)
 design_t17 <- df_t17 %>%
   as_survey_design(ids = psu, strata = strata, weights = wt_wave4, nest = TRUE)
 
 design_sv_t17 <- svydesign(
   ids     = ~psu,
   strata  = ~strata,
   weights = ~wt_wave4,
   nest    = TRUE,
   data    = df_t17   # MÊME sous-ensemble que design_t17
 )
 
 # ── Tableau de contingence pondéré ──────────────────────────────────────────
 tab_q17 <- design_t17 %>%
   group_by(quintile, consulte_fct) %>%
   summarise(n_pond = survey_total(vartype = NULL))
 
 cat("\nTableau de contingence pondéré (effectifs en milliers) :\n")
 
 tab_wide <- tab_q17 %>%
   select(quintile, consulte_fct, n_pond) %>%          # colonnes exactes seulement
   pivot_wider(names_from = consulte_fct,
               values_from = n_pond) %>%
   mutate(
     Total = Consulté + `Non consulté`,
     `Taux recours (%)` = round(Consulté / Total * 100, 1),
     across(c(Consulté, `Non consulté`, Total), ~ round(. / 1000, 1))
   )
 print(tab_wide)
 
 # ── Taux pondérés par quintile ───────────────────────────────────────────────
 taux_q17 <- design_t17 %>%
   group_by(quintile) %>%
   summarise(
     taux_recours     = survey_mean(consulte, vartype = "ci"),
     n_pond           = survey_total(vartype = NULL)
   )
 cat("\nTaux de recours aux soins par quintile (pondéré) :\n")
 print(taux_q17)
 
 # ── Test chi-deux de Rao-Scott ───────────────────────────────────────────────
 chi2_t17 <- svychisq(~ consulte_fct + quintile,
                      design    = design_sv_t17,
                      statistic = "Chisq")
 cat("\nTest chi-deux de Rao-Scott :\n")
 print(chi2_t17)
 
 # ── Test exact de Fisher si effectif observé < 5 ────────────────────────────
 mat_obs  <- xtabs(~ consulte_fct + quintile, data = df_t17)
 min_cell <- min(mat_obs)
 cat(sprintf("\nEffectif minimal observé : %d\n", min_cell))
 
 if (min_cell < 5) {
   cat("→ Effectif < 5 : test exact de Fisher appliqué\n")
   fisher_t17 <- fisher.test(mat_obs, simulate.p.value = TRUE, B = 10000)
   print(fisher_t17)
 } else {
   cat("→ Tous les effectifs ≥ 5 : test de Rao-Scott suffisant\n")
 }
 
 # ── V de Cramér sur effectifs PONDÉRÉS ──────────────────────────────────────
 mat_pond <- tab_q17 %>%
   select(quintile, consulte_fct, n_pond) %>%
   pivot_wider(
     names_from = consulte_fct,
     values_from = n_pond,
     values_fill = 0
   )
 
 # vérifier structure
 str(mat_pond)
 
 # enlever quintile correctement
 mat_pond_num <- mat_pond %>%
   select(-quintile) %>%
   mutate(across(everything(), as.numeric)) %>%
   as.matrix()
 mat_pond <- tab_q17 %>%
   select(quintile, consulte_fct, n_pond) %>%
   pivot_wider(
     names_from = consulte_fct,
     values_from = n_pond,
     values_fill = 0
   )
 
 # vérifier structure
 str(mat_pond)
 
 # enlever quintile correctement
 mat_pond_num <- mat_pond %>%
   ungroup() %>%  
   select(-quintile) %>%
   mutate(across(everything(), as.numeric)) %>%
   as.matrix()
 rownames(mat_pond_num) <- mat_pond$quintile
 
 chi2_pond <- chisq.test( mat_pond_num)$statistic
 
 k_min <- min(nrow( mat_pond_num), ncol( mat_pond_num)) - 1
 
 n_total <- sum( mat_pond_num)
 
 v_cramer <- sqrt(chi2_pond / (n_total * k_min))
 cat(sprintf("\nV de Cramér (effectifs pondérés) : %.4f\n", v_cramer))
 cat("(< 0.1 = faible | 0.1–0.3 = modéré | > 0.3 = fort)\n")
 
 # ── Barplot 100% empilé CORRIGÉ ──────────────────────────────────────────────

 plot_data <- taux_q17 %>%
   transmute(
     quintile,
     Consulté       = taux_recours,
     `Non consulté` = 1 - taux_recours
   ) %>%
   pivot_longer(cols      = c(Consulté, `Non consulté`),
                names_to  = "statut",
                values_to = "prop") %>%
   mutate(statut = factor(statut,
                          levels = c("Non consulté", "Consulté")))
 
 p17 <- plot_data %>%
   ggplot(aes(x = quintile, y = prop, fill = statut)) +
   geom_col(position = "stack",    # position stack, y = prop réelle → somme = 1
            width = 0.7,
            color = "white", linewidth = 0.3) +
   geom_text(
     aes(label = ifelse(prop > 0.04,
                        paste0(round(prop * 100, 1), "%"), "")),
     position  = position_stack(vjust = 0.5),  # centré dans chaque segment
     fontface  = "bold", size = 3.5, color = "white"
   ) +
   scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
   scale_fill_manual(values = c("#BDBDBD", "#1976D2"),
                     name   = "Statut de recours") +
   labs(
     title    = "Recours aux soins selon le quintile de consommation",
     subtitle = sprintf("Chi-deux Rao-Scott p = %.4f | V de Cramér (pondéré) = %.3f",
                        chi2_t17$p.value, v_cramer),
     x        = "Quintile de consommation",
     y        = "Proportion pondérée (%)",
     caption  = "Estimations pondérées (wt_wave4). Source : GHS-Panel W4 Nigeria."
   ) +
   theme_minimal(base_size = 12) +
   theme(
     plot.title         = element_text(face = "bold"),
     legend.position    = "top",
     panel.grid.major.x = element_blank()
   )
 
 print(p17)
 ggsave("fig_t17_recours_quintile.png", p17, width = 9, height = 5.5, dpi = 300)

 # ── Tableau gtsummary ────────────────────────────────────────────────────────
 # BUG 1 CORRIGÉ : on passe consulte_fct (factor) et non consulte (numérique)
 tbl_t17 <- design_t17 %>%
   tbl_svysummary(
     include   = consulte_fct,
     by        = quintile,
     statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
     label     = consulte_fct ~ "A consulté un praticien (4 sem.)"
   ) %>%
   add_overall() %>%
   add_p(test = list(all_categorical() ~ "svy.chisq.test")) %>%
   bold_labels() %>%
   modify_caption("**Tableau. Recours aux soins × Quintile de consommation**")
 
 print(tbl_t17)
 
 # TÂCHE 18 — Dépenses médianes Urbain vs Rural
 #            Test de Wilcoxon pondéré (svyranktest) + Violin + Boxplot

 # Filtrer sur les dépensants (dep_sante > 0)
 df_t18 <- DF %>%
   filter(dep_sante > 0, !is.na(wt_wave4), !is.na(milieu)) %>%
   mutate(milieu = factor(milieu, levels = c("Urbain", "Rural")))
 
 design_sv_t18 <- svydesign(
   ids     = ~psu,
   strata  = ~strata,
   weights = ~wt_wave4,
   nest    = TRUE,
   data    = df_t18
 )
 
 # Médianes pondérées par milieu
 med_urbain <- svyquantile(~dep_sante,
                           subset(design_sv_t18, milieu == "Urbain"),
                           quantiles = 0.5, ci = TRUE)
 med_rural  <- svyquantile(~dep_sante,
                           subset(design_sv_t18, milieu == "Rural"),
                           quantiles = 0.5, ci = TRUE)
 
 cat("\nMédiane pondérée Urbain :\n"); print(med_urbain)
 cat("\nMédiane pondérée Rural  :\n"); print(med_rural)
 
 # Test de Wilcoxon-Mann-Whitney pondéré (svyranktest)
 wilcox_t18 <- svyranktest(dep_sante ~ milieu,
                           design = design_sv_t18,
                           test   = "wilcoxon")
 cat("\nTest de Wilcoxon pondéré (svyranktest) :\n")
 print(wilcox_t18)
 
 # Annotation pour le graphique
 p_val_label <- ifelse(wilcox_t18$p.value < 0.001,
                       "p < 0.001",
                       paste0("p = ", round(wilcox_t18$p.value, 4)))
 
 get_median <- function(x) {
   val <- x[, "quantile", drop = TRUE]
   
   if (!is.numeric(val)) val <- as.numeric(val)
   if (length(val) != 1) stop("Problème : médiane non unique")
   
   return(val)
 }
 
 med_labels <- data.frame(
   milieu = factor(c("Urbain", "Rural"), levels = c("Urbain", "Rural")),
   med_pond = c(
     get_median(med_urbain$dep_sante),
     get_median(med_rural$dep_sante)
   )
 )
 
 # Violin + Boxplot superposé
 p18 <- df_t18 %>%
   ggplot(aes(x = milieu, y = dep_sante, fill = milieu,
              weight = wt_wave4)) +
   geom_violin(alpha = 0.6, trim = TRUE, color = NA) +
   geom_boxplot(width = 0.15, alpha = 0.9,
                outlier.shape = 21, outlier.size = 1,
                outlier.alpha = 0.25,
                color = "grey20") +
   # Médiane pondérée (point rouge)
   geom_point(data = med_labels,
              aes(x = milieu, y = med_pond),
              color = "red", size = 3.5, shape = 18,
              inherit.aes = FALSE) +
   geom_text(data = med_labels,
             aes(x = milieu, y = med_pond,
                 label = paste0("Méd. pond. = \n",
                                format(round(med_pond), big.mark = " "), " ₦")),
             hjust = -0.15, size = 3.2, color = "red",
             inherit.aes = FALSE) +
   scale_y_log10(labels = label_comma(suffix = " ₦")) +
   scale_fill_manual(values = c("#1976D2", "#43A047"), guide = "none") +
   annotate("text",
            x = 1.5, y = max(df_t18$dep_sante, na.rm = TRUE) * 0.5,
            label = paste0("Test Wilcoxon pondéré\n", p_val_label),
            size = 3.5, color = "grey30", fontface = "italic") +
   labs(
     title    = "Dépenses de santé : Urbain vs Rural",
     subtitle = "Individus ayant effectué des dépenses — Échelle logarithmique",
     x = NULL,
     y = "Dépenses de santé (₦, log)",
     caption  = paste0("Losange rouge = médiane pondérée (svyquantile).\n",
                       "Test de Wilcoxon pondéré (svyranktest). Source : GHS-Panel W4 Nigeria.")
   ) +
   theme_minimal(base_size = 12) +
   theme(
     plot.title = element_text(face = "bold"),
     panel.grid.major.x = element_blank()
   )
 
 print(p18)
 ggsave("fig_t18_dep_urbain_rural.png", p18, width = 8, height = 6, dpi = 300)

 