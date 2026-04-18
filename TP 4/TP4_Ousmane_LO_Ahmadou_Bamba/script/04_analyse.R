######################## Question 19 (suite). ######################################
############################################################################

### Informations sur les ménage : nombre de parcelle par ménage et superficie agricole de chaque ménage

menage_parcelle <- sect11a1_p4 |>
    group_by(hhid) |>
    summarise(
        nombre_parcelle = n_distinct(plotid , na.rm = TRUE) , 
        superficie_totale_menage = sum(superficie_ha  , na.rm = TRUE) 

    ) 
sect11a1_p4 <-  sect11a1_p4 |>
    left_join(menage_parcelle , by = "hhid")

## Valeurs manquantes et valeurs aberrantes pour les superficie agricoles
valeur_manquantes <- sum(is.na(sect11a1_p4$superficie_totale_menage))
valeur_manquantes
## valeurs aberrantes
valeur_aberrantes <- sect11a1_p4|>
    filter(
        superficie_totale_menage > 500 | superficie_totale_menage  < 0
    ) |> 
    select(superficie_totale_menage) |>
    count()

valeur_aberrantes

######################## Question 20. ########################################################
########## Analyse uni-varié de la superficie totale par ménage et par parcelle ###############
##############################################################################################

## Histogramme de la distribution agricole des ménage
hist_superficie_menage <- ggplot(
    sect11a1_p4 , aes( x = superficie_totale_menage , weight = wt_wave4 )) +
    geom_histogram( 
        fill = "steelblue" , color ="black", bins = 50 , alpha = 0.6)+
    scale_x_log10(
        breaks = c(0.1, 1, 10, 100, 500), 
        labels = c("0.1", "1", "10", "100", "500")
    )+
    labs(
        title="Distribution de la superficie agricole totale des ménages",
         y = "Nombre de ménage", 
        x = "superficie agricole en hectare (log10)" 
    ) +
    theme(
        panel.grid.minor=element_blank() , 
        panel.grid.major.x = element_blank() ,
        axis.title = element_text(face  ="bold") , 
        plot.title = element_text(hjust = 0.5, face = "italic" , size = 12)
        
    )
hist_superficie_menage
## boxplot des superficies agricoles des ménages
p2 <- ggplot(sect11a1_p4 , aes (y = superficie_totale_menage , weight = wt_wave4)) +
    geom_boxplot(fill = "steelblue")+
    scale_y_log10()+
    labs(title= "D. superficie totale", y  = " superficie en ha (log10)") +
    theme(
        plot.title= element_text(face = "italic", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )

save(hist_superficie_menage , file = "output/figures/hist_superficie_menage.RDATA")

## Distribution de la superficie agricole par parcelle
hist_superficie_parcelle <- ggplot(
    sect11a1_p4 , aes( x = superficie_ha , weight = wt_wave4)) +
    geom_histogram(
        fill = "skyblue" , color ="black", bins = 50 , alpha = 0.6)+
    scale_x_log10(
        breaks = c(0.1, 1, 10, 100, 500), 
        labels = c("0.1", "1", "10", "100", "500")
    )+
    labs(
        title="Distribution des superficies des parcelles agricoles" ,
        y = "Nombre de parcelle", 
        x = "superficie agricole (ha log10)" 
    ) +
    theme(
        panel.grid.minor=element_blank() , 
        panel.grid.major.x = element_blank() ,
        axis.title = element_text(face  ="bold") , 
        plot.title = element_text(hjust = 0.5, face = "italic", size = 12)
        
    )
hist_superficie_parcelle

##  boxplot des superficies des parcelles agricoles
p1 <- ggplot(sect11a1_p4 , aes(y= superficie_ha , weight = wt_wave4))+
    geom_boxplot(fill = "skyblue")+
    scale_y_log10()+
    labs( title="D. superficie des parcelles ", y = "superficie en ha (log10)") +
    theme(
        plot.title= element_text(face = "italic" , size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )

save(hist_superficie_parcelle , file = "output/figures/hist_superficie_parcelle.RDATA")

boxplot_mp <- p1+p2 # on utilise la bibliothèque patchwork pour rassembler les deux graphiques
boxplot_mp
save(boxplot_mp , file = "output/figures/boxplot_MP.RDATA")


## Statistique descriptive par décile : déciles de la superficie agricole des ménages
decile_parcelle_menage <- quantile( 
    sect11a1_p4$superficie_totale_menage ,
    probs = seq(0,1,0.1), #division en dix sous parties
    na.rm=TRUE
)
decile_parcelle_menage

## Comparaison superficie déclaré et GPS
sect11a1_p4$superficie_gps_ha  <- sect11a1_p4$superficie_gps_m2 / 10000

plot_SG <- ggplot(sect11a1_p4, aes(x = superficie_ha, y = superficie_gps_ha ,weigth = wt_wave4)) +
    geom_point(alpha = 0.5)+ 
    scale_x_log10(
        breaks = c(0.01 , 0.1 , 1 , 10 ,100 , 500) , 
        labels = c("0.01" , "0.1", "1", "10", "100", 500)
    )+
    scale_y_log10(
        breaks = c(0.01 , 0.1 , 1 , 10 ,100 , 500) , 
        labels = c("0.01" , "0.1", "1", "10", "100", "500")
        )+
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
        title = "Superficie déclarée vs GPS",
        x = "Superficie déclarée (ha)",
        y = "Superficie GPS (ha)"
    ) +
    theme(
        panel.grid = element_blank(), 
        axis.title = element_text(face = "bold")
    )
plot_SG
save(plot_SG , file = "output/figures/plot_SG.RDATA")

## Test de correlation de sperman
corel_spearman <- cor.test(sect11a1_p4$superficie_ha ,
                           sect11a1_p4$superficie_gps_ha ,
                           method = "spearman", use = "complete.obs")
corel_spearman

#============================              ============================================
# Question 21. Analyse du régime de tenure et test d'indépendance entre Rual et Urbain
#============================              ===================================================
## Analyse du régime de tenure 
## le type de tenure correspond à la variable s11b1q4 de la base sect11b_plantingw4 
## Fréquence et proportion des modalité 

tenure_freq <- sect11b1_p4  |> filter(!is.na(tenure) , !is.na(secteur) , !is.na(wt_wave4)) |> # on enlève d'abord les na
    count(tenure) |>
    mutate(prop = n/sum(n) * 100 ) |> ##  renommage : nom de la variable et les modalités et calcul  ) |>
    arrange(desc(prop)) |> # ranger par ordre décroissant les proportions
    mutate(tenure = factor(tenure , levels = unique(tenure)))

## représentation graphique (diagramme en barre horizontal)

bar_regime_tenure   <- ggplot( tenure_freq , aes(x = prop , y = tenure )) +
    geom_bar(stat = "identity", fill ="steelblue")+
    geom_text(aes( label= paste0(round(prop,1), "%")) , color = "saddlebrown", hjust = -0.1 , size= 3.5) +
    labs( y = "regime de tenure", x = "proportion (%)", title = "Distribution du régime de tenure")+
    theme(axis.title.x = element_blank(),   
          axis.ticks.x = element_blank(),
          panel.grid= element_blank())
bar_regime_tenure
save(bar_regime_tenure, file = "output/figures/bar_regime_tenure.RDATA")

## test de Khi deux entre milieu de résidence et régime de tenure
K2_test_secteur_tenure <- chisq.test(table(sect11b1_p4$secteur,
                                           sect11b1_p4$tenure))

K2_test_secteur_tenure 

#===================================             ===================================================
# Question 23. Analyse de la relation entre la superficie totale du ménage et le nombre de parcelle
#===================================             ===================================================

### Corelation de Spearman avec IC 95% entre superficie et nombre de parcelles
corel_spearma_SN <- cor.test(sect11a1_p4$nombre_parcelle ,
                             sect11a1_p4$superficie_totale_menage,
                             method = "spearman")

corel_spearma_SN 

##  Représentation graphique (scatter plot) : relation entre la superficie agricole du ménage et le nombre de parcelle
geom_point_SN <- ggplot(  sect11a1_p4 , 
                          aes( x = nombre_parcelle , 
                               y = superficie_totale_menage , 
                               weigth = wt_wave4
                          )
)+
    geom_point(alpha = 0.5 , color = "steelblue")+
    geom_smooth(method = "loess" , se = TRUE , color = "orange") +
    scale_y_log10(
        breaks = c(0.01 , 0.1 , 1, 10 , 100 , 500) , 
        labels = c("O.01", "0.1", "1", "10", "100" ,"500")
    )+
    scale_x_continuous(
        breaks = 1:13 , 
        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13")
    )+
    labs( x = "Nombre de parcelles du ménage",
          y ="Superficie agricole totale du ménage",
          title = "Superficie agricole vs Nombre de parcelle ")+
    theme(
        panel.grid = element_blank() ,
        axis.title= element_text(face = "bold") ,
        plot.title=element_text(face = "italic", size = 12)
    )

geom_point_SN
save(geom_point_SN , file = "output/figures/geom_point_SN.RDATA")

#============================             ========================================================
# Question 24. Analyse de la relation entre la superficie totale du ménage et le nombre de parcelle
#============================             ========================================================

# comme les données sont issus d'un seul vague, nous analysons les disparité géographique

# Plan d'enqête avec les poids
library(survey)
data_design  <- sect11a1_p4 |> filter(
    !is.na(wt_wave4)
)
design <- svydesign(ids = ~1, data = data_design , weights = ~wt_wave4)

# Calcul de la médiane pondérée par Etat
median_state <- svyby(
    ~superficie_totale_menage,
    ~state,
    design,
    svyquantile,
    quantiles = 0.5,
    ci = FALSE , 
    keep.var= FALSE
)

median_state <- median_state |> mutate(vague = "Vague 4" ,
                                       state_label = as_factor(state)
                                       )

# Heatmap
geom_heatmap <- ggplot(median_state, aes(x = vague, y = state_label, fill = statistic)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(statistic, 1)), color = "black", size = 3) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Superficie médiane des parcelle argicoles par État (Vague 4)",
         x = "Vague",
         y = "État",
         fill = "Superficie médiane (ha)") +
    theme_minimal() +
    theme(axis.text.x = element_text(face = "bold"),
          plot.title = element_text(face = "italic", size = 12))

geom_heatmap
save(geom_heatmap, file = "output/figures/heatmap.RDATA")

## Résumé des différentes tests statistiques
# Données des tests
tests_tbl <- tibble::tibble(
    Variable = c("Superficie déclarée vs GPS",
                 "Milieu de résidence × régime de tenure",
                 "Superficie totale × nombre de parcelles"),
    Test = c("Corrélation Spearman",
             "Khi-deux",
             "Corrélation Spearman"),
    Résultat = c(
        paste0("ρ = ", round(corel_spearman$estimate, 3),
               ", p = ", signif(corel_spearman$p.value, 3)),
        paste0("χ² = ", round(K2_test_secteur_tenure$statistic, 1),
               ", df = ", K2_test_secteur_tenure$parameter,
               ", p = ", signif(K2_test_secteur_tenure$p.value, 3)),
        paste0("ρ = ", round(corel_spearma_SN$estimate, 3),
               ", p = ", signif(corel_spearma_SN$p.value, 3))
    )
)

# Mise en forme avec flextable

library(flextable)
table_test <- flextable(tests_tbl) |>
    set_header_labels(
        Variable = "Variable analysée",
        Test = "Test utilisé",
        Résultat = "Résultat du test"
    ) |>
    add_header_row(values = "Résumé des tests statistiques", colwidths = 3) |>
    theme_booktabs() |>
    autofit()
save(table_test , file = "output/tables/table_test.RDATA")

## Tableaux de conversion en hectare

# Tableau 1 : Facteurs généraux de conversion en hectares
general_conv <- tibble::tibble(
    Zone = c("Tous", "Tous", "Tous", "Tous"),
    Unité = c("Plots", "Acres", "Hectares", "Mètres carrés"),
    Facteur_conversion = c(0.0667, 0.4, 1, 0.0001)
)

table_conv <- flextable(general_conv) |>
    set_header_labels(
        Zone = "Zone",
        Unité = "Unité",
        Facteur_conversion = "Facteur de conversion en hectares"
    ) |>
    add_header_row(values = "Facteurs généraux de conversion en hectares", colwidths = 3) |>
    theme_booktabs() |>
    autofit()
table_conv
save(table_conv , file = "output/tables/table_conv.RDATA")

# Tableau 2 : Facteurs spécifiques par zone
zone_conv <- tibble::tibble(
    Zone = c("North Central (1)", "North East (2)", "North West (3)",
             "South East (4)", "South South (5)", "South East (6)"),
    Heaps = c(0.00012, 0.00016, 0.00011, 0.00019, 0.00021, 0.00012),
    Ridges = c(0.0027, 0.004, 0.00494, 0.0023, 0.0023, 0.00001),
    Stands = c(0.00006, 0.00016, 0.00004, 0.00004, 0.00013, 0.00041)
)

table_conv_s <- flextable(zone_conv) |>
    set_header_labels(
        Zone = "Zone",
        Heaps = "Heaps",
        Ridges = "Ridges",
        Stands = "Stands"
    ) |>
    add_header_row(values = "Facteurs spécifiques de conversion en hectares", colwidths = 4) |>
    theme_booktabs() |>
    autofit()
table_conv_s
save(table_conv_s , file = "output/tables/table_conv_s.RDATA")

