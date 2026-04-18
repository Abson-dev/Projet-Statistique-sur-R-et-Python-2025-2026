#Importations des bibliothèques
library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(tibble)
library(tidyverse)
library(rstatix)
library(boot)
#Détermination des couleurs et codage de la fonction theme_tp4 pour l'harmonisation des couleurs et des graphiques
palette_secteur <- c("Urbain" = "#C0572B", "Rural" = "#5B7A52")
COL_ACCENT <- "#E8A045"   
COL_FOND <- "#FAF7F2"   
COL_GRILLE <- "#E8E2D9" 

theme_tp4 = function(base_size=12){
  theme_minimal(base_size = base_size)  %+replace% 
  theme(plot.background = element_rect(fill = COL_FOND, color = NA ),
        panel.background = element_rect(fill = COL_FOND, color = NA ),
        panel.grid.major = element_line(color = COL_GRILLE , linewidth = 0.45 ),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face= "bold" , size = base_size + 1 , color = "#2E2319", margin = margin(b= 4) ),
        axis.text = element_text(face = "bold" , size = base_size - 1, color = ),
        plot.title = element_text(face = "bold", size = base_size + 1, color = ),
        plot.caption = element_text(color = "#9C8B7A", size = base_size - 3,
                                    hjust = 0, margin = margin(t = 6)),
        plot.subtitle = element_text(color = "#6B5C4A", size = base_size - 1,
                                     margin = margin(b = 8)),
        legend.background = element_rect(fill = COL_FOND, color = NA),
        legend.key         = element_rect(fill = COL_FOND, color = NA),
        strip.background   = element_rect(fill = "#EDE6DC", color = NA),
        strip.text         = element_text(face = "bold", color = "#2E2319")
        )
}
#-----------------------------------------------------
#Importations des bases
#-----------------------------------------------------

#Importations des bases pour le traitement

base_parcelle = readRDS("data/processed/base_parcelle.rds")
exploitation_par_menage = readRDS("data/processed/exploitation_menages.rds")

#Importation de la base pour les pondérations 

sectiona = read_dta("data/row/secta_harvestw4.dta")

#extraction des pondérations de la base sectiona

ponderation = sectiona %>%
  select("hhid", "wt_wave4") %>%
  distinct(hhid, .keep_all = TRUE)

base_parcelle = base_parcelle %>%
  left_join(ponderation, by = "hhid")

exploitation_par_menage = exploitation_par_menage %>%
  left_join(ponderation, by = "hhid")

#-------------------------------------------------------------
#Analyse univariée de la superficie par parcelle et par ménage
#-------------------------------------------------------------

#Analyse univariée de la superficie par parcelle

stats_parcelle = base_parcelle %>%
  summarise(moyenne = round(mean(aire_hectare, na.rm = TRUE),2),
            mediane = round(median(aire_hectare, na.rm = TRUE),2),
            ecart_type = round(sd(aire_hectare, na.rm = TRUE), 2),
            maximum = max(aire_hectare, na.rm = TRUE),
            minimum = min(aire_hectare, na.rm = TRUE),
            D1 = round(quantile(aire_hectare, 0.1, na.rm = TRUE), 2),
            D2 = round(quantile(aire_hectare, 0.2, na.rm = TRUE), 2),
            D3 = round(quantile(aire_hectare, 0.3, na.rm = TRUE), 2),
            D4 = round(quantile(aire_hectare, 0.4, na.rm = TRUE), 2),
            D5 = round(quantile(aire_hectare, 0.5, na.rm = TRUE), 2),
            D6 = round(quantile(aire_hectare, 0.6, na.rm = TRUE), 2),
            D7 = round(quantile(aire_hectare, 0.7, na.rm = TRUE), 2),
            D8 = round(quantile(aire_hectare, 0.8, na.rm = TRUE), 2),
            D9 = round(quantile(aire_hectare, 0.9, na.rm = TRUE), 2)
)
cat("Les statistiques descriptives concernant la variable superficie par parcelle sont : \n"); print(stats_parcelle)

#Construction de l'histogramme

#Calcul du seuil pour le tri des données

seuil = quantile(base_parcelle$aire_hectare, probs = 0.99, na.rm = TRUE)

#Base de données pour les graphiques

base_plot = base_parcelle %>%
  filter(!is.na(aire_hectare), aire_hectare>0, aire_hectare <= seuil)

k = nclass.FD(log10(base_plot$aire_hectare)) #Calcul du nombre de classe avec la méthode de Freedman-Diaconis
#L'histogramme

hhist_parcelle = ggplot(data = base_plot, aes(x = log10((aire_hectare))))+
  geom_histogram(bins = k, color = "white", fill = "#2C6FAC" ,alpha = 0.8) +
  scale_x_continuous(labels = function(x) round(10^x,3))+
  labs(title = "Histogramme de la superficie par parcelle (Echelle log)",
       x = "Superficie",
       y = "Nombre de parcelle",
       caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)")+
  theme_tp4(base_size = 14)
cat("L'histogramme de la variable superficie par parcelles \n"); print(hhist_parcelle)
ggsave(filename = "outputs/fig03_hist_parcelle.png", plot = hhist_parcelle, height = 8, width = 10, dpi = 150)

#La construction du boxplot

boxplot_parcelle = ggplot(data = base_plot, aes(y = aire_hectare))+
  geom_boxplot(fill ="#C97C5D", color ="#2E2319" , alpha = 0.8 , width = 0.4 )+
  scale_x_continuous(limits = c(-1,1), breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 4,0.5))+
  labs(
    title   = "Boxplot de la superficie par parcelle",
    x       = "",
    y       = "Superficie (hectares)",
    caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +
  theme_tp4(base_size = 14)+
  theme(panel.grid= element_blank(),
        axis.ticks.y = element_blank(),   
        axis.line.y = element_blank() )
ggsave(filename = "outputs/fig04_boxplot_parcelle.png", plot = boxplot_parcelle, height = 8, width = 10, dpi = 150)
cat("Le boxplot de la variable superficie par parcelle : \n"); print(boxplot_parcelle)

#Analyse univariée de la superficie par ménage

stats_menage = exploitation_par_menage %>% 
  summarise(moyenne = round(mean(aire, na.rm = TRUE), 2),
            mediane = round(median(aire, na.rm = TRUE), 2),
            ecart_type = round(sd(aire, na.rm = TRUE), 2),
            maximum = max(aire, na.rm = TRUE),
            minimum = min(aire, na.rm = TRUE),
            D1      = quantile(aire, probs = 0.1, na.rm = TRUE),
            D2      = quantile(aire, probs = 0.2, na.rm = TRUE),
            D3      = quantile(aire, probs = 0.3, na.rm = TRUE),
            D4      = quantile(aire, probs = 0.4, na.rm = TRUE),
            D5      = quantile(aire, probs = 0.5, na.rm = TRUE),
            D6      = quantile(aire, probs = 0.6, na.rm = TRUE),
            D7      = quantile(aire, probs = 0.7, na.rm = TRUE),
            D8      = quantile(aire, probs = 0.8, na.rm = TRUE),
            D9      = quantile(aire, probs = 0.9, na.rm = TRUE)
)
cat("Les statistiques descriptives concernant la variable superficie par ménages : \n"); print(stats_menage)

#Construction de l'histogramme

#Calcul d'un seuil de tri des données

seuil2 = quantile(x = exploitation_par_menage$aire, probs = 0.99, na.rm = TRUE)

#Bases de données pour les graphiques

base2 = exploitation_par_menage %>%
  filter(!is.na(aire), aire>0, aire <= seuil2)

k2 = nclass.FD(log10(base2$aire)) #Calcul du nombre de classe pour l'histogramme par la méthode de Freedman

hhist_menage = ggplot(data = base2, aes(x = aire)) +
  geom_histogram(bins = k2, color = "white", fill = "#2E2319" ,alpha = 0.8)+
  scale_x_log10(labels = scales::label_log())+
  scale_y_continuous(breaks = seq(0,400, 50))+
  labs(
    title   = "histogramme de la superficie par ménages(échelle log)",
    x       = "",
    y       = "Nombre de ménages",
    caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  )+
  theme_tp4(base_size = 14)

ggsave(filename = "outputs/fig04_hist_menage.png", plot = hhist_menage, height = 8, width = 10, dpi = 150)
cat("L'histogramme de la variable superficie par ménages : \n") ; print(hhist_menage)

#Construction du boxplot

boxplot_menage = ggplot(data = base2, aes(y = aire))+
  geom_boxplot(fill ="#C97C5D", color ="#2E2319" , alpha = 0.8 , width = 0.4 )+
  geom_hline(yintercept = round(median(base2$aire, na.rm = TRUE), 2), color = COL_ACCENT, linetype = "dashed",
             linewidth = 0.85)+
  annotate("text",
           x = 0.8,
           y = round(median(base2$aire, na.rm = TRUE), 2)+0.2,
           label = paste0("mediane = ", round(median(base2$aire, na.rm = TRUE), 2)),
           color = "#8B5E2A",
           size = 3.3,
           fontface = "bold")+
  scale_x_continuous(limits = c(-1,1), breaks = NULL)+
  labs(
    title   = "Boxplot de la superficie par ménage",
    x       = "",
    y       = "Superficie (hectares)",
    caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +
  theme_tp4(base_size = 14)+
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
ggsave(filename = "outputs/fig05_boxplot_menage.png", plot = boxplot_menage, height = 8, width = 10, dpi = 150)
cat("Le boxplot de la variable superficie par ménage : \n"); print(boxplot_menage)

#-----------------------------------------------------------
#Comparaison des superficies déclarées vs. superficies GPS 
#-----------------------------------------------------------

#Calcul des seuils pour le tri des données

seuila = quantile(base_parcelle$aire_GPS, probs = 0.99, na.rm = TRUE)
seuilb = quantile(base_parcelle$aire_hectare , probs = 0.99, na.rm = TRUE)

#Les données pour le graphique

data_graphic = base_parcelle %>%
  filter(!is.na(aire_GPS), !is.na(aire_hectare),
         aire_GPS > 0, aire_hectare > 0,
         aire_GPS <= seuila, aire_hectare <= seuilb)
nrow(data_graphic)
#Le test de Corrélation de Spearman
set.seed(123)
mon_test = cor.test(x = data_graphic$aire_GPS, y = data_graphic$aire_hectare,
                    method = "spearman",
                    use = "complete.obs")

#Récupération des valeurs du test
rho = round(mon_test$estimate, 3)
p_value = round(mon_test$p.value)
rho
p_value
pvalue_label = ifelse(p_value < 0.001, "p_value <0.001", paste0("p_value = ", round(p_value, 3)))

#Construction du nuage de points

graphique <- ggplot(data = data_graphic, aes(x = aire_hectare, y = aire_GPS)) +
  geom_point(alpha = 0.4, color = "#2E2319", size = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linewidth = 0.8, linetype = "dashed") +
  annotate("text",
           x     = min(data_graphic$aire_hectare, na.rm = TRUE) * 2,
           y     = max(data_graphic$aire_GPS, na.rm = TRUE) * 0.8,
           label = paste0("Spearman ρ = ", rho, "\n", pvalue_label),
           size      = 4,
           color     = "#2E2319",
           fontface  = "bold") +
  scale_x_log10(labels = scales::label_log()) +
  scale_y_log10(labels = scales::label_log()) +
  labs(
    title   = "Comparaison superficie déclarée vs superficie GPS (hectares)",
    x       = "Superficie déclarée (hectares, échelle log)",
    y       = "Superficie GPS (hectares, échelle log)",
    caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +
  theme_tp4(base_size = 14)

graphique
ggsave(filename = "outputs/fig03_scatter_plot.png",  plot = graphique, width = 10, height = 8, dpi = 150)

#-----------------------------------------------------------
#Analyse du régime de tenure (variable tenure)
#-----------------------------------------------------------

#Construction du tableau des fréquences et des proportions
prop_freq = base_parcelle %>%
  filter(!is.na(tenure), !is.na(wt_wave4)) %>%
  group_by(tenure) %>%
  summarise(frequence = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  mutate(proportion = 100*frequence / sum(frequence), 
         tenure = factor(tenure, levels = levels(base_parcelle$tenure)),
         tenure = fct_reorder(tenure, frequence,.desc = FALSE)) 


cat("Le tableau des effectifs et des proportions : \n"); print(prop_freq)

#Construction du barplot horizontal

#Définition des couleurs pour les types de tenures
col_tenure = c(
  "propriété" = "#C0572B",
  "location"  = "#5B7A52",
  "prêt"      = "#E8A045",
  "communautaire" = "#9C8070",
  "héritage"  = "#D9985B",
  "metayage"  = "#7A5C48",
  "échange_temporaire" = "#A3B18A"
)

hhist = ggplot(data = prop_freq, aes(x = tenure, y = frequence, fill = tenure))+
  geom_bar(stat = "identity",alpha = 0.88, linewidth = 0.3) +
  geom_text(aes(label = scales::comma(frequence)),
            hjust = -0.1,
            size = 3.5,
            fontface = "bold",
            color = "#2E2319") +
  scale_fill_manual(values = col_tenure, guide = "none")+
  scale_y_continuous(expand = expansion(mult = c(0,0.18)), labels = scales::comma) +
  coord_flip()+
  labs(title = "Diagramme à barres de la variable tenure", x = NULL,
       y = "Effectifs", caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)") +
  theme_tp4(base_size = 14) +
  theme(panel.grid.major.y = element_blank())
ggsave("outputs/fig01_diagramme_tenure.png", hhist, width =9 , height =5 , dpi=150)

#--------------------------------------------------
#Le test de Khi-deux
#--------------------------------------------------

#Construction du tableau de contingence

#Format long

tab_contingence = base_parcelle %>%
  filter(!is.na(zone),!is.na(tenure),!is.na(wt_wave4)) %>%
  select(zone, tenure, wt_wave4) %>%
  group_by(zone, tenure) %>%
  summarise(effectifs = sum(wt_wave4, na.rm = TRUE), .groups = "drop")

#Format large

tab_contingence = tab_contingence %>%
                pivot_wider(names_from = tenure,
                            id_cols = zone, 
                            values_from = effectifs,
                            values_fill = 0)

#Transformation en matrice
tab_contingence = tab_contingence %>%
  column_to_rownames("zone") %>%
  as.matrix()

khi_deux = chisq.test(tab_contingence)
cat("Les résultats du test de khi-deux sont les suivants :");print(khi_deux)

#----------------------------------------------------------------------------------
#Analyse de la relation entre superficie totale du ménage et le nombre de parcelles
#----------------------------------------------------------------------------------

#Vérification des variables clés de l'étude

exploitation_par_menage %>%
  select(aire, parcelles) %>%
  summary()

#----------------------------------------------------------------------------------
#Filtrage des valeurs aberrantes dans la variable aire
#----------------------------------------------------------------------------------

#Calcul d'un seuil de tri des données et constitution de la base filtrée

seuil_aire = quantile(x = exploitation_par_menage $aire, probs = 0.99, na.rm = TRUE)
base_filtree = exploitation_par_menage %>%
  filter(aire > 0,aire <= seuil_aire, !is.na(aire), !is.na(parcelles))

cat("Nombre de lignes :", nrow(base_filtree), "\n")

#----------------------------------------------------------------------------------
#Test de Spearman sur les données filtrées
#----------------------------------------------------------------------------------

resultat_spearman = cor.test(x=base_filtree$aire,
                             y=base_filtree$parcelles,
                             method = "spearman",
                             use = "complete.obs")

#Récupération des valeurs du test

rho = round(resultat_spearman$estimate, 3)
p_value = resultat_spearman$p.value
rho
p_value
p_value_label = ifelse(p_value<0.001, "p_value <0.001", paste0("p_value = ", round(p_value, 3)))

#----------------------------------------------------------------------------------
#Construction de l'intervalle de confiance avec la méthode de bootstrap
#----------------------------------------------------------------------------------

#La fonction R pour le calcul du rho de Spearman

rho_spearman = function(base, indices){
  echantillon = base[indices, ]
  cor(x = echantillon$aire, y = echantillon$parcelles,method = "spearman", use = "complete.obs")
}

#Le processus de bootstrap

set.seed(123) #La reproductibilité

boot_spearman = boot(data = base_filtree,
                     statistic = rho_spearman,
                     R = 1000)

#Intervalle de confiance

boot.ci(boot_spearman, type = "perc", conf = 0.95)

#----------------------------------------------------------------------------------
#Construction du scatter plot
#----------------------------------------------------------------------------------

scatter = ggplot(data =base_filtree, aes(x = aire, y = parcelles))+
  geom_jitter(width = 0.05, height = 0.05, size = 1.5,
              color = "#2E2319", alpha = 0.2)+
  geom_smooth(method = "loess", se = TRUE, color = "#C97C5D", linewidth = 1)+
  scale_x_continuous(labels = label_number(accuracy = 0.1))+
  annotate("text", x = max(base_filtree$aire, na.rm = TRUE) * 0.6,
           y = max(base_filtree$parcelles, na.rm = TRUE) * 0.95,
           color = "#2E2319",
           size = 4,
           fontface  = "bold" ,
           label = paste0("Spearman ρ = ", rho, "\n", p_value_label))+
  labs(title = "Relation entre superficie totale et nombre de parcelles", ,
       x = "Superficie totale du ménage (hectares)" ,
       y = "Nombre de parcelles",
       caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)")+
  theme_tp4(base_size = 13)
ggsave(filename = "outputs/fig06_superficie_vs_parcelle.png", plot = scatter, height = 8, width = 10, dpi = 150)
scatter

#---------------------------------------------------------------------------
# Le heatmap (État × vague4) de la superficie médiane
#---------------------------------------------------------------------------

#Les données pour la construction du heatmap
heatmap_data = base_parcelle %>%
  filter(!is.na(etat)) %>%
  group_by(etat) %>%
  summarise(aire_median = median(aire_hectare, na.rm = TRUE), .groups = "drop")

#La construction du heatmap
heatmap_plot = ggplot(heatmap_data, aes(x = "vague 4", y = reorder(etat, aire_median), fill = aire_median))+
  geom_tile(color = "white", linewidth = 0.5)+
  geom_text(aes(label = round(aire_median, 2)), color = "black",
          size = 3, fontface = "bold") +
  scale_fill_gradient(
    low  = "#FFF5E1",
    high = "#8B2500",
    name = "Superficie\nmédiane (ha)"
  ) +
  labs(
    x       = NULL,
    y       = "État",
    title   = "Superficie médiane des exploitations par État (Vague 4)",
    caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +
  theme_tp4(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 8)
  )

heatmap_plot

ggsave(filename = "outputs/fig02_heatmap.png", plot = heatmap_plot, width = 10, height = 8, dpi = 150)

