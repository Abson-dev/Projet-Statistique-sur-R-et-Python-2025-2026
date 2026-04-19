#install.packages("haven") 
#install.packages("tidyverse")

library("haven")
library("tidyverse")
library("forcats")
library(scales)

df_secta3i <- read_dta("data/raw/secta3i_harvestw4.dta")
df_secta3ii <- read_dta("data/raw/secta3ii_harvestw4.dta")
df_secta11c2 <- read_dta("data/raw/secta11c2_harvestw4.dta")


# Ajouter les poids à la fin.

# Question 1: On va utiliser la section a3ii du questionnaire pour répondre à ce problème.

# On utilise les codes numériques tels qu'ils apparaissent dans Stata
df_top15 <- df_secta3ii %>%
    filter(!is.na(cropcode)) %>%
    mutate(cropcode_label = as_factor(cropcode)) %>%
    count(cropcode_label, cropcode) %>%
    slice_max(n, n = 15, with_ties = FALSE) %>%
    mutate(type_culture = case_when(
        cropcode %in% c(1080, 1070, 1100, 1110) ~ "Céréale",
        cropcode %in% c(1010, 2220, 2040, 1090) ~ "Légumineuse",
        cropcode %in% c(1020, 1121, 1040, 1123) ~ "Tubercule",
        cropcode %in% c(1060, 2120, 2190)       ~ "Culture de rente/Autre",
        TRUE                                   ~ "Autre"
    ))

# 2. Graphique avec coloration par groupe

ggplot(df_top15, aes(x = n, y = reorder(cropcode_label, n), fill = type_culture)) +
    
    # Barres avec coins arrondis
    geom_col(width = 0.7, 
             color = NA) +
    
    # Ajout des valeurs
    geom_text(aes(label = n),
              hjust = -0.15,
              size = 3.8,
              family = "sans",
              color = "#2D3436") +
    
    # Couleurs douces
    scale_fill_manual(values = c(
        "Céréale" = "#D68910",
        "Légumineuse" = "#229954",
        "Tubercule" = "#8E44AD",
        "Culture de rente/Autre" = "#566573"
    )) +
    
    # Espacement pour les nombres
    expand_limits(x = max(df_top15$n) * 1.15) +
    
    labs(
        x = "Nombre d'observations",
        y = NULL,
        fill = "Type de culture",
        title = "Top 15 des cultures",
        subtitle = "Regroupées par catégorie",
        caption = "Source : Données GHS/LSMS Nigeria 2018-2019"
    ) +
    
    theme_minimal(base_family = "sans") +
    
    theme(
        plot.title = element_text(
            size = 16,
            face = "bold",
            color = "#2C3E50"
        ),
        
        plot.subtitle = element_text(
            size = 11,
            color = "#7F8C8D"
        ),
        
        axis.text.y = element_text(
            size = 8,
            color = "#2C3E50"
        ),
        
        axis.text.x = element_text(
            color = "#7F8C8D"
        ),
        
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        
        legend.position = "bottom",
        
        plot.margin = margin(15, 15, 15, 15)
    )

# Question 2: On va utiliser la section a3ii

## Vérifier, que les ménages ont tous un seul secteur

df_secta3ii %>%
    group_by(hhid) %>%
    summarise(n_sector = n_distinct(sector)) %>%
    filter(n_sector > 1)

## Vérifier, que les ménages ont tous un seul secteur

hh_crop <- df_secta3ii %>% 
    select(hhid, sector, cropcode) %>% 
    group_by(hhid) %>% 
    summarise(nb_crops = sum(!is.na(cropcode)), sector = first(sector))

hh_crop

## Construisons un histogramme permettant de visualiser la distribution du nombre de cultures.

ggplot(hh_crop, aes(x = nb_crops)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    labs(
        title = "Distribution du nombre de cultures par ménage",
        x = "Nombre de cultures",
        y = "Fréquence"
    )
#Faire une petite analyse descriptive içi.

## Effectuons le test de Wilcoxon pour savoir s'il y'a des differences.

wilcox.test(nb_crops ~ sector, data = hh_crop)

## Le test de Wilcoxon indique une différence statistiquement significative du nombre de cultures entre zones rurales et urbaines (p-value = 6.06e-09). On rejette l'hypothèse nulle d'égalité des distributions.

hh_crop <- hh_crop %>% 
    mutate(milieu = factor(sector))

hh_crop$milieu <- factor(
    hh_crop$milieu,
    levels = c(1, 2),
    labels = c("Rural", "Urbain")
)

## Violin plot

ggplot(hh_crop, aes(x = milieu, y = nb_crops)) +
    geom_violin(fill = "lightblue") +
    labs(
        title = "Nombre de cultures selon le secteur",
        x = "Secteur",
        y = "Nombre de cultures"
    )

## Autre version

ggplot(hh_crop, aes(x = milieu, y = nb_crops, fill = milieu)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    scale_fill_manual(values = c("#4C78A8", "#F58518")) +
    labs(
        title = "Distribution du nombre de cultures selon le milieu de résidence",
        x = "Milieu de résidence",
        y = "Nombre de cultures"
    ) +
    theme_minimal() +
    theme(legend.position = "none")


### Analyser l'utilisation des anglais:
## Taux d'utilisation par type (Organique, Chimique, NPK, Uree), selon la Zone et l'Etat (Diagramme en barre avec intervalle de confiance à 95%) puis faire un test de Khi-Deux pour l'association Zone et Utilisation d'engrais. Base de données: section a 11c2 nomméein put use on plot.

### On commence par recoder proprement les variables d'utilisation d'engrais. Ici les valeurs manquantes sont cohérentes avec les réponses fournies, donc on les considèreras comme des cas de non utilisations des engrais. Notons que l'identification de l'utilisation se fait ici par parcelle cultivée.

fert_plot <- df_secta11c2 %>%
    mutate(
        use_inorganic = ifelse(s11dq1a == 1, 1, 0),
        organic = ifelse(s11dq36 == 1, 1, 0),
        npk = ifelse(s11c2q36_1 == 1, 1, 0),
        urea = ifelse(s11c2q36_2 == 1, 1, 0),
        other = ifelse(s11c2q36_99 == 1, 1, 0)
    )

## On remplace les na par 0 partout car ils correspondent pour l'urée, le npk ou autre, à des cas où les ménages n'utilisaient même pas d'engrais inorganique.

fert_plot <- fert_plot %>%
    mutate(
        across(c(npk, urea, organic, other), ~replace_na(., 0))
    )

## Quand on s'interesse à la variable dummy engrais inorganique, on observe que l'unique valeur manquante vient de l'observation d'id "99002" qui n'a répondu à aucune question de cette section. Pour la suite du travail nous la supprimerons

fert_plot <- fert_plot %>% 
    filter(hhid !=99002)



## Ensuite on agrege au niveau menage. Un menage peut avoir plusieurs parcelles, donc si une parcelle utilise fertilizer x, alors le menage utilise ce fertilizer x. Pour faire ce groupement, nous utilisons la fonction max.)

fert_hh <- fert_plot %>%
    group_by(hhid) %>%
    summarise(
        use_fertilizer = max(use_inorganic, organic, na.rm = TRUE),
        inorganic = max(use_inorganic, na.rm = TRUE),
        organic = max(organic, na.rm = TRUE),
        npk = max(npk, na.rm = TRUE),
        urea = max(urea, na.rm = TRUE),
        other = max(other, na.rm = TRUE),
        zone = first(zone),
        state = first(state),
        sector = first(sector)
    )

## Maintenant, nous pouvons éffectuer les calculs demandés

fert_rate_interim <- fert_hh %>% 
    group_by(sector, state) %>% 
    summarise(
        organic_rate = mean(organic),
        inorganic_rate = mean(inorganic),
        npk_rate = mean(npk),
        urea_rate = mean(urea),
        n = n(),
        .groups = "drop"
    )

fert_rate <- fert_hh %>% 
    group_by(sector, state) %>% 
    summarise(
        rate = mean(inorganic),
        n = n(),
        se = sqrt(rate*(1-rate)/n),
        lower = rate - 1.96*se,
        upper = rate + 1.96*se,
        .groups = "drop"
    )

fert_rate <- fert_rate %>%
    mutate(
        sector = factor(
            sector,
            levels = c(1,2),
            labels = c("Rural", "Urbain")
        )
    )

# Global par zone rural ou urbaine

ggplot(fert_rate, aes(x = sector, y = rate, fill = sector)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_errorbar(
        aes(ymin = lower, ymax = upper),
        width = 0.15
    ) +
    scale_fill_manual(
        values = c("#2E86AB", "#F18F01")
    ) +
    theme_minimal()

# Par etat
ggplot(fert_rate, aes(x = sector, y = rate)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(ymin = lower, ymax = upper),
        width = 0.2
    ) +
    facet_wrap(~state) +
    labs(
        title = "Taux d'utilisation d'engrais inorganique",
        y = "Taux",
        x = "Milieu"
    )

