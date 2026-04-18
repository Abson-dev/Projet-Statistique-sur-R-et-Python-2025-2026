library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

secta3i <- read_dta("data/secta3i_harvestw4.dta")
secta3ii <- read_dta("data/secta3ii_harvestw4.dta")
sect11a1 <- read_dta("data/sect11a1_plantingw4.dta")
#1. 
secta3i <- secta3i %>%
  mutate(crop_label = as_factor(cropcode),
         crop_label = str_remove(crop_label, "^\\d+\\.\\s*"))

freq_crop_w4 <- secta3i %>%
  filter(!is.na(cropcode)) %>%
  filter(sa3iq3 == 1) %>%  
  mutate(
    crop_label = as_factor(cropcode),
    crop_label = str_remove(crop_label, "^\\d+\\.\\s*")
  ) %>%
  distinct(hhid, crop_label) %>%
  count(crop_label, sort = TRUE) %>%
  slice_head(n = 15)

freq_crop_w4 <- freq_crop_w4 %>%
  mutate(
    crop_type = case_when(
      str_detect(crop_label, "MAIZE|RICE|SORGHUM|MILLET") ~ "Céréale",
      str_detect(crop_label, "CASSAVA|YAM") ~ "Racines",
      str_detect(crop_label, "SESAME") ~ "Oléagineux",
      str_detect(crop_label, "BEANS|COWPEA|GROUNDNUT|GROUND NUT/PEANUTS") ~ "Légumineuse",
      TRUE ~ "Autres cultures"
    )
  )
freq_crop_w4


ggplot(freq_crop_w4,
       aes(x = n,
           y = fct_reorder(crop_label, n),
           fill = crop_type)) +
  geom_col() +
  labs(
    title = "Les 15 cultures les plus fréquentes",
    subtitle = "Fréquence mesurée au niveau ménage-culture",
    x = "Nombre de ménages",
    y = "Culture",
    fill = "Type"
  ) +
  scale_fill_manual(values = c(
    "Céréale"   = "#E07B39",
    "Légumineuse"    = "#4C9A5A",
    "Tubercule" = "#C4A435",
    "Culture de rente"     = "#D95F8A"
  )) +
  theme_minimal()

#2. 
# Nettoyage + sélection cultures récoltées
diversification <- secta3i %>%
  filter(!is.na(cropcode)) %>%
  filter(sa3iq3 == 1) %>%   # vérifier codage
  distinct(hhid, cropcode) %>%   # clé importante !!!
  group_by(hhid) %>%
  summarise(
    n_cultures = n(),
    .groups = "drop"
  )

sector_data <- secta3i %>%
  select(hhid, sector) %>%
  distinct()

diversification <- diversification %>%
  left_join(sector_data, by = "hhid")
diversification <- diversification %>%
  mutate(
    zone = case_when(
      sector == 1 ~ "Rural",
      sector == 2 ~ "Urbain",
      TRUE ~ NA_character_
    )
  )

ggplot(diversification, aes(x = n_cultures)) +
  geom_histogram(binwidth = 1, fill = "#4C9A5A", color = "white") +
  labs(
    title = "Distribution de l'indice de diversification culturale",
    x = "Nombre de cultures",
    y = "Nombre de ménages"
  ) +
  theme_minimal()

wilcox_test <- wilcox.test(
  n_cultures ~ zone,
  data = diversification
)

wilcox_test

ggplot(diversification,
       aes(x = zone,
           y = n_cultures,
           fill = zone)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, color = "black") +
  labs(
    title = "Diversification culturale selon le milieu",
    x = "Zone",
    y = "Nombre de cultures"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#4. 
library(dplyr)
library(haven)
library(stringr)

prod <- secta3ii %>%
  mutate(
    crop_label = as_factor(cropcode),
    crop_label = str_remove(crop_label, "^\\d+\\.\\s*")
  ) %>%
  filter(str_detect(crop_label, "MAIZE|MILLET")) %>%
  group_by(hhid, cropcode) %>%
  summarise(
    production = sum(sa3iiq1a, na.rm = TRUE),
    .groups = "drop"
  )

area_plot <- sect11a1 %>%
  mutate(
    area_ha = case_when(
      s11aq4b == 6 ~ s11aq4aa,             # hectares
      s11aq4b == 5 ~ s11aq4aa * 0.404686,  # acres -> hectares
      s11aq4b == 7 ~ s11aq4aa / 10000,     # m² -> hectares
      s11aq4b %in% c(1, 2, 3) ~ NA_real_,  # heaps, ridges, stands
      TRUE ~ NA_real_
    )
  )
area_plot <- area_plot %>% select(hhid, plotid, area_ha, state)

crop_plot <- secta3i %>%
  mutate(
    crop_label = as_factor(cropcode),
    crop_label = str_remove(crop_label, "^\\d+\\.\\s*")
  ) %>%
  select(hhid, plotid, cropcode, crop_label, state)

area_crop_plot <- area_plot %>%
  inner_join(crop_plot, by = c("hhid", "plotid"))
nrow(area_plot)
nrow(crop_plot)
nrow(area_crop_plot)

area_crop_plot <- area_crop_plot %>%
  filter(str_detect(crop_label, "MAIZE|MILLET"))

area_crop_plot %>%
  count(hhid, plotid) %>%
  arrange(desc(n)) %>%
  head(10)

prod_data <- secta3ii %>%
  select(hhid, cropcode, sa3iiq1_conv, state)

prod_data <- prod_data %>%
  filter(!is.na(sa3iiq1_conv), sa3iiq1_conv > 0)

area_crop_plot_mm <- area_crop_plot %>%
  filter(str_detect(crop_label, "MAIZE|MILLET"))

area_crop <- area_crop_plot_mm %>%
  group_by(hhid, cropcode) %>%
  summarise(
    area_ha = sum(area_ha, na.rm = TRUE),
    .groups = "drop"
  )

area_crop <- area_crop %>%
  mutate(cropcode = as.numeric(cropcode))

prod_data <- prod_data %>%
  mutate(cropcode = as.numeric(cropcode))
yield_data <- area_crop %>%
  inner_join(prod_data, by = c("hhid", "cropcode"))

yield_data <- yield_data %>%
  mutate(
    crop_label = as_factor(cropcode)
  )

yield_data <- yield_data %>%
  mutate(
    yield_kg_ha = sa3iiq1_conv / area_ha
  )

summary(yield_data$yield_kg_ha)

yield_data %>%
  summarise(
    n = n(),
    n_na = sum(is.na(yield_kg_ha)),
    n_inf = sum(is.infinite(yield_kg_ha)),
    min_yield = min(yield_kg_ha, na.rm = TRUE),
    q1 = quantile(yield_kg_ha, 0.25, na.rm = TRUE),
    median = median(yield_kg_ha, na.rm = TRUE),
    q3 = quantile(yield_kg_ha, 0.75, na.rm = TRUE),
    max_yield = max(yield_kg_ha, na.rm = TRUE)
  )
yield_data <- yield_data %>%
  filter(
    !is.na(area_ha),
    !is.na(sa3iiq1_conv),
    area_ha > 0,
    sa3iiq1_conv > 0,
    !is.na(yield_kg_ha),
    is.finite(yield_kg_ha)
  )

Q1 <- quantile(yield_data$yield_kg_ha, 0.25, na.rm = TRUE)
Q3 <- quantile(yield_data$yield_kg_ha, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 3 * IQR_val
upper_bound <- Q3 + 3 * IQR_val

yield_clean <- yield_data %>%
  filter(
    yield_kg_ha >= lower_bound,
    yield_kg_ha <= upper_bound
  )

yield_data %>%
  summarise(n_before = n())

yield_clean %>%
  summarise(n_after = n())

# Boxplot par Etat
ggplot(yield_clean, aes(x = factor(state), y = yield_kg_ha)) +
  geom_boxplot() +
  labs(
    title = "Distribution des rendements par État",
    x = "État",
    y = "Rendement (kg/ha)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  