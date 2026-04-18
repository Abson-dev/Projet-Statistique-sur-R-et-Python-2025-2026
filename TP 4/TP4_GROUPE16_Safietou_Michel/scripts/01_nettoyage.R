# Installer si nécessaire
#install.packages(c("haven","dplyr","ggplot2","ggrepel","scales","patchwork","rstatix","gtsummary","viridis"))

#install.packages("gtsummary")
# ===============================
# 1. LIBRAIRIES
# ===============================
library(haven)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(patchwork)
library(rstatix)
library(viridis)
library(srvyr)
library(here)


#install.packages(c("rlang", "vctrs", "dplyr", "cli", "glue"), dependencies = TRUE)

# ===============================
# 2. IMPORTATION DES DONNÉES
# ===============================




baseA <- haven::read_dta(
  here::here("data", "raw", "sect11a1_plantingw4.dta")
)

baseB <- haven::read_dta(
  here::here("data", "raw", "sect11b1_plantingw4.dta")
)

poids <- haven::read_dta(
  here::here("data", "raw", "secta_harvestw4.dta")
)

#glimpse(baseA)
#glimpse(baseB)

# ===============================
# 3. RECODAGE DES VARIABLES
# ===============================
baseA <- baseA %>%
  mutate(across(c(s11aq4b, sector, s11aq4a, s11aq4b1, s11b1q27), as.numeric)) %>%
  
  mutate(
    sector = recode(sector,
                    `1` = "URBAIN",
                    `2` = "RURAL"),
    
    s11aq4a = recode(s11aq4a,
                     `1` = "YES",
                     `2` = "NO"),
    
    s11aq4b1 = recode(s11aq4b1,
                      `1` = "TOO FAR",
                      `2` = "THE FIELD IS OUT OF LGA",
                      `3` = "UNWILLINGNESS OF THE HOLDER",
                      `4` = "OTHER"),
    
    s11b1q27 = recode(s11b1q27,
                      `1` = "YES",
                      `2` = "NO"),
    
    s11aq4b = recode(s11aq4b,
                     `1` = "HEAPS",
                     `2` = "RIDGES",
                     `3` = "STANDS",
                     `5` = "ACRES",
                     `6` = "HECTARES",
                     `7` = "SQUARE METERS")
  )

# ===============================
# 4. CALCUL DE LA SUPERFICIE (HA)
# ===============================
baseA <- baseA %>%
  mutate(
    coef = case_when(
      # HEAPS
      s11aq4b == "HEAPS" & zone == 1 ~ 0.00012,
      s11aq4b == "HEAPS" & zone == 2 ~ 0.00016,
      s11aq4b == "HEAPS" & zone == 3 ~ 0.00011,
      s11aq4b == "HEAPS" & zone == 4 ~ 0.00019,
      s11aq4b == "HEAPS" & zone == 5 ~ 0.00021,
      s11aq4b == "HEAPS" & zone == 6 ~ 0.00012,
      
      # RIDGES
      s11aq4b == "RIDGES" & zone == 1 ~ 0.00270,
      s11aq4b == "RIDGES" & zone == 2 ~ 0.00400,
      s11aq4b == "RIDGES" & zone == 3 ~ 0.00494,
      s11aq4b == "RIDGES" & zone == 4 ~ 0.00230,
      s11aq4b == "RIDGES" & zone == 5 ~ 0.00230,
      s11aq4b == "RIDGES" & zone == 6 ~ 0.00001,
      
      # STANDS
      s11aq4b == "STANDS" & zone == 1 ~ 0.00006,
      s11aq4b == "STANDS" & zone == 2 ~ 0.00016,
      s11aq4b == "STANDS" & zone == 3 ~ 0.00004,
      s11aq4b == "STANDS" & zone == 4 ~ 0.00004,
      s11aq4b == "STANDS" & zone == 5 ~ 0.00013,
      s11aq4b == "STANDS" & zone == 6 ~ 0.00041,
      
      # unités standards
      s11aq4b == "ACRES" ~ 0.404686,
      s11aq4b == "HECTARES" ~ 1,
      s11aq4b == "SQUARE METERS" ~ 0.0001,
      
      TRUE ~ NA_real_
    ),
    
    superficie_ha = s11aq4aa * coef
  )

# ===============================
# 5. IMPUTATION (PAR MENAGE)
# ===============================
# Moyenne globale (backup)
moyenne_globale <- mean(baseA$superficie_ha, na.rm = TRUE)

baseA <- baseA %>%
  group_by(hhid) %>%
  mutate(
    moyenne_menage = mean(superficie_ha, na.rm = TRUE),
    superficie_ha = ifelse(
      is.na(superficie_ha),
      ifelse(is.nan(moyenne_menage), moyenne_globale, moyenne_menage),
      superficie_ha
    )
  ) %>%
  ungroup() %>%
  select(-moyenne_menage)

# ===============================
# 6. TRAITEMENT DES OUTLIERS
# ===============================
baseA <- baseA %>%
  mutate(
    superficie_ha = case_when(
      superficie_ha > 500 ~ 500,
      superficie_ha < 0 ~ 0,
      TRUE ~ superficie_ha
    )
  )

# ===============================
# 7. SUPERFICIE TOTALE PAR MENAGE
# ===============================
baseA_menage <- baseA %>%
  group_by(hhid) %>%
  summarise(superficie_menage = sum(superficie_ha, na.rm = TRUE))

# ===============================
# 8. VERIFICATIONS
# ===============================
sum(is.na(baseA$superficie_ha))

outliers <- baseA %>%
  filter(superficie_ha < 0 | superficie_ha > 500)

nrow(outliers)

# ===============================
# 9. TRAITEMENT BASE B
# ===============================
baseB <- baseB %>%
  mutate(tenure = case_when(
    s11b1q8_1 == 1 ~ "CERTIFICAT D'OCCUPATION",
    s11b1q8_2 == 1 ~ "DROIT D'OCCUPATION",
    s11b1q8_3 == 1 ~ "CERTIFICAT COUTUMIER D'OCCUPATION",
    s11b1q8_5 == 1 ~ "TITRE DE PROPRIÉTÉ",
    TRUE ~ NA_character_
  ))

# ===============================
# 10. INTEGRATION DU POIDS
# ===============================
baseA <- baseA %>%
  left_join(poids %>% select(hhid, wt_wave4, wt_longpanel), by = "hhid")

baseB <- baseB %>%
  left_join(poids %>% select(hhid, wt_wave4, wt_longpanel), by = "hhid")


# ===============================
# 10. EXPORT
# ===============================
knitr::opts_knit$set(root.dir = "..")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

haven::write_dta(baseA, "data/processed/baseA.dta")
haven::write_dta(baseB, "data/processed/baseB.dta")
haven::write_dta(baseA_menage, "data/processed/baseA_menage.dta")