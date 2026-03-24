# =============================================================================
# Script 01 : Taux de morbidité pondéré par sexe et groupe d'âge (Tâche 13)
#
# PONDÉRATIONS :
#   Les taux de morbidité sont calculés avec svymean() / svyciprop() en
#   tenant compte de wt_wave4. Les IC à 95% sont également pondérés
#   (méthode de Korn & Graubard adaptée au plan de sondage).
#
# Auteurs : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(haven); library(dplyr); library(ggplot2)
library(scales); library(survey); library(srvyr)

# --------------------------------------------------------------------------
# 1. CHARGEMENT ET JOINTURE
# --------------------------------------------------------------------------
sect4a <- read_dta("data/raw/sect4a_harvestw4.dta")
sect1  <- read_dta("data/raw/sect1_harvestw4.dta")
secta  <- read_dta("data/raw/secta_harvestw4.dta")

# Poids au niveau ménage
poids <- secta %>% select(hhid, wt_wave4) %>% filter(!is.na(wt_wave4))

sect1_clean <- sect1 %>%
  select(hhid, indiv, s1q2, s1q4, sector) %>%
  rename(sexe = s1q2, age = s1q4, milieu = sector) %>%
  mutate(
    sexe_label   = factor(sexe,   levels=c(1,2), labels=c("Homme","Femme")),
    milieu_label = factor(milieu, levels=c(1,2), labels=c("Urbain","Rural")),
    groupe_age   = cut(age,
                       breaks=c(0,14,24,34,44,54,64,Inf),
                       labels=c("0-14","15-24","25-34","35-44","45-54","55-64","65+"),
                       right=TRUE, include.lowest=TRUE)
  )

sect4a_clean <- sect4a %>%
  select(hhid, indiv, s4aq3, s4aq1, s4aq3b_1, s4aq6a, s4aq9, s4aq14, s4aq17) %>%
  mutate(
    malade   = if_else(s4aq3 == 1, 1L, 0L, missing=NA_integer_),
    consulte = if_else(s4aq1 == 1, 1L, 0L, missing=NA_integer_)
  )

df_health <- sect4a_clean %>%
  left_join(sect1_clean, by=c("hhid","indiv")) %>%
  left_join(poids, by="hhid")   # ajout du poids wt_wave4

cat("Individus dans le jeu fusionné :", nrow(df_health), "\n")
cat("Avec poids renseigné           :", sum(!is.na(df_health$wt_wave4)), "\n")
cat("Avec morbidité renseignée      :", sum(!is.na(df_health$malade)), "\n")

saveRDS(df_health, "data/processed/df_health_base.rds")

# --------------------------------------------------------------------------
# 2. PLAN DE SONDAGE
# --------------------------------------------------------------------------
df_morb <- df_health %>% filter(!is.na(malade), !is.na(wt_wave4))

plan_sante <- svydesign(ids=~1, weights=~wt_wave4, data=df_morb)
svy_sante  <- as_survey_design(plan_sante)

# --------------------------------------------------------------------------
# 3. TAUX GLOBAL PONDÉRÉ
# --------------------------------------------------------------------------
taux_global_pond <- svymean(~malade, plan_sante, na.rm=TRUE)
ic_global        <- confint(taux_global_pond)

cat("\n=== Taux de morbidité global pondéré ===\n")
cat("Taux  :", round(coef(taux_global_pond)*100, 1), "%\n")
cat("IC 95%: [", round(ic_global[1]*100,1), "%;",
    round(ic_global[2]*100,1), "%]\n")

# --------------------------------------------------------------------------
# 4. TAUX PAR SEXE (pondéré)
# --------------------------------------------------------------------------
taux_sexe <- svy_sante %>%
  filter(!is.na(sexe_label)) %>%
  group_by(sexe_label) %>%
  summarise(
    taux   = survey_mean(malade, na.rm=TRUE, vartype="ci"),
    n_pond = survey_total(1, vartype=NULL)
  ) %>%
  rename(ic_low=taux_low, ic_high=taux_upp)

cat("\n=== Taux par sexe (pondéré) ===\n"); print(taux_sexe)

marge  <- 0.012
ylim_s <- max(taux_sexe$ic_high, na.rm=TRUE) + 0.04

p_sexe <- ggplot(taux_sexe, aes(x=sexe_label, y=taux, fill=sexe_label)) +
  geom_col(width=0.5, show.legend=FALSE) +
  geom_errorbar(aes(ymin=ic_low, ymax=ic_high),
                width=0.12, linewidth=0.8, color="grey25") +
  geom_text(aes(y=ic_high+marge,
                label=paste0(round(taux*100,1),"%")),
            vjust=0, size=5, fontface="bold", color="grey20") +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0, ylim_s), expand=c(0,0)) +
  scale_fill_manual(values=c("Homme"="#3A86FF","Femme"="#FF6B6B")) +
  labs(title="Taux de morbidité pondéré par sexe",
       subtitle="Estimations pondérées (wt_wave4) — IC à 95% pondérés\nNigeria GHS Panel Wave 4 (2018)",
       x=NULL, y="Taux de morbidité (pondéré)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | Pondérations : wt_wave4") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40",size=11),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(size=12,face="bold"))

ggsave("outputs/figures/01a_morbidite_sexe.png", p_sexe,
       width=7, height=5.5, dpi=300)
cat("-> 01a_morbidite_sexe.png\n")

# --------------------------------------------------------------------------
# 5. TAUX PAR GROUPE D'ÂGE (pondéré)
# --------------------------------------------------------------------------
taux_age <- svy_sante %>%
  filter(!is.na(groupe_age)) %>%
  group_by(groupe_age) %>%
  summarise(
    taux   = survey_mean(malade, na.rm=TRUE, vartype="ci"),
    n_pond = survey_total(1, vartype=NULL)
  ) %>%
  rename(ic_low=taux_low, ic_high=taux_upp)

cat("\n=== Taux par groupe d'âge (pondéré) ===\n"); print(taux_age)

ylim_a <- max(taux_age$ic_high, na.rm=TRUE) + 0.05

p_age <- ggplot(taux_age, aes(x=groupe_age, y=taux, fill=groupe_age)) +
  geom_col(width=0.65, show.legend=FALSE) +
  geom_errorbar(aes(ymin=pmax(0,ic_low), ymax=ic_high),
                width=0.2, linewidth=0.7, color="grey25") +
  geom_text(aes(y=ic_high+marge,
                label=paste0(round(taux*100,1),"%")),
            vjust=0, size=4, fontface="bold", color="grey20") +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0,ylim_a), expand=c(0,0)) +
  scale_fill_brewer(palette="Blues", direction=1) +
  labs(title="Taux de morbidité par groupe d'âge",
       subtitle="Estimations pondérées (wt_wave4) — Nigeria GHS Panel Wave 4 (2018)",
       x="Groupe d'âge", y="Taux de morbidité (pondéré)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | IC à 95% pondérés") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40"),
        panel.grid.major.x=element_blank())

ggsave("outputs/figures/01b_morbidite_age.png", p_age,
       width=9, height=5.5, dpi=300)
cat("-> 01b_morbidite_age.png\n")

# --------------------------------------------------------------------------
# 6. TAUX CROISÉ SEXE × GROUPE D'ÂGE (pondéré)
# --------------------------------------------------------------------------
taux_sa <- svy_sante %>%
  filter(!is.na(sexe_label), !is.na(groupe_age)) %>%
  group_by(groupe_age, sexe_label) %>%
  summarise(
    taux   = survey_mean(malade, na.rm=TRUE, vartype="ci"),
    .groups = "drop"
  ) %>%
  rename(ic_low=taux_low, ic_high=taux_upp)

dodge_w <- 0.7
ylim_sa <- max(taux_sa$ic_high, na.rm=TRUE) + 0.05

p_sa <- ggplot(taux_sa, aes(x=groupe_age, y=taux, fill=sexe_label)) +
  geom_col(position=position_dodge(width=dodge_w), width=0.65) +
  geom_errorbar(aes(ymin=pmax(0,ic_low), ymax=ic_high),
                position=position_dodge(width=dodge_w),
                width=0.18, linewidth=0.65, color="grey25") +
  geom_text(aes(y=ic_high+marge,
                label=paste0(round(taux*100,1),"%")),
            position=position_dodge(width=dodge_w),
            vjust=0, size=2.8, fontface="bold", color="grey20") +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0,ylim_sa), expand=c(0,0)) +
  scale_fill_manual(values=c("Homme"="#3A86FF","Femme"="#FF6B6B"), name="Sexe") +
  labs(title="Taux de morbidité — sexe × groupe d'âge",
       subtitle="Estimations pondérées (wt_wave4) — Nigeria GHS Panel Wave 4 (2018)",
       x="Groupe d'âge", y="Taux de morbidité (pondéré)",
       caption="Source : NBS Nigeria, GHS-Panel W4 | IC à 95% pondérés") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=14),
        plot.subtitle=element_text(color="grey40"),
        legend.position="top",
        panel.grid.major.x=element_blank())

ggsave("outputs/figures/01c_morbidite_sexe_age.png", p_sa,
       width=11, height=5.5, dpi=300)
cat("-> 01c_morbidite_sexe_age.png\n")

# Sauvegarde des résultats pour le rapport
saveRDS(list(global=taux_global_pond, ic_global=ic_global,
             sexe=taux_sexe, age=taux_age, sexe_age=taux_sa),
        "data/processed/resultats_morbidite.rds")

cat("\n=== Script 01 terminé ===\n")
