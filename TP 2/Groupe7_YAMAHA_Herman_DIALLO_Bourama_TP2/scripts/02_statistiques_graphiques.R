# =============================================================================
# Script 02 : Statistiques descriptives et représentations graphiques
# Thème 2 – Éducation et alphabétisation des membres des ménages
# Enquête GHS Nigeria 2018 (Wave 4)
# Réalisé par : Herman YAMAHA, Bourama DIALLO 
# =============================================================================

library(dplyr)
library(ggplot2)
library(forcats)
library(rstatix)
library(ggpubr)
library(gtsummary)
library(viridis)
library(patchwork)
library(scales)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

df_adultes <- readRDS("data/processed/df_adultes.rds")
df_educ    <- readRDS("data/processed/df_educ.rds")
df_scol    <- readRDS("data/processed/df_scol.rds")

# Palette personnalisée pour les niveaux d'instruction
couleurs_niveaux <- c(
  "Aucun"            = "#B2182B",
  "Primaire"         = "#E66101",
  "Junior Secondary" = "#FDB863",
  "Senior Secondary" = "#5E3C99",
  "Tertiaire"        = "#1B7837"
)

# ============================================================
# TÂCHE 8 : Diagramme en barres – répartition du niveau d'instruction
# ============================================================
cat("\n--- Tâche 8 : Répartition globale du niveau d'instruction ---\n")

tab_freq <- df_adultes %>%
  count(niveau_educ) %>%
  mutate(pct   = n / sum(n) * 100,
         etiq  = paste0(round(pct, 1), "%"))
print(tab_freq)

g8 <- ggplot(tab_freq,
             aes(x = pct, y = fct_rev(niveau_educ), fill = niveau_educ)) +
  geom_col(width = 0.55, color = "grey20", linewidth = 0.3) +
  geom_text(aes(label = etiq), hjust = -0.08, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = couleurs_niveaux, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Répartition du niveau d'instruction des adultes (18 ans et +)",
       subtitle = "Données GHS – Wave 4, Nigeria (2018)",
       x = "Pourcentage (%)", y = NULL,
       caption = "Source : GHS-W4, Nigeria 2018 | Traitement : Groupe 7") +
  theme_light(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#2C3E50"),
        plot.subtitle = element_text(colour = "#5D6D7E"),
        panel.grid.major.y = element_blank())

ggsave("outputs/fig01_barplot_niveau_instruction.png", g8,
       width = 8, height = 5, dpi = 150)
cat("  ✓ fig01 exportée\n")

# ============================================================
# TÂCHE 9 : Barres empilées 100% selon le sexe + test du Chi2 + V de Cramér
# ============================================================
cat("\n--- Tâche 9 : Instruction selon le genre ---\n")

df_genre_niv <- df_adultes %>%
  filter(!is.na(sexe_label)) %>%
  count(sexe_label, niveau_educ) %>%
  group_by(sexe_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

g9 <- ggplot(df_genre_niv,
             aes(x = sexe_label, y = pct, fill = niveau_educ)) +
  geom_col(position = "fill", color = "grey30", width = 0.45) +
  geom_text(aes(label = ifelse(pct >= 3, paste0(round(pct, 1), "%"), "")),
            position = position_fill(vjust = 0.5),
            size = 3.1, color = "white", fontface = "bold") +
  scale_fill_manual(values = couleurs_niveaux, name = "Niveau d'instruction") +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Profil éducatif selon le genre (adultes 18+)",
       subtitle = "Représentation en barres empilées à 100% – GHS-W4, 2018",
       x = NULL, y = "Part relative",
       caption = "Source : GHS-W4, Nigeria 2018 | Traitement : Groupe 7") +
  coord_flip() +
  theme_light(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#2C3E50"),
        legend.position = "bottom")

ggsave("outputs/fig02_barres100_genre.png", g9,
       width = 9, height = 5, dpi = 150)
cat("  ✓ fig02 exportée\n")

# Table de contingence + test statistique
table_croisee <- table(df_adultes$sexe_label, df_adultes$niveau_educ)
cat("\nTableau croisé Sexe x Niveau :\n")
print(table_croisee)

res_chi2 <- chisq.test(table_croisee)
cat("\nRésultat du test du Chi-deux :\n"); print(res_chi2)

n_total  <- sum(table_croisee)
dim_min  <- min(nrow(table_croisee) - 1, ncol(table_croisee) - 1)
v_cramer <- sqrt(res_chi2$statistic / (n_total * dim_min))
cat(sprintf("\nV de Cramér : %.4f\n", v_cramer))

saveRDS(list(table = table_croisee, chi2 = res_chi2, v_cramer = v_cramer),
        "data/processed/resultats_tache9.rds")

# ============================================================
# TÂCHE 10 : Boîtes à moustaches par tranche d'âge + Kruskal-Wallis + Dunn
# ============================================================
cat("\n--- Tâche 10 : Instruction par tranche d'âge ---\n")

df_age <- df_adultes %>%
  filter(!is.na(groupe_age)) %>%
  mutate(niveau_num = as.numeric(niveau_educ))

test_kw  <- kruskal.test(niveau_num ~ groupe_age, data = df_age)
cat("\nTest de Kruskal-Wallis :\n"); print(test_kw)

test_dunn <- df_age %>%
  dunn_test(niveau_num ~ groupe_age, p.adjust.method = "bonferroni")
cat("\nComparaisons multiples de Dunn :\n"); print(test_dunn)

g10 <- ggplot(df_age,
              aes(x = groupe_age, y = niveau_num, fill = groupe_age)) +
  geom_boxplot(alpha = 0.70, outlier.alpha = 0.10, width = 0.55) +
  geom_jitter(alpha = 0.035, width = 0.12, size = 0.4, color = "grey50") +
  scale_y_continuous(breaks = 1:5, labels = levels(df_adultes$niveau_educ)) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title    = "Niveau d'instruction par tranche d'âge (adultes 18+)",
       subtitle = paste0("Kruskal-Wallis : H = ", round(test_kw$statistic, 2),
                         ", p < 0,001"),
       x = "Tranche d'âge", y = "Niveau d'instruction",
       caption = "Source : GHS-W4, Nigeria 2018 | Traitement : Groupe 7") +
  theme_light(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#2C3E50"),
        panel.grid.major.x = element_blank())

ggsave("outputs/fig03_boxplot_age.png", g10,
       width = 9, height = 6, dpi = 150)
cat("  ✓ fig03 exportée\n")

saveRDS(list(kruskal = test_kw, dunn = test_dunn),
        "data/processed/resultats_tache10.rds")

# ============================================================
# TÂCHE 11 : Taux de scolarisation 6-17 ans – urbain vs rural
# ============================================================
cat("\n--- Tâche 11 : Scolarisation des 6-17 ans selon le milieu ---\n")
# s2aq13 = actuellement inscrit à l'école (1=Oui, 2=Non)

tab_scol <- table(df_scol$zone_scol, df_scol$scolarise)
colnames(tab_scol) <- c("Non scolarisé", "Scolarisé")
cat("\nTableau croisé Milieu x Scolarisation :\n"); print(tab_scol)

chi2_scol <- chisq.test(tab_scol)
cat("\nChi-deux de la scolarisation :\n"); print(chi2_scol)

df_taux <- df_scol %>%
  group_by(zone_scol) %>%
  summarise(n      = n(),
            n_scol = sum(scolarise, na.rm = TRUE),
            prop   = n_scol / n,
            se     = sqrt(prop * (1 - prop) / n),
            borne_inf = pmax(0, prop - 1.96 * se),
            borne_sup = pmin(1, prop + 1.96 * se))
cat("\nTaux de scolarisation et IC à 95% :\n"); print(df_taux)

g11 <- ggplot(df_taux,
              aes(x = zone_scol, y = prop * 100, fill = zone_scol,
                  ymin = borne_inf * 100, ymax = borne_sup * 100)) +
  geom_col(width = 0.45, alpha = 0.90) +
  geom_errorbar(width = 0.12, color = "grey25", linewidth = 0.7) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            vjust = -1.9, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Urbain" = "#2166AC", "Rural" = "#B2182B"),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 108),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Taux de scolarisation des 6-17 ans selon le milieu de résidence",
       subtitle = paste0("χ² = ", round(chi2_scol$statistic, 1),
                         ", p < 0,001 | Barres d'erreur : IC à 95%"),
       x = NULL, y = "Taux de scolarisation (%)",
       caption = "Source : GHS-W4, Nigeria 2018 | Traitement : Groupe 7") +
  theme_light(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#2C3E50"))

ggsave("outputs/fig04_scolarisation_milieu.png", g11,
       width = 7, height = 5, dpi = 150)
cat("  ✓ fig04 exportée\n")

saveRDS(list(table = tab_scol, chi2 = chi2_scol, taux = df_taux),
        "data/processed/resultats_tache11.rds")

# ============================================================
# TÂCHE 12 : Cartes de chaleur par État
# ============================================================
cat("\n--- Tâche 12 : Cartes de chaleur – analphabétisme par État ---\n")

df_heatmap <- df_adultes %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(n_total    = n(),
            n_aucun    = sum(niveau_educ == "Aucun", na.rm = TRUE),
            pct_aucun  = n_aucun / n_total * 100) %>%
  ungroup() %>%
  mutate(quintile = ntile(pct_aucun, 5))

cat("\n10 États avec le plus fort taux de non-instruction :\n")
print(df_heatmap %>% arrange(desc(pct_aucun)) %>% head(10))

g12a <- ggplot(df_heatmap,
               aes(x = 1,
                   y = fct_reorder(state_name, pct_aucun),
                   fill = pct_aucun)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct_aucun, 1), "%")),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "% sans instruction",
                       labels = function(x) paste0(x, "%")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title    = "Proportion d'adultes (18+) non instruits par État",
       subtitle = "Intensité colorimétrique – GHS Wave 4, 2018",
       x = NULL, y = NULL,
       caption = "Source : GHS-W4, Nigeria 2018 | Traitement : Groupe 7") +
  theme_light(base_size = 10) +
  theme(plot.title   = element_text(face = "bold", size = 12, colour = "#2C3E50"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid   = element_blank())

ggsave("outputs/fig05_heatmap_non_instruction_etat.png", g12a,
       width = 8, height = 11, dpi = 150)
cat("  ✓ fig05 exportée\n")

# Carte de chaleur croisée État x Niveau d'instruction
df_etat_niv <- df_adultes %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name, niveau_educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(state_name) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

g12b <- ggplot(df_etat_niv,
               aes(x = niveau_educ,
                   y = fct_reorder(state_name,
                                   ifelse(niveau_educ == "Aucun", pct, 0),
                                   .fun = max),
                   fill = pct)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = ifelse(pct >= 5, paste0(round(pct, 0), "%"), "")),
            size = 2.5, color = "white") +
  scale_fill_viridis_c(option = "cividis",
                       name = "% des adultes",
                       labels = function(x) paste0(x, "%")) +
  labs(title    = "Ventilation du niveau d'instruction par État",
       subtitle = "Adultes de 18 ans et plus – GHS Wave 4, 2018",
       x = "Niveau d'instruction", y = NULL,
       caption = "Source : GHS-W4, Nigeria 2018 | Traitement : Groupe 7") +
  theme_light(base_size = 9) +
  theme(plot.title  = element_text(face = "bold", size = 11, colour = "#2C3E50"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid  = element_blank())

ggsave("outputs/fig06_heatmap_etat_x_niveau.png", g12b,
       width = 10, height = 12, dpi = 150)
cat("  ✓ fig06 exportée\n")

saveRDS(df_heatmap, "data/processed/df_heatmap.rds")

cat("\n====== Ensemble des analyses réalisées ======\n")
for (f in list.files("outputs/", pattern = "*.png"))
  cat(" -", f, "\n")
