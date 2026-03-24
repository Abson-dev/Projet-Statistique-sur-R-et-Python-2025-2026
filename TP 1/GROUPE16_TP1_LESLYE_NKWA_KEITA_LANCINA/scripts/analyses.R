# scripts des analyses.
# Analyses statistiques pondérées et visualisations

# Palette de couleurs (selon nos recherches sur les conventions des organisations internationales)
col_homme  <- "#0058AB"  # WHO/HMD
col_femme  <- "#E8416F"  # WHO/HMD
col_rural  <- "#80BD41"  # FAO/IFPRI
col_urbain <- "#6C757D"  # ONU-HABITAT
col_bleu   <- "#1CABE2"  # UNICEF

theme_tp1 <- theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(size = 11, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9,  color = "grey40"),
    plot.caption  = element_text(size = 7,  color = "grey50", hjust = 1),
    axis.title    = element_text(size = 9),
    legend.position = "bottom"
  )

theme_set(theme_tp1)

# Chargement des objets produits par le script nettoyage.R
sect1_clean  <- readRDS(here("data", "processed", "sect1_clean.rds"))
menages      <- readRDS(here("data", "processed", "menages.rds"))
plan_sondage <- readRDS(here("data", "processed", "plan_sondage.rds"))
plan_menages <- readRDS(here("data", "processed", "plan_menages.rds"))

age_vec <- sect1_clean$age[!is.na(sect1_clean$age)]

# 1. Statistiques descriptives de l'âge

stats_age <- plan_sondage |>
  summarise(
    N          = unweighted(sum(!is.na(age))),
    moyenne    = survey_mean(age, na.rm = TRUE, vartype = NULL),
    mediane    = survey_median(age, na.rm = TRUE, vartype = NULL),
    q1         = survey_quantile(age, quantiles = 0.25, na.rm = TRUE, vartype = NULL),
    q3         = survey_quantile(age, quantiles = 0.75, na.rm = TRUE, vartype = NULL),
    ecart_type = survey_sd(age, na.rm = TRUE, vartype = NULL),
    asymetrie  = unweighted(round(moments::skewness(age, na.rm = TRUE), 4))
  )

cat("Statistique pondérées sur l'âge\n")
print(stats_age)

sw <- shapiro.test(sample(age_vec, min(5000, length(age_vec))))
cat(sprintf("Shapiro-Wilk : W = %.4f, p < 0.001\n", sw$statistic))

# Figure 1 , l'histogramme de l'âge
fig01 <- ggplot(sect1_clean |> filter(!is.na(age)), aes(x = age)) +
  geom_histogram(binwidth = 5, fill = col_bleu, color = "white", alpha = 0.85) +
  geom_vline(
    xintercept = as.numeric(stats_age$mediane),
    linetype = "dashed", color = "#374EA2", linewidth = 0.8
  ) +
  annotate("text",
    x = as.numeric(stats_age$mediane) + 4,
    y = Inf, vjust = 2,
    label = paste0("Médiane = ", round(stats_age$mediane), " ans"),
    color = "#374EA2", size = 3, fontface = "italic"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title    = "Distribution de l'âge des membres des ménages",
    subtitle = paste0("GHS Panel W4 — n = ", format(length(age_vec), big.mark = " "),
                      " membres (s1q4a = 1 ou NA)"),
    x = "Âge (années)", y = "Effectif",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig01_histogramme_age.png"),
       fig01, width = 8, height = 4.5, dpi = 300)
cat("fig01 exportée\n")

# Figure 2 ,  le boxplot de l'âge
fig02 <- ggplot(sect1_clean |> filter(!is.na(age)), aes(x = age, y = "")) +
  geom_boxplot(fill = col_bleu, alpha = 0.6, outlier.color = "#E8416F",
               outlier.size = 0.8, outlier.alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title   = "Boîte à moustaches de l'âge",
    x = "Âge (années)", y = NULL,
    caption = source_ghs
  ) +
  theme(axis.text.y = element_blank())

ggsave(here("outputs", "figures", "fig02_boxplot_age.png"),
       fig02, width = 8, height = 2.5, dpi = 300)
cat("fig02 exportée\n")

# Combinons ces figures

# Supprimer les captions des figures individuelles avant assemblage
fig01_clean <- fig01 + labs(caption = NULL)
fig02_clean <- fig02 + labs(caption = NULL)

# Recréer fig02 verticalement avec des annotations
fig02_clean <- ggplot(sect1_clean |> filter(!is.na(age)), 
                      aes(x = "", y = age)) +
  geom_boxplot(fill = col_bleu, alpha = 0.65, color = "#374EA2",
               outlier.color = col_bleu, outlier.size = 0.5,
               outlier.alpha = 0.3, width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18,
               size = 3, color = "#E8416F") +
  annotate("text", x = 1.35, y = as.numeric(stats_age$mediane),
           label = paste0("Méd.=", round(stats_age$mediane)),
           color = "#374EA2", size = 2.8, hjust = 0, fontface = "bold") +
  annotate("text", x = 1.35, y = as.numeric(stats_age$q1_q25),
           label = paste0("Q1=", round(stats_age$q1_q25)),
           color = "grey30", size = 2.5, hjust = 0) +
  annotate("text", x = 1.35, y = as.numeric(stats_age$q3_q75),
           label = paste0("Q3=", round(stats_age$q3_q75)),
           color = "grey30", size = 2.5, hjust = 0) +
  annotate("text", x = 1.35, y = as.numeric(stats_age$moyenne),
           label = paste0("Moy.=", round(stats_age$moyenne)),
           color = "#E8416F", size = 2.5, hjust = 0) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  labs(
    title    = "Boîte à moustaches - Âge",
    subtitle = paste0("Q1=", round(stats_age$q1_q25),
                      " | Méd.=", round(stats_age$mediane),
                      " | Q3=", round(stats_age$q3_q75)),
    x = NULL, y = "Âge (années)"
  ) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

# QQ-plot
qq_data <- data.frame(
  theorique = qnorm(ppoints(length(age_vec))),
  observe   = sort(age_vec)
)

p_qq <- ggplot(qq_data, aes(x = theorique, y = observe)) +
  geom_point(color = col_bleu, size = 0.4, alpha = 0.4) +
  geom_abline(slope     = sd(age_vec),
              intercept = mean(age_vec),
              color = "#E8416F", linewidth = 0.9) +
  labs(
    title    = "QQ-plot - Âge vs distribution normale",
    subtitle = paste0("W = ", round(sw$statistic, 4),
                      " | p = ", formatC(sw$p.value, format = "e", digits = 2)),
    x = "Quantiles théoriques", y = "Quantiles observés"
  )

fig_01_02_combined <- (fig01_clean | fig02_clean | p_qq) +
  plot_annotation(
    title   = "Analyse univariée de l'âge",
    caption = source_ghs,
    theme   = theme(
      plot.title   = element_text(size = 11, face = "bold", hjust = 0),
      plot.caption = element_text(size = 7,  color = "grey50", hjust = 1)
    )
  ) &
  theme_minimal(base_size = 9) &
  theme(
    plot.title    = element_text(size = 9,   face = "bold"),
    plot.subtitle = element_text(size = 7.5, color = "grey40"),
    axis.title    = element_text(size = 8)
  )

ggsave(here("outputs", "figures", "fig_01_02_combined.png"),
       fig_01_02_combined, width = 14, height = 5.5, dpi = 200)
cat("fig_01_02_combined exportée\n")

#  2. la pyramide des âges 

pyramide_data <- sect1_clean |>
  filter(!is.na(age), !is.na(sexe), !is.na(groupe_age)) |>
  count(groupe_age, sexe, wt = wt_wave4) |>
  mutate(n = if_else(sexe == "Homme", -n, n))

fig03 <- ggplot(pyramide_data, aes(x = groupe_age, y = n, fill = sexe)) +
  geom_bar(stat = "identity", width = 0.85) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(abs(x) / 1e3, big.mark = " ", suffix = "k")
  ) +
  scale_fill_manual(values = c("Homme" = col_homme, "Femme" = col_femme)) +
  labs(
    title    = "Pyramide des âges par sexe (pondérée)",
    subtitle = "GHS Panel W4 — effectifs pondérés par wt_wave4 (poids ménage)",
    x = "Groupe d'âge", y = "Effectif pondéré (milliers)",
    fill = "Sexe",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig03_pyramide_ages.png"),
       fig03, width = 7, height = 8, dpi = 300)
cat("fig03 exportée\n")

# 3. Lien de parenté

parente_pondere <- plan_sondage |>
  filter(!is.na(parente)) |>
  group_by(parente) |>
  summarise(
    effectif_pondere = survey_total(vartype = "ci"),
    proportion       = survey_mean(vartype = "ci")
  ) |>
  arrange(desc(proportion))

cat("Parenté pondérée \n")
print(parente_pondere)

parente_plot <- sect1_clean |>
  filter(!is.na(parente)) |>
  count(parente, wt = wt_wave4, name = "n_pond") |>
  mutate(
    prop    = n_pond / sum(n_pond),
    pct_lbl = paste0(round(prop * 100, 1), "%")
  ) |>
  arrange(desc(prop))

fig04 <- ggplot(parente_plot, aes(x = reorder(parente, prop), y = prop)) +
  geom_col(fill = col_bleu, alpha = 0.85, width = 0.65) +
  geom_text(aes(label = pct_lbl), hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Composition des ménages par lien de parenté (pondérée)",
    subtitle = "Proportions issues des poids wt_wave4",
    x = NULL, y = "Proportion (%)",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig04_parente_barplot.png"),
       fig04, width = 7, height = 4, dpi = 300)
cat("fig04 exportée\n")

#  4. Comparaison rural-urbain sur la taille des ménages 
# Le rapport officiel indique une taille moyenne 5,9 (rural) et 4,8 (urbain)

stats_taille <- plan_menages |>
  filter(!is.na(milieu)) |>
  group_by(milieu) |>
  summarise(
    N          = unweighted(n()),
    moyenne    = survey_mean(taille_menage, vartype = NULL),
    mediane    = survey_median(taille_menage, vartype = NULL),
    q1         = survey_quantile(taille_menage, quantiles = 0.25, vartype = NULL),
    q3         = survey_quantile(taille_menage, quantiles = 0.75, vartype = NULL),
    ecart_type = survey_sd(taille_menage, vartype = NULL)
  )

cat(" Taille ménages par milieu (pondérée) \n")
print(stats_taille)


wx <- wilcox.test(
  taille_menage ~ milieu,
  data = menages |> filter(!is.na(milieu)),
  conf.int = TRUE
)
effet <- menages |>
  filter(!is.na(milieu)) |>
  rstatix::wilcox_effsize(taille_menage ~ milieu)

cat(sprintf("Wilcoxon W = %.0f, p < 0.001, r = %.3f\n",
            wx$statistic, effet$effsize))


# Figure 5 :le Boxplot de la taille des ménages
fig05 <- ggplot(menages |> filter(!is.na(milieu)),
                aes(x = milieu, y = taille_menage, fill = milieu)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.7, outlier.alpha = 0.3,
               width = 0.5) +
  scale_fill_manual(values = c("Urbain" = col_urbain, "Rural" = col_rural)) +
  annotate("text", x = 1.5, y = max(menages$taille_menage, na.rm = TRUE) * 0.95,
           label = sprintf("Wilcoxon p < 0,001\nr = %.3f (effet petit)",
                           effet$effsize),
           size = 3, color = "grey30") +
  labs(
    title    = "Taille des ménages selon le milieu de résidence",
    subtitle = "Un point par ménage — Test de Wilcoxon-Mann-Whitney",
    x = NULL, y = "Nombre de membres",
    caption = source_ghs
  ) +
  theme(legend.position = "none")

ggsave(here("outputs", "figures", "fig05_boxplot_taille_menage.png"),
       fig05, width = 5, height = 4.5, dpi = 300)
cat("fig05 exportée\n")

#  5. Un tableau gtsummary pondéré final

# Définir le tableau
tab_demo <- plan_sondage |>
  srvyr::filter(!is.na(milieu)) |>
  tbl_svysummary(
    by      = milieu,
    include = c(age, sexe, taille_menage),
    label   = list(
      age           ~ "Âge (années)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du ménage"
    ),
    statistic = list(
      all_continuous()  ~ "{median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = list(
      all_continuous()  ~ 0,
      all_categorical() ~ 0
    ),
    missing = "no"
  ) |>
  add_overall(last = FALSE) |>
  add_p() |>
  modify_header(
    label      ~ "Variable",
    all_stat_cols() ~ "**{level}**  \nn = {n}"
  ) |>
  modify_caption(
    "Caractéristiques démographiques par milieu de résidence (pondérées, wt_wave4). Statistiques pondérées ; effectifs d'échantillon entre parenthèses dans l'en-tête."
  )

# Fonction de mise en forme  pour flextable

style_lsms <- function(ft) {
  bleu_header <- "#1A3C5E"
  bleu_clair  <- "#DCE9F5"
  blanc       <- "#FFFFFF"
  gris_bord   <- "#A0A0A0"
  nrow_ft     <- nrow(ft$body$dataset)
  
  ft |>
    flextable::font(fontname = "Garamond", part = "all") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9.5, part = "header") |>
    flextable::bg(bg = bleu_header, part = "header") |>
    flextable::color(color = blanc, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = 1, align = "left", part = "header") |>
    flextable::bg(i = seq(1, nrow_ft, 2), bg = blanc,      part = "body") |>
    flextable::bg(i = seq(2, nrow_ft, 2), bg = bleu_clair, part = "body") |>
    flextable::align(j = 1, align = "left",    part = "body") |>
    flextable::align(j = 2:5, align = "center", part = "body") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all",
                            border = officer::fp_border(color = bleu_header, width = 1.5)) |>
    flextable::border_inner_h(part = "body",
                              border = officer::fp_border(color = gris_bord, width = 0.4)) |>
    flextable::hline(part = "header",
                     border = officer::fp_border(color = blanc, width = 0.6)) |>
    flextable::hline_bottom(part = "header",
                            border = officer::fp_border(color = bleu_header, width = 2)) |>
    flextable::width(j = 1, width = 1.8) |>
    flextable::width(j = 2:4, width = 1.4) |>
    flextable::width(j = 5, width = 0.9) |>
    flextable::hrule(rule = "exact", part = "body") |>
    flextable::height_all(height = 0.45, part = "body") |>
    flextable::height_all(height = 0.55, part = "header") |>
    flextable::padding(padding.left = 6, padding.right = 6,
                       padding.top = 3, padding.bottom = 3, part = "all")
}

# Export HTML pour une bonne visualisation
tab_demo |>
  as_gt() |>
  gt::gtsave(here("outputs", "tables", "tab_demo_gtsummary.html"))

# Export Word via flextable avec le style
tab_word <- tab_demo |>
  as_flex_table() |>
  style_lsms()

# Sauvegarder l'objet flextable pour le rapport
saveRDS(tab_word, here("data", "processed", "tab_demo_word.rds"))

tab_demo |>
  as_tibble() |>
  write.csv(here("outputs", "tables", "tab_demo_gtsummary.csv"),
            row.names = FALSE)


cat("Tableau gtsummary exporté\n")

# Sauvegarde pour le rapport
saveRDS(stats_age,       here("data", "processed", "stats_age.rds"))
saveRDS(parente_pondere, here("data", "processed", "parente_pondere.rds"))
saveRDS(stats_taille,    here("data", "processed", "stats_taille.rds"))
saveRDS(list(wx = wx, effet = effet),
        here("data", "processed", "wilcox_taille.rds"))
#Script 2 terminé
