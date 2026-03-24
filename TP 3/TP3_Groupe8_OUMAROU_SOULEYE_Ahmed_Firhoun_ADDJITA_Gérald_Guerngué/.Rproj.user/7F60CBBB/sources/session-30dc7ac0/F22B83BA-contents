# ================================================================ #
#   TP3 — ACCES AUX SOINS DE SANTE & CHOCS SANITAIRES            #
#   Nigeria GHS Panel — Wave 4 (2018)                             #
#   ENSAE ISE1 | Projet Statistique R & Python 2025-2026          #
#   Base sante    : sect4a_harvestw4.dta  (variables s4aq*)       #
#   Base individus: sect1_harvestw4.dta   (sexe, age)             #
#   Base poids    : totcons_final.dta     (wt_wave4, quintiles)   #
# ================================================================ #
# NOTES IMPORTANTES :                                              #
#  - La base SANTE est sect4a_harvestw4 (pas sect3a !)            #
#  - sect4a contient deja : sector (milieu), zone, state           #
#  - On joint sect1 uniquement pour s1q2 (sexe) et s1q4 (age)    #
#  - Poids wt_wave4 joint depuis totcons_final via hhid            #
# ================================================================ #

# ── 0. PACKAGES ─────────────────────────────────────────────────
packages <- c(
  "haven", "dplyr", "tidyr", "ggplot2", "naniar",
  "gtsummary", "rstatix", "patchwork", "scales",
  "forcats", "moments", "gt", "openxlsx",
  "knitr", "kableExtra", "labelled",
  "survey", "srvyr"
)
installed  <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0)
  install.packages(to_install, dependencies = TRUE,
                   repos = "https://cran.rstudio.com/")
invisible(lapply(packages, library, character.only = TRUE))


# ================================================================ #
#  PALETTE PERSONNALISEE (identique TP1)                          #
# ================================================================ #
col_ocre   <- "#B7472A"
col_savane <- "#5B8C5A"
col_ciel   <- "#1B6CA8"
col_lagune <- "#2AABB7"
col_aurore <- "#C45BAA"
col_miel   <- "#D4A017"
col_nuit   <- "#3D2B6B"
col_terre  <- "#7D5A3C"
col_argile <- "#C7956C"

pal_tp3 <- c(col_ocre, col_lagune, col_savane,
             col_miel, col_nuit, col_argile,
             col_aurore, col_ciel, col_terre)

# ── Thème ggplot (identique TP1) ────────────────────────────────
theme_tp3 <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15,
                                    color = "#1C1C2E",
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 11,
                                    color = "#4A4A6A",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 9,
                                    color = "#9A9AB0",
                                    hjust = 1),
    axis.title       = element_text(face = "bold", size = 11,
                                    color = "#1C1C2E"),
    axis.text        = element_text(size = 10,
                                    color = "#4A4A6A"),
    panel.grid.major = element_line(color = "#EBEBF0",
                                    linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "#FAFAFA",
                                    color = NA),
    panel.background = element_rect(fill = "#FFFFFF",
                                    color = NA),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 10),
    strip.text       = element_text(face = "bold", size = 11,
                                    color = "#1C1C2E"),
    strip.background = element_rect(fill = "#EBF3FA",
                                    color = NA),
    plot.margin      = margin(15, 15, 10, 15)
  )
theme_set(theme_tp3)

# ── Helpers GT (identiques TP1) ─────────────────────────────────
appliquer_style_gt <- function(gt_obj, n_lignes) {
  lg <- seq(2, n_lignes, 2)
  lg <- lg[lg <= n_lignes]
  if (length(lg) > 0)
    gt_obj <- gt_obj %>%
    tab_style(style     = cell_fill(color = "#EBF5FB"),
              locations = cells_body(rows = lg))
  gt_obj
}

freq_gt_quali <- function(data, var, nom_var,
                          titre, sous_titre,
                          col_bord = col_lagune) {
  df <- data %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}, name = "Effectif") %>%
    arrange(desc(Effectif)) %>%
    rename(Modalite = 1) %>%
    mutate(
      Ntot       = sum(Effectif),
      Proportion = round(Effectif / Ntot * 100, 2),
      IC_inf     = round(vapply(Effectif, function(x)
        binom.test(x, Ntot[1])$conf.int[1]*100,
        numeric(1)), 2),
      IC_sup     = round(vapply(Effectif, function(x)
        binom.test(x, Ntot[1])$conf.int[2]*100,
        numeric(1)), 2)
    ) %>%
    select(-Ntot) %>%
    mutate(Proportion = paste0(Proportion, " %"),
           IC_inf     = paste0(IC_inf, " %"),
           IC_sup     = paste0(IC_sup, " %"))
  
  n_val  <- nrow(data %>% filter(!is.na({{ var }})))
  n_miss <- sum(is.na(data %>% pull({{ var }})))
  nr     <- nrow(df)
  
  gt_obj <- df %>%
    gt() %>%
    tab_header(
      title    = md(paste0("**", titre, "**")),
      subtitle = md(paste0("*", sous_titre, "*<br>",
                           "*N valide = ",
                           format(n_val, big.mark = " "),
                           " | Manquants = ",
                           format(n_miss, big.mark = " "),
                           "*"))
    ) %>%
    cols_label(
      Modalite   = md(paste0("**", nom_var, "**")),
      Effectif   = md("**Effectif**"),
      Proportion = md("**Proportion**"),
      IC_inf     = md("**IC inf. 95 %**"),
      IC_sup     = md("**IC sup. 95 %**")
    ) %>%
    cols_align("left",   columns = Modalite) %>%
    cols_align("center",
               columns = c(Effectif, Proportion,
                           IC_inf, IC_sup)) %>%
    grand_summary_rows(
      columns = Effectif,
      fns     = list(Total ~ sum(.)),
      fmt     = ~ fmt_integer(., use_seps = TRUE)) %>%
    tab_style(
      style = list(cell_fill(color = "#1C1C2E"),
                   cell_text(color = "white",
                             weight = "bold")),
      locations = cells_column_labels()) %>%
    tab_style(
      style = list(cell_fill(color = "#F0F4F8"),
                   cell_text(weight = "bold")),
      locations = cells_grand_summary()) %>%
    tab_options(
      table.width = pct(80), table.font.size = 13,
      heading.title.font.size    = 16,
      heading.subtitle.font.size = 12,
      table.border.top.width     = px(3),
      table.border.top.color     = col_bord,
      table.border.bottom.width  = px(2),
      table.border.bottom.color  = col_bord,
      column_labels.background.color = "#1C1C2E") %>%
    tab_source_note(md(
      "*IC 95% : binom.test | Source : World Bank LSMS-ISA*"))
  
  appliquer_style_gt(gt_obj, nr)
}

cat("\n", strrep("=", 64), "\n")
cat("  TP3 — SANTE, MORBIDITE & RECOURS AUX SOINS\n")
cat("  Nigeria GHS Panel — Wave 4 (2018)\n")
cat(strrep("=", 64), "\n\n")

# ================================================================ #
#  CHARGEMENT DES BASES                                           #
# ================================================================ #
cat("-- CHARGEMENT DES BASES --------------------------------\n\n")

# ► Adapter les chemins si nécessaire
# IMPORTANT : base santé = sect4a_harvestw4 (pas sect3a !)
sect4a <- read_dta("data/raw/sect4a_harvestw4.dta")
sect1  <- read_dta("data/raw/sect1_harvestw4.dta")
cons   <- read_dta("data/raw/totcons_final.dta")

cat("sect4a_harvestw4 :", nrow(sect4a), "obs. x",
    ncol(sect4a), "variables\n")
cat("sect1_harvestw4  :", nrow(sect1),  "obs. x",
    ncol(sect1),  "variables\n")
cat("totcons_final    :", nrow(cons),   "obs. x",
    ncol(cons),   "variables\n\n")

# ── Poids et quintiles depuis cons ──────────────────────────────
poids_cons <- cons %>%
  select(hhid, wt_wave4, totcons_adj) %>%
  filter(!is.na(totcons_adj), totcons_adj > 0) %>%
  mutate(
    quintile_num      = ntile(totcons_adj, 5),
    Quintile_richesse = factor(quintile_num,
                               levels = 1:5,
                               labels = c("Q1 (Pauvres)", "Q2", "Q3",
                                          "Q4", "Q5 (Riches)"))
  ) %>%
  distinct(hhid, .keep_all = TRUE)

# ── Sexe et age depuis sect1 (renommés pour éviter conflits) ────
# sect4a a déjà : sector, zone, state, hhid, indiv
# On prend seulement s1q2 (sexe) et s1q4 (age) depuis sect1
sect1_reduit <- sect1 %>%
  select(hhid, indiv,
         Sexe_brut = s1q2,   # renommé tout de suite
         Age       = s1q4)   # renommé tout de suite

# ================================================================ #
#  TACHE 13 — STRUCTURE DES BASES & TAUX DE MORBIDITE            #
# ================================================================ #
cat("-- TACHE 13 : Structure des bases & morbidite ---------\n\n")

# ── 13.0 Structure des bases ─────────────────────────────────────
cat("13.0 Structure des bases :\n")
cat(strrep("-", 50), "\n")

cat("BASE 1 : sect4a_harvestw4.dta (BASE SANTE)\n")
cat("  Dimensions    :", nrow(sect4a), "lignes x",
    ncol(sect4a), "colonnes\n")
cat("  Unite stat.   : Individu (membre de menage)\n")
cat("  Cle           : hhid + indiv\n")
cat("  Menages uniq. :", n_distinct(sect4a$hhid), "\n")
cat("  Contient      : sector (milieu), zone, state\n\n")
str(sect4a, max.level = 1)

cat("\n", strrep("-", 50), "\n")
cat("BASE 2 : totcons_final.dta\n")
cat("  Dimensions    :", nrow(cons), "lignes x",
    ncol(cons), "colonnes\n")
cat("  Unite stat.   : Menage\n")
cat("  Poids         : wt_wave4\n")
cat("  Conso. ajust. : totcons_adj\n\n")

# Dictionnaire sect4a
cat("Dictionnaire — sect4a_harvestw4 :\n\n")
dico_s4a <- tibble(
  N        = seq_len(ncol(sect4a)),
  Variable = names(sect4a),
  Label    = sapply(sect4a, function(x) {
    l <- attr(x, "label")
    if (is.null(l)) "— (pas de label)" else as.character(l)
  }),
  Type     = sapply(sect4a, function(x) class(x)[1])
)
print(dico_s4a, n = Inf)

# ── 13.1 Jointure des bases ──────────────────────────────────────
cat("\n13.1 Jointure des bases :\n")

# sect4a → joint sect1_reduit (sexe + age) + poids/quintiles
# NB : sector (milieu) est déjà dans sect4a, pas besoin de le joindre
data_sante <- sect4a %>%
  left_join(sect1_reduit, by = c("hhid", "indiv")) %>%
  left_join(
    poids_cons %>% select(hhid, wt_wave4,
                          totcons_adj,
                          Quintile_richesse),
    by = "hhid"
  ) %>%
  mutate(
    # Milieu depuis sect4a (sector est dans sect4a)
    Milieu_label = factor(sector,
                          levels = c(1, 2),
                          labels = c("Rural", "Urbain")),
    # Sexe depuis sect1
    Sexe_label   = factor(Sexe_brut,
                          levels = c(1, 2),
                          labels = c("Homme", "Femme")),
    # Poids
    Poids_menage = wt_wave4,
    # Groupe d'age depuis sect1
    Groupe_age   = cut(Age,
                       breaks = c(0, 5, 15, 30, 45, 60, Inf),
                       labels = c("0-4 ans","5-14 ans","15-29 ans",
                                  "30-44 ans","45-59 ans","60+ ans"),
                       right  = FALSE),
    # Variables santé
    Malade       = factor(s4aq3, levels = c(1, 2),
                          labels = c("Oui", "Non")),
    Consulte     = factor(s4aq1, levels = c(1, 2),
                          labels = c("Oui", "Non")),
    Hospitalise  = factor(s4aq15, levels = c(1, 2),
                          labels = c("Oui", "Non")),
    Prestataire  = as_factor(s4aq6a),
    Type_maladie = as_factor(s4aq3b_1),
    # Dépense totale = consultation + transport + médicaments
    Depense_totale = rowSums(
      cbind(
        ifelse(is.na(s4aq9),  0, as.numeric(s4aq9)),
        ifelse(is.na(s4aq10), 0, as.numeric(s4aq10)),
        ifelse(is.na(s4aq14), 0, as.numeric(s4aq14))
      ), na.rm = TRUE),
    Depense_totale = ifelse(Depense_totale == 0,
                            NA_real_, Depense_totale)
  )

cat("Dimensions apres jointure :", nrow(data_sante),
    "x", ncol(data_sante), "\n")
cat("Avec sexe valide          :",
    sum(!is.na(data_sante$Sexe_label)), "\n")
cat("Avec age valide           :",
    sum(!is.na(data_sante$Age)), "\n")
cat("Avec milieu valide        :",
    sum(!is.na(data_sante$Milieu_label)), "\n")
cat("Avec poids (wt_wave4)     :",
    sum(!is.na(data_sante$Poids_menage)), "\n")
cat("Avec quintile richesse    :",
    sum(!is.na(data_sante$Quintile_richesse)), "\n\n")

# ── 13.2 Valeurs manquantes → Excel ──────────────────────────────
cat("13.2 Valeurs manquantes :\n")

vars_cles_s <- c("Malade", "Consulte", "Prestataire",
                 "Type_maladie", "Depense_totale",
                 "Hospitalise", "Sexe_label",
                 "Milieu_label", "Groupe_age",
                 "Quintile_richesse")

miss_s4 <- data_sante %>%
  select(all_of(vars_cles_s)) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "Variable",
               values_to = "N_manquants") %>%
  mutate(
    N_total       = nrow(data_sante),
    N_valides     = N_total - N_manquants,
    Pct_manquants = round(N_manquants / N_total * 100, 2),
    Pct_valides   = round(N_valides   / N_total * 100, 2)
  ) %>%
  arrange(desc(N_manquants)) %>%
  select(Variable, N_total, N_valides,
         Pct_valides, N_manquants, Pct_manquants)

cat("Variables cles — valeurs manquantes :\n")
print(miss_s4)

# Export Excel → output/tables/
wb <- createWorkbook()
addWorksheet(wb, "Valeurs_manquantes")
sty_hdr <- createStyle(
  fontName = "Calibri", fontSize = 12,
  fontColour = "white", fgFill = "#1C1C2E",
  halign = "CENTER", textDecoration = "bold",
  border = "TopBottomLeftRight", wrapText = TRUE)
sty_pair   <- createStyle(fgFill = "#EBF5FB",
                          border = "TopBottomLeftRight")
sty_impair <- createStyle(fgFill = "white",
                          border = "TopBottomLeftRight")

writeData(wb, "Valeurs_manquantes",
          paste0("Valeurs manquantes | sect4a_harvestw4 | ",
                 "GHS Panel Wave 4 (2018) | N = ",
                 format(nrow(data_sante), big.mark = " ")),
          startRow = 1, startCol = 1)
addStyle(wb, "Valeurs_manquantes",
         createStyle(fontSize = 13,
                     textDecoration = "bold",
                     fontColour = "#1C1C2E"),
         rows = 1, cols = 1)

en_t <- c("Variable", "N total", "N valides",
          "% valides", "N manquants", "% manquants")
writeData(wb, "Valeurs_manquantes",
          as.data.frame(t(en_t)),
          startRow = 2, startCol = 1,
          colNames = FALSE)
addStyle(wb, "Valeurs_manquantes", sty_hdr,
         rows = 2, cols = 1:6, gridExpand = TRUE)
writeData(wb, "Valeurs_manquantes", miss_s4,
          startRow = 3, startCol = 1,
          colNames = FALSE)
for (i in seq_len(nrow(miss_s4))) {
  s <- if (i %% 2 == 0) sty_pair else sty_impair
  addStyle(wb, "Valeurs_manquantes", s,
           rows = i + 2, cols = 1:6,
           gridExpand = TRUE)
}
setColWidths(wb, "Valeurs_manquantes", 1:6,
             c(25, 10, 10, 10, 13, 13))
freezePane(wb, "Valeurs_manquantes",
           firstActiveRow = 3)
saveWorkbook(wb, "output/tables/TP3_T13_valeurs_manquantes.xlsx",
             overwrite = TRUE)
cat("OK — output/tables/TP3_T13_valeurs_manquantes.xlsx sauvegarde.\n")

# Carte des manquants → output/figures/
p_miss_s4 <- vis_miss(
  select(data_sante, all_of(vars_cles_s)),
  warn_large_data = FALSE
) +
  labs(
    title    = "Carte des valeurs manquantes — Variables cles sante",
    subtitle = paste0("sect4a_harvestw4 | N = ",
                      format(nrow(data_sante), big.mark = " ")),
    caption  = "Source : World Bank LSMS-ISA"
  )
ggsave("output/figures/TP3_T13_carte_manquantes.png", p_miss_s4,
       width = 10, height = 5, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T13_carte_manquantes.png sauvegarde.\n")

# ── 13.3 Taux de morbidite ───────────────────────────────────────
cat("\n13.3 Taux de morbidite :\n")

morb_data <- data_sante %>%
  filter(!is.na(s4aq3), !is.na(Groupe_age),
         !is.na(Sexe_label))

# Taux global non pondéré
taux_global_np <- mean(morb_data$s4aq3 == 1,
                       na.rm = TRUE)
cat("  Taux global (non pondere) :",
    round(taux_global_np * 100, 1), "%\n")

# Taux global pondéré
morb_pond <- morb_data %>%
  filter(!is.na(Poids_menage)) %>%
  mutate(Malade_num = as.integer(s4aq3 == 1))

if (nrow(morb_pond) > 0) {
  design_morb <- morb_pond %>%
    as_survey_design(weights = Poids_menage)
  taux_global_p <- as.numeric(
    svymean(~Malade_num, design_morb))
  cat("  Taux global (pondere wt_wave4) :",
      round(taux_global_p * 100, 1), "%\n")
}

# Taux par sexe × groupe d'age avec IC 95%
taux_morb <- morb_data %>%
  group_by(Sexe_label, Groupe_age) %>%
  summarise(
    N_total  = n(),
    N_malade = sum(s4aq3 == 1, na.rm = TRUE),
    Taux     = N_malade / N_total,
    IC_inf   = binom.test(N_malade,
                          N_total)$conf.int[1],
    IC_sup   = binom.test(N_malade,
                          N_total)$conf.int[2],
    .groups  = "drop"
  )

cat("\n  Taux par sexe et groupe d'age :\n")
print(taux_morb %>%
        mutate(
          Taux   = paste0(round(Taux*100, 1), "%"),
          IC_inf = paste0(round(IC_inf*100, 1), "%"),
          IC_sup = paste0(round(IC_sup*100, 1), "%")))

# Barplot taux de morbidite → output/figures/
p_morb <- ggplot(taux_morb,
                 aes(x = Groupe_age, y = Taux * 100,
                     fill = Sexe_label, group = Sexe_label)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.68, alpha = 0.90) +
  geom_errorbar(
    aes(ymin = IC_inf * 100,
        ymax = IC_sup * 100),
    position = position_dodge(width = 0.75),
    width = 0.25, linewidth = 0.9,
    color = "#1C1C2E") +
  geom_text(
    aes(label = paste0(round(Taux*100, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -1.5, size = 3.2,
    fontface = "bold", color = "#1C1C2E") +
  scale_fill_manual(
    values = c("Homme" = col_lagune,
               "Femme" = col_aurore),
    name = "Sexe") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0,
               max(taux_morb$IC_sup) * 100 * 1.30),
    expand = c(0, 0)) +
  labs(
    title    = "Taux de morbidite par sexe et groupe d'age",
    subtitle = paste0(
      "Part ayant declare une maladie/blessure ",
      "(4 dernieres semaines) | IC 95 % | GHS Panel W4 (2018)"),
    x       = "Groupe d'age",
    y       = "Taux de morbidite (%)",
    caption = "Source : World Bank LSMS-ISA | sect4a_harvestw4")
ggsave("output/figures/TP3_T13_taux_morbidite.png", p_morb,
       width = 13, height = 7, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T13_taux_morbidite.png sauvegarde.\n")

# ================================================================ #
#  TACHE 14 — TYPES DE MALADIES DECLARES                         #
# ================================================================ #
cat("\n-- TACHE 14 : Types de maladies declares --------------\n\n")

cat("Modalites de s4aq3b_1 :\n")
print(table(data_sante$s4aq3b_1, useNA = "ifany"))
cat("\nEtiquettes de valeur :\n")
print(attr(data_sante$s4aq3b_1, "labels"))

# Parmi les individus déclarant une maladie
maladies_data <- data_sante %>%
  filter(s4aq3 == 1, !is.na(s4aq3b_1)) %>%
  mutate(Type_maladie = as_factor(s4aq3b_1))

n_malades <- nrow(maladies_data)
cat("\nN individus malades avec type declare :",
    n_malades, "\n")

freq_maladies <- maladies_data %>%
  count(Type_maladie, name = "Effectif") %>%
  arrange(desc(Effectif)) %>%
  slice_head(n = 10) %>%
  mutate(
    N_tot        = sum(Effectif),
    Proportion   = round(Effectif / N_tot * 100, 1),
    Type_maladie = fct_reorder(Type_maladie, Effectif),
    label_text   = paste0(Proportion, "%\n(n=",
                          format(Effectif,
                                 big.mark = " "), ")")
  )

cat("\nTop 10 maladies declares :\n")
print(select(freq_maladies, Type_maladie,
             Effectif, Proportion))

# Catégorisation basée sur les libellés réels
freq_maladies <- freq_maladies %>%
  mutate(
    Categorie = case_when(
      grepl(paste0("malaria|fever|typhoid|diarrhea|",
                   "respirat|cold|cough|infection|",
                   "cholera|tuberculosis|hiv|aids"),
            tolower(as.character(Type_maladie))) ~
        "Infectieuse",
      grepl(paste0("injury|accident|fracture|",
                   "wound|burn|trauma|fall"),
            tolower(as.character(Type_maladie))) ~
        "Traumatique",
      grepl(paste0("diabetes|hypertension|heart|",
                   "stroke|cancer|asthma|chronic|",
                   "arthritis|kidney|liver"),
            tolower(as.character(Type_maladie))) ~
        "Chronique",
      TRUE ~ "Autre"
    )
  )

pal_cat <- c("Infectieuse" = col_ocre,
             "Traumatique" = col_miel,
             "Chronique"   = col_nuit,
             "Autre"       = col_argile)

p_maladies <- ggplot(freq_maladies,
                     aes(x = Type_maladie, y = Proportion,
                         fill = Categorie)) +
  geom_col(width = 0.72, alpha = 0.90) +
  geom_text(aes(label = label_text),
            hjust = -0.08, size = 3.2,
            fontface = "bold", color = "#1C1C2E") +
  scale_fill_manual(values = pal_cat,
                    name = "Categorie") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0,
               max(freq_maladies$Proportion) * 1.60),
    expand = c(0, 0)) +
  coord_flip() +
  labs(
    title    = "Top 10 affections declarees (4 dernieres semaines)",
    subtitle = paste0(
      "Parmi les individus malades | ",
      "GHS Panel Wave 4 (2018)"),
    x       = NULL,
    y       = "Proportion (%)",
    caption = "Source : World Bank LSMS-ISA | sect4a_harvestw4"
  ) +
  theme(panel.grid.major.y = element_blank())
ggsave("output/figures/TP3_T14_types_maladies.png", p_maladies,
       width = 13, height = 7, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T14_types_maladies.png sauvegarde.\n")

# ================================================================ #
#  TACHE 15 — RECOURS AUX SOINS PAR TYPE DE PRESTATAIRE          #
# ================================================================ #
cat("\n-- TACHE 15 : Recours aux soins par prestataire -------\n\n")

cat("Modalites de s4aq6a :\n")
print(table(data_sante$s4aq6a, useNA = "ifany"))
cat("\nEtiquettes :\n")
print(attr(data_sante$s4aq6a, "labels"))

recours_data <- data_sante %>%
  filter(!is.na(s4aq6a)) %>%
  mutate(Prestataire = as_factor(s4aq6a))

freq_prest <- recours_data %>%
  count(Prestataire, name = "Effectif") %>%
  arrange(desc(Effectif)) %>%
  mutate(
    N_tot       = sum(Effectif),
    Proportion  = round(Effectif / N_tot * 100, 1),
    IC_inf      = round(vapply(Effectif, function(x)
      binom.test(x, N_tot[1])$conf.int[1]*100,
      numeric(1)), 1),
    IC_sup      = round(vapply(Effectif, function(x)
      binom.test(x, N_tot[1])$conf.int[2]*100,
      numeric(1)), 1),
    Prestataire = fct_reorder(Prestataire, Effectif),
    label_text  = paste0(Proportion, "%\n(n=",
                         format(Effectif,
                                big.mark = " "), ")")
  )

cat("\nFrequences par prestataire :\n")
print(select(freq_prest, Prestataire, Effectif,
             Proportion, IC_inf, IC_sup))

# Tableau GT T15 → output/tables/
nr_prest <- nrow(freq_prest)
gt_prest <- freq_prest %>%
  select(Prestataire, Effectif, Proportion,
         IC_inf, IC_sup) %>%
  mutate(
    Proportion = paste0(Proportion, " %"),
    IC_inf     = paste0(IC_inf, " %"),
    IC_sup     = paste0(IC_sup, " %")) %>%
  gt() %>%
  tab_header(
    title    = md(
      "**Recours aux soins par type de prestataire**"),
    subtitle = md(paste0(
      "*sect4a_harvestw4 | GHS Panel Wave 4 (2018)*<br>",
      "*N valide = ",
      format(nrow(recours_data), big.mark = " "),
      " individus ayant consulte*"))
  ) %>%
  cols_label(
    Prestataire = md("**Type de prestataire**"),
    Effectif    = md("**Effectif**"),
    Proportion  = md("**Proportion**"),
    IC_inf      = md("**IC inf. 95 %**"),
    IC_sup      = md("**IC sup. 95 %**")
  ) %>%
  cols_align("left",  columns = Prestataire) %>%
  cols_align("center",
             columns = c(Effectif, Proportion,
                         IC_inf, IC_sup)) %>%
  grand_summary_rows(
    columns = Effectif,
    fns     = list(Total ~ sum(.)),
    fmt     = ~ fmt_integer(., use_seps = TRUE)) %>%
  tab_style(
    style = list(cell_fill(color = "#1C1C2E"),
                 cell_text(color = "white",
                           weight = "bold")),
    locations = cells_column_labels()) %>%
  tab_style(
    style = list(cell_fill(color = "#F0F4F8"),
                 cell_text(weight = "bold")),
    locations = cells_grand_summary()) %>%
  tab_style(
    style = cell_text(weight = "bold",
                      color = col_ocre),
    locations = cells_body(
      columns = Proportion,
      rows    = Effectif == max(Effectif))) %>%
  tab_options(
    table.width = pct(82), table.font.size = 13,
    heading.title.font.size    = 16,
    heading.subtitle.font.size = 12,
    table.border.top.width     = px(3),
    table.border.top.color     = col_savane,
    table.border.bottom.width  = px(2),
    table.border.bottom.color  = col_savane,
    column_labels.background.color = "#1C1C2E") %>%
  tab_source_note(md(
    "*IC 95% : binom.test | Source : World Bank LSMS-ISA*"))
gt_prest <- appliquer_style_gt(gt_prest, nr_prest)
gtsave(gt_prest, "output/tables/TP3_T15_prestataires_gt.html")
cat("OK — output/tables/TP3_T15_prestataires_gt.html sauvegarde.\n")

# Barplot prestataires → output/figures/
n_niv_prest <- nrow(freq_prest)
p_prest <- ggplot(freq_prest,
                  aes(x = Prestataire, y = Proportion,
                      fill = Prestataire)) +
  geom_col(width = 0.70, alpha = 0.90,
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = IC_inf,
                    ymax = IC_sup),
                width = 0.25, linewidth = 0.9,
                color = "#1C1C2E") +
  geom_text(aes(label = label_text),
            hjust = -0.08, size = 3.2,
            fontface = "bold", color = "#1C1C2E") +
  scale_fill_manual(
    values = setNames(
      colorRampPalette(pal_tp3)(n_niv_prest),
      levels(freq_prest$Prestataire))) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(freq_prest$IC_sup) * 1.55),
    expand = c(0, 0)) +
  coord_flip() +
  labs(
    title    = "Recours aux soins par type de prestataire",
    subtitle = paste0(
      "Ordonne par frequence | IC 95 % | GHS Panel W4 (2018)"),
    x       = NULL,
    y       = "Proportion (%)",
    caption = "Source : World Bank LSMS-ISA") +
  theme(panel.grid.major.y = element_blank())
ggsave("output/figures/TP3_T15_recours_prestataire.png", p_prest,
       width = 13, height = 7, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T15_recours_prestataire.png sauvegarde.\n")

# ================================================================ #
#  TACHE 16 — DISTRIBUTION DES DEPENSES DE SANTE                 #
# ================================================================ #
cat("\n-- TACHE 16 : Distribution des depenses de sante ------\n\n")

dep_data <- data_sante %>%
  filter(!is.na(Depense_totale), Depense_totale > 0)

cat("N individus avec depenses > 0 :",
    nrow(dep_data), "\n")

# Déciles
deciles_dep <- dep_data %>%
  reframe(
    Decile  = paste0("D", 1:10),
    Montant = format(
      round(quantile(Depense_totale,
                     probs = seq(0.1, 1, 0.1),
                     na.rm = TRUE)),
      big.mark = " ")
  )
cat("\nDepenses par decile (Nairas) :\n")
print(deciles_dep)

# Outliers (IQR x 3)
q1_d  <- quantile(dep_data$Depense_totale, 0.25)
q3_d  <- quantile(dep_data$Depense_totale, 0.75)
iqr_d <- q3_d - q1_d
n_out <- sum(dep_data$Depense_totale >
               q3_d + 3 * iqr_d, na.rm = TRUE)
cat("\nOutliers (> Q3 + 3*IQR) :", n_out,
    "(", round(n_out / nrow(dep_data) * 100, 1), "%)\n")

# Tableau GT statistiques dépenses → output/tables/
med_dep  <- median(dep_data$Depense_totale)
moy_dep  <- mean(dep_data$Depense_totale)

stats_dep <- tibble(
  Groupe = c(
    rep("Effectifs", 2),
    rep("Tendance centrale", 2),
    rep("Dispersion", 4),
    rep("Valeurs extremes", 2)
  ),
  Statistique = c(
    "N individus avec depenses",
    "N individus sans depenses / manquants",
    "Moyenne (Nairas)",
    "Mediane (Nairas)",
    "Ecart-type",
    "Q1 (25e perc.)",
    "Q3 (75e perc.)",
    "IQR",
    "Minimum",
    "Maximum"
  ),
  Valeur = c(
    format(nrow(dep_data), big.mark = " "),
    format(sum(is.na(data_sante$Depense_totale)),
           big.mark = " "),
    format(round(moy_dep), big.mark = " "),
    format(round(med_dep), big.mark = " "),
    format(round(sd(dep_data$Depense_totale)),
           big.mark = " "),
    format(round(q1_d), big.mark = " "),
    format(round(q3_d), big.mark = " "),
    format(round(iqr_d), big.mark = " "),
    format(round(min(dep_data$Depense_totale)),
           big.mark = " "),
    format(round(max(dep_data$Depense_totale)),
           big.mark = " ")
  )
)

nr_dep <- nrow(stats_dep)
gt_dep_stats <- stats_dep %>%
  gt(groupname_col = "Groupe") %>%
  tab_header(
    title    = md(
      "**Statistiques des depenses de sante**"),
    subtitle = md(paste0(
      "*sect4a_harvestw4 | GHS Panel Wave 4 (2018)*<br>",
      "*Depense = consultation + transport + medicaments*"))
  ) %>%
  cols_label(
    Statistique = md("**Statistique**"),
    Valeur      = md("**Depenses (Nairas)**")) %>%
  cols_align("left",   columns = Statistique) %>%
  cols_align("center", columns = Valeur) %>%
  tab_style(
    style = list(cell_fill(color = "#1C1C2E"),
                 cell_text(color = "white",
                           weight = "bold")),
    locations = cells_column_labels()) %>%
  tab_style(
    style = list(cell_fill(color = "#EBF5FB"),
                 cell_text(color = "#1C1C2E",
                           weight = "bold",
                           size = px(12))),
    locations = cells_row_groups()) %>%
  tab_options(
    table.width = pct(72), table.font.size = 13,
    heading.title.font.size    = 16,
    heading.subtitle.font.size = 12,
    table.border.top.width     = px(3),
    table.border.top.color     = col_miel,
    table.border.bottom.width  = px(2),
    table.border.bottom.color  = col_miel,
    row_group.background.color = "#EBF5FB",
    column_labels.background.color = "#1C1C2E") %>%
  tab_source_note(md(
    "*Source : World Bank LSMS-ISA | ENSAE ISE1 2025-2026*"))
gt_dep_stats <- appliquer_style_gt(gt_dep_stats, nr_dep)
gtsave(gt_dep_stats, "output/tables/TP3_T16_stats_depenses.html")
cat("OK — output/tables/TP3_T16_stats_depenses.html sauvegarde.\n")

# Histogramme échelle log → output/figures/
p_hist_dep <- ggplot(dep_data,
                     aes(x = Depense_totale)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = col_savane, color = "white",
                 alpha = 0.85, bins = 40) +
  geom_density(color = col_ocre, linewidth = 1.3) +
  geom_vline(xintercept = med_dep,
             color = col_nuit,
             linetype = "dashed", linewidth = 1.1) +
  annotate("text",
           x = med_dep * 3, y = Inf, vjust = 1.5,
           label = paste0("Med. = ",
                          format(round(med_dep),
                                 big.mark = " "),
                          " N"),
           color = col_nuit,
           fontface = "bold", size = 4) +
  scale_x_log10(labels = label_comma(suffix = " N")) +
  labs(
    title    = "Distribution des depenses de sante (echelle log)",
    subtitle = paste0(
      "N = ", format(nrow(dep_data), big.mark = " "),
      " | Courbe = densite | GHS Panel W4 (2018)"),
    x       = "Depenses totales de sante (Nairas, log10)",
    y       = "Densite",
    caption = "Source : World Bank LSMS-ISA")

# Boxplot par prestataire
dep_prest <- data_sante %>%
  filter(!is.na(Depense_totale),
         Depense_totale > 0,
         !is.na(s4aq6a)) %>%
  mutate(Prestataire = as_factor(s4aq6a))

n_prest_dep <- n_distinct(dep_prest$Prestataire,
                          na.rm = TRUE)

dep_prest <- dep_prest %>%
  mutate(Prest_ord = fct_reorder(Prestataire,
                                 Depense_totale,
                                 .fun    = median,
                                 .na_rm  = TRUE))

n_prest_dep <- nlevels(dep_prest$Prest_ord)
pal_prest   <- colorRampPalette(pal_tp3)(n_prest_dep)

suppressWarnings({
  p_box_dep <- ggplot(dep_prest,
                      aes(x = Prest_ord,
                          y = Depense_totale,
                          fill = Prest_ord)) +
    geom_violin(alpha = 0.28, color = NA,
                trim = TRUE) +
    geom_boxplot(width = 0.38,
                 outlier.shape = 21,
                 outlier.alpha = 0.3,
                 outlier.size  = 1.5,
                 linewidth     = 0.8) +
    scale_fill_manual(
      values = setNames(pal_prest,
                        levels(dep_prest$Prest_ord)),
      guide  = "none") +
    scale_y_log10(labels = label_comma(suffix = " N")) +
    coord_flip() +
    labs(
      title    = "Depenses de sante par type de prestataire",
      subtitle = "Mediane annotee | Echelle log | GHS Panel W4 (2018)",
      x       = NULL,
      y       = "Depenses (Nairas, log10)",
      caption = "Source : World Bank LSMS-ISA")
})

p_t16 <- p_hist_dep / p_box_dep +
  plot_annotation(
    title = "Tache 16 — Distribution des depenses de sante",
    theme = theme(plot.title = element_text(
      face = "bold", size = 14,
      color = "#1C1C2E")))
ggsave("output/figures/TP3_T16_depenses_sante.png", p_t16,
       width = 13, height = 11, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T16_depenses_sante.png sauvegarde.\n")

# ================================================================ #
#  TACHE 17 — RECOURS AUX SOINS × QUINTILE DE RICHESSE           #
# ================================================================ #
cat("\n-- TACHE 17 : Recours aux soins x Quintile ------------\n\n")

quint_data <- data_sante %>%
  filter(!is.na(s4aq1),
         !is.na(Quintile_richesse)) %>%
  mutate(
    Consulte = factor(s4aq1,
                      levels = c(1, 2),
                      labels = c("Consulte",
                                 "Non consulte"))
  )

cat("N individus avec quintile + consultation :",
    nrow(quint_data), "\n")

tab_quint <- table(quint_data$Consulte,
                   quint_data$Quintile_richesse)
cat("\nTableau Consultation x Quintile :\n")
print(addmargins(tab_quint))

# Test approprié
chi2_q    <- suppressWarnings(chisq.test(tab_quint))
min_exp_q <- min(chi2_q$expected)
cat("Effectif attendu minimum :",
    round(min_exp_q, 2), "\n")

if (min_exp_q < 5) {
  fish_q  <- fisher.test(tab_quint,
                         simulate.p.value = TRUE,
                         B = 10000)
  p_val_q <- fish_q$p.value
  test_q  <- "Fisher exact"
} else {
  p_val_q <- chi2_q$p.value
  test_q  <- "Chi-deux"
  cat("Chi-deux : chi2 =",
      round(chi2_q$statistic, 3),
      "| p =", format(p_val_q, digits = 4), "\n")
}
cat(test_q, ": p =",
    format(p_val_q, digits = 4), "\n")

# V de Cramér
n_q    <- sum(tab_quint)
chi2_v <- suppressWarnings(
  chisq.test(tab_quint)$statistic)
v_q    <- if (!is.nan(chi2_v) && chi2_v > 0) {
  round(sqrt(chi2_v /
               (n_q * (min(dim(tab_quint)) - 1))), 4)
} else {
  NA_real_
}
cat("V de Cramer =",
    ifelse(is.na(v_q), "N/A", v_q), "\n")

# Taux de consultation par quintile
taux_consul_q <- quint_data %>%
  group_by(Quintile_richesse) %>%
  summarise(
    N      = n(),
    N_oui  = sum(s4aq1 == 1, na.rm = TRUE),
    Taux   = round(N_oui / N * 100, 1),
    IC_inf = round(
      binom.test(N_oui, N)$conf.int[1] * 100, 1),
    IC_sup = round(
      binom.test(N_oui, N)$conf.int[2] * 100, 1),
    .groups = "drop"
  )
cat("\nTaux de consultation par quintile :\n")
print(taux_consul_q)

# Barplot 100% empilé → output/figures/
prop_quint <- quint_data %>%
  count(Quintile_richesse, Consulte) %>%
  group_by(Quintile_richesse) %>%
  mutate(prop  = n / sum(n),
         label = paste0(round(prop * 100, 0), "%")) %>%
  ungroup()

annot_q <- paste0(
  test_q, " : p = ",
  format(p_val_q, digits = 3),
  if (!is.na(v_q)) {
    paste0(" | V Cramer = ", v_q)
  } else {
    ""
  })

p_quint <- ggplot(prop_quint,
                  aes(x = Quintile_richesse, y = prop,
                      fill = Consulte)) +
  geom_col(position = "fill", width = 0.65,
           alpha = 0.90) +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            size = 3.8, fontface = "bold",
            color = "white") +
  scale_fill_manual(
    values = c("Consulte"     = col_savane,
               "Non consulte" = col_ocre),
    name = "Recours aux soins") +
  scale_y_continuous(labels = percent_format()) +
  annotate("text", x = 3, y = 1.07,
           label = annot_q, size = 3.5,
           color = "#4A4A6A",
           fontface = "italic", hjust = 0.5) +
  labs(
    title    = "Recours aux soins selon le quintile de richesse",
    subtitle = "Barres 100% empilees | GHS Panel W4 (2018)",
    x       = "Quintile de consommation (niveau de richesse)",
    y       = "Proportion (%)",
    caption = paste0(
      "Source : World Bank LSMS-ISA | ",
      "sect4a + totcons_final")) +
  theme(legend.position = "bottom")
ggsave("output/figures/TP3_T17_recours_quintile.png", p_quint,
       width = 12, height = 7, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T17_recours_quintile.png sauvegarde.\n")

# ================================================================ #
#  TACHE 18 — DEPENSES MEDIANES : RURAL vs URBAIN                 #
# ================================================================ #
cat("\n-- TACHE 18 : Depenses Rural vs Urbain ----------------\n\n")

dep_zone <- data_sante %>%
  filter(!is.na(Depense_totale),
         Depense_totale > 0,
         !is.na(Milieu_label))

cat("N individus avec depenses + milieu :",
    nrow(dep_zone), "\n")

stats_dep_zone <- dep_zone %>%
  group_by(Milieu_label) %>%
  summarise(
    N_individus = n(),
    Moyenne     = round(mean(Depense_totale)),
    Mediane     = round(median(Depense_totale)),
    Ecart_type  = round(sd(Depense_totale)),
    Q1          = round(quantile(Depense_totale, 0.25)),
    Q3          = round(quantile(Depense_totale, 0.75)),
    .groups     = "drop"
  )
cat("\nStatistiques depenses par zone :\n")
print(stats_dep_zone)

wt_dep <- wilcox.test(Depense_totale ~ Milieu_label,
                      data  = dep_zone,
                      exact = FALSE,
                      conf.int = TRUE)
z_dep  <- qnorm(wt_dep$p.value / 2)
r_dep  <- round(abs(z_dep) /
                  sqrt(nrow(dep_zone)), 4)

cat("\nTest de Wilcoxon-Mann-Whitney :\n")
cat("  W =", wt_dep$statistic,
    "| p =",
    format(wt_dep$p.value, scientific = TRUE,
           digits = 3), "\n")
cat("  Diff. mediane =",
    round(wt_dep$estimate, 0), "Nairas\n")
cat("  IC 95 % : [",
    round(wt_dep$conf.int[1], 0), ";",
    round(wt_dep$conf.int[2], 0), "]\n")
cat("  r =", r_dep, "->",
    case_when(abs(r_dep) < 0.10 ~ "Negligeable",
              abs(r_dep) < 0.30 ~ "Faible",
              abs(r_dep) < 0.50 ~ "Modere",
              TRUE              ~ "Fort"), "\n")

# Tableau GT T18 → output/tables/
nr_t18 <- nrow(stats_dep_zone)
gt_dep_zone <- stats_dep_zone %>%
  mutate(across(c(Moyenne, Mediane, Ecart_type,
                  Q1, Q3),
                ~ format(., big.mark = " "))) %>%
  gt() %>%
  tab_header(
    title    = md(
      "**Depenses de sante selon le milieu**"),
    subtitle = md(paste0(
      "*sect4a_harvestw4 | GHS Panel Wave 4 (2018)*<br>",
      "*Wilcoxon : W = ", wt_dep$statistic,
      " | p = ",
      format(wt_dep$p.value,
             scientific = TRUE, digits = 3),
      " | r = ", r_dep, "*"))
  ) %>%
  cols_label(
    Milieu_label = md("**Zone**"),
    N_individus  = md("**N individus**"),
    Moyenne      = md("**Moyenne (N)**"),
    Mediane      = md("**Mediane (N)**"),
    Ecart_type   = md("**Ecart-type**"),
    Q1           = md("**Q1**"),
    Q3           = md("**Q3**")
  ) %>%
  cols_align("left",   columns = Milieu_label) %>%
  cols_align("center",
             columns = c(N_individus, Moyenne,
                         Mediane, Ecart_type,
                         Q1, Q3)) %>%
  tab_style(
    style = list(cell_fill(color = "#1C1C2E"),
                 cell_text(color = "white",
                           weight = "bold")),
    locations = cells_column_labels()) %>%
  tab_style(
    style = list(cell_fill(color = "#D5F5E3"),
                 cell_text(weight = "bold")),
    locations = cells_body(
      rows = Milieu_label == "Rural")) %>%
  tab_style(
    style = list(cell_fill(color = "#D6EAF8"),
                 cell_text(weight = "bold")),
    locations = cells_body(
      rows = Milieu_label == "Urbain")) %>%
  tab_footnote(
    footnote = md(paste0(
      "Wilcoxon : r = ", r_dep, " (effet ",
      case_when(abs(r_dep) < 0.10 ~ "negligeable",
                abs(r_dep) < 0.30 ~ "faible",
                abs(r_dep) < 0.50 ~ "modere",
                TRUE              ~ "fort"),
      ")")),
    locations = cells_column_labels(Mediane)) %>%
  tab_options(
    table.width = pct(90), table.font.size = 13,
    heading.title.font.size    = 16,
    heading.subtitle.font.size = 12,
    table.border.top.width     = px(3),
    table.border.top.color     = col_ciel,
    table.border.bottom.width  = px(2),
    table.border.bottom.color  = col_ciel,
    column_labels.background.color = "#1C1C2E") %>%
  tab_source_note(md(
    "*Source : World Bank LSMS-ISA | ENSAE ISE1 2025-2026*"))
gtsave(gt_dep_zone, "output/tables/TP3_T18_depenses_zone_gt.html")
cat("OK — output/tables/TP3_T18_depenses_zone_gt.html sauvegarde.\n")

# Violin + Boxplot T18 → output/figures/
meds_dep <- dep_zone %>%
  group_by(Milieu_label) %>%
  summarise(med = median(Depense_totale),
            .groups = "drop")

p_violin_dep <- ggplot(dep_zone,
                       aes(x = Milieu_label, y = Depense_totale,
                           fill = Milieu_label)) +
  geom_violin(alpha = 0.28, color = NA,
              trim = TRUE, scale = "width") +
  geom_boxplot(width = 0.32,
               outlier.shape = 21,
               outlier.size = 1.5,
               outlier.fill = "white",
               outlier.color = "#9A9AB0",
               outlier.alpha = 0.4,
               linewidth = 0.85) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 5,
               color = "white",
               fill = "#1C1C2E") +
  geom_text(data = meds_dep,
            aes(x = Milieu_label, y = med,
                label = paste0("Med.\n",
                               format(round(med),
                                      big.mark = " "),
                               " N")),
            nudge_x = 0.44, fontface = "bold",
            size = 3.8, color = "#1C1C2E",
            inherit.aes = FALSE) +
  scale_fill_manual(
    values = c("Rural"  = col_savane,
               "Urbain" = col_ciel),
    guide  = "none") +
  scale_y_log10(labels = label_comma(suffix = " N")) +
  annotate("text", x = 1.5,
           y = max(dep_zone$Depense_totale) * 0.7,
           label = paste0(
             "Wilcoxon p = ",
             format(wt_dep$p.value, digits = 3),
             "\nr = ", r_dep),
           size = 3.8, color = "#4A4A6A",
           fontface = "italic", hjust = 0.5) +
  labs(
    title    = "Depenses de sante : Rural vs Urbain",
    subtitle = paste0(
      "Violin + Boxplot | Losange = Moyenne | ",
      "Echelle log | GHS Panel W4 (2018)"),
    x       = "Milieu de residence",
    y       = "Depenses de sante (Nairas, log10)",
    caption = "Source : World Bank LSMS-ISA | sect4a_harvestw4")
ggsave("output/figures/TP3_T18_depenses_zone.png", p_violin_dep,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("OK — output/figures/TP3_T18_depenses_zone.png sauvegarde.\n")

# ================================================================ #
#  TABLEAU GTSUMMARY PROFESSIONNEL                                #
# ================================================================ #
cat("\n-- TABLEAU GTSUMMARY ----------------------------------\n\n")

gt_base_s4 <- data_sante %>%
  filter(!is.na(Milieu_label)) %>%
  mutate(Jours_arret = as.numeric(s4aq5)) %>%
  select(Milieu_label, Sexe_label, Malade,
         Consulte, Hospitalise,
         Depense_totale, Jours_arret)

tableau_s4 <- gt_base_s4 %>%
  tbl_summary(
    by       = Milieu_label,
    label    = list(
      Sexe_label     ~ "Sexe",
      Malade         ~ "Maladie/blessure (4 sem.)",
      Consulte       ~ "A consulte un prestataire",
      Hospitalise    ~ "Hospitalise",
      Depense_totale ~ "Depenses sante (Nairas)",
      Jours_arret    ~ "Jours d'arret d'activite"
    ),
    statistic = list(
      all_continuous()  ~
        "{median} [{p25} - {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits   = list(all_continuous() ~ 0),
    missing  = "ifany",
    missing_text = "Manquant"
  ) %>%
  add_overall(last      = FALSE,
              col_label = "**Ensemble**") %>%
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "chisq.test"),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  add_n(statistic = "{N_nonmiss}",
        footnote  = FALSE) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(
    label   ~ "**Variable**",
    stat_0  ~ "**Ensemble**\n(N = {N})",
    stat_1  ~ "**Rural**\n(n = {n})",
    stat_2  ~ "**Urbain**\n(n = {n})",
    p.value ~ "**p-value**",
    n       ~ "**N valide**"
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Milieu de residence**") %>%
  modify_caption(paste0(
    "**Tableau 3.** Sante et recours aux soins ",
    "par milieu de residence | ",
    "Nigeria GHS Panel Wave 4 (2018) | N = ",
    format(nrow(gt_base_s4), big.mark = " "),
    " | Source : World Bank LSMS-ISA | ENSAE ISE1 2025-2026"
  )) %>%
  modify_footnote(
    all_stat_cols() ~
      "Continues : Mediane [Q1 - Q3] | Categoriel : n (%)")

print(tableau_s4)

gt_final_s4 <- tableau_s4 %>%
  as_gt() %>%
  tab_style(
    style = list(
      cell_fill(color = "#1C1C2E"),
      cell_text(color = "white",
                weight = "bold", size = px(13))),
    locations = cells_column_labels()) %>%
  tab_style(
    style = list(
      cell_fill(color = "#12121E"),
      cell_text(color = "white",
                weight = "bold", size = px(12))),
    locations = cells_column_spanners()) %>%
  tab_style(
    style = cell_text(color = col_ocre,
                      weight = "bold"),
    locations = cells_body(
      columns = p.value,
      rows    = !is.na(p.value) & p.value < 0.05)) %>%
  tab_options(
    table.width = pct(98), table.font.size = 13,
    heading.title.font.size   = 16,
    table.border.top.width    = px(3),
    table.border.top.color    = col_lagune,
    table.border.bottom.width = px(2),
    table.border.bottom.color = col_lagune,
    column_labels.background.color = "#1C1C2E",
    row_group.background.color     = "#F8F9FA")
gtsave(gt_final_s4, "output/tables/TP3_T_gtsummary.html")
cat("OK — output/tables/TP3_T_gtsummary.html sauvegarde.\n")

# ================================================================ #
#  FIGURE DE SYNTHESE                                             #
# ================================================================ #
cat("\n-- Assemblage figure de synthese ----------------------\n")

p_synthese_s4 <-
  p_morb /
  (p_maladies | p_prest) /
  (p_quint    | p_violin_dep) +
  plot_annotation(
    title    = "TP3 — Sante, Morbidite & Recours aux Soins",
    subtitle = "Nigeria GHS Panel — Wave 4 (2018)",
    caption  = paste0(
      "Source : World Bank LSMS-ISA | ",
      "ENSAE ISE1 2025-2026"),
    theme = theme(
      plot.title = element_text(
        face = "bold", size = 18,
        color = "#1C1C2E", hjust = 0.5),
      plot.subtitle = element_text(
        size = 13, color = "#4A4A6A",
        hjust = 0.5),
      plot.caption = element_text(
        size = 10, color = "#9A9AB0"))
  ) +
  plot_layout(heights = c(1.2, 1, 1))
ggsave("output/figures/TP3_SYNTHESE_complete.png", p_synthese_s4,
       width = 18, height = 20, dpi = 300,
       bg = "white")
cat("OK — output/figures/TP3_SYNTHESE_complete.png sauvegarde.\n")

# ================================================================ #
#  RESUME FINAL                                                   #
# ================================================================ #
cat("\n", strrep("=", 64), "\n")
cat("  RESUME DES RESULTATS — TP3\n")
cat(strrep("=", 64), "\n\n")

cat("Bases :\n")
cat("  sect4a_harvestw4 :", nrow(sect4a), "obs.\n")
cat("  sect1_harvestw4  :", nrow(sect1),  "obs.\n")
cat("  totcons_final    :", nrow(cons),   "obs.\n")
cat("  Apres jointure   :", nrow(data_sante), "obs.\n\n")

cat("T13 — Taux de morbidite :\n")
cat("  Non pondere :",
    round(taux_global_np * 100, 1), "%\n")
if (exists("taux_global_p"))
  cat("  Pondere     :",
      round(taux_global_p * 100, 1), "%\n")

cat("\nT14 — Top 3 maladies :\n")
for (i in seq_len(min(3, nrow(freq_maladies))))
  cat(" ", i, ".",
      as.character(freq_maladies$Type_maladie[i]),
      "-", freq_maladies$Proportion[i], "%\n")

cat("\nT15 — Prestataire le plus frequente :\n")
cat("  ", as.character(freq_prest$Prestataire[1]),
    ":", freq_prest$Proportion[1], "%\n")

cat("\nT16 — Depenses de sante :\n")
cat("  Mediane :", format(round(med_dep),
                          big.mark = " "),
    "Nairas\n")
cat("  Outliers :", n_out, "\n")

cat("\nT17 — Recours x Quintile :\n")
cat(" ", test_q, ": p =",
    format(p_val_q, digits = 4), "\n")
cat("  V Cramer =",
    ifelse(is.na(v_q), "N/A", v_q), "\n")

cat("\nT18 — Depenses Rural vs Urbain :\n")
for (i in seq_len(nrow(stats_dep_zone)))
  cat("  *",
      as.character(stats_dep_zone$Milieu_label[i]),
      "— Med. :",
      format(stats_dep_zone$Mediane[i],
             big.mark = " "), "N\n")
cat("  Wilcoxon p =",
    format(wt_dep$p.value,
           scientific = TRUE, digits = 3),
    "| r =", r_dep, "\n\n")

cat(strrep("-", 64), "\n")
cat("  Fichiers generes :\n")
cat("  output/tables/\n")
cat("    * TP3_T13_valeurs_manquantes.xlsx\n")
cat("    * TP3_T15_prestataires_gt.html\n")
cat("    * TP3_T16_stats_depenses.html\n")
cat("    * TP3_T18_depenses_zone_gt.html\n")
cat("    * TP3_T_gtsummary.html\n")
cat("  output/figures/\n")
cat("    * TP3_T13_carte_manquantes.png\n")
cat("    * TP3_T13_taux_morbidite.png\n")
cat("    * TP3_T14_types_maladies.png\n")
cat("    * TP3_T15_recours_prestataire.png\n")
cat("    * TP3_T16_depenses_sante.png\n")
cat("    * TP3_T17_recours_quintile.png\n")
cat("    * TP3_T18_depenses_zone.png\n")
cat("    * TP3_SYNTHESE_complete.png\n")
cat(strrep("=", 64), "\n")