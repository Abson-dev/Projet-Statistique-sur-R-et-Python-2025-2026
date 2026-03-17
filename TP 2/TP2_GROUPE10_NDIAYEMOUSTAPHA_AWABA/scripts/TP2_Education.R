##############################################################################
#  TP2 — ÉDUCATION ET ALPHABÉTISATION DES MÉNAGES NIGÉRIANS
#  Nigeria GHS Panel — Vague 4 (2018) | ENSAE ISE 1 | 2025-2026
#  Auteurs : Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba
##############################################################################
#
#  BASES UTILISÉES :
#    sect2_harvestw4.dta  → éducation : s2aq9 (niveau), s2aq5 (alphab.),
#                           s2aq6 (ever attended), s2aq13a (scolarisation actuelle)
#    sect1_harvestw4.dta  → sexe, âge, secteur (pour jointure)
#
#  NOTES MÉTHODOLOGIQUES IMPORTANTES :
#
#  1. STRUCTURE DES FICHIERS ÉDUCATION SELON LES VAGUES :
#     - W1/W2 : deux fichiers séparés
#       * sect2a = individus SORTIS définitivement de l'école (s2aq9 = niveau complété)
#       * sect2b = individus ENCORE SCOLARISÉS (s2bq3 = niveau en cours)
#     - W3/W4 : un seul fichier sect2_harvestw4, toute la population 3 ans et +
#
#  2. CLASSIFICATION DU NIVEAU D'ÉDUCATION (s2aq9) :
#     - S'applique à ceux qui ont s2aq6 = "YES" (jamais quitté)
#     - Pour s2aq6 = "NO" (jamais allé à l'école) → AUCUN directement
#       Sans cette règle, 3 054 adultes seraient faussement exclus (NA)
#
#  3. SCOLARISATION 6-17 ANS :
#     - W4 scinde la question en deux :
#       * s2aq13  = "a-t-il fréquenté en 2017/2018 ?" (année passée)
#       * s2aq13a = "fréquente-t-il en 2018/2019 ?" (actuelle) → À UTILISER
#
#  4. FILTRE MEMBRES PRÉSENTS (identique à TP1) :
#     - s1q4a = "NO" → membres absents → à exclure avant jointure
##############################################################################

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr, viridis)

# ── 1. CHEMINS AUTOMATIQUES ───────────────────────────────────────────────────
SCRIPT_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
TP_DIR     <- dirname(SCRIPT_DIR)
DATA_DIR   <- file.path(TP_DIR, "data", "NGA_2018_GHSP-W4_v03_M_Stata12")
OUT        <- file.path(TP_DIR, "outputs")
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

f_sect2 <- file.path(DATA_DIR, "sect2_harvestw4.dta")
f_sect1 <- file.path(DATA_DIR, "sect1_harvestw4.dta")
if (!file.exists(f_sect2) || !file.exists(f_sect1))
  stop("Fichiers introuvables. Extraire NGA_2018_GHSP-W4_v03_M_Stata12.zip dans TP2/data/")
message("✓ Fichiers trouvés\n")

# ── 2. PALETTE & THÈME ────────────────────────────────────────────────────────
COLS_EDUC <- c("Aucun"="#E74C3C","Primaire"="#F39C12",
               "Junior Sec."="#F1C40F","Senior Sec."="#27AE60","Tertiaire"="#2980B9")

THEME <- theme_minimal(base_size = 12) +
  theme(plot.title=element_text(face="bold",size=14,margin=margin(b=6)),
        plot.subtitle=element_text(size=10,color="grey40",margin=margin(b=8)),
        plot.caption=element_text(size=8,color="grey55",hjust=0),
        axis.title=element_text(size=10,face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="grey92"),
        plot.background=element_rect(fill="white",color=NA),
        panel.background=element_rect(fill="white",color=NA),
        strip.text=element_text(face="bold"))

# ── 3. CLASSIFICATION NIVEAU D'ÉDUCATION (s2aq9) ─────────────────────────────
# Codes GHS W4 → 5 catégories ordonnées
# Codes: 0,1-3(pré-scol),51-52(coranique) → Aucun
# 11-16,61 → Primaire | 21-23 → Junior Sec. | 24-28,31-35,321,322 → Senior Sec.
# 34-35,41-43,411,412,421-424 → Tertiaire
map_educ <- function(ever_attended, s2aq9_val) {
  # Convertir en chaîne et gérer les NA explicitement
  ever_str  <- as.character(ever_attended)
  s2aq9_str <- as.character(s2aq9_val)
  # RÈGLE 1 : NA de ever_attended → traiter comme inconnu
  if (is.na(ever_attended) || ever_str == "NA") {
    # Si s2aq9 est aussi NA → NA, sinon classer via s2aq9
    code <- suppressWarnings(as.integer(str_extract(s2aq9_str, "^\\d+")))
    if (is.na(code)) return(NA_character_)
  } else if (grepl("^2", ever_str)) {
    # RÈGLE 2 : jamais scolarisé (s2aq6 = "No") → Aucun
    return("Aucun")
  }
  # RÈGLE 3 : niveau complété selon s2aq9
  code <- suppressWarnings(as.integer(str_extract(s2aq9_str, "^\\d+")))
  if (is.na(code)) return(NA_character_)
  if (code %in% c(0, 1:3, 51, 52))                   return("Aucun")
  if (code %in% c(11:16, 61))                         return("Primaire")
  if (code %in% 21:23)                                return("Junior Sec.")
  if (code %in% c(24:28, 31:35, 321, 322))            return("Senior Sec.")
  if (code %in% c(34, 35, 41:43, 411, 412, 421:424))  return("Tertiaire")
  return(NA_character_)
}

# ── 4. CHARGEMENT & NETTOYAGE ─────────────────────────────────────────────────
message("Chargement des données...")

# sect1 — membres présents uniquement (filtre s1q4a)
df_s1 <- read_dta(f_sect1) %>%
  mutate(s1q4a_num = as.integer(str_extract(as.character(s1q4a), "^\\d+"))) %>%
  filter(s1q4a_num != 2 | is.na(s1q4a_num)) %>%
  mutate(sex       = as.integer(str_extract(as.character(s1q2), "^\\d+")),
         sex_label = factor(case_when(sex==1~"Homme",sex==2~"Femme")),
         age       = as.numeric(s1q4),
         age       = if_else(age>110|age<0, NA_real_, age),
         sec_num   = as.integer(str_extract(as.character(sector), "^\\d+")),
         sec_label = factor(case_when(sec_num==1~"Urbain",sec_num==2~"Rural"),
                            levels=c("Rural","Urbain")),
         state_clean = str_remove(as.character(state),"^\\d+\\.\\s*"))

# sect2 — jointure avec sect1
df_educ <- read_dta(f_sect2) %>%
  left_join(df_s1 %>% select(hhid,indiv,sex_label,age,sec_label,state_clean),
            by = c("hhid","indiv")) %>%
  mutate(
    # Classification niveau éducation (CORRIGÉE : s2aq6 = jamais → Aucun)
    educ_level = mapply(map_educ, s2aq6, s2aq9),
    educ_ord   = factor(educ_level, levels=names(COLS_EDUC), ordered=TRUE),
    educ_num   = as.integer(educ_ord),
    # Alphabétisation (s2aq5)
    alphab     = case_when(str_detect(as.character(s2aq5),"^1")~"Alphabétisé",
                           str_detect(as.character(s2aq5),"^2")~"Non alphab.",
                           TRUE~NA_character_),
    # Scolarisation ACTUELLE 2018/2019 (s2aq13a — PAS s2aq13 qui est 2017/18)
    scol_actuelle = case_when(str_detect(as.character(s2aq13a),"^1")~1L,
                              str_detect(as.character(s2aq13a),"^2")~0L,
                              TRUE~NA_integer_)
  )

# Adultes 18+ avec niveau éducation
adults <- df_educ %>% filter(!is.na(age), age >= 18, !is.na(educ_level))
message(sprintf("✓ Adultes 18+ avec éducation : %d (NA restants : %d)\n",
                nrow(adults),
                df_educ %>% filter(!is.na(age), age>=18) %>% pull(educ_level) %>% is.na() %>% sum()))

# ── 5. TÂCHE 7 — VALEURS MANQUANTES ──────────────────────────────────────────
message("=== Tâche 7 : Valeurs manquantes ===")
cat(sprintf("sect2_harvestw4 : %d lignes | adults 18+ : %d | NA niveau : %d\n",
            nrow(df_educ), nrow(adults), sum(is.na(adults$educ_level))))
cat(sprintf("dont 'jamais scolarisés' (s2aq6=No) → Aucun : %d\n",
            sum(str_detect(as.character(df_educ$s2aq6),"^2"), na.rm=TRUE)))

# ── 6. TÂCHE 8 — DISTRIBUTION NIVEAU D'ÉDUCATION ─────────────────────────────
message("\n=== Tâche 8 : Distribution niveau éducation ===")
educ_dist <- adults %>%
  count(educ_level) %>%
  mutate(pct = n/sum(n)*100,
         educ_f = fct_reorder(factor(educ_level, levels=names(COLS_EDUC)), n))
cat("\n--- Distribution ---\n"); print(educ_dist)

p_educ_dist <- ggplot(educ_dist, aes(x=pct, y=educ_f, fill=educ_f)) +
  geom_col(alpha=0.87, width=0.68) +
  geom_text(aes(label=sprintf("%.1f%%   n=%s", pct, comma(n))),
            hjust=-0.05, size=3.5, fontface="bold") +
  scale_fill_manual(values=COLS_EDUC, guide="none") +
  scale_x_continuous(labels=function(x) paste0(x,"%"),
                     expand=expansion(mult=c(0,.22))) +
  labs(title="Niveau d'éducation des adultes (18+) — W4 (2018)",
       subtitle=sprintf("N = %s adultes | s2aq6='No' → Aucun (corrigé)",
                        comma(nrow(adults))),
       x="Proportion (%)", y="Niveau d'éducation",
       caption="Source : GHS Panel W4 | s2aq9 + s2aq6 — sect2_harvestw4") + THEME
ggsave(file.path(OUT,"T8_educ_distribution.png"), p_educ_dist, w=10, h=6, dpi=200, bg="white")
print(p_educ_dist)

# ── 7. TÂCHE 9 — ÉDUCATION × SEXE + CHI² + V CRAMÉR ─────────────────────────
message("\n=== Tâche 9 : Éducation × Sexe ===")
educ_sex <- adults %>%
  filter(!is.na(sex_label)) %>%
  count(educ_level, sex_label) %>%
  group_by(sex_label) %>% mutate(pct=n/sum(n)*100) %>% ungroup() %>%
  mutate(educ_level=factor(educ_level, levels=names(COLS_EDUC)))

cont <- table(adults$educ_level[!is.na(adults$sex_label)],
              adults$sex_label[!is.na(adults$sex_label)])
chi2 <- chisq.test(cont)
V_cr <- sqrt(chi2$statistic/(sum(cont)*(min(nrow(cont),ncol(cont))-1)))
message(sprintf("Chi²=%.2f, df=%d, p=%.2e | V Cramér=%.3f",
                chi2$statistic, chi2$parameter, chi2$p.value, V_cr))

p_educ_sex <- ggplot(educ_sex, aes(x=sex_label, y=pct, fill=educ_level)) +
  geom_col(position="fill", alpha=0.87, width=0.6) +
  geom_text(aes(label=if_else(pct>3.5, sprintf("%.0f%%",pct),"")),
            position=position_fill(vjust=0.5),
            size=3.4, color="white", fontface="bold") +
  scale_fill_manual(values=COLS_EDUC, name="Niveau") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  annotate("label", x=1.5, y=1.07,
           label=sprintf("Chi²=%.1f · df=%d · p<0.001\nV Cramér=%.3f",
                         chi2$statistic, chi2$parameter, V_cr),
           size=3.2, hjust=0.5, fill="white", label.size=0.3) +
  labs(title="Niveau d'éducation par sexe (adultes 18+) — W4 (2018)",
       subtitle="Barres 100% empilées | Chi² de Pearson + V de Cramér",
       x="Sexe", y="Proportion",
       caption=sprintf("Chi²=%.1f · V Cramér=%.3f\nSource : GHS Panel W4",
                       chi2$statistic, V_cr)) +
  THEME + theme(legend.position="right")
ggsave(file.path(OUT,"T9_educ_par_sexe.png"), p_educ_sex, w=9, h=7, dpi=200, bg="white")
print(p_educ_sex)

# ── 8. TÂCHE 10 — ÂGE × ÉDUCATION (KW + DUNN) ───────────────────────────────
message("\n=== Tâche 10 : Âge × éducation ===")
adults <- adults %>%
  mutate(age_grp=cut(age, breaks=c(18,30,45,60,Inf),
                     labels=c("18–30","31–45","46–60","60+"),
                     right=FALSE, include.lowest=TRUE))

kw <- kruskal.test(educ_num ~ age_grp, data=adults %>% filter(!is.na(age_grp)))
message(sprintf("Kruskal-Wallis : H=%.2f, df=%d, p=%.2e",
                kw$statistic, kw$parameter, kw$p.value))
dunn_r <- dunn_test(adults %>% filter(!is.na(age_grp)),
                    educ_num ~ age_grp, p.adjust.method="bonferroni")
print(dunn_r)

p_age_educ <- ggplot(adults %>% filter(!is.na(educ_level),!is.na(age_grp)),
                     aes(x=age_grp, y=educ_num, fill=age_grp)) +
  geom_boxplot(alpha=0.8, outlier.alpha=0.15, width=0.5) +
  stat_summary(fun=mean, geom="point", shape=18, size=5, color="black") +
  stat_summary(fun=mean, geom="text",
               aes(label=sprintf("♦%.2f", after_stat(y))),
               vjust=-1.1, size=2.9) +
  scale_fill_viridis_d(option="plasma", direction=-1, guide="none") +
  scale_y_continuous(breaks=1:5,
                     labels=c("Aucun","Primaire","Jr Sec.","Sr Sec.","Tertiaire")) +
  annotate("label", x=3.5, y=5.5,
           label=sprintf("Kruskal-Wallis\nH=%.1f · p<0.001",kw$statistic),
           size=3.2, hjust=0.5, fill="white", label.size=0.3) +
  labs(title="Niveau d'éducation par groupe d'âge (adultes 18+) — W4",
       subtitle="♦ Score moyen | Kruskal-Wallis + post-hoc Dunn (Bonferroni)",
       x="Groupe d'âge", y="Niveau d'éducation",
       caption=sprintf("H=%.2f · p=%.2e | Source : GHS Panel W4",
                       kw$statistic, kw$p.value)) + THEME
ggsave(file.path(OUT,"T10_age_education.png"), p_age_educ, w=10, h=7, dpi=200, bg="white")
print(p_age_educ)

# ── 9. TÂCHE 11 — SCOLARISATION 6-17 ANS (s2aq13a) ───────────────────────────
message("\n=== Tâche 11 : Scolarisation 6-17 ans (s2aq13a = 2018/2019) ===")
children <- df_educ %>%
  filter(!is.na(age), between(age,6,17), !is.na(sec_label), !is.na(scol_actuelle))
cat(sprintf("Enfants 6-17 ans avec info scolarisation : %d\n", nrow(children)))

cont_sc <- table(children$scol_actuelle, children$sec_label)
chi_sc  <- chisq.test(cont_sc)
message(sprintf("Chi² scolarisation×secteur : %.2f, p=%.4f",
                chi_sc$statistic, chi_sc$p.value))

enroll_df <- children %>%
  group_by(sec_label) %>%
  summarise(N=n(), n_scol=sum(scol_actuelle), .groups="drop") %>%
  mutate(pct    = n_scol/N*100,
         pct_lo = purrr::map2_dbl(n_scol, N, ~binom.test(.x,.y)$conf.int[1]*100),
         pct_hi = purrr::map2_dbl(n_scol, N, ~binom.test(.x,.y)$conf.int[2]*100))
print(enroll_df)

p_enroll <- ggplot(enroll_df, aes(x=sec_label, y=pct, fill=sec_label)) +
  geom_col(alpha=0.87, width=0.52) +
  geom_errorbar(aes(ymin=pct_lo,ymax=pct_hi), width=0.15, linewidth=1, color="grey20") +
  geom_text(aes(label=sprintf("%.1f%%\n(n=%s/%s)", pct, comma(n_scol), comma(N))),
            vjust=-0.35, size=3.8, fontface="bold") +
  scale_fill_manual(values=c("Rural"="#27AE60","Urbain"="#E67E22"), guide="none") +
  scale_y_continuous(labels=function(x) paste0(x,"%"), limits=c(0,108)) +
  annotate("label", x=1.5, y=95,
           label=sprintf("Chi²=%.2f\np=%.4f", chi_sc$statistic, chi_sc$p.value),
           size=3.3, hjust=0.5, fill="white", label.size=0.3) +
  labs(title="Scolarisation actuelle (2018/2019) des 6-17 ans — W4",
       subtitle="Variable : s2aq13a (année en cours) | IC 95% Clopper-Pearson",
       x="Milieu", y="Taux de scolarisation (%)",
       caption=sprintf("N=%s enfants 6-17 ans | Chi²=%.2f · p=%.4f\nSource : GHS Panel W4 | s2aq13a",
                       comma(nrow(children)), chi_sc$statistic, chi_sc$p.value)) + THEME
ggsave(file.path(OUT,"T11_scolarisation.png"), p_enroll, w=9, h=7, dpi=200, bg="white")
print(p_enroll)

# ── 10. TÂCHE 12 — HEATMAP ÉTAT × SANS INSTRUCTION ────────────────────────────
message("\n=== Tâche 12 : Heatmap analphabétisme par État ===")
state_educ <- adults %>%
  filter(!is.na(state_clean)) %>%
  group_by(state_clean) %>%
  summarise(N=n(), N_aucun=sum(educ_level=="Aucun",na.rm=TRUE),
            pct_aucun=N_aucun/N*100, .groups="drop") %>%
  filter(N>=15) %>%
  mutate(state_clean=fct_reorder(state_clean, pct_aucun))

p_hm <- ggplot(state_educ, aes(x="Sans instruction", y=state_clean, fill=pct_aucun)) +
  geom_tile(color="white", linewidth=0.5) +
  geom_text(aes(label=sprintf("%.1f%%",pct_aucun)),
            size=2.7, color=ifelse(state_educ$pct_aucun>40,"white","black")) +
  scale_fill_gradientn(
    colors=c("#2ECC71","#F1C40F","#E67E22","#E74C3C","#7B241C"),
    name="% sans\ninstruction", labels=function(x) paste0(x,"%")) +
  labs(title="Taux d'adultes sans instruction par État — W4 (2018)",
       subtitle="Adultes 18+ | s2aq9 + s2aq6='No' → Aucun | Classés croissant",
       x=NULL, y="État nigérian",
       caption="Nord-Ouest (Zamfara, Sokoto, Jigawa) = taux les plus élevés\nSource : GHS Panel W4 | sect2_harvestw4") +
  THEME +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.grid=element_blank())
ggsave(file.path(OUT,"T12_heatmap_etats.png"), p_hm, w=9, h=12, dpi=200, bg="white")
print(p_hm)

# ── 11. TABLEAU GTSUMMARY ─────────────────────────────────────────────────────
message("\n=== Tableau gtsummary ===")
print(
  tbl_summary(
    adults %>% filter(!is.na(sex_label),!is.na(sec_label)) %>%
      mutate(`Niveau éducation`=factor(educ_level,levels=names(COLS_EDUC)),
             Sexe=sex_label, Milieu=sec_label, `Âge`=age) %>%
      select(Sexe, Milieu, `Âge`, `Niveau éducation`),
    by=Sexe,
    statistic=list(all_continuous()~"{mean} ({sd})", all_categorical()~"{n} ({p}%)")
  ) %>%
    add_p(test=list(`Niveau éducation`~"chisq.test",`Âge`~"wilcox.test")) %>%
    add_overall() %>% bold_labels() %>%
    modify_caption("**Tableau 2. Éducation par sexe — Adultes 18+, W4 (2018)**")
)

# ── 12. FIGURE DE SYNTHÈSE ─────────────────────────────────────────────────────
message("\n=== Figure de synthèse TP2 ===")
fig_tp2 <- (p_educ_dist | p_educ_sex) /
           (p_age_educ  | p_enroll)   /
            p_hm +
  plot_layout(heights=c(1.2, 1.2, 2.2)) +
  plot_annotation(
    title    = "TP2 — Éducation & Alphabétisation · Nigeria GHS Panel W4 (2018)",
    subtitle = "Auteurs : Cheikh M. M. Ndiaye & Awa Ba | ENSAE ISE 1 | 2025-2026",
    caption  = paste0("Correction clé : s2aq6='No' (jamais scolarisés) → 'Aucun' | ",
                      "Scolarisation : s2aq13a (2018/2019 actuelle)\n",
                      "Tests : Chi² · V Cramér · Kruskal-Wallis · Dunn · Binomial exact"),
    theme=theme(plot.title=element_text(face="bold",size=15),
                plot.subtitle=element_text(size=10,color="grey35"),
                plot.caption=element_text(size=8,color="grey50"))
  )
ggsave(file.path(OUT,"TP2_Figure_Synthese.png"), fig_tp2,
       w=20, h=26, dpi=200, bg="white")
message(sprintf("✓ Figure de synthèse → %s", file.path(OUT,"TP2_Figure_Synthese.png")))
message("\n✅ TP2 TERMINÉ")
