##############################################################################
#  TP3 — ACCÈS AUX SOINS ET DÉPENSES DE SANTÉ
#  Nigeria GHS Panel — Vague 4 (2018) | ENSAE ISE 1 | 2025-2026
#  Auteurs : Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba
##############################################################################
#
#  BASES UTILISÉES :
#    sect4a_harvestw4.dta  → santé individuelle (maladie, consultation, dépenses)
#    sect1_harvestw4.dta   → sexe, âge, secteur (jointure)
#    totcons_final.dta     → consommation totale (quintiles de richesse)
#
#  NOTES MÉTHODOLOGIQUES IMPORTANTES :
#
#  1. SECTION SANTÉ = sect4a (PAS sect3a) :
#     sect3a_harvestw4 = section EMPLOI (heures travaillées, type activité)
#     sect3b_harvestw4 = assurance NHIS (enrollment ménage)
#     sect4a_harvestw4 = VRAIE section santé (maladie, consultation, dépenses)
#
#  2. TAUX DE MORBIDITÉ — construction correcte :
#     La variable s4aq3 ("souffert d'une maladie en 4 sem.") a 4 385 NA.
#     Ces NA correspondent EXACTEMENT aux individus avec s4aq1=YES (ont consulté).
#     La raison de consultation (s4aq2a) confirme : 4 207 = maladie, 178 = blessure.
#     → Ces 4 385 personnes sont bien MALADES et doivent être comptées.
#     → Taux correct = (s4aq3=YES + consulté pour maladie) / total
#
#  3. DÉPENSES TOTALES DE SANTÉ — construction complète :
#     s4aq9  = frais de consultation (₦)
#     s4aq10 = frais de transport aller-retour pour consultation (₦)
#     s4aq14 = médicaments achetés en pharmacie/kiosque (₦)
#     s4aq17 = frais d'hospitalisation (₦)
#     → Dépense totale = somme des 4 composantes (0 si NA = pas de dépense)
#
#  4. FILTRE MEMBRES PRÉSENTS (identique à TP1) :
#     s1q4a = "NO" → membres absents → exclus avant jointure
##############################################################################

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr)

# ── 1. CHEMINS AUTOMATIQUES ───────────────────────────────────────────────────
SCRIPT_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
TP_DIR     <- dirname(SCRIPT_DIR)
DATA_DIR   <- file.path(TP_DIR, "data", "NGA_2018_GHSP-W4_v03_M_Stata12")
OUT        <- file.path(TP_DIR, "outputs")
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

f_health <- file.path(DATA_DIR, "sect4a_harvestw4.dta")
f_sect1  <- file.path(DATA_DIR, "sect1_harvestw4.dta")
f_cons   <- file.path(DATA_DIR, "totcons_final.dta")
if (!all(file.exists(c(f_health, f_sect1, f_cons))))
  stop("Fichiers introuvables. Extraire NGA_2018_GHSP-W4_v03_M_Stata12.zip dans TP3/data/")
message("✓ Fichiers trouvés\n")

# ── 2. PALETTE & THÈME ────────────────────────────────────────────────────────
COL_M <- "#1B6CA8"; COL_F <- "#C0392B"
COL_R <- "#27AE60"; COL_U <- "#E67E22"

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

# ── 3. CHARGEMENT & NETTOYAGE ─────────────────────────────────────────────────
message("Chargement des données...")

# sect1 — membres présents
df_s1 <- read_dta(f_sect1) %>%
  mutate(s1q4a_num = as.integer(str_extract(as.character(s1q4a),"^\\d+"))) %>%
  filter(s1q4a_num != 2 | is.na(s1q4a_num)) %>%
  mutate(sex       = as.integer(str_extract(as.character(s1q2),"^\\d+")),
         sex_label = factor(case_when(sex==1~"Homme",sex==2~"Femme")),
         age       = as.numeric(s1q4),
         age       = if_else(age>110|age<0, NA_real_, age),
         sec_num   = as.integer(str_extract(as.character(sector),"^\\d+")),
         sec_label = factor(case_when(sec_num==1~"Urbain",sec_num==2~"Rural"),
                            levels=c("Rural","Urbain")))

# sect4a — santé
df_h_raw <- read_dta(f_health)

df_h <- df_h_raw %>%
  left_join(df_s1 %>% select(hhid,indiv,sex_label,age,sec_label), by=c("hhid","indiv")) %>%
  mutate(
    # ── TAUX DE MORBIDITÉ CORRECT ──────────────────────────────────────────
    # s4aq3 = maladie/blessure déclarée (posée si s4aq1=NO ou séparément)
    # Les NA de s4aq3 = individus qui ont consulté (s4aq1=YES)
    # s4aq2a = raison consultation : 1=maladie, 2=blessure, autre=préventif
    ill_s4aq3      = str_detect(as.character(s4aq3),"^1"),
    cons_reason    = as.integer(str_extract(as.character(s4aq2a),"^\\d+")),
    # Malade = déclaré malade OU consulté pour maladie/blessure
    malade = case_when(
      ill_s4aq3 ~ TRUE,
      !is.na(cons_reason) & cons_reason %in% c(1L,2L) ~ TRUE,
      TRUE ~ FALSE
    ),
    # Consultation
    consulted = str_detect(as.character(s4aq1),"^1"),
    # ── DÉPENSES TOTALES SANTÉ ─────────────────────────────────────────────
    # NA = pas de dépense de ce type → remplacer par 0
    dep_consult   = pmax(0, as.numeric(str_extract(as.character(s4aq9), "^\\d+\\.?\\d*")), na.rm=TRUE),
    dep_transport = pmax(0, as.numeric(str_extract(as.character(s4aq10),"^\\d+\\.?\\d*")), na.rm=TRUE),
    dep_meds      = pmax(0, as.numeric(str_extract(as.character(s4aq14),"^\\d+\\.?\\d*")), na.rm=TRUE),
    dep_hosp      = pmax(0, as.numeric(str_extract(as.character(s4aq17),"^\\d+\\.?\\d*")), na.rm=TRUE),
    # Total dépenses santé = somme des 4 composantes
    dep_total     = dep_consult + dep_transport + dep_meds + dep_hosp,
    # Nettoyer outliers extrêmes pour l'analyse (garder pour stats descriptives)
    dep_total_viz = if_else(dep_total > quantile(dep_total[dep_total>0], .99, na.rm=TRUE),
                            NA_real_, dep_total),
    # Type de maladie
    ill_type = str_remove(as.character(s4aq3b_1),"^\\d+\\.\\s*"),
    # Prestataire
    prov_code = str_extract(as.character(s4aq6a),"^\\d+"),
    # Lieu consultation
    lieu_code = str_extract(as.character(s4aq7),"^\\d+"),
    # Groupe d'âge
    age_grp = cut(age, breaks=c(0,5,15,30,45,60,Inf),
                  labels=c("0–4","5–14","15–29","30–44","45–59","60+"),
                  right=FALSE, include.lowest=TRUE)
  )

# Quintiles consommation
df_cons <- read_dta(f_cons) %>%
  select(hhid, totcons_pc) %>%
  mutate(quintile     = ntile(totcons_pc, 5),
         quint_label  = paste0("Q", quintile),
         quint_desc   = paste0("Q", quintile, c(" (Très pauvres)","(Pauvres)",
                                                "(Classe moy.)","(Aisés)","(Riches)")[quintile]))

df_h <- df_h %>% left_join(df_cons, by = "hhid")

message(sprintf("✓ sect4a_harvestw4 : %d individus chargés", nrow(df_h)))

# ── 4. TÂCHE 13 — TAUX DE MORBIDITÉ (CORRECT) ────────────────────────────────
message("\n=== Tâche 13 : Taux de morbidité ===")
morb_tot <- mean(df_h$malade) * 100
cat(sprintf("Taux de morbidité W4 : %.1f%%\n", morb_tot))
cat(sprintf("  dont s4aq3=YES : %d\n", sum(df_h$ill_s4aq3, na.rm=TRUE)))
cat(sprintf("  dont consulté pour maladie (s4aq2a=1,2) : %d\n",
            sum(df_h$cons_reason %in% c(1L,2L) & is.na(df_h$s4aq3), na.rm=TRUE)))
cat(sprintf("  Total malades : %d / %d\n\n", sum(df_h$malade), nrow(df_h)))

# IC 95% par sexe
morb_sex <- df_h %>%
  filter(!is.na(sex_label)) %>%
  group_by(sex_label) %>%
  summarise(N=n(), n_ill=sum(malade), .groups="drop") %>%
  mutate(pct    = n_ill/N*100,
         pct_lo = purrr::map2_dbl(n_ill, N, ~binom.test(.x,.y)$conf.int[1]*100),
         pct_hi = purrr::map2_dbl(n_ill, N, ~binom.test(.x,.y)$conf.int[2]*100))

morb_age <- df_h %>%
  filter(!is.na(age_grp)) %>%
  group_by(age_grp) %>%
  summarise(N=n(), n_ill=sum(malade), .groups="drop") %>%
  mutate(pct    = n_ill/N*100,
         pct_lo = purrr::map2_dbl(n_ill, N, ~binom.test(.x,.y)$conf.int[1]*100),
         pct_hi = purrr::map2_dbl(n_ill, N, ~binom.test(.x,.y)$conf.int[2]*100))

p_morb_sex <- ggplot(morb_sex, aes(x=sex_label, y=pct, fill=sex_label)) +
  geom_col(alpha=0.87, width=0.52) +
  geom_errorbar(aes(ymin=pct_lo,ymax=pct_hi), width=0.14, linewidth=1, color="grey20") +
  geom_text(aes(label=sprintf("%.1f%%\n(n=%s)",pct,comma(n_ill))),
            vjust=-0.4, size=3.8, fontface="bold") +
  scale_fill_manual(values=c("Homme"=COL_M,"Femme"=COL_F), guide="none") +
  scale_y_continuous(labels=function(x) paste0(x,"%"), limits=c(0,35)) +
  labs(title="Taux de morbidité par sexe — W4 (2018)",
       subtitle="IC 95% Clopper-Pearson | Malades = s4aq3=YES + consultés pour maladie",
       x="Sexe", y="Taux de morbidité (%)",
       caption="Source : GHS Panel W4 | s4aq3 + s4aq2a — sect4a_harvestw4") + THEME

p_morb_age <- ggplot(morb_age, aes(x=age_grp, y=pct, fill=pct)) +
  geom_col(alpha=0.87) +
  geom_errorbar(aes(ymin=pct_lo,ymax=pct_hi), width=0.2, linewidth=1, color="grey20") +
  geom_text(aes(label=sprintf("%.1f%%",pct)), vjust=-0.8, size=3.3, fontface="bold") +
  scale_fill_gradient(low="#AED6F1", high="#1A5276", guide="none") +
  scale_y_continuous(labels=function(x) paste0(x,"%"), limits=c(0,60)) +
  labs(title="Morbidité par groupe d'âge — W4",
       subtitle="Nette augmentation après 45 ans",
       x="Groupe d'âge", y="Taux (%)",
       caption="Source : GHS Panel W4 | s4aq3 + s4aq2a") + THEME

p_morb_comb <- (p_morb_sex | p_morb_age) +
  plot_annotation(title="Tâche 13 — Morbidité par sexe et groupe d'âge · W4 (2018)")
ggsave(file.path(OUT,"T13_morbidite.png"), p_morb_comb, w=14,h=6,dpi=200,bg="white")
print(p_morb_comb)

# ── 5. TÂCHE 14 — TYPES DE MALADIES ──────────────────────────────────────────
message("\n=== Tâche 14 : Types de maladies ===")
ill_cat_fn <- function(v) {
  c <- suppressWarnings(as.integer(str_extract(v,"^\\d+")))
  case_when(c %in% c(1:8)         ~ "Infectieuse",
            c %in% c(9,10,15,16,17) ~ "Respiratoire",
            c == 11                ~ "Traumatique",
            c %in% 20:27           ~ "Chronique",
            !is.na(c)              ~ "Autre",
            TRUE                   ~ NA_character_)
}
ill_df <- df_h %>%
  filter(!is.na(s4aq3b_1)) %>%
  mutate(ill_name=str_remove(as.character(s4aq3b_1),"^\\d+\\.\\s*"),
         ill_cat =ill_cat_fn(as.character(s4aq3b_1))) %>%
  filter(!str_detect(ill_name,"^NA$")) %>%
  count(ill_name, ill_cat) %>% arrange(desc(n)) %>% head(10) %>%
  mutate(ill_name=fct_reorder(ill_name,n))

COLS_ILL <- c("Infectieuse"="#E74C3C","Respiratoire"="#3498DB",
              "Traumatique"="#E67E22","Chronique"="#8E44AD","Autre"="#95A5A6")

p_ill <- ggplot(ill_df, aes(x=n, y=ill_name, fill=ill_cat)) +
  geom_col(alpha=0.87, width=0.72) +
  geom_text(aes(label=sprintf("%s  (%.1f%%)", comma(n), n/sum(n)*100)),
            hjust=-0.05, size=3.4, fontface="bold") +
  scale_fill_manual(values=COLS_ILL, name="Catégorie", na.value="#95A5A6") +
  scale_x_continuous(expand=expansion(mult=c(0,.20)), labels=comma) +
  labs(title="Top 10 des maladies/blessures déclarées — W4 (2018)",
       subtitle="s4aq3b_1 | Paludisme = 1ère cause | Coloré par catégorie",
       x="Nombre de cas", y=NULL,
       caption="Source : GHS Panel W4 | sect4a_harvestw4") +
  THEME + theme(legend.position="right")
ggsave(file.path(OUT,"T14_types_maladies.png"), p_ill, w=11,h=7,dpi=200,bg="white")
print(p_ill)

# ── 6. TÂCHE 15 — RECOURS AUX SOINS ──────────────────────────────────────────
message("\n=== Tâche 15 : Recours aux soins ===")
prov_map <- c("0"="Aucun soignant","1"="Guérisseur trad.","2"="Médecin",
              "3"="Dentiste","4"="Infirmier(e)","5"="Assist. médical",
              "6"="Sage-femme","7"="Pharmacist","8"="Chemist/Pharmacien",
              "9"="Accoucheuse","10"="Bénévole santé","11"="Autre","15"="Agent CHEW")
prov_df <- df_h %>%
  filter(!is.na(s4aq6a)) %>%
  mutate(prov_n=coalesce(prov_map[prov_code], paste0("Code ",prov_code))) %>%
  count(prov_n) %>%
  mutate(pct=n/sum(n)*100, prov_n=fct_reorder(prov_n,n))

p_prov <- ggplot(prov_df, aes(x=pct, y=prov_n, fill=fct_rev(prov_n))) +
  geom_col(alpha=0.87, width=0.72) +
  geom_text(aes(label=sprintf("%.1f%%  (%s)", pct, comma(n))),
            hjust=-0.05, size=3.4, fontface="bold") +
  scale_fill_viridis_d(option="turbo", direction=-1, guide="none") +
  scale_x_continuous(labels=function(x) paste0(x,"%"),
                     expand=expansion(mult=c(0,.28))) +
  labs(title="Recours aux soins par type de prestataire — W4 (2018)",
       subtitle="s4aq6a | Chemist = 1er recours (46%)",
       x="Proportion (%)", y=NULL,
       caption="Source : GHS Panel W4 | sect4a_harvestw4") + THEME
ggsave(file.path(OUT,"T15_recours_soins.png"), p_prov, w=11,h=7,dpi=200,bg="white")
print(p_prov)

# ── 7. TÂCHE 16 — DÉPENSES TOTALES DE SANTÉ ──────────────────────────────────
message("\n=== Tâche 16 : Dépenses totales de santé ===")
dep_pos <- df_h %>% filter(dep_total > 0) %>% pull(dep_total)
cat(sprintf("N individus avec dépenses > 0 : %s\n", comma(length(dep_pos))))
cat(sprintf("Composition : consult=%s₦ | transport=%s₦ | méds=%s₦ | hosp=%s₦\n",
            comma(round(mean(df_h$dep_consult))),
            comma(round(mean(df_h$dep_transport))),
            comma(round(mean(df_h$dep_meds))),
            comma(round(mean(df_h$dep_hosp)))))
cat(sprintf("Médiane totale : %s₦ | Moyenne : %s₦\n",
            comma(median(dep_pos)), comma(round(mean(dep_pos)))))
cat("Déciles :\n")
print(round(quantile(dep_pos, seq(.1,.9,.1)), 0))

# Seuil outliers IQR×3
q1_d <- quantile(dep_pos,.25); q3_d <- quantile(dep_pos,.75)
seuil_out <- q3_d + 3*(q3_d-q1_d)
cat(sprintf("Outliers (> %s₦) : %d\n", comma(seuil_out),
            sum(dep_pos > seuil_out)))

p_dep_hist <- ggplot(df_h %>% filter(dep_total > 0, dep_total < seuil_out),
                     aes(x = dep_total)) +
  geom_histogram(bins = 55, fill = COL_M, color = "white", alpha = 0.85) +
  scale_x_log10(labels = label_dollar(prefix="₦", big.mark=" "),
                breaks = c(10,50,200,500,2000,5000,20000)) +
  geom_vline(xintercept = median(dep_pos), color = COL_F,
             linewidth = 1.2, linetype = "dashed") +
  annotate("text", x=median(dep_pos)*2, y=Inf, vjust=1.6,
           label=sprintf("Médiane = %s₦", comma(median(dep_pos))),
           color=COL_F, size=3.5, fontface="bold") +
  labs(title="Distribution des dépenses totales de santé — W4 (2018)",
       subtitle="s4aq9 + s4aq10 + s4aq14 + s4aq17 | Échelle log | Outliers exclus",
       x="Dépenses totales (₦, log scale)", y="Effectif",
       caption=sprintf("N=%s | Méd=%s₦ | Moy=%s₦\nSource : GHS Panel W4 | sect4a_harvestw4",
                       comma(length(dep_pos)), comma(median(dep_pos)),
                       comma(round(mean(dep_pos))))) + THEME
ggsave(file.path(OUT,"T16_depenses_sante.png"), p_dep_hist, w=10,h=6,dpi=200,bg="white")
print(p_dep_hist)

# ── 8. TÂCHE 17 — RECOURS × QUINTILE RICHESSE ────────────────────────────────
message("\n=== Tâche 17 : Recours × quintile richesse ===")
df_hq <- df_h %>% filter(!is.na(quintile), !is.na(consulted))

cont_q <- table(as.integer(df_hq$consulted), df_hq$quint_label)
if (min(cont_q) < 5) {
  test_q <- fisher.test(cont_q, simulate.p.value=TRUE)
  cat(sprintf("Fisher p=%.4f\n", test_q$p.value))
} else {
  test_q <- chisq.test(cont_q)
  V_q    <- sqrt(test_q$statistic/(sum(cont_q)*(min(nrow(cont_q),ncol(cont_q))-1)))
  cat(sprintf("Chi²=%.2f · p=%.2e · V Cramér=%.3f\n",
              test_q$statistic, test_q$p.value, V_q))
}

q_consult <- df_hq %>%
  group_by(quint_label) %>%
  summarise(N=n(), n_cons=sum(consulted), pct=n_cons/N*100, .groups="drop")

p_quint <- ggplot(q_consult, aes(x=quint_label, y=pct, fill=quint_label)) +
  geom_col(alpha=0.87, width=0.62) +
  geom_text(aes(label=sprintf("%.1f%%",pct)), vjust=-0.5, size=3.8, fontface="bold") +
  scale_fill_viridis_d(option="plasma", guide="none") +
  scale_y_continuous(labels=function(x) paste0(x,"%"), limits=c(0,35)) +
  annotate("label", x=3, y=30,
           label=sprintf("Chi²=%.1f\np<0.001\nV Cramér=%.3f",
                         test_q$statistic, if(exists("V_q")) V_q else NA),
           size=3.3, hjust=0.5, fill="white", label.size=0.3) +
  labs(title="Taux de consultation × quintile de consommation — W4 (2018)",
       subtitle="Q1=Très pauvres → Q5=Riches | Gradient = inégalités d'accès",
       x="Quintile de consommation", y="Taux de consultation (%)",
       caption="Source : GHS Panel W4 | sect4a × totcons_final") + THEME
ggsave(file.path(OUT,"T17_recours_quintile.png"), p_quint, w=10,h=7,dpi=200,bg="white")
print(p_quint)

# ── 9. TÂCHE 18 — DÉPENSES RURAL vs URBAIN (VIOLIN) ──────────────────────────
message("\n=== Tâche 18 : Dépenses Rural vs Urbain ===")
dep_sec <- df_h %>% filter(dep_total>0, dep_total<seuil_out, !is.na(sec_label))
wx_e <- wilcox.test(dep_total ~ sec_label, data=dep_sec, exact=FALSE)
n1 <- sum(dep_sec$sec_label=="Rural"); n2 <- sum(dep_sec$sec_label=="Urbain")
r_e <- as.numeric(wx_e$statistic)/(n1*n2)
cat(sprintf("Wilcoxon W=%.0f · p=%.4f · r=%.3f\n", wx_e$statistic, wx_e$p.value, r_e))

med_sec <- dep_sec %>%
  group_by(sec_label) %>%
  summarise(med=median(dep_total), moy=round(mean(dep_total)), .groups="drop")

p_viol <- ggplot(dep_sec, aes(x=sec_label, y=dep_total, fill=sec_label)) +
  geom_violin(alpha=0.70, scale="width", trim=TRUE) +
  geom_boxplot(width=0.11, fill="white", color="grey20",
               outlier.shape=NA, linewidth=0.9) +
  stat_summary(fun=median, geom="point", shape=21, size=5, fill="white") +
  geom_text(data=med_sec,
            aes(x=sec_label, y=med,
                label=sprintf("Méd\n%s₦",comma(med))),
            vjust=-1.3, size=3.4, fontface="bold") +
  scale_fill_manual(values=c("Rural"=COL_R,"Urbain"=COL_U), guide="none") +
  scale_y_log10(labels=label_dollar(prefix="₦",big.mark=" "),
                breaks=c(10,50,200,500,2000,5000,20000)) +
  annotate("label", x=1.5, y=max(dep_sec$dep_total)*0.8,
           label=sprintf("Wilcoxon\nW=%s\np=%.4f\nr=%.3f",
                         comma(wx_e$statistic), wx_e$p.value, r_e),
           size=3.3, hjust=0.5, fill="white", label.size=0.3) +
  labs(title="Dépenses totales santé : Rural vs Urbain — W4 (2018)",
       subtitle="Violin + boxplot | Échelle log | s4aq9+s4aq10+s4aq14+s4aq17",
       x="Milieu", y="Dépenses totales (₦, log scale)",
       caption=sprintf("Wilcoxon W=%s · p=%.4f · r=%.3f\nSource : GHS Panel W4",
                       comma(wx_e$statistic), wx_e$p.value, r_e)) + THEME
ggsave(file.path(OUT,"T18_depenses_rural_urbain.png"), p_viol, w=9,h=8,dpi=200,bg="white")
print(p_viol)

# ── 10. TABLEAU GTSUMMARY ─────────────────────────────────────────────────────
message("\n=== Tableau gtsummary ===")
print(
  tbl_summary(
    df_h %>% filter(!is.na(sec_label)) %>%
      mutate(`Malade (4 sem.)`=factor(as.integer(malade),0:1,c("Non","Oui")),
             Consultation=factor(as.integer(consulted),0:1,c("Non","Oui")),
             `Dép. santé totales (₦)`=if_else(dep_total>0,dep_total,NA_real_),
             Milieu=sec_label, Sexe=sex_label, `Âge`=age) %>%
      select(Milieu,Sexe,`Âge`,`Malade (4 sem.)`,Consultation,`Dép. santé totales (₦)`),
    by=Milieu,
    statistic=list(all_continuous()~"{median} [{p25}–{p75}]",
                   all_categorical()~"{n} ({p}%)")
  ) %>%
    add_p(test=list(all_categorical()~"chisq.test",all_continuous()~"wilcox.test")) %>%
    add_overall() %>% bold_labels() %>%
    modify_caption("**Tableau 3. Indicateurs de santé par milieu — W4 (2018)**") %>%
    modify_footnote(everything()~"Dépenses = s4aq9+s4aq10+s4aq14+s4aq17 (4 composantes)")
)

# ── 11. FIGURE DE SYNTHÈSE ────────────────────────────────────────────────────
message("\n=== Figure de synthèse TP3 ===")
fig_tp3 <- (p_morb_sex | p_morb_age) /
           (p_ill       | p_prov)     /
           (p_viol      | p_quint)    +
  plot_annotation(
    title    = "TP3 — Accès aux Soins & Dépenses de Santé · GHS Panel W4 (2018)",
    subtitle = "Auteurs : Cheikh M. M. Ndiaye & Awa Ba | ENSAE ISE 1 | 2025-2026",
    caption  = paste0("Bases : sect4a_harvestw4 (SANTÉ) + sect1_harvestw4 + totcons_final\n",
                      "Morbidité : s4aq3 + s4aq2a (1=maladie,2=blessure) | ",
                      "Dépenses : s4aq9+s4aq10+s4aq14+s4aq17"),
    theme=theme(plot.title=element_text(face="bold",size=15),
                plot.subtitle=element_text(size=10,color="grey35"),
                plot.caption=element_text(size=8,color="grey50"))
  )
ggsave(file.path(OUT,"TP3_Figure_Synthese.png"), fig_tp3,
       w=20, h=22, dpi=200, bg="white")
message(sprintf("✓ Figure de synthèse → %s", file.path(OUT,"TP3_Figure_Synthese.png")))
message("\n✅ TP3 TERMINÉ")
