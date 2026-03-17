# Produire une table des fichiers vraiment utiles par analyse

source("R/01_packages.R")
source("R/02_paths.R")

catalogue <- readr::read_csv(
  here::here("data", "processed", "final", "catalogue_fichiers_dta.csv"),
  show_col_types = FALSE
)

plan_analyses <- tibble::tibble(
  analyse = c(
    "Analyse 1 - Démographie ménage",
    "Analyse 1 - Démographie ménage",
    "Analyse 2 - Education",
    "Analyse 2 - Education",
    "Analyse 2 - Education",
    "Analyse 3 - Santé",
    "Analyse 3 - Santé",
    "Analyse 3 - Santé"
  ),
  wave = c(
    "W1_2010",
    "W4_2018",
    "W1_2010",
    "W1_2010",
    "W4_2018",
    "W1_2010",
    "W1_2010",
    "W4_2018"
  ),
  fichier_recherche = c(
    "sect1_harvestw1.dta",
    "sect1_harvestw4.dta",
    "sect2a_harvestw1.dta",
    "sect2b_harvestw1.dta",
    "sect2_harvestw4.dta",
    "sect3a_harvestw1.dta",
    "sect3b_harvestw1.dta",
    "sect3a_harvestw4.dta"
  )
)

plan_detaille <- plan_analyses |>
  dplyr::left_join(
    catalogue |>
      dplyr::select(wave, file_name, full_path),
    by = c("wave" = "wave", "fichier_recherche" = "file_name")
  ) |>
  dplyr::mutate(
    fichier_trouve = !is.na(full_path)
  ) |>
  dplyr::arrange(analyse, wave, fichier_recherche)

readr::write_csv(
  plan_detaille,
  here::here("data", "processed", "final", "plan_analyses_fichiers.csv")
)

print(plan_detaille, n = Inf)

cat("\nRésumé des fichiers trouvés / non trouvés :\n")
print(plan_detaille |>
        dplyr::count(analyse, fichier_trouve))