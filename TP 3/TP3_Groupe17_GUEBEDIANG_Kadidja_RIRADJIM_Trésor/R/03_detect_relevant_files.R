# =========================================================
# 03_detect_relevant_files.R
# Détection explicite des fichiers utiles pour l'analyse 3
# =========================================================

inventory <- readr::read_csv(
  file.path(path_logs, "inventory_dta_files.csv"),
  show_col_types = FALSE
) %>%
  mutate(file_name_lower = tolower(file_name))

find_exact_file <- function(target_name, inventory_df) {
  hit <- inventory_df %>%
    filter(file_name_lower == tolower(target_name)) %>%
    slice(1)
  
  if (nrow(hit) == 0) return(NA_character_)
  hit$full_path[[1]]
}

relevant_files <- list(
  indiv_w4  = find_exact_file("sect1_harvestw4.dta", inventory),
  health_w4 = find_exact_file("sect4a_harvestw4.dta", inventory),
  cons_w4   = find_exact_file("totcons_final.dta", inventory)
)

relevant_files_df <- tibble::tibble(
  dataset = names(relevant_files),
  path = unlist(relevant_files)
)

readr::write_csv(
  relevant_files_df,
  file.path(path_logs, "relevant_files_detected.csv")
)

cat("Fichiers utiles détectés :\n")
print(relevant_files_df)

if (is.na(relevant_files$indiv_w4)) {
  stop("Le fichier sect1_harvestw4.dta est introuvable.")
}
if (is.na(relevant_files$health_w4)) {
  stop("Le fichier sect4a_harvestw4.dta est introuvable.")
}
if (basename(relevant_files$health_w4) != "sect4a_harvestw4.dta") {
  stop("Erreur de détection : la base santé W4 n'est pas sect4a_harvestw4.dta.")
}
if (is.na(relevant_files$cons_w4)) {
  stop("Le fichier totcons_final.dta est introuvable, alors qu'il est nécessaire pour les pondérations.")
}
