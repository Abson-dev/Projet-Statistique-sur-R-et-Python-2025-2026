# =========================================================
# 02_inventory_and_extract.R
# Recense les zip et les décompresse proprement
# =========================================================

zip_files <- list.files(path_raw, pattern = "\\.zip$", full.names = TRUE)

if (length(zip_files) == 0) {
  stop("Aucun fichier .zip trouvé dans data_raw/. Copie d'abord les données dans ce dossier.")
}

cat("ZIP détectés :\n")
print(basename(zip_files))

# Décompression dans un sous-dossier par zip
for (zf in zip_files) {
  zip_name <- tools::file_path_sans_ext(basename(zf))
  out_dir  <- file.path(path_unzipped, zip_name)
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  existing_files <- list.files(out_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(existing_files) == 0) {
    unzip(zipfile = zf, exdir = out_dir)
    cat("Décompressé :", basename(zf), "\n")
  } else {
    cat("Déjà décompressé :", basename(zf), "\n")
  }
}

# Inventaire de tous les .dta disponibles
all_dta_files <- list.files(
  path_unzipped,
  pattern = "\\.dta$",
  recursive = TRUE,
  full.names = TRUE
)

inventory <- data.frame(
  file_name = basename(all_dta_files),
  full_path = normalizePath(all_dta_files, winslash = "/", mustWork = FALSE),
  stringsAsFactors = FALSE
) %>%
  distinct(file_name, .keep_all = TRUE) %>%
  arrange(file_name)

readr::write_csv(inventory, file.path(path_logs, "inventory_dta_files.csv"))

cat("Nombre de fichiers .dta trouvés :", nrow(inventory), "\n")