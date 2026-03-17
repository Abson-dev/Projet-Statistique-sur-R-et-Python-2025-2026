# =========================================================
# 01_paths.R
# Définit tous les chemins du projet + télécharge les données
# depuis GitHub Raw si elles ne sont pas présentes
# =========================================================

project_root <- getwd()

path_raw        <- file.path(project_root, "data_raw")
path_unzipped   <- file.path(project_root, "data_unzipped")
path_output     <- file.path(project_root, "output")
path_figures    <- file.path(path_output, "figures")
path_tables     <- file.path(path_output, "tables")
path_logs       <- file.path(path_output, "logs")
path_clean      <- file.path(path_output, "data_clean")
path_R          <- file.path(project_root, "R")
path_report     <- file.path(project_root, "report")

dir.create(path_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(path_unzipped, recursive = TRUE, showWarnings = FALSE)
dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_logs, recursive = TRUE, showWarnings = FALSE)
dir.create(path_clean, recursive = TRUE, showWarnings = FALSE)
dir.create(path_report, recursive = TRUE, showWarnings = FALSE)

cat("Projet :", project_root, "\n")

# =========================================================
# Paramètres GitHub
# =========================================================
github_owner  <- "Herman-YAMAHA"
github_repo   <- "NYHP"
github_commit <- "fd1c118273326e9b0b8acc5a7b494c047bb42934"
github_folder <- "Données_TP"

data_files <- c(
  "NGA_2010_GHSP-W1_v03_M_STATA.zip",
  "NGA_2012_GHSP-W2_v02_M_STATA.zip",
  "NGA_2015_GHSP-W3_v02_M_Stata.zip",
  "NGA_2018_GHSP-W4_v03_M_Stata12.zip"
)

# =========================================================
# Fonction téléchargement via raw.githubusercontent.com
# =========================================================
download_raw_github_data <- function(owner, repo, commit, folder, files, dest_dir) {
  for (f in files) {
    dest_file <- file.path(dest_dir, f)
    
    if (!file.exists(dest_file)) {
      url <- sprintf(
        "https://raw.githubusercontent.com/%s/%s/%s/%s/%s",
        owner, repo, commit, utils::URLencode(folder, reserved = TRUE), utils::URLencode(f, reserved = TRUE)
      )
      
      cat("Téléchargement :", f, "\n")
      download.file(url = url, destfile = dest_file, mode = "wb", quiet = FALSE)
    } else {
      cat("Déjà présent :", f, "\n")
    }
  }
}

# =========================================================
# Téléchargement si fichiers absents
# =========================================================
download_raw_github_data(
  owner   = github_owner,
  repo    = github_repo,
  commit  = github_commit,
  folder  = github_folder,
  files   = data_files,
  dest_dir = path_raw
)

# =========================================================
# Décompression des zip s'ils ne sont pas encore extraits
# =========================================================
for (zip_name in data_files) {
  zip_path <- file.path(path_raw, zip_name)
  
  # nom du dossier d'extraction basé sur le zip
  extract_dir <- file.path(
    path_unzipped,
    tools::file_path_sans_ext(zip_name)
  )
  
  if (file.exists(zip_path) && !dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Décompression :", zip_name, "\n")
    unzip(zipfile = zip_path, exdir = extract_dir)
  } else if (dir.exists(extract_dir)) {
    cat("Déjà décompressé :", zip_name, "\n")
  }
}