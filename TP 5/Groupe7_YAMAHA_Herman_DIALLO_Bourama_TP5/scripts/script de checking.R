# Charger le package haven (déjà utilisé pour read_dta)
library(haven)
library(labelled)

# Supposons que tu as déjà importé ton fichier :
# s3i_w4 <- read_dta("data/raw/secta3i_harvestw4.dta")

# Vérifier la structure de la variable cropcode
str(s3i_w4$cropcode)

# Récupérer les labels associés aux valeurs de cropcode
labels_crop <- labelled::val_labels(s3i_w4$cropcode)

# Afficher les étiquettes
labels_crop
