# ==============================================================================
# assemble_tp1.R
# Compile le rapport TP1 + insère la page de garde + TOC -> document final
# Prérequis dans le même dossier :
#   - Rapport.Rmd
#   - template.docx   (styles académiques)
#   - page_garde.docx (page de garde professionnelle)
# ==============================================================================

library(officer)

dossier <- "C:/Users/HP/Desktop/ISE1 CL/Semestre2/Projet statistique avec R ou python/Dossier R/TP/TP1/docs"
setwd(dossier)

# Vérification des fichiers
cat("Fichiers présents :\n")
cat("- Rapport.Rmd :", file.exists("Rapport.Rmd"), "\n")
cat("- template.docx        :", file.exists("template.docx"), "\n")
cat("- page_garde.docx      :", file.exists("page_garde.docx"), "\n\n")

# 1. Compiler le Rmd avec le template
cat("Compilation du Rmd...\n")
rmarkdown::render(
  input         = "Rapport.Rmd",
  output_format = rmarkdown::word_document(
    reference_docx  = "template.docx",
    toc             = FALSE,
    number_sections = TRUE
  ),
  output_file = "corps_temp.docx"
)
cat("Compilation terminée.\n\n")

# 2. Assembler : on part du corps (qui a les styles du template)
cat("Assemblage en cours...\n")

corps <- read_docx("corps_temp.docx")

# Insérer un saut de page en tête (entre TOC et chapitre 1)
corps <- cursor_begin(corps)
corps <- body_add_break(corps, pos = "before")

# Insérer la TOC
corps <- cursor_begin(corps)
corps <- body_add_toc(corps, level = 3, pos = "before")

# Insérer le titre "Table des matières" avant la TOC
corps <- cursor_begin(corps)
corps <- body_add_par(
  corps,
  value = "Table des matieres",
  style = "heading 1",
  pos   = "before"
)

# Insérer saut de page avant le titre TOC
corps <- cursor_begin(corps)
corps <- body_add_break(corps, pos = "before")

# Insérer la page de garde tout au début
corps <- cursor_begin(corps)
corps <- body_add_docx(corps, src = "page_garde.docx", pos = "before")

# 3. Sauvegarder
print(corps, target = "Rapport.docx")

# 4. Nettoyage
file.remove("corps_temp.docx")

cat("Termine : Rapport.docx\n")

