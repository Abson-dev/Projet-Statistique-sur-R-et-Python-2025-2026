# ==============================================================================
# assemble_tp2.R
# ==============================================================================

library(officer)

dossier <- "C:/Users/HP/Desktop/ISE1 CL/Semestre2/Projet statistique avec R ou python/Dossier R/TP/TP2/docs"
setwd(dossier)

cat("Fichiers présents :\n")
cat("- Rapport.Rmd :", file.exists("Rapport.Rmd"), "\n")
cat("- template.docx        :", file.exists("template.docx"), "\n")
cat("- page_garde.docx      :", file.exists("page_garde.docx"), "\n\n")

# 1. Compiler le Rmd
cat("Compilation du Rmd...\n")
rmarkdown::render(
  input         = "Rapport.Rmd",
  output_format = rmarkdown::word_document(
    reference_docx  = "template.docx",
    toc             = FALSE,
    number_sections = FALSE
  ),
  output_file = "corps_temp.docx"
)
cat("Compilation terminée.\n\n")

# 2. Assembler
cat("Assemblage en cours...\n")

corps <- read_docx("corps_temp.docx")

corps <- cursor_begin(corps)
corps <- body_add_toc(corps, level = 3, pos = "before")

corps <- cursor_begin(corps)
corps <- body_add_par(
  corps,
  value = "Table des matieres",
  style = "heading 1",
  pos   = "before"
)

corps <- cursor_begin(corps)
corps <- body_add_break(corps, pos = "before")

corps <- cursor_begin(corps)
corps <- body_add_docx(corps, src = "page_garde.docx", pos = "before")

# 3. Sauvegarder
print(corps, target = "Rapport.docx")

# 4. Nettoyage
file.remove("corps_temp.docx")

cat("Termine : Rapport.docx\n")
cat("Ouvrez dans Word et faites Ctrl+A puis F9 pour mettre a jour la TOC.\n")
