source("R/fonctions.R")

#Importation des données
secta3i<- read_dta("data/raw/NGA-GHSP-W4/secta3i_harvestw4.dta")
View(secta3i)
vis_miss(secta3i)
attr(secta3i$cropcode, "labels")
unique(secta3i$hhid)
table(secta3i$cropcode)


secta3ii<- read_dta("data/raw/NGA-GHSP-W4/secta3ii_harvestw4.dta")
View(secta3ii)
attr(secta3ii$cropcode, "labels")
attr(secta3ii$hhid, "labels")
unique(secta3ii$hhid)
table(secta3ii$cropcode)

secta1<-read_dta("data/raw/NGA-GHSP-W4/secta1_harvestw4.dta")
View(secta1)

sect11f<- read_dta("data/raw/NGA-GHSP-W4/sect11f_plantingw4.dta")
View(sect11f)

secta_p <- read_dta("data/raw/NGA-GHSP-W4/secta_plantingw4.dta")
View(secta_p)
attr(secta_p$interview_result,"labels")

secta_h<- read_dta("data/raw/NGA-GHSP-W4/secta_harvestw4.dta")
View(secta_h)
attr(secta_h$interview_result,"labels")

secta11c2<- read_dta("data/raw/NGA-GHSP-W4/secta11c2_harvestw4.dta")
View(secta11c2)
n_distinct(secta11c2$hhid)

sect11a1<- read_dta("data/raw/NGA-GHSP-W4/sect11a1_plantingw4.dta")
View(sect11a1)
