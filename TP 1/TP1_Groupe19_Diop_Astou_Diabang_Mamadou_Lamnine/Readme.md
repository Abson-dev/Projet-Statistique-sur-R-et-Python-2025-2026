\# TP1 — Profil démographique des ménages nigérians



\## Informations générales



| | |

|---|---|

| \*\*Cours\*\* | Projet Statistique sous R et Python 2025-2026 |

| \*\*Établissement\*\* | ENSAE Pierre Ndiaye, Dakar |

| \*\*Programme\*\* | ISE1-CL |

| \*\*Enseignant\*\* | Aboubacar HEMA |

| \*\*Auteurs\*\* | Astou DIOP \& Mamadou Lamine DIABANG |



\---



\## Description



Ce projet propose une \*\*analyse descriptive du profil démographique des ménages nigérians\*\* à partir des données du \*\*Nigeria General Household Survey (GHS) Panel\*\*, couvrant quatre vagues d’enquête (2010–2018).



L’objectif est d’examiner les principales caractéristiques démographiques de la population et d’identifier certaines évolutions entre les différentes vagues de l’enquête.



L’analyse se concentre sur plusieurs variables clés :



\- âge des individus  

\- sexe  

\- statut matrimonial  

\- lien de parenté avec le chef de ménage  

\- taille des ménages  

\- milieu de résidence (urbain / rural)



\---



\## Tâches réalisées



1\. Vérification de la qualité des variables (valeurs manquantes et cohérence des données).

2\. Analyse descriptive de la distribution de l’âge de la population.

3\. Étude de la répartition de la population selon le sexe.

4\. Analyse de la distribution du statut matrimonial.

5\. Analyse du lien de parenté avec le chef de ménage.

6\. Construction d’une pyramide des âges par sexe.

7\. Analyse de la relation entre la taille des ménages et le milieu de résidence.

8\. Étude de l’évolution des caractéristiques démographiques entre les vagues W1, W2, W3 et W4.



\---



\## Structure du projet



TP1/

├── data/                                   <- Données brutes du GHS Panel (.dta)

│   ├── GHS\_W1.dta

│   ├── GHS\_W2.dta

│   ├── GHS\_W3.dta

│   └── GHS\_W4.dta

│

├── script.R                                 <- Script principal d'analyse sous R

│

├── outputs/                                <- Graphiques générés automatiquement

│   ├── T1\_qualite\_variables.png             <- Vérification de la qualité des variables

│   ├── T2\_distribution\_age.png              <- Distribution de l’âge

│   ├── T3\_distribution\_sexe.png             <- Distribution du sexe / taux de féminisation

│   ├── T4\_statut\_matrimonial.png            <- Distribution du statut matrimonial

│   ├── T5\_pyramide\_ages.png                 <- Pyramide des âges par sexe

│   ├── T6\_evolution\_age.png                 <- Évolution de la distribution des âges

│   ├── T7\_lien\_parente.png                  <- Lien de parenté avec le chef de ménage

│   └── T8\_taille\_menage\_milieu.png          <- Taille des ménages selon le milieu

│

│   ├── TP1.pdf                     <- Rapport final

│

└── README.md                               <- Description du projet





\---



\## Livrables



| Livrable | Fichier |

|---|---|

| Vérification de la qualité des variables | `outputs/T1\_qualite\_variables.png` |

| Distribution de l’âge | `outputs/T2\_distribution\_age.png` |

| Distribution du sexe | `outputs/T3\_distribution\_sexe.png` |

| Distribution du statut matrimonial | `outputs/T4\_statut\_matrimonial.png` |

| Pyramide des âges par sexe | `outputs/T5\_pyramide\_ages.png` |

| Évolution de la distribution des âges | `outputs/T6\_evolution\_age.png` |

| Lien de parenté avec le chef de ménage | `outputs/T7\_lien\_parente.png` |

| Taille des ménages selon le milieu | `outputs/T8\_taille\_menage\_milieu.png` |



\---





\## Source des données



Nigeria General Household Survey (GHS) Panel, Waves 1–4 (2010–2018).

Programme LSMS-ISA, Banque Mondiale.

https://microdata.worldbank.org/index.php/catalog/lsms

