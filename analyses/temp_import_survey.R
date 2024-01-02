#!/usr/bin/env Rscript

library("RPostgreSQL")
library("data.table")

# database and user parameters
host <- "cbgp-gaia2" ; port <- 5432 ; db   <- "rongeurs" ; user <- "mus" ; pwd  <- "musculus" # read-only account (safe !)
conn <- dbConnect(PostgreSQL(), host=host, port=port, user=user, password=pwd, dbname=db)

# The query
sql <- "SELECT mi.code_mission, 
	lo.localite, 
	li.date_2 AS date_pose, li.numero_ligne,
	re.date_releve_2 AS date_releve, re.numero_releve,
	pg.numero AS numero_piege, pg.latitude AS latitude_piege, pg.longitude AS longitude_piege,
	pi.code_resultat,
	ttr.affichage AS abbrev_resultat, ttr.resultat,
	ca.numero_terrain, ca.commentaire,
	txc.taxon_name AS taxon_capture,
	dis.numero_centre, dis.date_dissection_2 AS date_dissection, dis.sexe, dis.observations,
	iden.methode_identification,
	txi.taxon_name AS taxon_dissection
FROM t_missions AS mi
LEFT JOIN t_lignes AS li ON li.id_mission = mi.id
LEFT JOIN t_localites AS lo ON li.id_localite = lo.id
LEFT JOIN t_releves AS re ON re.id_ligne = li.id
LEFT JOIN t_pieges AS pg ON pg.id_ligne = li.id
LEFT JOIN t_piegeages AS pi ON (pi.id_piege = pg.id AND pi.id_releve = re.id)
LEFT JOIN t_types_resultats AS ttr ON pi.code_resultat = ttr.code_resultat
LEFT JOIN t_captures AS ca ON ca.id_piegeage = pi.id
LEFT JOIN t_taxons_nomenclature AS txnc ON ca.id_nomenclature = txnc.id
LEFT JOIN t_taxons AS txc ON txnc.id_taxon = txc.id
LEFT JOIN t_dissections AS dis ON dis.id_capture = ca.id
LEFT JOIN t_identifications AS iden ON iden.id_dissection = dis.id
LEFT JOIN t_taxons AS txi ON iden.id_taxon = txi.id
WHERE mi.code_mission ~* 'BePrep'
ORDER BY mi.date_debut_2, li.numero_ligne, re.date_releve_2, pg.numero, dis.numero_centre
;"

# Get query (wait a few seconds)
DATA0 <- as.data.table(dbGetQuery(conn, sql))

# Create one column by identification method
DATA <- dcast(DATA0, code_mission+localite+date_pose+numero_ligne+date_releve+numero_releve+numero_piege+latitude_piege+longitude_piege+code_resultat+abbrev_resultat+resultat+numero_terrain+commentaire+taxon_capture+numero_centre+date_dissection+sexe+observations+taxon_dissection ~ methode_identification, value.var="taxon_dissection",fun =length)

# Save results
fwrite(DATA, here::here("data/", "raw-data/", "export_alois20231214.csv") )

dbDisconnect(conn)




################################################################################

#BPM data quality control :

View(DATA %>%
  filter(!is.na(taxon_capture) & is.na(taxon_dissection) ) 
  )

View(DATA %>%
  filter(is.na(taxon_capture) & !is.na(taxon_dissection) ) 
  )

View(DATA %>%
       filter(code_resultat == 1) %>%
       filter(is.na(taxon_capture) & is.na(taxon_dissection) ) 
     )

View(bpm_beprep |>
       dplyr::filter( code_resultat %in% c("1") ) |>
       dplyr::filter( morphologie == "0") 
     )


# Read file

bpm_beprep <- readr::read_csv( here::here("data/", "raw-data/", "export_alois20231214.csv") )


# Data management brut - Rapport

## identification renaming
unique(bpm_beprep$taxon_dissection)

bpm_beprep$taxon_dissection <- as.character(bpm_beprep$taxon_dissection)

bpm_beprep$taxon_dissection[bpm_beprep$taxon_dissection == "Apodemus sylvaticus"] <- "Apodemus"
bpm_beprep$taxon_dissection[bpm_beprep$taxon_dissection == "Sorex"] <- "Insectivore"
bpm_beprep$taxon_dissection[bpm_beprep$taxon_dissection == "Soricidae"] <- "Insectivore"

## Attributing line modality

# Glossary:
# C: Connected hedgerow / NC: Non-connected hedgerow
# CT: Control pine edge
# LB: Low Broadleaved density / HB: High Broadleaved

bpm_beprep$line_treatment <- NA

bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 1] <- "NC_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 2] <- "C_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 3] <- "NC_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 4] <- "NC_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 5] <- "CT_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 6] <- "C_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 7] <- "C_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 8] <- "CT_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 9] <- "NC_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 10] <- "CT_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 11] <- "C_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 12] <- "C_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 13] <- "NC_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 14] <- "CT_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 15] <- "CT_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 16] <- "NC_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 17] <- "C_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 18] <- "CT_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 19] <- "NC_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 20] <- "C_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 21] <- "NC_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 22] <- "CT_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 23] <- "CT_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 24] <- "C_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 25] <- "NC_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 26] <- "CT_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 27] <- "NC_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 28] <- "C_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 29] <- "NC_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 30] <- "C_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 31] <- "NC_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 32] <- "CT_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 33] <- "C_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 34] <- "CT_HB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 35] <- "CT_LB"
bpm_beprep$line_treatment[bpm_beprep$numero_ligne == 36] <- "C_LB"

## Attributing line type according to modality

bpm_beprep <- bpm_beprep |>
  dplyr::mutate(line_type = ifelse(grepl("C_|NC_", line_treatment), "hedge",
                                   ifelse(grepl("CT_", line_treatment), "pine",
                                          "missing")))


# Table making test

## Trapping success for ALL TAXA - ONE MISSION ONLY - 3 DAYS
bpm_beprep |>
  dplyr::filter(date_releve - date_pose <= 3) |>
  dplyr::filter(code_mission == "FRA - BePrep - Octobre 2023" & code_resultat %in% c("0", "1")) |>
  dplyr::filter(morphologie == 1 | `NA` == 1 ) |>
  dplyr::group_by(numero_ligne) |>
  dplyr::summarise(
    treatment = unique(line_treatment),
    line_type = unique(line_type),
    trap_nights = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus")),
    vole = sum(taxon_dissection %in% c("Myodes glareolus")),
    insectivore = sum(taxon_dissection %in% c("Insectivore"))
    ) |>
  dplyr::mutate(succes_rate = round( 100*(apodemus + vole + insectivore) / trap_nights, digits = 1 ) ) 


# Statistical analyses

## Apodemus trapping probability models

### random effect (logit) (ligne = replicat, numero releve : nuit piegeage)
m_trapping_r <- bpm_beprep |>
  dplyr::filter( date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1") ) |>
  dplyr::filter( morphologie == 1 | `NA` == 1 ) |>
  dplyr::filter( taxon_dissection %in% c("Apodemus", NA) ) |>
  lme4::glmer( formula = code_resultat ~ line_treatment + (1|code_mission) + (1|numero_ligne) + (1|numero_releve),
              family = binomial( link = "logit" ),
              na.action = "na.fail",
              control = lme4::glmerControl( optimizer="bobyqa", optCtrl=list(maxfun=2e5) ) ) 

m_trapping_r |>
  RVAideMemoire::overdisp.glmer() # overdispersion testing

DHARMa::simulateResiduals(m_trapping_r) |> #alignement testing
  DHARMa::testResiduals() 

performance::icc(m_trapping_r) # Intraclass Correlation Coefficient
performance ::r2(m_trapping_r) # R2

drop1(m_trapping_r,.~.,test="Chisq") 


## Line trapping rate modelling for Apodemus only

# model all season combine : one data per replicat

bpm_beprep |>
  dplyr::filter( date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1") ) |>
  dplyr::filter( morphologie == 1 | `NA` == 1 ) |>
  dplyr::filter( taxon_dissection %in% c("Apodemus", NA) ) |>
  dplyr::group_by(numero_ligne) |>
  dplyr::summarise(
    treatment = unique(line_treatment),
    trap_nights = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus") ) ) |>
  dplyr::mutate(succes_rate = round( 100*(apodemus) / trap_nights, digits = 1 ) )

  
  
# Faire sur toutes saisons
# soit faire taux piegeage (par ligne) en fonction traitement avec replicat en facteur aleat (ex : 6 replicat par modalite puisque 6 ligne)
# soit faire Capture oui/non par nuit.piege avec nuit releve et ou piege comme facteur aleat (voir replicats ? pertinence)
# faire carte sur R tester


