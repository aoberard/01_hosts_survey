#!/usr/bin/env Rscript

library("RPostgreSQL")
library("data.table")
library("dplyr")



# EXTRACT BPM DATABASE DATA TICKS ----

# database and user parameters 
host <- "cbgp-gaia2" ; port <- 5432 ; db   <- "rongeurs" ; user <- "mus" ; pwd  <- "musculus" # read-only account (safe !)
conn <- dbConnect(PostgreSQL(), host=host, port=port, user=user, password=pwd, dbname=db)



sql_filer_only_tick <- "SELECT mi.code_mission, 
lo.localite, 
li.date_2 AS date_pose, li.numero_ligne,
re.date_releve_2 AS date_releve, re.numero_releve,
pg.numero AS numero_piege, pg.latitude AS latitude_piege, pg.longitude AS longitude_piege,
pi.code_resultat,
ttr.affichage AS abbrev_resultat, ttr.resultat,
ca.numero_terrain, ca.commentaire,
txc.taxon_name AS taxon_capture,
dis.numero_centre, dis.date_dissection_2 AS date_dissection, dis.observations, 
dis.sexe, dis.poids, dis.longueur_tete_corps, dis.longueur_queue,
dis.testicules, dis.testicules_longueur, dis.vesicule_seminale_taille,
dis.vulve_ouverte, dis.lactation, dis.cicatrices_placentaires_presentes, dis.gestation, dis.uterus,
iden.methode_identification,
txi.taxon_name AS taxon_dissection,
par.effectif AS effectif_tique,
txp.taxon_name AS parasite
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
LEFT JOIN t_parasites AS par ON par.id_dissection = dis.id
LEFT JOIN t_taxons_nomenclature AS txnp ON par.id_nomenclature = txnp.id
LEFT JOIN t_taxons AS txp ON txnp.id_taxon = txp.id
WHERE mi.code_mission ~* 'BePrep'
AND (txp.taxon_name~* 'Ixodida' or txp.taxon_name is NULL)
ORDER BY mi.date_debut_2, li.numero_ligne, re.date_releve_2, pg.numero, dis.numero_centre
;"

# Get query (wait a few seconds)
bpm_rodent_ticks <- as.data.table(dbGetQuery(conn, sql_filer_only_tick))

# Save results
fwrite(bpm_rodent_ticks, here::here("data/", "raw-data/", "export_rodent_ticks_alois20240328.csv") )


sql_rodent_fast_parasite <- "SELECT mi.code_mission, 
lo.localite, 
li.date_2 AS date_pose, li.numero_ligne,
re.date_releve_2 AS date_releve,
pi.code_resultat, dis.numero_centre, txi.taxon_name AS taxon_dissection,
dis.sexe, dis.poids, par.effectif AS effectif_tique, txp.taxon_name AS parasite
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
LEFT JOIN t_parasites AS par ON par.id_dissection = dis.id
LEFT JOIN t_taxons_nomenclature AS txnp ON par.id_nomenclature = txnp.id
LEFT JOIN t_taxons AS txp ON txnp.id_taxon = txp.id
WHERE mi.code_mission ~* 'BePrep'
AND pi.code_resultat = 1
ORDER BY mi.date_debut_2, li.numero_ligne, re.date_releve_2, pg.numero, dis.numero_centre
;"
# Get query (wait a few seconds)
bpm_fast_parasite <- as.data.table(dbGetQuery(conn, sql_rodent_fast_parasite))
# Save results
fwrite(bpm_fast_parasite, here::here("data/", "raw-data/", "export_fast_parasite_alois20240328.csv") )



sql_collected_ticks <- "SELECT mi.code_mission, 
lo.localite,
li.numero_ligne,
co.date_collecte,
co.observations
FROM t_collectes AS co
LEFT JOIN t_localites AS lo ON co.id_localite = lo.id
LEFT JOIN t_missions AS mi ON co.id_mission = mi.id
LEFT JOIN t_lignes AS li ON (li.id_mission = mi.id AND li.id_localite = lo.id)
WHERE mi.code_mission ~* 'BePrep'	
ORDER BY co.date_collecte DESC, li.numero_ligne
;"
# Get query (wait a few seconds)
bpm_site_ticks <- as.data.table(dbGetQuery(conn, sql_collected_ticks))
# Save results
fwrite(bpm_site_ticks, here::here("data/", "raw-data/", "export_site_ticks_alois20240328.csv") )

dbDisconnect(conn)


# Collected ticks analysis ----

## Data management ----

# Data file containing tick collected on sites 
site_tique <- data.table::fread(file = here::here( "data","raw-data/","export_site_ticks_alois20240328.csv"))

# Modality attribution 
C <- c(2, 6, 20, 24, 30, 7, 11, 12, 28, 33, 36, 17)
CT <- c(8, 14, 18, 23, 26, 34, 5, 10, 15, 22, 32, 35)
NC <- c(3, 4, 13, 25, 27, 29, 1, 9, 16, 19, 21, 31)

LB <- c(7, 11, 12, 28, 33, 36, 5, 10, 15, 22, 32, 35, 1, 9, 16, 19, 21, 31)
HB <- c(17, 2, 6, 20, 24, 30, 8, 14, 18, 23, 26, 34, 3, 4, 13, 25, 27, 29)

site_tique <- site_tique %>%
  mutate(connectivity = 
           case_when(
             numero_ligne %in% C ~ "C",
             numero_ligne %in% CT ~ "CT",
             numero_ligne %in% NC ~ "NC",
             .default = "problem"
           )
  ) %>%
  mutate(broadleaved_status =
           case_when(
             numero_ligne %in% LB ~ "LB",
             numero_ligne %in% HB ~ "HB",
             .default = "problem"
           )
  ) %>%
  mutate(line_treatment =
           paste0(connectivity, "_", broadleaved_status)
  )

# Cleaning dirty Observation column to extract values
site_tique <- site_tique %>%
  filter(observations == gsub("O T", "0 t", observations )) %>%
  mutate(observations = gsub("[^0-9]", "", observations)) %>%
  mutate(observations = as.numeric(observations))

# Replacing NA observation to 0
site_tique <- site_tique %>%
  mutate(observations = if_else( is.na(observations), 0, observations))

# Data exploration
site_tique %>%
  group_by(code_mission) %>%
  filter(!is.na(x = numero_ligne)) %>%
  summarise(
    tick_number = sum(observations)
  )

# Making data frame for glm model /!\ only  june 2023 now because of missing values for autumn
site_tique_2023 <- site_tique %>%
  filter(!is.na(x = numero_ligne)) %>%
  filter(code_mission == "FRA - BePrep - Juin 2023")

hist(site_tique_2023$observations)



## GLM making (random negative binomial) ----
m_site_tique <- lme4::glmer.nb(
  formula = observations ~ broadleaved_status * connectivity +(1|numero_ligne),
  data = site_tique_2023,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

DHARMa::simulateResiduals(m_site_tique, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)

m_site_tique |>
  RVAideMemoire::overdisp.glmer()

SelectionModels<- MuMIn::dredge(m_site_tique, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels

m_site_tique <- lme4::glmer.nb(
  formula = observations ~ connectivity +(1|numero_ligne),
  data = site_tique_2023,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
DHARMa::simulateResiduals(m_site_tique, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)
drop1(m_site_tique,.~.,test="Chisq")
summary(m_site_tique)

em <- emmeans(m_site_tique, "connectivity", type = "response")
em
plot(em, comparisons = TRUE)

contrast(em, "pairwise", adjust = "Tukey")


# Ticks taken from rodent analysis ----

## Data management ----

# Data file containing microfluidic data
microfluidic <- data.table::fread(file = here::here( "data","raw-data/","20240411_microfluidic_tick.txt"))


# Data file containing only ticks who were found on rodent (no human)
host_tique_pathos <- microfluidic %>%
  filter(sample_nature == "tick" & host_nature =="small_mammal")

# Generate a species column
host_tique_pathos_beta <- host_tique_pathos %>%
  dplyr::select(-`E..coli.(temoin.PCR)`) %>%
  dplyr::mutate(tick_species = dplyr::case_when(
    sample_nature == "tick" & Dermacentor.reticulatus_ITS2 == 1 & Ixodes.ricinus_ITS2 == 0 & Dermacentor.marginatus_ITS2 == 0 ~ "Dermacentor reticulatus",
    sample_nature == "tick" & Dermacentor.reticulatus_ITS2 == 0 & Ixodes.ricinus_ITS2 == 1 & Dermacentor.marginatus_ITS2 == 0 ~ "Ixodes ricinus",
    sample_nature == "tick" & Dermacentor.reticulatus_ITS2 == 0 & Ixodes.ricinus_ITS2 == 0 & Dermacentor.marginatus_ITS2 == 1 ~ "Dermacentor marginatus",
    sample_nature == "tick" & Dermacentor.reticulatus_ITS2 == 0 & Ixodes.ricinus_ITS2 == 0 & Dermacentor.marginatus_ITS2 == 0 & Tique.spp_16S == 1 ~ "Unknown tick",
    sample_nature == "tick" & rowSums(select(., c("Dermacentor.reticulatus_ITS2", "Ixodes.ricinus_ITS2", "Dermacentor.marginatus_ITS2"))) >1 ~ "multiple affiliation",
    TRUE ~ NA_character_ 
  ))

# Delete column without detection
host_tique_pathos_beta <- host_tique_pathos_beta %>%
  select(where(~ any(. != 0)))

# Identify column of pathogen that are not empty
tick_pathos_name <- host_tique_pathos_beta %>%
  select(where(is.integer)) %>%
  select( -c("Ixodes.ricinus_ITS2","Tique.spp_16S","Dermacentor.reticulatus_ITS2","Dermacentor.marginatus_ITS2")) %>%
  colnames()

# Join host data to access locality information (using bpm_beprep from temp_import_survey)
host_tique_pathos_beta <- dplyr::left_join(host_tique_pathos_beta, bpm_beprep, by = c("host_name" = "numero_centre"))

# Calculate prevalence for pathogens - global
prev_tique_tique_pathos <- host_tique_pathos_beta %>%
  filter(sample_nature == "tick") %>%
  summarise( across(all_of(tick_pathos_name), ~ sum(. > 0) / n() ))  %>%
  unlist() 

# Pathogens with more than 10% prevalence
names(prev_tique_tique_pathos[prev_tique_tique_pathos >= 0.10])


## GLM ----

data_for_host_tick_m <- host_tique_pathos_beta %>%
  filter(! is.na(tick_species))

### Model : Borrelia.spp._23S ----
m_borrelia_r <- lme4::glmer(
  formula = Borrelia.spp._23S ~ broadleaved_status * connectivity + tick_species + (1|numero_ligne) + (1|host_name),
  family = binomial(link = "logit"),
  data = data_for_host_tick_m,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

SelectionModels<- MuMIn::dredge(m_borrelia_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels



### Model : Rickettsia.spp._gltA ----
m_ricketssia_r <- lme4::glmer(
  formula = Rickettsia.spp._gltA ~ broadleaved_status * connectivity + tick_species + (1|numero_ligne)+ (1|host_name),
  family = binomial(link = "logit"),
  data = data_for_host_tick_m,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

SelectionModels<- MuMIn::dredge(m_ricketssia_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels

m_ricketssia_r <- lme4::glmer(
  formula = Rickettsia.spp._gltA ~ tick_species + (1|numero_ligne) + (1|host_name),
  family = binomial(link = "logit"),
  data = data_for_host_tick_m,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

DHARMa::simulateResiduals(m_ricketssia_r, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)

drop1(m_ricketssia_r,.~.,test="Chisq")
summary(m_ricketssia_r)

em <- emmeans::emmeans(m_ricketssia_r, specs = pairwise ~ tick_species, adjust = "Tukey", type = "response" )
em$contrasts

plot(em, comparisons = TRUE)


### Model : Bartonella.spp._ssrA ----
m_barto_tick_r <- lme4::glmer(
  formula = Bartonella.spp._ssrA ~ broadleaved_status + connectivity  +  (1|numero_ligne)+ (1|host_name),
  family = binomial(link = "logit"),
  data = data_for_host_tick_m,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

SelectionModels<- MuMIn::dredge(m_barto_tick_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels



### Model : Francisella.tularensis_fopA. ----
m_francis_r <- lme4::glmer(
  formula = Francisella.tularensis_fopA. ~ broadleaved_status * connectivity + (1|numero_ligne)+ (1|host_name),
  family = binomial(link = "logit"),
  data = data_for_host_tick_m,
  na.action = "na.fail",                                  
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

SelectionModels<- MuMIn::dredge(m_francis_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels


data_for_host_tick_m %>%
  count(Francisella.tularensis_fopA.)




## Explore microfluidic tick data ----
host_tique_pathos_beta %>%
  filter(host_nature == "small_mammal" & sample_nature == "tick") %>%
  count(mission)

host_tique_pathos_beta %>%
  filter(host_nature == "small_mammal" & sample_nature == "tick") %>%
  count(tick_species, stage)

# Table making for prevalence
tt <- host_tique_pathos_beta %>%
  filter(sample_nature == "tick") %>%
  group_by(tick_species, stage) %>%
  summarise( across(all_of(tick_pathos_name), ~ round(sum(. > 0) / n(), digits = 3)) )
# Convert tibble to data frame
tt_df <- as.data.frame(tt)
# Transpose the data frame
tt_transposed <- t(tt_df)
# Convert the transposed data back to a data frame
tt_transposed_df <- as.data.frame(tt_transposed)
# Set the column names to the first row of the transposed data
colnames(tt_transposed_df) <- tt_transposed_df[1, ]
# Remove the first row (which contains the original column names)
tt_transposed_df <- tt_transposed_df[-1, ]
tt_transposed_df<- cbind(rownames(tt_transposed_df), data.frame(tt_transposed_df, row.names=NULL))











# Macroparasite per host grouping (ticks an flea in BPM)----

# Import file
bpm_host_macroparasite <- data.table::fread(file = here::here( "data","raw-data/","export_fast_parasite_alois20240328.csv"))

# Replace blank space in data
bpm_host_macroparasite$parasite[bpm_host_macroparasite$parasite == ""] <- NA

# Rename columns
bpm_host_macroparasite <- bpm_host_macroparasite %>%
  mutate( parasite = gsub(" ", "_", parasite))

## Generate tidy dataframe with macroparasite (0/1) ----
bpm_host_macroparasite_tidy <- bpm_host_macroparasite %>%
  filter( !is.na(parasite)) %>%
  mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = parasite,
    values_from = value,
    values_fill = list(value = 0),
    id_cols = numero_centre
  )

# Add tick number when available
bpm_host_macroparasite_tidy <- left_join( x = bpm_host_macroparasite_tidy,
                                          y = bpm_host_macroparasite %>%
                                           filter(parasite == "Ixodida") %>%
                                           select( c("numero_centre", "effectif_tique")),
                                          by = "numero_centre")

## Explore Macroparasite data from BPM ----

# How many ticks per season for rodent in BPM 
bpm_host_macroparasite %>%
  group_by(code_mission) %>%
  summarise(total_effectif_tique = sum(effectif_tique, na.rm = TRUE))

