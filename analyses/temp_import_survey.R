#!/usr/bin/env Rscript

library("RPostgreSQL")
library("data.table")

library("emmeans")

library("dplyr")
library("ggplot2")

# database and user parameters ----
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
	dis.numero_centre, dis.date_dissection_2 AS date_dissection, dis.observations, 
	dis.sexe, dis.poids, dis.longueur_tete_corps, dis.longueur_queue,
	dis.testicules, dis.testicules_longueur, dis.vesicule_seminale_taille,
	dis.vulve_ouverte, dis.lactation, dis.cicatrices_placentaires_presentes, dis.gestation, dis.uterus,
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
DATA <- dcast(DATA0, code_mission+localite+date_pose+numero_ligne+date_releve+numero_releve+numero_piege+latitude_piege+longitude_piege+code_resultat+abbrev_resultat+resultat+numero_terrain+commentaire+taxon_capture+numero_centre+date_dissection+observations+sexe+poids+longueur_tete_corps+longueur_queue+testicules+testicules_longueur+vesicule_seminale_taille+vulve_ouverte+lactation+cicatrices_placentaires_presentes+gestation+uterus+taxon_dissection ~ methode_identification, value.var="taxon_dissection",fun =length)

# Save results
fwrite(DATA, here::here("data/", "raw-data/", "export_alois20240227.csv") )

dbDisconnect(conn)




################################################################################

# Read file

bpm_beprep <- readr::read_csv( here::here("data/", "raw-data/", "export_alois20240227.csv") )



set.seed(123)

# Data management brut ----

## identification renaming
unique(bpm_beprep$taxon_dissection)

bpm_beprep$taxon_dissection <- as.character(bpm_beprep$taxon_dissection)

bpm_beprep$taxon_dissection[bpm_beprep$taxon_dissection == "Apodemus sylvaticus"] <- "Apodemus"
bpm_beprep$taxon_dissection[bpm_beprep$taxon_dissection == "Sorex"] <- "Insectivore"
bpm_beprep$taxon_dissection[bpm_beprep$taxon_dissection == "Soricidae"] <- "Insectivore"

#### /!\ PROVISOIRE TANT QUE JULIEN PAS INTRODUIT DATA IDENTIF MOLECULAIRE  ############################## !!!!!
bpm_beprep$taxon_dissection[bpm_beprep$numero_centre == "NCHA100050"] <- "Mus musculus"
bpm_beprep$taxon_dissection[bpm_beprep$numero_centre == "JPRA000086"] <- "Sorex coronatus"


## Attributing line modality

# Glossary:
# C: Connected hedgerow / NC: Non-connected hedgerow
# CT: Control pine edge
# LB: Low Broadleaved density / HB: High Broadleaved

## Modality decomposed

C <- c(2, 6, 20, 24, 30, 7, 11, 12, 28, 33, 36, 17)
CT <- c(8, 14, 18, 23, 26, 34, 5, 10, 15, 22, 32, 35)
NC <- c(3, 4, 13, 25, 27, 29, 1, 9, 16, 19, 21, 31)

LB <- c(7, 11, 12, 28, 33, 36, 5, 10, 15, 22, 32, 35, 1, 9, 16, 19, 21, 31)
HB <- c(17, 2, 6, 20, 24, 30, 8, 14, 18, 23, 26, 34, 3, 4, 13, 25, 27, 29)

bpm_beprep <- bpm_beprep %>%
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



## BPM data quality control : ----

# DATA %>%
#   filter(!is.na(taxon_capture) & is.na(taxon_dissection) ) # obs avec identif capture, pas dissection
# 
# DATA %>%
#   filter(is.na(taxon_capture) & !is.na(taxon_dissection) ) # obs avec identif dissection, pas capture
# 
# DATA %>%
#   filter(code_resultat == 1) %>%
#   filter(is.na(taxon_capture) & is.na(taxon_dissection) ) #obs capture sans identif capture ou dissect 
# 
# DATA |>
#   dplyr::filter( code_resultat %in% c("1") ) |>
#   dplyr::filter( morphologie == "0")  # obs capturé, pas d'identif morpho
# 
# DATA |>
#   filter(`NA` == 1 & morphologie == 1) # obs avec identif en double
# 
# bpm_beprep |>
#   filter(code_resultat == 1) |>
#   filter(taxon_dissection == "Apodemus") |>
#   filter(is.na(sexe))                      # Mulot disséqué, sans sexe attribué



# Table making test

## Trapping success for ALL TAXA - ALL MISSION ONLY - 3 DAYS
bpm_beprep |>
  dplyr::filter(date_releve - date_pose <= 3) |>
  dplyr::filter(code_resultat %in% c("0", "1")) |>
  dplyr::filter(morphologie == 1 | `NA` == 1 ) |>
  dplyr::group_by(numero_ligne) |>
  dplyr::summarise(
    treatment = unique(line_treatment),
    trap_nights = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus")),
    souris = sum(taxon_dissection %in% c("Mus musculus")),
    vole = sum(taxon_dissection %in% c("Myodes glareolus")),
    crocidure = sum(taxon_dissection %in% c("Insectivore")),
    sorex = sum(taxon_dissection %in% c("Sorex coronatus"))
  ) |>
  dplyr::mutate(succes_rate = log( 1 - ((apodemus + vole + crocidure + sorex + souris) / trap_nights) * (-100) ) ) 



# Statistical analyses ----

## Apodemus trapping probability models ----

### random effect (logit) (ligne = replicat, numero releve : nuit piegeage, année-saison:code_mission)
### Create data ----
data_for_apo_trapping <- bpm_beprep |>
  dplyr::filter( date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1") ) |>
  dplyr::filter( morphologie == 1 | `NA` == 1 ) |>
  dplyr::mutate( code_resultat = ifelse(!(taxon_dissection %in% c("Apodemus", NA)), 0, code_resultat)) 

count(x =  data_for_apo_trapping, line_treatment, code_resultat)
count(x =  data_for_apo_trapping, broadleaved_status, code_resultat)
count(x =  data_for_apo_trapping, connectivity, code_resultat)
count(x =  data_for_apo_trapping, broadleaved_status, connectivity, code_resultat)


### Create global model ----
#### Random ----
m_apo_trapping_r <- lme4::glmer(
  formula = code_resultat ~ broadleaved_status * connectivity  + code_mission + (1|numero_ligne),
  family = binomial(link = "logit"),
  data = data_for_apo_trapping,
  na.action = "na.fail",
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

m_apo_trapping_r |>
  RVAideMemoire::overdisp.glmer() # overdispersion testing

DHARMa::simulateResiduals(m_apo_trapping_r, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)


### Select best model
SelectionModels<- MuMIn::dredge(m_apo_trapping_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels


# to modify |||||||||||||||
bestmodel <- lme4::glmer(
  formula = code_resultat ~  code_mission + (1|numero_ligne),
  family = binomial(link = "logit"),
  data = data_for_apo_trapping,
  na.action = "na.fail",
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

DHARMa::simulateResiduals(bestmodel, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)
performance::icc(bestmodel) # Intraclass Correlation Coefficient
performance ::r2(bestmodel) # R2
drop1(bestmodel,.~.,test="Chisq")
summary(bestmodel) 




## Line trapping rate modelling for Apodemus only ----

### Create data ----
data_for_m_ltrapping <- bpm_beprep |>
  dplyr::filter(date_releve - date_pose <= 3) |>
  dplyr::filter(code_resultat %in% c("0", "1")) |>
  dplyr::filter(morphologie == 1 | `NA` == 1 ) |>
  dplyr::group_by(numero_ligne, code_mission) |>
  dplyr::summarise(
    line_treatment = unique(line_treatment),
    broadleaved_status = unique(broadleaved_status),
    connectivity = unique(connectivity),
    trap_nights = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus")),
    souris = sum(taxon_dissection %in% c("Mus musculus")),
    vole = sum(taxon_dissection %in% c("Myodes glareolus")),
    crocidure = sum(taxon_dissection %in% c("Insectivore")),
    sorex = sum(taxon_dissection %in% c("Sorex coronatus"))
  ) |>
  dplyr::mutate(apo_succes_rate = log( 1 - ((apodemus) / trap_nights) * (-100) ) )


library(ggpubr)

g1 <- data_for_m_ltrapping %>%
  ggplot( aes(x = apo_succes_rate/100)) +
  geom_histogram()

g2 <- data_for_m_ltrapping %>%
  ggplot( aes(x = (apodemus / trap_nights) ) ) +
  geom_histogram()

ggarrange(g1, g2 + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)


### Create global model ----

#### Non-random ----

#Two models are tried, one with raw success values, one with ajusted values of trapping success

#Non adjusted values of success
m_ltrapping_1 <- stats::glm(
  formula = apodemus / trap_nights  ~ broadleaved_status * connectivity + code_mission ,
  weights = trap_nights,
  family =  binomial(link = "logit"),
  data = data_for_m_ltrapping,
  na.action="na.fail"
)

SelectionModels<- MuMIn::dredge(m_ltrapping_1, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels

# to modify |||||||||||||||
bestmodel <- stats::glm(
  formula = apodemus / trap_nights  ~ broadleaved_status + code_mission,
  weights = trap_nights,
  family =  binomial(link = "logit"),
  data = data_for_m_ltrapping,
  na.action="na.fail"
)
DHARMa::simulateResiduals(bestmodel, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)
drop1(bestmodel,.~.,test="Chisq")




# The other model with adjusted values :
m_ltrapping_2 <- stats::glm(
  formula = (apo_succes_rate/100) ~  broadleaved_status * connectivity + code_mission,
  family =  binomial(link = "logit"),
  weights = trap_nights,
  data = data_for_m_ltrapping,
  na.action="na.fail"
)

SelectionModels1<- MuMIn::dredge(m_ltrapping_2, rank = "AICc")              
TopModels<-subset(SelectionModels1, delta<2)
TopModels

# to modify |||||||||||||||
bestmodel <- stats::glm(
  formula = (apo_succes_rate/100) ~  code_mission,
  family =  binomial(link = "logit"),
  weights = trap_nights,
  data = data_for_m_ltrapping,
  na.action="na.fail"
)
DHARMa::simulateResiduals(bestmodel, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)
drop1(bestmodel,.~.,test="Chisq")




#### Random ----
m_ltrapping_r <- lme4::glmer(
  formula = apodemus / trap_nights ~ broadleaved_status * connectivity + code_mission + (1|numero_ligne),
  family = binomial( link = "logit" ),
  weights = trap_nights,
  data = data_for_m_ltrapping,
  na.action = "na.fail",
  control = lme4::glmerControl( optimizer="bobyqa", optCtrl=list(maxfun=2e5) ) )

DHARMa::simulateResiduals(m_ltrapping_r) |> #alignement testing
  DHARMa::testResiduals()

drop1(m_ltrapping_r, .~.,test="Chisq")

SelectionModels<- MuMIn::dredge(m_ltrapping_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels

# to modify |||||||||||||||
bestmodel <- lme4::glmer(
  formula = apodemus / trap_nights ~ code_mission + (1|numero_ligne),
  family = binomial( link = "logit" ),
  weights = trap_nights,
  data = data_for_m_ltrapping,
  na.action = "na.fail",
  control = lme4::glmerControl( optimizer="bobyqa", optCtrl=list(maxfun=2e5) ) )
DHARMa::simulateResiduals(bestmodel, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)
drop1(bestmodel,.~.,test="Chisq")


em <- emmeans(bestmodel, "code_mission")
em
plot(em, comparisons = TRUE)

contrast(em, "pairwise", adjust = "Tukey")





### Graph making  ----

graph_sequence <- c("CT_LB", "CT_HB", "NC_LB", "NC_HB", "C_LB", "C_HB" )

# graph for apodemus ONLY capture rate 
apo_succes <- bpm_beprep |>
  dplyr::filter( date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1") ) |>
  dplyr::filter( morphologie == 1 | `NA` == 1 ) |>
  dplyr::filter( taxon_dissection %in% c("Apodemus", NA) ) |>
  dplyr::group_by(numero_ligne, code_mission) |>
  dplyr::summarise(
    treatment = unique(line_treatment),
    trap_nights = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus") ) ) |>
  dplyr::mutate( succes_rate = log( 1 - (apodemus / trap_nights) * (-100) ) )  |>
  ggplot(aes(x =  factor(treatment, levels = graph_sequence), y = succes_rate, fill = code_mission ) ) +
  geom_boxplot(position = position_dodge(1)) +
  geom_dotplot(binaxis = 'y', stackdir='center', position = position_dodge(1), dotsize = 0.5  )+
  scale_fill_manual(values = c("FRA - BePrep - Juin 2023" = "#5e8d5a", "FRA - BePrep - Octobre 2023" = "#f68f3c")) +
  theme_minimal()+
  labs(x = "Modalité paysagère",
       y = "Taux de capture ajusté (%)",
       fill = "Période d'échantillonage")
  

apo_succes

ggsave(filename = here::here("figures/", "20240314_aposuccesrate.pdf"), plot = apo_succes , device = NULL, path = NULL, scale = 1, width = 200, height = 150, units ="mm")





#robot 2000 merci
summary_data <- bpm_beprep |>
  dplyr::filter( date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1") ) |>
  dplyr::filter( morphologie == 1 | `NA` == 1 ) |>
  dplyr::filter( taxon_dissection %in% c("Apodemus", NA) ) |>
  dplyr::group_by(numero_ligne, code_mission) |>
  dplyr::summarise(
    treatment = unique(line_treatment),
    trap_nights = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus") ) ) |>
  dplyr::mutate( succes_rate = apodemus / trap_nights *100  )

# Calculer le nombre d'observations par `treatment` et `code_mission`
observation_counts <- summary_data |> 
  dplyr::group_by(treatment, code_mission) |> 
  dplyr::summarise(n = sum(apodemus),
                   max_rate = max(succes_rate))


ggplot(summary_data, aes(x = factor(treatment, levels = graph_sequence), y = succes_rate, fill = code_mission)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(1), dotsize = 0.5) +
  geom_text(data = observation_counts, aes(label = n, y = max_rate+0.05), position = position_dodge(width = 1), vjust = -0.5, size = 3.25) + 
  scale_fill_manual(values = c("FRA - BePrep - Juin 2023" = "#5e8d5a", "FRA - BePrep - Octobre 2023" = "#f68f3c")) +
  theme_minimal() +
  labs(x = "Modalité paysagère", y = "Taux de capture (%)", fill = "Période d'échantillonnage")
  



## Richness index ----

### Create data ----
data_richness <- bpm_beprep %>%
  dplyr::filter(date_releve - date_pose <= 3) %>%
  dplyr::filter(code_resultat %in% c("0", "1")) %>%
  dplyr::filter(morphologie == 1 | 'NA' == 1 ) %>%
  dplyr::group_by(numero_ligne, code_mission) %>%
  dplyr::summarise(
    treatment = unique(line_treatment),
    broadleaved = unique(broadleaved_status),
    connected = unique(connectivity),
    trapped = dplyr::n(),
    apodemus = sum(taxon_dissection %in% c("Apodemus")),
    vole = sum(taxon_dissection %in% c("Myodes glareolus")),
    crocidure = sum(taxon_dissection %in% c("Insectivore")),
    sorex = sum(taxon_dissection %in% c("Sorex coronatus")),
    souris = sum(taxon_dissection %in% c("Mus musculus")),
    species_richness = sum(apodemus > 0, vole > 0, crocidure > 0, sorex > 0, souris > 0)
  )
data_richness

data_richness %>%
  ggplot( aes(x = species_richness)) +
  geom_histogram()



### Create global model ----

#### Poisson ----
##### Non-random ----
m_richness_poisson <- stats::glm(
  formula = species_richness  ~ broadleaved*connected,
  family = poisson(link="log"),
  data = data_richness,
  na.action="na.fail"
)

DHARMa::simulateResiduals(m_richness_poisson, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
plot(rank = TRUE)

# Select best model 
SelectionModels<- MuMIn::dredge(m_richness_poisson, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels
# ModelSelected
# DHARMa::simulateResiduals(ModelSelected, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
# plot(rank = TRUE)
# drop1(ModelSelected, .~.,test="Chisq")
#summary(ModelSelected)


##### Random ----
m_richness_poisson_r<-lme4::glmer(
  formula = species_richness-1  ~ broadleaved*connected + code_mission + (1|numero_ligne),
  family = poisson(link="log"),
  data = data_richness,
  na.action="na.fail",
  control = lme4::glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=20000))
)

DHARMa::simulateResiduals(m_richness_poisson_r, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)

m_richness_poisson_r |>
  RVAideMemoire::overdisp.glmer()

SelectionModels<- MuMIn::dredge(m_richness_poisson_r, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels

bestmodel <- lme4::glmer(
  formula = species_richness-1  ~ code_mission + (1|numero_ligne),
  family = poisson(link="log"),
  data = data_richness,
  na.action="na.fail",
  control = lme4::glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=20000))
)

bestmodel |>
  RVAideMemoire::overdisp.glmer()

DHARMa::simulateResiduals(bestmodel, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)
drop1(bestmodel,.~.,test="Chisq")

em <- emmeans(bestmodel, "code_mission", type = "response")
em
plot(em, comparisons = TRUE)

contrast(em, "pairwise", adjust = "Tukey")

summary(bestmodel)



### Grap making ----

rich_graph_sequence0 <- c("CT_LB", "CT_HB", "NC_LB", "NC_HB", "C_LB", "C_HB" )

data_richness  |>
  ggplot(aes(x =  factor(treatment, levels = rich_graph_sequence0), y = species_richness) ) +
  geom_boxplot(position = position_dodge(1)) +
  geom_dotplot(binaxis = 'y', stackdir='center', position = position_dodge(1), dotsize = 0.5  ) +
  theme_minimal()+
  labs(x = "Modalité paysagère",
       y = "Richesse spécifique")+
  scale_y_continuous(limits = c(0, NA))

rich_graph_sequence1 <- c("LB", "HB")

data_richness  |>
  ggplot(aes(x =  factor(broadleaved, levels = rich_graph_sequence1), y = species_richness ) ) +
  geom_boxplot(position = position_dodge(1)) +
  geom_dotplot(binaxis = 'y', stackdir='center', position = position_dodge(1), dotsize = 0.5  ) +
  theme_minimal()+
  labs(x = "Modalité paysagère",
       y = "Richesse spécifique")+
  scale_y_continuous(limits = c(0, NA))

rich_graph_sequence2 <- c("CT", "NC", "C")

data_richness  |>
  ggplot(aes(x =  factor(connected, levels = rich_graph_sequence2), y = species_richness ) ) +
  geom_boxplot(position = position_dodge(1)) +
  geom_dotplot(binaxis = 'y', stackdir='center', position = position_dodge(1), dotsize = 0.5  ) +
  theme_minimal()+
  labs(x = "Modalité paysagère",
       y = "Richesse spécifique")+
  scale_y_continuous(limits = c(0, NA))

rich_graph_sequence3 <- c("FRA - BePrep - Juin 2023", "FRA - BePrep - Octobre 2023")

data_richness  |>
  ggplot(aes(x =  factor(code_mission, levels = rich_graph_sequence3), y = species_richness , fill = code_mission) ) +
  geom_boxplot(position = position_dodge(1)) +
  geom_dotplot(binaxis = 'y', stackdir='center', position = position_dodge(1), dotsize = 0.5  ) +
  theme_minimal()+
  labs(x = "Période d'échantillonnage",
       y = "Richesse spécifique")+
  scale_y_continuous(limits = c(0, NA))



# TENTATIVE CALCUL CSI ----

### Success rate for all taxa, per season ----
bpm_beprep |>
  dplyr::filter(date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1") ) |>
  group_by(code_mission) |>
  summarise(ratio = log( 1 - (sum(code_resultat == "1") / n())  ) *(-100)  ) 


## Mean number of rodent caught per line for each missions ----
bpm_beprep |>
  dplyr::filter(date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("0", "1")) |>
  dplyr::filter(morphologie == 1 | `NA` == 1 ) |>
  dplyr::filter(code_mission == "FRA - BePrep - Juin 2023") |>
  dplyr::group_by(numero_ligne) |>
  dplyr::summarise(
    apodemus = sum(taxon_dissection %in% c("Apodemus")),
    vole = sum(taxon_dissection %in% c("Myodes glareolus")),
    insectivore = sum(taxon_dissection %in% c("Insectivore"))
  ) |>
  dplyr::mutate(rodent_number = apodemus + vole ) |> 
  dplyr::summarise(avg_rodent_per_line = mean(rodent_number))



## Sex bias ----
bpm_beprep |>
  dplyr::filter( date_releve - date_pose <= 3) |>
  dplyr::filter( code_resultat %in% c("1") ) |>
  dplyr::filter( morphologie == 1 | `NA` == 1 ) |>
  dplyr::filter( taxon_dissection %in% c("Apodemus", NA) ) |>
  dplyr::group_by(sexe) |>
  dplyr::count()

chisq.test(table(bpm_beprep$sexe, bpm_beprep$line_treatment)) #sex difference by treatment?


## Weight ----

apopoids <- bpm_beprep |>
  dplyr::filter(code_resultat %in% c("1")) |>
  dplyr::filter(morphologie == 1 | `NA` == 1 )  |>
  dplyr::filter( taxon_dissection %in% c("Apodemus", NA)) |>
  dplyr::filter(!is.na(sexe))

hist(apopoids$poids)


ggplot(apopoids, aes(x = poids, fill = sexe)) + 
  geom_density(aes(alpha = 0.5))


shapiro.test(x = apopoids$poids)

m_poids <- lme4::lmer(poids ~ broadleaved_status * connectivity + sexe +code_mission + (1 | numero_ligne),
                data = apopoids,
                na.action = "na.fail",
                REML = FALSE)                  #attention estim biaisée mais requis pour comp model dredge je crois ?

SelectionModels<- MuMIn::dredge(m_poids, rank = "AICc")              
TopModels<-subset(SelectionModels, delta<2)
TopModels


m_poids <- lme4::lmer(poids ~ broadleaved_status  +(1 | numero_ligne),
                      data = apopoids,
                      na.action = "na.fail")


plot(m_poids)
DHARMa::simulateResiduals(m_poids, n = 250, refit = F, integerResponse = NULL, plot = T, seed = 123) |>
  plot(rank = TRUE)

drop1(m_poids,.~.,test="Chisq")
summary(m_poids)

em <- emmeans(m_poids, c("broadleaved_status") , type = "response")
em
plot(em, comparisons = TRUE)

contrast(em, "pairwise", adjust = "Tukey")






# Faire sur toutes saisons
# soit faire taux piegeage (par ligne) en fonction traitement avec replicat en facteur aleat (ex : 6 replicat par modalite puisque 6 ligne)
# soit faire Capture oui/non par nuit.piege avec nuit releve et ou piege comme facteur aleat (voir replicats ? pertinence)
# faire carte sur R tester
