#Package 
library(readxl)
library(ggplot2)
library(rstatix)
library(gridExtra)
library(cutpointr)
library(dplyr)

# Warning ----
### /!\ Script provisoire utilise le dataframe "bpm_beprep" de temp_import
# Le but est de voir si on peut les seuils de la littérature sont similaires à ce qu'on obtient ici pour distinguer les adultes/matures des autres pour le poids
# Et aussi de voir si la longueur testiculaire serait un bon seuil pour les males
# Le but n'est pas d'utiliser uniquement cet outil pour classer les classes d'ages, mais plutôt de déterminer si les individus sans 



# Check rapide nombre d'occurences de caratères
bpm_beprep %>%
  group_by(sexe) %>%
  count(testicules, vesicule_seminale_taille, gestation, cicatrices_placentaires_presentes, lactation, vulve_ouverte, uterus)


# Attribution maturite oui/non pour chaque sexe ----
#Si au moins critère + alors individu mature, si que - alors immature ici, si des - mais na alors NA
# cas special de la gestation, un na ou un - ne signifie rien, mais un + oui


bpm_beprep <- bpm_beprep %>%
  mutate(breeding_status = 
           case_when(
             code_resultat != 1 ~ NA,
             taxon_dissection == "Insectivore" ~ NA,
             is.na(sexe) ~ "sexe_ind",
             sexe == "M" & (testicules == "+" | vesicule_seminale_taille == "+") ~ "1",
             sexe == "M" & (is.na(testicules) | is.na(vesicule_seminale_taille)) ~ "missing_values",
             sexe == "M" & (testicules == "-" & vesicule_seminale_taille == "-") ~ "0",
             sexe == "F" & (gestation == "+" | cicatrices_placentaires_presentes == "+" | lactation == "+" | vulve_ouverte == "+" | uterus == "+") ~ "1",
             sexe == "F" & (is.na(cicatrices_placentaires_presentes) | is.na(lactation) | is.na(vulve_ouverte) | is.na(uterus)) ~ "missing_values",
             sexe == "F" & (cicatrices_placentaires_presentes == "-" & lactation == "-" & vulve_ouverte == "-" & uterus == "-") ~ "0",
             .default = "problem"
           )
  )

print(bpm_beprep %>%
        filter(taxon_dissection == "Apodemus") %>%
        count(sexe, breeding_status)
)


# SEUIL de Poids sans prise en compte envi ----

## Males ----

### Testicules ----

#Creation d'un seuil avec fonction cutpointr, sur dfclean, seuil basé sur variable poids, avec pour variable "catégorie" : maturite sexuelle
#, réalisé pour les differents sexes donc 2 sous-pops mâles femelles, pour quoi la valeur positif (mature)=1 
opt_cut_testi <- bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe == "M") %>%
  cutpointr::cutpointr(testicules_longueur, breeding_status, sexe, boot_runs=1000, boot_stratify = TRUE, pos_class = "1", direction = ">=")

summary(opt_cut_testi) #résumé du modèle : donne optimal cutpoint : valeur seuil variable choisie, AUC : area under the ROC curve (plus grand mieux c'est, 0,75 grand minimum)
# acc : accuracy, sensibilité, specificité, tp: vrai positif, fn faux neg, fp, faux pos, tn vrai neg

plot(opt_cut_testi)    #graphs, on retrouve notamment la distribution des seuil optimaux, la courbe ROC et une estimation out of the bag c'est à dire en interne 

bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe == "M") %>%
ggplot() + 
  geom_density(aes(x=testicules_longueur, color=breeding_status), size = 1.25) + 
  theme_light() + 
  labs(title = "Density of testes length according to breeding status",
       x = "Testes length (mm)",
       y = "Density",
       color = "Breeding status")+
  geom_vline(xintercept = opt_cut_testi$optimal_cutpoint)


### Poids ----

opt_cut_mweight <- bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe == "M") %>%
  cutpointr::cutpointr(poids, breeding_status, sexe, boot_runs=1000, boot_stratify = TRUE, pos_class = "1", direction = ">=")

summary(opt_cut_mweight)

plot(opt_cut_mweight)

bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe == "M") %>%
  ggplot()+
  geom_density(aes(x=poids, color=breeding_status ), size = 1.25) +
  xlim(0,35)+
  theme_light() + 
  labs(title = "Density of males weight according to breeding status",
       x = "Weight (g)",
       y = "Density",
       color = "Breeding status") +
  geom_vline(xintercept = opt_cut_mweight$optimal_cutpoint)

## Females ----

### Poids ----

opt_cut_fweight <- bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe == "F") %>%
  cutpointr::cutpointr(poids, breeding_status, sexe, boot_runs=1000, boot_stratify = TRUE, pos_class = "1", direction = ">=")

summary(opt_cut_fweight)

plot(opt_cut_fweight)

bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe == "F") %>% 
  ggplot()+
  geom_density(aes(x=poids, color=breeding_status ), size = 1.25) +
  xlim(0,35)+
  theme_light() + 
  labs(title = "Density of females weight according to breeding status",
       x = "Weight (g)",
       y = "Density",
       color = "Breeding status") +
  geom_vline(xintercept = opt_cut_fweight$optimal_cutpoint)

## BOTH simultaneously ----

### Poids ----

opt_cut_weight <- bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe %in% c("M","F")) %>% 
  cutpointr::cutpointr(poids, breeding_status, boot_runs=1000, boot_stratify = TRUE, pos_class = "1", direction = ">=")

summary(opt_cut_weight)

plot(opt_cut_weight)

bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe %in% c("M","F")) %>% 
  ggplot()+
  geom_density(aes(x=poids, color=breeding_status), size = 1.25) +
  xlim(0,35)+
  theme_light() + 
  labs(title = "Weight density according to breeding status for both sexes",
       x = "Weight (g)",
       y = "Density",
       color = "Breeding status") +
  geom_vline(xintercept = opt_cut_weight$optimal_cutpoint) +
  facet_grid(rows = vars(code_mission))


### Tail length ----

opt_cut_tail <- bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe %in% c("M","F")) %>% 
  filter(!is.na(longueur_queue)) %>%
  cutpointr::cutpointr(longueur_queue, breeding_status, boot_runs=1000, boot_stratify = TRUE, pos_class = "1", direction = ">=")

summary(opt_cut_tail)

plot(opt_cut_tail)

bpm_beprep %>%
  filter(breeding_status == "1" | breeding_status == "0") %>%
  filter(taxon_dissection == "Apodemus") %>%
  filter(numero_centre != "NCHA100050") %>%
  filter(sexe %in% c("M","F")) %>% 
  filter(!is.na(longueur_queue)) %>%
  ggplot()+
  geom_density(aes(x=longueur_queue, color=breeding_status), size = 1.25) +
  theme_light() + 
  labs(title = "Density of tails length according to breeding status",
       x = "Tail length (mm)",
       y = "Density",
       color = "Breeding status") +
  geom_vline(xintercept = opt_cut_tail$optimal_cutpoint)
