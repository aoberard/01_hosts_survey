#short description 


# Packages call

library(targets)
library(tarchetypes)

# Function's directory

targets::tar_source(here::here("R"))


######## /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\
# pour plus tard : voir si il faut alléger les fonctions pour les rendre réutilisables plutot que faire une fonction 
#par fichiers a chaque fois (voir projet de groupe, fonctions réutilisés, mais apportait rien par rapport fonction packages)
####### /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\




# Pipeline :  ------------------------------------------------------------------


list(
  
  # 1 Data management ----------------------------------------------------------
  
  ### 1.1 - Download raw data ----
  tar_target(tar_dl_raw_bpm, bpm_extraction(overwrite = TRUE))
  
  
  ### 1.2 - Raw files path ----
  ,tar_target(tar_raw_bpm_file, tar_dl_raw_bpm, format = "file" )
  
  
  ### 1.3 - Read data ----
  ,tar_target(tar_read_raw_bpm, read_bpm_query(tar_raw_bpm_file))
  
  
  ### 1.4 - Identification method grouping ----
  
  
)
