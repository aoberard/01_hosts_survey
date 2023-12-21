#' 01_hosts_survey: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Aloïs Berard \email{aloisberard@outlook.fr}
#' 
#' @date 2023/12/14



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here()) #switch off if using targets


## Targets configuration

# targets::tar_config_set(
#   store = "outputs/targets_outputs",
#   script = "analyses/targets_pipeline.R"
# )


## Global Variables ----

set.seed(123)


## Run Project ----

# List all R scripts in a sequential order and using the following form:

# source(here::here("analyses", "script_X.R"))

# OR Target pipeline :

# targets::tar_visnetwork()
# targets::tar_make()
