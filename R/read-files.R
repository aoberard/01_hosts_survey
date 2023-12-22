#' Read BPM query 
#'
#' @description 
#' This function reads the .csv file created thanks to a SQL query asking the
#' CBGP Database on Small Mammals : the "BPM". 
#' The file `bpm_query.csv` is stored in `data/raw-data/`.
#'   
#' **Note:** the function [bpm_extraction()] must have been run first.
#'
#' @param file a character of length 1. The path to the .csv file.
#'
#' @return A `tibble` containing the data contained by the BPM as asked by the 
#' query
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Download data ----
#' bpm_extraction()
#' 
#' ## Import data ----
#' bpm <- read_bpm_query(here::here("data","raw-data", "bpm_query.csv"))
#' }


read_bpm_query <- function(file) {
  
  ## Check if file exists ----
  
  if (!file.exists(file)) {
    stop("The file '", file, "' does not exist. Please run ", 
         "'bpm_extraction()' to download it.", call. = FALSE)
  }
  
  
  ## Check if file name is good ----
  
  if (basename(file) != "bpm_query.csv") {
    stop("Wrong file name ('bpm_query.csv')", call. = FALSE)
  }
  
  
  ## Read file ----
  
  suppressMessages(readr::read_csv(file))
  
  
}
