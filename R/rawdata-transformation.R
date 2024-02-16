#' PURPOSE
#'
#' @description 
#' For each combination of the specified columns
#' This function create an unique combination of the specified columns as rows and generate a row 
#' values of identification methodes (methode_identification) as columns.
#' For example, the last few columns (NA, morphologie) represent the counts of occurrences for each 
#' combination of the other columns under the corresponding methode_identification value.
#' 
#' @param data a tibble originating from the BPM Database extraction
#'
#' @return a `tibble` containing the unique combinations of the specified columns and the number of time they appear for each 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dcast(bpm)


identif_method_cast <- function (data) {
  
  ## Casting by 
  
  dcast(data, 
        code_mission + localite + date_pose + numero_ligne + date_releve + numero_releve + numero_piege + latitude_piege + longitude_piege + code_resultat + abbrev_resultat + resultat + numero_terrain + commentaire + taxon_capture + numero_centre + date_dissection + sexe + observations + taxon_dissection ~ methode_identification,
        value.var = "taxon_dissection",
        fun = length )
  
}
