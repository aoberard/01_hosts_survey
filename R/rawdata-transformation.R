#' PURPOSE
#'
#' @description 
#' This function 
#' 
#' unique combination of the specified columns as rows and the different 
#' values of methode_identification as columns. The counts of occurrences are provided in each cell.
#' For example, the last few columns (NA, morphologie) represent the counts of occurrences for each 
#' combination of the other columns under the corresponding methode_identification value.
#' 
#' @param 
#'
#' @return 
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
