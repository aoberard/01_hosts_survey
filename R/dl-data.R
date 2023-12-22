#' Download CBGP Small Mammals Database's raw data
#'
#' @description 
#' This function downloads raw data (csv files) hosted on the "BPM", the :
#' CBGP Small Mammals Database <http://vminfotron-dev.mpl.ird.fr/bdrss/index.php/>.
#' The files won't be downloaded if already exist locally (except if `overwrite = TRUE`).
#' 
#' @param overwrite a logical. If `TRUE`, the files will be downloaded again 
#'   and the previous versions will be erased.
#'
#' @return Filepath of the newly downloaded file
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Download data ----
#' bpm_extraction()


library("RPostgreSQL")
library("data.table")


bpm_extraction <- function(overwrite = FALSE) { 

  
  ## Destination location ---- 
  
  path <- here::here("data","raw-data")
  
  
  ## File name  ---- 
  
  filename <- "bpm_query.csv"
  
  
  ## Check if file already exists locally ----
  
  if (file.exists(file.path(path, filename)) && !overwrite) {
    
    message("The filename already exists. Use 'overwrite = TRUE' to replace it")
    
    } else {
      
      ### Download file  -----------------
      
      ## Database and user parameters ---- 
      
      host <- "cbgp-gaia2" ; port <- 5432 ; db   <- "rongeurs" ; user <- "mus" ; pwd  <- "musculus" # read-only account (safe !)
      
      
      ## Establish connection with database  ---- 
      
      conn <- dbConnect(PostgreSQL(), host=host, port=port, user=user, password=pwd, dbname=db)
      
      
      ## The SQL query ---- 
      
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
      
      
      ## Write query as a file (can take a few seconds) ----
      
      fwrite( as.data.table(dbGetQuery(conn, sql) ), file = file.path(path, filename) )
      
      
      ## Close the database connection ----
      dbDisconnect(conn)
      
    }
  
  ## Return the path of the downloaded file ----
  
  return(file.path(path, filename))
  
}

