#' Krigeage des valeurs placettes
#'
#' @description Conversion données ponctuelles en données surfaciques
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import gstat
#'
#' @examples
#' k <- IlotKrigeage()
#' plot(k["var1.pred"])
#'
#' @author Bruciamacchie Max
#'
#' @export


IlotKrigeage <- function(pas=25){

  rep <- ProjetChoisir()
  verif <- list.dirs(rep, full.names=F, recursive = F)
  if("Tables" %in% verif) {
    fich <- list.files(rep, pattern="\\.Rdata$", recursive=T)
    load(paste(rep, fich, sep="/"))
  } else {
    stop("Merci d'utiliser la fonction IlotDataImport")
  }

  #################### Création GRID et variogramme ####################
  grd <- st_make_grid(zone, cellsize=pas, what="centers") # Création grid
  grd <- grd[zone]

  #################### Krigeage uniquement avec distance ####################

  # ------------ Kriggeage Gha
  pos <- which("GTOT" == names(Placette))[[1]]
  Gha <- KrigeageFonc(grd, Placette, idvar=pos, pas=25)
  names(Gha) <- c("Gha", "GhaVar", "geometry")
  st_write(Gha, paste(rep, "Rasters/PredictGha.gpkg", sep= "/"), delete_layer = TRUE)

  # ------------ Krigeage VcHa


  # ------------ Krigeage Maturite
  pos <- which("MATURE" == names(Placette))[[1]]
  Gmature <- KrigeageFonc(grd, Placette, idvar=pos, pas=25)
  names(Gmature) <- c("Mature", "MatureVar", "geometry")
  st_write(Gmature, paste(rep, "Rasters/PredictGmature.gpkg", sep= "/"), delete_layer = TRUE)


  # ------------ Krigeage AnCoupe





  return(k)

}
