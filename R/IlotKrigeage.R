#' Krigeage des valeurs placettes
#'
#' @description Conversion données ponctuelles en données surfaciques
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import automap
#'
#' @param rep = dossier en cours
#' @param shp = perimètre
#' @param pas = résolution
#'
#' @examples
#' IlotKrigeage(perim)
#'
#' @author Bruciamacchie Max
#'
#' @export


IlotKrigeage <- function(rep, shp, pas=25){
  # shp = perim
  # if(!("rep" %in% ls())) {
  #   rep <- ProjetChoisir()
  # }

  verif <- list.dirs(rep, full.names=F, recursive = F)
  if("Tables" %in% verif) {
    fich <- list.files(rep, pattern="\\.Rdata$", recursive=F)
    load(paste(rep, fich, sep="/"))
  } else {
    stop("Merci d'utiliser la fonction IlotDataImport")
  }

  #################### Création GRID et variogramme ####################
  grd <- st_make_grid(shp, cellsize=pas, what="centers") # Création grid
  grd <- grd[shp]

  #################### Krigeage uniquement avec distance ####################

  # ------------ Krigeage Gha
  pos <- which("GTOT" == names(Placette))[[1]]
  Gha <- KrigeageFonc(grd, Placette, idvar=pos, pas=25)
  names(Gha) <- c("Gha", "geometry")
  st_write(Gha, paste(rep, "Rasters/PredictGha.gpkg", sep= "/"), delete_layer = TRUE)

  # ------------ Krigeage VcHa
  pos <- which("VcHa" == names(Placette))[[1]]
  VcHa <- KrigeageFonc(grd, Placette, idvar=pos, pas=25)
  names(VcHa) <- c("VcHa", "geometry")
  st_write(VcHa, paste(rep, "Rasters/PredictVcHa.gpkg", sep= "/"), delete_layer = TRUE)

  # ------------ Krigeage Maturite
  pos <- which("Mature" == names(Placette))[[1]]
  Gmature <- KrigeageFonc(grd, Placette, idvar=pos, pas=25)
  names(Gmature) <- c("Mature", "geometry")
  st_write(Gmature, paste(rep, "Rasters/PredictGmature.gpkg", sep= "/"), delete_layer = TRUE)

  #################### Extraction ####################

  # ------------ Info AnCoupe
  Coupe <- ParcelleUG %>%
    dplyr::select(DateCoupe)

  AnCoupe <- grd %>%
    st_sf() %>%
    st_intersection(Coupe)

  st_write(AnCoupe, paste(rep, "Rasters/AnCoupe.gpkg", sep= "/"), delete_layer = TRUE)

}
