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


IlotKrigeage <- function(pas=50){

  rep <- ProjetChoisir()
  verif <- list.dirs(rep, full.names=F, recursive = F)
  if("Tables" %in% verif) {
    fich <- list.files(rep, pattern="\\.Rdata$", recursive=T)
    load(paste(rep, fich, sep="/"))
  }

  #################### Création GRID et variogramme ####################
  grd <- st_make_grid(zone, cellsize=pas, what="centers") # Création grid
  grd <- grd[zone]

  #################### Krigeage uniquement avec distance ####################

  # ------------ Krigeage Gha
  v <- variogram(GTOT ~ 1, data=Placette, cutoff=2000, width = 50)
  vmf <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat", "Ste"), psill=100, range = 2000, nugget = 1))
  plot(v, pl = T, model = vmf)
  k <- krige(GTOT ~ 1, locations = Placette, newdata = grd, model = vmf) %>%
    st_as_sf()

  st_write(k, paste(rep, "Rasters/PredictGha.gpkg", sep= "/"))

  return(k)

}
