#' Fonction Krigeage
#'
#' @description Fonction krigeage pour conversion données ponctuelles
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import gstat
#'
#' @examples
#' k <- IlotKrigeage()
#'
#' @author Bruciamacchie Max
#'
#' @export

KrigeageFonc <- function(shpZone, shpPlac, grd, idvar=2, shpVar1=NULL, pas=50) {
  # ------------ Vérification
  if (missing(shpPlac)) stop("missing shpPlac")
  if (missing(shpZone)) stop("missing shpZone")
  if(st_crs(shpPlac) != st_crs(shpZone)) {
    shpPlac <- shpPlac %>% st_transform(st_crs(shpZone))
  }

  # ----------------- Krigeage cas1 : la prédiction ne dépend que des valeurs voisines
  names(shpPlac)[idvar] <- "Y"
  v <- variogram(Y ~ 1, data=shpPlac, cutoff=2000, width = 50)
  vmf <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat", "Ste"), psill=100, range = 2000, nugget = 1))
  plot(v, pl = T, model = vmf)
  k <- krige(Y ~ 1, locations = shpPlac, newdata = grd, model = vmf) %>%
    st_as_sf()

  return(k)
}
