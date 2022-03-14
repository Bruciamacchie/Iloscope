#' Fonction Krigeage
#'
#' @description Fonction krigeage pour conversion données ponctuelles
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import gstat
#' @import automap
#'
#' @examples
#' k <- IlotKrigeage()
#'
#' @author Bruciamacchie Max
#'
#' @export

KrigeageFonc <- function(grd, shpPlac, idvar=2, pas=50) {
  # shpPlac = Placette
  # ------------ Vérification
  if (missing(shpPlac)) stop("missing shpPlac")
  if (missing(grd)) stop("missing grd")
  if(st_crs(shpPlac) != st_crs(grd)) {
    shpPlac <- shpPlac %>% st_transform(st_crs(grd))
  }

  # ----------------- Krigeage cas1 : la prédiction ne dépend que des valeurs voisines
  names(shpPlac)[idvar] <- "Y"
  shpPlac <- shpPlac %>%
    mutate(Y = ifelse(is.na(Y), 0, Y))
  # v <- variogram(Y ~ 1, data=shpPlac, cutoff=2500, width = 50)
  # vmf <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat", "Ste"), psill=100, range = 2000, nugget = 1))
  # plot(v, pl = T, model = vmf)
  # k <- krige(Y ~ 1, locations = shpPlac, newdata = grd, model = vmf) %>%
  #   st_as_sf()

  k = autoKrige(Y ~ 1, as(shpPlac, "Spatial"), as(grd, "Spatial"))

  k <- k$krige_output %>%
    st_as_sf() %>%
    dplyr::select(var1.pred)


  return(k)
}
