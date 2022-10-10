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

KrigeageFonc <- function(grd, shpPlac, idvar=2) {
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

  k = autoKrige(Y ~ 1, as(shpPlac, "Spatial"), as(grd, "Spatial"))

  k <- k$krige_output %>%
    st_as_sf() %>%
    dplyr::select(var1.pred)

  return(k)
}
