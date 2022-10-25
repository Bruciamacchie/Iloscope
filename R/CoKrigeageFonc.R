#' Fonction Krigeage avec peuplement
#'
#' @description Fonction krigeage pour conversion des données ponctuelles. Prise en compte des peuplements
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import gstat
#' @import automap
#'
#' @param grd = grid
#' @param shpPlac = placettes au format sf POINT
#' @param shp = fichier géoréférencé au format sf POLYGON correspondant aux peuplements ou aux
#' unités de gestion
#' @param idvar = variable Y retenue
#'
#' @examples
#' k <- CoKrigeageFonc(grd, Plac, Peuplt)
#'
#' @author Bruciamacchie Max
#'
#' @export

CoKrigeageFonc <- function(grd, shpPlac, shp, idvar="VcHa") {
  # shpPlac = Placettes
  # shpPeuplt = Peuplement
  # ------------ Vérification
  if (missing(grd)) stop("missing grd")
  if (missing(shpPlac)) stop("missing shpPlac")
  if (missing(shpPeuplt)) stop("missing shpPeuplt")
  if (!(idvar %in% names(shpPlac))) stop(paste("La variable",idvar," n'existe pas"))
  if(st_crs(shpPlac) != st_crs(grd)) {
    shpPlac <- shpPlac %>% st_transform(st_crs(grd))
  }
  if(st_crs(shpPeuplt) != st_crs(grd)) {
    shpPeuplt <- shpPeuplt %>% st_transform(st_crs(grd))
  }

  # ----------------- Krigeage : la prédiction dépend également des peuplements
  shpPeuplt <- shpPeuplt %>%
    mutate(Peuplt = as.factor(Peuplt))

  grd <- grd %>%
    st_sf() %>%
    st_intersection(shpPeuplt) %>%
    filter(!is.na(Peuplt))

  pos <- which(names(shpPlac) == idvar)
  names(shpPlac)[pos] <- "Y"

  shpPlac <- shpPlac %>%
    mutate(Y = ifelse(is.na(Y), 0, Y)) %>%
    st_intersection(shpPeuplt) %>%
    mutate(Peuplt = as.factor(Peuplt))

  k = autoKrige(Y ~ Peuplt, as(shpPlac, "Spatial"), as(grd, "Spatial"))$krige_output %>%
    st_as_sf()

  v <- variogram(Y ~ Peuplt, data=shpPlac, cutoff=2000, width = 50)
  vmf <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat", "Ste"), psill=100, range = 2000, nugget = 1))
  # plot(v, pl = T, model = vmf)
  kplt <- krige(Y ~ Peuplt, locations = shpPlac, newdata = grd, model = vmf) %>%
    st_as_sf() %>%
    rename(Gha = var1.pred)


  st_write(kplt, "PredictGhaPeuplt.shp")


  return(k)
}
