#' Liste et import les couches vecteurs necessaires a l'iloscope
#'
#' @description Imports des différents fichiers vecteurs constitutifs d'un projet
#'
#' @import sf
#' @import tidyverse
#' @import readxl
#'
#' @param projet = nom de la table en sortie
#'
#' @examples
#' IlotDataImport("MotteServolex")
#'
#' @author Bruciamacchie Max
#'
#' @export

IlotDataImport <- function(projet = NULL) {
  # le code ci-dessous permet d'aller directement au répertoire qui contient le fichier source
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  rep <- ProjetChoisir()

  # -------- Lecture du fichier Couches.xlsx ---------
  couches  <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="Couches")
  DataPlac <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="Placettes")
  Haut     <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="Haut")
  DataUG   <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="ParcelleUG")
  ParamEss <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="ParamEss")
  EssReg   <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="EssReg")

  # -------- Fonction Extractions du fichier Couches.xlsx ---------
  lectureShape <- function(theme, select=0){
    fich <- couches %>%
      filter(Theme == theme) %>%
      slice(1)
    nom = pull(fich, Nom)
    buffer = pull(fich, Buffer)
    buffer <- ifelse(is.na(buffer),0,buffer)

    shp <- st_read(paste(rep,"Vecteurs",nom, sep="/"), quiet=T) %>%
      st_transform(2154)

    if (is.numeric(buffer)) {
      zoneB <- st_geometry(st_buffer(perim, dist=buffer))
      shp <- st_intersection(shp, zoneB)
    }
    if (select !=0) {
      champ = pull(fich, Champ)
      shp <- shp %>%
        dplyr::select(champ)
    }
    return(shp)
  }

  # -------- Zone d'étude ---------
  perim <- lectureShape("Périmètre") %>% dplyr::select(geom)

  # -------- Import des vecteurs ---------
  for (i in 2:dim(couches)[1]) {
    shp <- lectureShape(couches$Theme[i])
    assign(couches$Theme[i], shp)
  }

  # -------- Fusion tables ---------
  if (dim(DataPlac)[1] > 0) {

    PlacGtot <- DataPlac %>%
      filter(is.na(Catégorie)) %>%
      group_by(NumPlac) %>%
      summarise(GTOT = sum(Gha))

    PlacGtotEss <- DataPlac %>%
      filter(is.na(Catégorie)) %>%
      dplyr::select(-Catégorie) %>%
      left_join(Placette[,c("NumPlac", "geom")], by = "NumPlac")

    PlacMature <- DataPlac %>%
      left_join(ParamEss, by = c("Essence", "Catégorie")) %>%
      mutate(Mature = CoefftMature * Gha,
             Vha = FH * Gha,
             VcHa = Vha * PU) %>%
      group_by(NumPlac) %>%
      summarise(Mature = sum(Mature, na.rm=T),
                Vha = sum(Vha, na.rm=T),
                VcHa = sum(VcHa, na.rm=T))

    Placette <- Placette %>%
      dplyr::select(NumPlac, geom) %>%
      left_join(PlacGtot, by = "NumPlac") %>%
      left_join(PlacMature, by = "NumPlac")

  }

  if (dim(DataUG)[1] > 0) {
    ParcelleUG <- ParcelleUG %>%
      dplyr::select(IIDTN_FRT,IIDTN_PRF,NumParc,NumUG) %>%
      left_join(DataUG,  by = c("IIDTN_FRT", "IIDTN_PRF", "NumParc", "NumUG")) %>%
      distinct()
  }

  # -------- Sauvegarde ---------
  dir.create(paste(rep,"Tables", sep="/"), showWarnings = F)
  rm(couches, lectureShape, i, shp)
  if (is.null(projet)) {
    projet= "MaTable"
  }
  save(list = ls(all.names = TRUE), file= paste0(paste(rep,"Tables", sep="/"),"/", projet, ".Rdata"))
  # save.image(file= paste0(paste(rep,"Tables", sep="/"),"/", projet, ".Rdata"))
}

