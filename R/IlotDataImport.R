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

IlotDataImport <- function(MaTable) {
  # le code ci-dessous permet d'aller directement au répertoire qui contient le fichier source
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  rep <- ProjetChoisir()

  # -------- Lecture du fichier Couches.xlsx ---------
  couches <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="Couches")

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

    if (!is.na(buffer) & is.numeric(buffer)) {
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
  Acces             <- lectureShape("Acces")
  Parcellaire       <- lectureShape("Parcellaire")
  Peuplement        <- lectureShape("Peuplement", 1)
  DateCoupe         <- lectureShape("DateCoupe", 1)
  Placette          <- lectureShape("Placette")

  ProtectionStatut  <- lectureShape("ProtectionStatut")
  HorsSylv          <- lectureShape("HorsSylviculture")
  Exploitation      <- lectureShape("Exploitation")
  Captage           <- lectureShape("Captage")
  Patrimoine        <- lectureShape("Patrimoine")
  Ilots             <- lectureShape("Ilots")
  Corridors         <- lectureShape("Corridor")
  Mature            <- lectureShape("Maturité")

  Equipes           <- lectureShape("Equipes")
  Solution          <- lectureShape("Solution")
  Organisation      <- lectureShape("Organisation")

  # -------- Sauvegarde ---------
  dir.create(paste(rep,"Tables", sep="/"))
  save(perim,Acces,Parcellaire,DateCoupe,Peuplement,Placette,ProtectionStatut,HorsSylv,
       Exploitation,Captage,Patrimoine,Ilots,Corridors,Mature,
       Equipes,
       file= paste0(paste(rep,"Tables", sep="/"),"/", MaTable, ".Rdata"))
}

