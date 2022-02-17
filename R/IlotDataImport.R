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

IlotDataImport <- function(projet) {
  # le code ci-dessous permet d'aller directement au répertoire qui contient le fichier source
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  rep <- ProjetChoisir()

  # -------- Lecture du fichier Couches.xlsx ---------
  couches <- read_excel(paste(rep,"Couches.xlsx", sep="/"), sheet="Data")

  # -------- Fonction Extractions du fichier Couches.xlsx ---------
  lectureShape <- function(theme, select=0){
    fich <- couches %>%
      filter(Theme == theme) %>%
      slice(1)
    nom = pull(fich, Nom)
    buffer = pull(fich, Buffer)
    shp <- st_read(paste(rep,"Vecteurs",nom, sep="/"), quiet=T)
    buffer <- ifelse(is.na(buffer),0,buffer)
    if (is.numeric(buffer)) {
      zoneB <- st_geometry(st_buffer(zone, dist=buffer))
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
  zone <- lectureShape("Zone") %>%
    dplyr::select(geom)

  # -------- Import des vecteurs ---------
  Acces             <- lectureShape("Accès")
  Parcellaire       <- lectureShape("Parcellaire") # -------- Import Parcellaire
  DateCoupe         <- lectureShape("DateCoupe", 1)
  Peuplement        <- lectureShape("Peuplement", 1)
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


  # -------- Sauvegarde ---------
  dir.create(paste(rep,"Tables", sep="/"))
  save(zone,Acces,Parcellaire,DateCoupe,Peuplement,Placette,ProtectionStatut,HorsSylv,
       Exploitation,Captage,Patrimoine,Ilots,Corridors,Mature,
       Equipes,
       file= paste0(paste(rep,"Tables", sep="/"),"/", projet, ".Rdata"))
}

