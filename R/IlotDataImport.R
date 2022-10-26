#' Liste et import les couches vecteurs necessaires a l'iloscope
#'
#' @description Imports des différents fichiers vecteurs constitutifs d'un projet
#'
#' @import sf
#' @import tidyverse
#' @import readxl
#' @import rmapshaper
#'
#' @examples
#' IlotDataImport(rep, "MotteServolex")
#'
#' @author Bruciamacchie Max
#'
#' @export

IlotDataImport <- function() {
  rm(list = ls())
  rep <- ProjetChoisir()

  Nomfichs <- list.files(paste(rep, "Excel", sep="/"), pattern = "\\.xlsx$")
  if (length(Nomfichs) > 1) {
    NomTemp <- tk_select.list(as.character(Nomfichs), preselect = NULL,
                            multiple = FALSE, title = "Choisir une fichier")

  } else{
    NomTemp <- Nomfichs[1]
  }

  # -------- Lecture du fichier Couches.xlsx ---------
  couches  <- read_excel(paste(rep,"Excel",NomTemp, sep="/"), sheet="Couches")
  DataPlac <- read_excel(paste(rep,"Excel",NomTemp, sep="/"), sheet="Placettes")
  # Haut     <- read_excel(paste(rep,fich, sep="/"), sheet="Haut")
  DataUG   <- read_excel(paste(rep,"Excel",NomTemp, sep="/"), sheet="ParcelleUG")
  ParamEss <- read_excel(paste(rep,"Excel",NomTemp, sep="/"), sheet="ParamEss")
  EssReg   <- read_excel(paste(rep,"Excel",NomTemp, sep="/"), sheet="EssReg")
  Cout     <- read_excel(paste(rep,"Excel",NomTemp, sep="/"), sheet="CoutEspece")

  # -------- Fonction Extractions du fichier Couches.xlsx ---------
  lectureShape <- function(theme, select=0){
    fich <- couches %>%
      filter(Theme == theme) %>%
      slice(1)
    nom = pull(fich, Nom)
    buffer = pull(fich, Buffer)
    buffer <- ifelse(is.na(buffer), 0, buffer)

    shp <- st_read(paste(rep,"Vecteurs",nom, sep="/"), quiet=T) %>%
      st_transform(2154) %>%
      st_make_valid()

    if (theme != "Perimetre") {
      if (is.numeric(buffer)) {
        zoneB <- st_geometry(st_buffer(perim, dist=buffer))
        shp <- st_intersection(shp, zoneB)
      }
      if (select !=0) {
        champ = pull(fich, Champ)
        shp <- shp %>%
          dplyr::select(champ)
      }
    }

    return(shp)
  }

  # -------- Zone d'étude ---------
  perim <- lectureShape("Perimetre") %>% dplyr::select(geom)

  # -------- Import des vecteurs ---------
  for (i in 2:dim(couches)[1]) {
    shp <- lectureShape(couches$Theme[i])
    assign(couches$Theme[i], shp)
  }

  # -------- Fusion tables ---------
  if (dim(DataPlac)[1] > 0) {

    PlacGtot <- DataPlac %>%
      filter(!is.na(Categorie)) %>%
      group_by(NumPlac) %>%
      dplyr::summarise(GTOT = sum(Gha))

    PlacMature <- DataPlac %>%
      left_join(ParamEss, by = c("Essence", "Categorie")) %>%
      mutate(Mature = CoefftMature * Gha,
             Vha = FH * Gha,
             VcHa = Vha * PU) %>%
      group_by(NumPlac) %>%
      summarise(Mature = sum(Mature, na.rm=T),
                Vha = sum(Vha, na.rm=T),
                VcHa = sum(VcHa, na.rm=T))

    Placettes <- Placettes %>%
      dplyr::select(NumPlac, geom) %>%
      left_join(PlacGtot, by = "NumPlac") %>%
      left_join(PlacMature, by = "NumPlac") %>%
      mutate(GTOT = ifelse(is.na(GTOT), 0, GTOT),
             Vha = ifelse(is.na(Vha), 0, Vha),
             VcHa = ifelse(is.na(VcHa), 0, VcHa),
             Mature = ifelse(is.na(Mature), 50, Mature))

    PlacGtotEss <- DataPlac %>%
      filter(!is.na(Categorie)) %>%
      dplyr::select(-Categorie)

  }

  if (dim(DataUG)[1] > 0) {
    ParcelleUG <- ParcelleUG %>%
      dplyr::select(IIDTN_FRT,IIDTN_PRF,NumPar,NumUG) %>%
      left_join(DataUG,  by = c("IIDTN_FRT", "IIDTN_PRF", "NumPar", "NumUG")) %>%
      distinct()
  }


  # -------- Lignes ---------
  Lignes <- ParcelleUG %>%
    group_by(IIDTN_FRT, IIDTN_PRF, NumPar) %>%
    summarise() %>%
    ms_lines()  %>%
    ms_explode()


  # -------- Sauvegarde ---------
  dir.create(paste(rep,"Tables", sep="/"), showWarnings = F)
  # nom = NomProjet
  rm(couches, lectureShape, i, shp)
  save(list = ls(all.names = TRUE), file= paste0(rep,"/Tables/Archives.Rdata"))

  out <- c(rep, perim)
  names(out) <- c("rep", "perim")

  return(out)
}

