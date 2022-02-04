#' Liste et import les couches necessaire a l'iloscope
#'
#' @description Imports des différents fichiers constitutifs d'un projet
#'
#' @format data frame contenant près de 142 lignes et 2 variables
#'
#' @import tidyverse
#' @import readxl
#'
#' @param Nom = nom de la table en sortie
#'
#' @examples
#' data(CodesEssIFN)
#'
#' @author Bruciamacchie Max
#'
#' @export

IlotDataImport <- function() {
  # le code ci-dessous permet d'aller directement au répertoire qui contient le fichier source
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("..")
  print(getwd()) # vérifie le répertoire de travail

  # -------- lecture du fichier Couches.xlsx ---------
  polys <- read_excel("Data/Couches.xlsx", sheet="Data")

  # -------- Import zone ---------
  fich <- polys %>%
    filter(Theme == "Zone") %>%
    slice(1) %>%
    pull(Nom)
  zone <- st_read(paste("Data/DataBrutes/Fait",fich, sep="/"), quiet=T)
}

