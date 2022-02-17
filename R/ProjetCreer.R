#' Iloscope : creer un nouveau projet projet
#'
#' @description Imports des différents fichiers vecteurs constitutifs d'un projet
#'
#' @param projet = nom de la table en sortie
#'
#' @examples
#' ProjetCreer("Test")
#'
#' @author Bruciamacchie Max
#'
#' @export

ProjetCreer <- function(Nom) {
  rep <- rstudioapi::selectDirectory()
  if (is.null(rep)) {
    print("Merci de choisir un répertoire")
    stop()
  } else{
    print(paste("Le répertoire retenu est .....", rep))
    dir.create(paste(rep, Nom, sep="/"), showWarnings=F)
    dir.create(paste(rep, Nom, "Vecteurs", sep="/"), showWarnings=F)
    dir.create(paste(rep, Nom,"Rasters", sep="/"), showWarnings=F)
  }
}
