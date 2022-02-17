#' Iloscope : Choix du projet
#'
#' @description Choix du projet en tour
#'
#' @examples
#' ProjetChoisir()
#'
#' @author Bruciamacchie Max
#'
#' @export

ProjetChoisir <- function() {
  rep <- rstudioapi::selectDirectory()
  if (is.null(rep)) {
    print("Merci de choisir un répertoire")
    stop()
  } else{
    print(paste("Le répertoire retenu est .....", rep))
  }
  return(rep)
}
