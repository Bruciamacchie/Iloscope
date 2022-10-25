#' Nettoyage
#'
#' @description Suppression des fichiers inutiles
#'
#' @param rep = dossier à nettoyer
#'
#'#' @author Nolwenn
#'
#' @examples
#' cleanfolder()
#'
#' @export



cleanfolder <- function(){

  rep <- ProjetChoisir()
  setwd(rep)

  #-------delete files-------
  rules <- c(".tex",".log",".synctex.gz")
  file.remove(list.files(pattern = paste0('\\',rules ,'$', collapse = '|')))

}



