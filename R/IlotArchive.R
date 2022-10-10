#' Load Rdata
#'
#' @description charge les archives
#'
#'
#' @examples
#' IlotArchive(rep)
#'
#' @author Bruciamacchie Max
#'
#' @export


IlotArchive <- function(rep){

  verif <- list.dirs(rep, full.names=F, recursive = F)
  if("Tables" %in% verif) {
    fich <- list.files(rep, pattern="\\.Rdata$", recursive=T)
    # load(paste(rep, fich, sep="/"))
  } else {
    stop("Merci d'utiliser la fonction IlotDataImport")
  }
  return(fich)
}
