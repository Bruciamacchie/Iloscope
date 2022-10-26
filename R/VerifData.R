#' Edition par équipe
#' @description La fonction permet d'analyser le choix de chaque équipe (format .pdf)
#'
#' @import knitr
#'
#' @param rep = dossier en cours
#'
#' @examples
#' IlotPrintEquipe(rep)
#'
#' @author Bruciamacchie Max
#'
#' @export


VerifData <- function(rep) {
  # setwd(rep)

  fpath <- system.file("extdata/rnw", "VerifData.Rnw", package="Iloscopes")
  dir.create(paste(rep,"PDF", sep="/"), showWarnings =F)

  knit2pdf(
    input = fpath,
    compiler = "pdflatex",
    envir = globalenv(),
    quiet = TRUE
  )
}



