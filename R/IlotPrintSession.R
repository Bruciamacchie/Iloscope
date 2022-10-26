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


IlotPrintSession <- function(rep) {
  # setwd(rep)
  Nomfichs <- list.files(paste(rep,"Tables", sep="/"))
  if (!("Archives.Rdata" %in% Nomfichs)) {
    print("ATTENTION ...... Il faut d'abord créer une archive avec la fonction IlotDataImport")
    stop()
  }
  load(paste(rep,"Tables/Archives.Rdata", sep="/"))

  sessions <- unique(Equipes$SESSION)
  NomTemp <- tk_select.list(as.character(sessions), preselect = NULL,
                            multiple = FALSE, title = "Choisir une session")
  EnTour <- Equipes %>%
    st_drop_geometry() %>%
    filter(SESSION == NomTemp) %>%
    distinct(NUMEQUIPE)

  for (i in 1:dim(EnTour)[1]) {

    NumEquipe = EnTour$NUMEQUIPE[i]
    Equipe <- Equipes %>%  filter(NUMEQUIPE == NumEquipe)
    save(Equipe, file=paste(rep,"Tables/Equipe.Rdata", sep="/"))

    fpath <- system.file("extdata/rnw", "Iloscope1.Rnw", package="Iloscopes")
    dir.create(paste(rep,"PDF", sep="/"), showWarnings =F)

    knit2pdf(
      input = fpath,
      compiler = "pdflatex",
      envir = globalenv(),
      quiet = TRUE
    )

    file.rename("Iloscope1.pdf", paste0("Iloscope_équipe", NumEquipe, ".pdf"))
    file.copy(paste0("Iloscope_équipe", NumEquipe, ".pdf"), "PDF")
    unlink(paste0("Iloscope_équipe", NumEquipe, ".pdf"))
  }

  # unlink(paste0("Iloscope1.tex"))
  # unlink(paste0("Iloscope1.log"))
  # unlink(paste0("Iloscope1-concordance.tex"))


}


