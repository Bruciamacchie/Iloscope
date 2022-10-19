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
  setwd(rep)
  Nomfichs <- list.files("Tables")
  if (length(Nomfichs) == 0) {
    print("ATTENTION ...... Il faut d'abord créer une archive avec la fonction IlotDataImport")
    stop()
  }
  load("Tables/Archives.Rdata")

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
    save(Equipe, file="Tables/Equipe.Rdata")

    knit2pdf(
      input = "Iloscope1.Rnw",
      compiler = "pdflatex",
      quiet = TRUE
    )

    file.rename("Iloscope1.pdf", paste0("Iloscope_équipe", NumEquipe, ".pdf"))
  }

  unlink(paste0("Iloscope1.tex"))
  unlink(paste0("Iloscope1.log"))
  unlink(paste0("Iloscope1-concordance.tex"))


}


