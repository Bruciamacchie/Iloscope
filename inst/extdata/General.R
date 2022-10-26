# Etape 0 : Penser à installer la librairie librarian
# Cette étape n'est à faire qu'une fois
install.packages("librarian")

# -------- Librairies à installer si besoin et à activer -------
librarian::shelf(tidyverse,sf,rmapshaper,readxl,stars,automap,knitr,tcltk)
# library(gstat)

# ------- Etape 1 : installer le package Iloscope -----------

library(Iloscopes)

# -------  Etape 2 : import des données du classeur -----------
VerifData(rep)

res <- Iloscopes::IlotDataImport()
rep <- res$rep
perim <- res$perim

# -------  Etape 3 : Import archive Rdata -----------
rep <- ProjetChoisir()
load(paste(rep,"Tables/Archives.Rdata", sep="/"))

# -------  Etape 4 : krigeage -----------
IlotKrigeage(rep, perim)

# -------  Etape 5 : Edition des PDF pour une session -----------
IlotPrintSession(rep)


# -------  Etape 6 : Comparatif entre équipes pour une session -----------


# -------  Etape 7 : Nettoyage -----------
cleanfolder()

