# Etape 0 : Penser à installer les librairies suivantes
# puis à les activer

# -------- Librairies -------
library(tidyverse)
library(sf)
library(rmapshaper)
library(readxl)
# library(gstat)
library(stars)
library(automap)
library(Iloscopes)
library(knitr)
library(tcltk)

# ------- Etape 1 : installer le package Iloscope -----------

library(Iloscopes)

# -------  Etape 2 : import des données du classeur -----------
VerifIlot() # à modifier
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

