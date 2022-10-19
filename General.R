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

# Etape 2 : import des données du classeur
# VerifIlot()
perim <- Iloscopes::IlotDataImport()

# Etape 3 : krigeage
rep <- ProjetChoisir()
fich <- IlotArchive(rep)
load(paste(rep,"Tables", fich, sep="/"))
IlotKrigeage(rep, perim)

# Etape 4 : Edition des PDF pour une session
IlotPrintSession(rep)


# Etape 5 : Comparatif entre équipes pour une session

