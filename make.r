
# packages

library(network)
library(plyr)
library(qdap)
library(XML)

# folders

dir.create("plots", showWarnings = FALSE)

# scrapers

source("dossiers-ch.r") # takes ~  3 hours
source("dossiers-se.r") # takes ~ 13 hours

# kthxbye
