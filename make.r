
# packages

library(network)
library(plyr)
library(qdap)
library(XML)

# folders

dir.create("plots", showWarnings = FALSE)

# parties

colors = c(
  "Agalev-Ecolo" = "#4DAF4A", # green; leftwing [ 47-50 ]
  "ECOLO" = "#4DAF4A", # green; leftwing [ 47-50 ]
  "Ecolo-Groen" = "#4DAF4A", # green; leftwing [ 53 ]
  "Ecolo-Groen!" = "#4DAF4A", # green; leftwing [ 52, fixed excl. mark ]
  "SP" = "#E41A1C", # red; Socialistische Partij Anders / Ostbelgien [ 47-50 ]
  "sp.a" = "#E41A1C", # red; SP Regionalverband Anders [ 50 ]
  "sp.a-spirit" = "#E41A1C", # red; SP Regionalverband Anders [ 51, fixed to 'sp.a' ]
  "sp.a+Vl.Pro" = "#E41A1C", # red; SP Regionalverband Anders [ 52, fixed to 'sp.a' ]
  "PS" = "#E41A1C", # red; sociaux-démocrates francophones [ 47-50 ]
  "PSC" = "#FF7F00",  # orange; chrétiens-démocrates 1945-1968, à présent CDV
  "CVP" = "#FF7F00",  # orange; chrétiens-démocrates, à présent CDV [ 47-50 ]
  "CD&V" = "#FF7F00", # orange; chrétiens-démocrates [ 50-51 ]
  "cdH" = "#FF7F00",  # orange; chrétiens-démocrates [ 50-51 ]
  "PRL" = "#377EB8", # blue; ex allié FDF, aile flam = PVV [ 47-48 ]
  "PVV" = "#377EB8", # blue; libéral flamand, à présent OpenVLD [ 47-48 ]
  "VLD" = "#377EB8", # blue; libéral flamand, à présent OpenVLD [ 48-50 ]
  "Open Vld" = "#377EB8", # blue; libéral flamand [ 51, fixed to 'VLD' ]
  "MR" = "#377EB8", # blue; coalition libérale [ 50-51 ]
  "FDF" = "#F781BF", # pink, ex allié PRL [ 47 ]
  "FDFPPW" = "#F781BF", # pink, ex allié PRL [ 48 ]
  "PRLFDF" = "#F781BF", # pink, alliance PRL [ 49-50 ]
  "VB" = "#A65628", # brown; far-right [ 48-51 ]
  "ROSSEM" = "#AAAAAA", # grey; Jean-Pierre Van Rossem
  "INDEP" = "#AAAAAA", # grey
  "VU" = "#FFFF33", # yellow; Volksunie, nationalistes
  "VU-ID" = "#FFFF33", # yellow; Volksunie, nationalistes (?)
  "CD&V - N-VA" = "#FFFF33", # yellow; ex-Volksunie, nationalistes (?) [ 52 ]
  "N-VA" = "#FFFF33", # yellow; ex-Volksunie, nationalistes
  "LDD" = "#984EA3" # violet; Jean-Marie Dedecker, libertarian
)

order = c("Agalev-Ecolo", "Ecolo-Groen", "ECOLO",
          "PS", "SP", "sp.a", "CVP", "PSC", "CD&V", "cdH",
          "PRL", "PVV", "VLD", "MR", "FDF", "FDFPPW", "PRLFDF",
          "CD&V - N-VA", "N-VA", "VU", "VU-ID", "LDD",
          "VB", "INDEP", "ROSSEM")

# scrapers

source("dossiers-ch.r") # takes ~  3 hours
source("dossiers-se.r") # takes ~ 13 hours

# kthxbye
