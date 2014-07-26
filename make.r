
# packages

library(igraph)
library(ggplot2)
library(GGally) # network plots
library(network)
library(plyr)
library(qdap) # clean strings
library(XML)  # parse XPath

# folders

dir.create("plots", showWarnings = FALSE)

# parties

colors = c(
  # leftwing
  "ECOLO" = "#4DAF4A", # green [ "Agalev-Ecolo" C.47-50 and S.49-50, "Ecolo" or "ECOLO" C.51 and S.49-53, "Ecolo-Groen" C.52-53 and S.49, 52-53 ]
  "SOC-V" = "#FB8072", # light red
  "SOC-F" = "#E41A1C", # red [ "PS" all, "SP" C.47-50 and S.49-50, "sp.a" C.50-53 and S.49-53, "sp.a-spirit" C.51, "sp.a+Vl.Pro" C.52 ]
  # conservatives
  "C-DEM-V" = "#FDB462",  # light orange [ "PSC" C.47-50 and S.49-50, "CVP" C.47.50 and S.49-50, "CD&V" C.50-53 and S.50-53, "cdH" C.50-53 and S.49-53 ]
  "C-DEM-F" = "#FF7F00", # orange
  "C-DEM-V/VOLKS" = "#FFFFB3", # light yellow, nationalists allied to Christian-democrats [ C.52 ]
  # liberals
  "LIB-F" = "#377EB8", # blue [ "VLD" or "Open Vld" C.48-53, S.49-53, "PVV" C.47-48, "PRL" C.47-48 and S.52, "MR" C.50-53, S.49-53 ]
  "LIB-V" = "#80B1D3", # light blue [ VLD ]
  # federalists
  # "FDF" = "#F781BF", # pink [ C.47, 53 ]
  # "FDF/PPW" = "#FCCDE5", # light pink [ C.48 ]
  # "FDF/PRL" = "#FCCDE5", # light pink [ C.49-50, S.49 ]
  # nationalists
  "VOLKS" = "#FFFF33", # yellow [ C.47-49 and S.49, "VU-ID" S.49-50, "N-VA" C.51-53 and S.52-53 ]
  # extreme-right
  "VLAAMS" = "#A65628", # brown [ C.48-53, S.49-53 ]
  "FN" = "#444444", # dark grey [ S.51 ]
  # personalist parties
  "LDD" = "#984EA3", # purple; Jean-Marie Dedecker [ C.52-53, S.52 ]
  "ROSSEM" = "#BEBADA", # light purple; Jean-Pierre Van Rossem [ C.48 ]
  # residual
  "INDEP" = "#AAAAAA" # light grey
)

order = c("ECOLO", "SOC-F", "SOC-V", "LIB-F", "LIB-V", 
          "C-DEM-F", "C-DEM-V", "C-DEM-V/VOLKS",
          "VOLKS", # "FDF", "FDF/PPW", "FDF/PRL", 
          "ROSSEM", "LDD", "VLAAMS", "FN", "INDEP")

# scrapers

update = FALSE

source("dossiers-ch.r")
source("dossiers-se.r")

m = sapply(mget(ls(pattern = "net_(ch|se)")), function(x) x %n% "modularity")
w = sapply(mget(ls(pattern = "net_(ch|se)")), function(x) x %n% "modularity_walktrap")
l = sapply(mget(ls(pattern = "net_(ch|se)")), function(x) x %n% "modularity_louvain")
m = cbind(names(m), m, w, l)
m = data.frame(id = m[, 1],
               ch = ifelse(substr(m[, 1], 5, 6) == "se", "Sénat", "Chambre"),
               legislature = gsub("\\D", "", m[, 1]),
               Empirical.Modularity = as.numeric(m[, 2]),
               Maximized.Walktrap = as.numeric(m[, 3]),
               Maximized.Louvain = as.numeric(m[, 4]))
m$Maximization.Ratio = m$Empirical.Modularity / apply(m[, 5:6 ], 1, max)

qplot(data = melt(m, c("id", "ch", "legislature")),
      x = legislature, group = ch, y = value, geom = c("line", "point")) +
  facet_grid(ch ~ variable) +
  theme_linedraw(16) +
  labs(y = NULL, x = NULL) +
  theme(panel.grid = element_blank())

ggsave("plots/modularity.pdf", width = 12, height = 6)
ggsave("plots/modularity.png", width = 12, height = 6, dpi = 72)

# qplot(data = m, x = legislature, y = ratio, group = ch,
#       geom = c("line", "point")) +
#   facet_grid(. ~ ch) +
#   theme_linedraw(16) +
#   labs(y = "Empirical / Maximized\n", x = NULL) +
#   theme(legend.position = "none", panel.grid = element_blank())

# kthxbye
