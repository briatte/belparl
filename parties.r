# party colors

colors = c(
  "PT" = "#B2182B", # dark red [C.54 "PTB-GO!"]
  # "Groen" and "Agalev-Ecolo" C.47-50 and S.49-50
  # "Ecolo" or "ECOLO" C.51 and S.49-53
  # "Ecolo-Groen" C.52-53 and S.49, 52-53
  "ECOLO" = "#4DAF4A", # green
  # "PS" all
  # "SP" C.47-50 and S.49-50
  # "sp.a" C.50-53 and S.49-53
  # "sp.a-spirit" C.51
  # "sp.a+Vl.Pro" C.52
  "SOC-F" = "#E41A1C", # red
  "SOC-V" = "#FB8072", # light red
  # "PSC" C.47-50 and S.49-50,
  # "CVP" C.47.50 and S.49-50,
  # "CD&V" C.50-53 and S.50-53,
  # "cdH" C.50-53 and S.49-53
  "CDEM-F" = "#FF7F00", # orange
  "CDEM-V" = "#FDB462", # light orange
  # personalistic lists
  "LDD" = "#984EA3",    # purple; Jean-Marie Dedecker [ C.52-53, S.52 ]
  "ROSSEM" = "#BEBADA", # light purple; Jean-Pierre Van Rossem [ C.48 ] (not used)
  # nationalists allied to Christian-democrats [ C.52 ]
  "CDEM-V/VOLKS" = "#FFFFB3", # light yellow
  # nationalists
  # "VOLKS" C.47-49 and S.49
  # "VU-ID" S.49-50
  # "N-VA" C.51-53 and S.52-53
  "VOLKS" = "#FFFF33", # yellow
  # liberals
  # "VLD" or "Open Vld" C.48-53, S.49-53
  # "MR" C.50-53, S.49-53
  # "PVV" C.47-48
  # "PRL" C.47-48 and S.52
  "LIB-F" = "#377EB8", # blue 
  "LIB-V" = "#80B1D3", # light blue [ VLD ]
  "PP" = "#984EA3", # purple [C.54, no color clash with LDD]
  "DLB" = "#444444", # dark grey [ C.54, no color clash with FN ]
  # federalists (not used)
  # "FDF" = "#F781BF", # pink [ C.47, 53 ]
  # "FDF/PPW" = "#FCCDE5", # light pink [ C.48 ]
  # "FDF/PRL" = "#FCCDE5", # light pink [ C.49-50, S.49 ]
  # extreme-right
  "VLAAMS" = "#A65628", # brown [ C.48-53, S.49-53 ]
  "FN" = "#444444",     # dark grey [ S.51 ]
  # unaffiliateds/independents
  "IND" = "#AAAAAA" # light grey
)

# party group names

groups = c(
  "PT" = "Worker's Party",
  "ECOLO" = "Greens",
  "SOC-F" = "Francophone Socialists",
  "SOC-V" = "Flemish Socialists",
  "CDEM-F" = "Francophone Conservatives",
  "CDEM-V" = "Flemish Conservatives",
  "LDD" = "Libertair, Direct, Democratisch",
  "ROSSEM" = "ROSSEM",
  "CDEM-V/VOLKS" = "Flemish Conservatives + Volksunie",
  "VOLKS" = "Volksunie",
  "LIB-F" = "Francophone Liberals",
  "LIB-V" = "Flemish Liberals",
  "PP" = "People's Party",
  "DLB" = "Debout Les Belges!",
  "VLAAMS" = "Vlaams Blok",
  "FN" = "Front National",
  "IND" = "Independent"
)

# ParlGov Left/Right scores

scores = c(
  "PT" = 1.3,
  "ECOLO" = 2.6,
  "SOC-F" = 2.9,
  "SOC-V" = 3.3,
  "CDEM-F" = 5.5,
  "CDEM-V" = 5.8,
  "LDD" = 6,
  "ROSSEM" = 6,
  "CDEM-V/VOLKS" = 6.2,
  "VOLKS" = 6.5,
  "LIB-F" = 6.8,
  "LIB-V" = 7,
  "PP" = 7.4,
  "DLB" = 8.7,
  "VLAAMS" = 9.7,
  "FN" = 9.7,
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
