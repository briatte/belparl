# hi Belgium

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos_ch.zip"))
  unzip("photos_ch.zip")

dir.create("photos_ch", showWarnings = FALSE)

if (file.exists("photos_se.zip"))
  unzip("photos_se.zip")

dir.create("photos_se", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw", showWarnings = FALSE)

# parameters

years = c("47" = 1987, "48" = 1991, "49" = 1995, "50" = 1999, "51" = 2003,
          "52" = 2007, "53" = 2010, "54" = 2014, "55" = 2019)

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Belgium",
  "lang" = "fr", # Wikipedia language for chamber and constituencies
  "ch" = "Chambre_des_représentants_de_Belgique",
  "se" = "Sénat_(Belgique)",
  "type-ch" = "Lower",
  "type-se" = "Upper",
  "ipu-ch" = 2029,
  "ipu-se" = 2030,
  "seats-ch" = 150,
  "seats-se" = 60
)

# build routine

source("data-ch.r")
source("build-ch.r")
source("comm-ch.r")

source("data-se.r")
source("build-se.r")

save(list = ls(pattern = "^(co)?(net|edges|bills)_be_(ch|se)\\d{4}$"),
     file = "data/net_be.rda")

# have a nice day
