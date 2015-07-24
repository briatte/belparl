# hi Belgium

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"      , showWarnings = FALSE)
dir.create("photos_ch" , showWarnings = FALSE)
dir.create("photos_se" , showWarnings = FALSE)
dir.create("plots"     , showWarnings = FALSE)

# parameters

years = c("47" = 1987, "48" = 1991, "49" = 1995, "50" = 1999, "51" = 2003,
          "52" = 2007, "53" = 2010, "54" = 2014, "55" = 2019)

plot = TRUE
gexf = TRUE

meta = c("Belgium", "Parlement")
mode = "fruchtermanreingold"

# build routine

source("data-ch.r")
source("build-ch.r")
source("comm-ch.r")
source("data-se.r")
source("build-se.r")

# have a nice day
