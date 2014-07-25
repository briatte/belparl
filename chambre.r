library(network)
library(qdap)
library(XML)

dir.create("plots", showWarnings = FALSE)

root = "http://www.dekamer.be/kvvcr/showpage.cfm?section=/flwb&language=fr&cfm=Listdocument.cfm?legislat="

# scraper

if(!file.exists("dossiers.log")) {
 
  cat("Scraping raw data (be patient, takes round three hours)...\n")

  sink("dossiers.log")
  cat("Launched:", as.character(Sys.time()), "\n\n")
  
  # do not add 54 just yet (not enough data) -- 2014-07-25
  for(i in 47:53) {
    
    file = paste0("dossiers", i, ".csv")
    if(!file.exists(file)) {
      
      cat("Scraping legislature", i, "... ")
      
      time = Sys.time()
      dossiers = data.frame()
      
      h = htmlParse(paste0(root, i))
      h = xpathSApply(h, "//a[@class='link']/@href")
      
      cat(length(h), "pages\n")
      for(j in h) {
        
        hh = htmlParse(paste0("http://www.dekamer.be/kvvcr/", j))
        hh = xpathSApply(hh, "//div[@class='linklist_0']/a/@href")
        hh = gsub("(.*)&dossierID=(.*)", "\\2", as.character(hh))
        
        cat("Page", sprintf("%3.0f", which(h == j)),
            "scraping", sprintf("%3.0f", length(hh)), "dossiers:", range(hh), "\n")
        
        for(k in hh) {
          
          t = paste0("http://www.dekamer.be/kvvcr/showpage.cfm?section=flwb&language=fr&leftmenu=none&cfm=/site/wwwcfm/search/fiche.cfm?ID=", i, "K", k, "&db=FLWB&legislat=", i)
          t = try(readHTMLTable(t, which = 1, header = FALSE, stringsAsFactors = FALSE))
          if(!"try-error" %in% class(t)) {
            
            names(t) = c("variable", "value")
            t$variable = scrubber(t$variable)
            t$value = scrubber(t$value)
            
            t = data.frame(legislature = i, dossier = k,
                           t[ t$variable != "" | !is.na(t$value), ])
            dossiers = rbind(dossiers, t)
            
          } else {
            
            cat("Error: dossier", k, "legislature", i, "\n")
            
          }
          
        }
        
      }
      
      write.csv(dossiers, file, row.names = FALSE)
      
      cat("Scraped", nrow(dossiers), "rows over",
          length(unique(dossiers$dossier)), "dossiers in",
          round(as.numeric(Sys.time() - time), 1), "minutes\n\n")
      
    }
    
  }
  
  cat("Ended:", as.character(Sys.time()), "\n\n")  
  sink()
  
}

# parser

dir = dir(pattern = "dossiers\\d{2}.csv")
for(j in dir) {
  
  file = gsub("dossiers", "sponsors", j)
  cat("Parsing", j, "...\n")
  
  if(!file.exists(file)) {
    
    d = read.csv(j, stringsAsFactors = FALSE)  
    d$uid = paste0(d$legislature, "K", sprintf("%04.0f", d$dossier))
    
    authors = data.frame()
    for(i in unique(d$uid)) {
      
      dd = subset(d, uid == i & !grepl("^NL|NL$", d$variable))
      # cat("Parsing dossier uid", i)
      
      sd = which(dd$variable == "Document principal")
      dd = dd[ min(sd):nrow(dd), ]
      
      sd = which(dd$variable == "Sous-documents")
      if(length(sd))
        dd = dd[ 1:min(sd), ]
      
      au = which(dd$value == "AUTEUR")
      if(length(au)) {
        au = sapply(au, function(x) paste(dd$value[ x + 2 ], dd$value[ x + 1 ], "[", dd$value[ x + 3 ], "]"))
        au = data.frame(uid = i, name = au, status = "author", stringsAsFactors = FALSE)
      } else {
        warning("No authors at ", i)
        au = data.frame()
      }
      
      cs = which(dd$value == "SIGNATAIRE")
      if(length(cs)) {
        cs = sapply(cs, function(x) paste(dd$value[ x + 2 ], dd$value[ x + 1 ], "[", dd$value[ x + 3 ], "]"))
        cs = data.frame(uid = i, name = cs, status = "cosponsor", stringsAsFactors = FALSE)
        au = rbind(au, cs)
      }
      
      if(nrow(au) > 1 & !any(grepl("ZZZ", au$name))) {
        authors = rbind(authors, au)
        # if(nrow(au) > 1)
        #   cat(":", sum(au$status == "author"), "author(s)", sum(au$status == "cosponsor"), "cosponsor(s)\n")
        # else
        #   cat(": single-authored\n")
      } # else {
      # cat(": government bill\n")
      # }
      
    }
    
    write.csv(authors, file, row.names = FALSE)
    
  }
  
  cat("Parsing", file, "...\n")
  a = read.csv(file)
  
  # edge list
  
  edges = lapply(unique(a$uid), function(i) {
    
    d = subset(a, uid == i)
    d = expand.grid(d$name, d$name)
    d = subset(d, Var1 != Var2)
    d$uid = apply(d, 1, function(x) paste0(sort(x), collapse = "_"))
    d = unique(d$uid)
    if(length(d)) {
      d = data.frame(i = gsub("(.*)_(.*)", "\\1", d),
                     j = gsub("(.*)_(.*)", "\\2", d),
                     w = 1 / length(d))
      return(d)
    } else {
      return(data.frame())
    }
    
  })
  
  edges = rbind.fill(edges)
  edges$uid = apply(edges, 1, function(x) paste0(sort(x[1:2]), collapse = "_"))
  edges = aggregate(w ~ uid, sum, data = edges)

  edges$uid = gsub("!", "", edges$uid)
  edges$uid = gsub("sp.a(.*)", "sp.a", edges$uid)
  edges$uid = gsub("(.*)VLD$|(.*)Vld$", "VLD", edges$uid)
  
  edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                     j = gsub("(.*)_(.*)", "\\2", edges$uid),
                     w = edges$w)
  
  # network
  
  n = network(edges[, 1:2 ], directed = FALSE)
  
  n %v% "party" = gsub("(.*) \\[ (.*) \\]", "\\2", network.vertex.names(n))
  
  network.vertex.names(n) = gsub("(.*) \\[ (.*) \\]", "\\1", network.vertex.names(n))
  
  edges[, 1] = gsub("(.*) \\[ (.*) \\]", "\\1", edges[, 1])
  network::set.edge.attribute(n, "source", edges[, 1])
  
  edges[, 2] = gsub("(.*) \\[ (.*) \\]", "\\1", edges[, 2])
  network::set.edge.attribute(n, "target", edges[, 2])
  
  network::set.edge.attribute(n, "weight", edges[, 3])
  
  # party colors
  
  colors = c(
    "Agalev-Ecolo" = "#4DAF4A", # green; leftwing [ 47-50 ]
    "ECOLO" = "#4DAF4A", # green; leftwing [ 47-50 ]
    "Ecolo-Groen" = "#4DAF4A", # green; leftwing [ 53 ]
    "Ecolo-Groen!" = "#4DAF4A", # green; leftwing [ 52 ]
    "SP" = "#E41A1C", # red; Socialistische Partij Anders / Ostbelgien [ 47-50 ]
    "sp.a" = "#E41A1C", # red; SP Regionalverband Anders [ 50 ]
    "sp.a-spirit" = "#E41A1C", # red; SP Regionalverband Anders [ 51 ]
    "sp.a+Vl.Pro" = "#E41A1C", # red; SP Regionalverband Anders [ 52 ]
    "PS" = "#E41A1C", # red; sociaux-démocrates francophones [ 47-50 ]
    "PSC" = "#FF7F00",  # orange; chrétiens-démocrates 1945-1968, à présent CDV
    "CVP" = "#FF7F00",  # orange; chrétiens-démocrates, à présent CDV [ 47-50 ]
    "CD&V" = "#FF7F00", # orange; chrétiens-démocrates [ 50-51 ]
    "cdH" = "#FF7F00",  # orange; chrétiens-démocrates [ 50-51 ]
    "PRL" = "#377EB8", # blue; ex allié FDF, aile flam = PVV [ 47-48 ]
    "PVV" = "#377EB8", # blue; libéral flamand, à présent OpenVLD [ 47-48 ]
    "VLD" = "#377EB8", # blue; libéral flamand, à présent OpenVLD [ 48-50 ]
    "Open Vld" = "#377EB8", # blue; libéral flamand [ 51 ]
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
  
  order = c("Agalev-Ecolo", "Ecolo-Groen!", "Ecolo-Groen", "ECOLO",
            "PS", "SP", "sp.a", "sp.a-spirit", "sp.a+Vl.Pro", "CVP", "PSC", "CD&V", "cdH",
            "PRL", "PVV", "VLD", "Open Vld", "MR", "FDF", "FDFPPW", "PRLFDF",
            "CD&V - N-VA", "N-VA", "VU", "VU-ID", "LDD", "VB", "INDEP", "ROSSEM")
  
  # edge colors
  
  party = n %v% "party"
  names(party) = network.vertex.names(n)

  i = colors[ party[ n %e% "source" ] ]
  j = colors[ party[ n %e% "target" ] ]
  
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"
  
  print(table(n %v% "party", exclude = NULL))
  g = ggnet(n, size = 0, segment.alpha = 1/2,
            segment.color = party) +
    geom_point(size = 9, alpha = 1/3, aes(color = n %v% "party")) +
    geom_point(size = 6, alpha = 1/2, aes(color = n %v% "party")) +
    scale_color_manual("", values = colors, breaks = order)
  
  ggsave(gsub("csv", "pdf", gsub("sponsors", "plots/network", file)), g, width = 12, height = 9)
  ggsave(gsub("csv", "jpg", gsub("sponsors", "plots/network", file)), g + theme(legend.position = "none"),
         width = 12, height = 9)
  
  assign(paste0("net", gsub("\\D", "", j)), n)
  
}

save(list = ls(pattern = "net\\d{2}"), file = "networks.rda")

# job's done
