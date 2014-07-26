# scraper

if(!file.exists("dossiers-se.log")) {
  
  root = "http://www.senate.be"
  cat("Scraping raw upper chamber data (be patient, takes a few hours)...\n")

  sink("dossiers-se.log")
  cat("Launched:", as.character(Sys.time()), "\n\n")
  
  for(k in 1:5) {
    
    file = paste0("dossiers-se", k + 48, ".csv")
    if(!file.exists(file)) {
      
      cat("Scraping legislature", k + 48, "... ")
      h = htmlParse("http://www.senate.be/www/?MIval=/Dossiers/NieuweDossiers&LANG=fr")
      
      h = xpathSApply(h, paste0("//a[contains(@href, 'LEG=", k, "&')]/@href"))
      cat(length(h), "pages\n\n")
      
      time = Sys.time()
      d = data.frame()
      for(i in h) {
        
        cat("Parsing", i, "... ")
        
        hh = htmlParse(paste0(root, i))
        hh = xpathSApply(hh, "//a[contains(@href, 'NR=')]/@href")
        
        cat(length(hh), "records\n")
        r = data.frame()
        
        for(j in hh) {
          
          t = try(htmlParse(paste0(root, j)))
          if(!"try-error" %in% class(t)) {
            
            u = xpathSApply(t, "//a[contains(@href, 'showSenator')]/@href")
            
            if(length(u))
              r = rbind(r, data.frame(legislature = 48 + as.numeric(gsub("(.*)LEG=(\\d+)(.*)", "\\2", j)),
                                      dossier = sprintf("%04.0f", as.numeric(gsub("(.*)NR=(\\d+)(.*)", "\\2", j))),
                                      sid = gsub("(.*)ID=(\\d+)(.*)", "\\2", u),
                                      name = xpathSApply(t, "//a[contains(@href, 'showSenator')]", xmlValue),
                                      stringsAsFactors = FALSE))
          }
          
        }
        
        d = rbind(r, d)
        
      }
      
      write.csv(d, file, row.names = FALSE)
      
      cat("Scraped", nrow(d), "rows over",
          length(unique(d$dossier)), "dossiers in",
          round(as.numeric(Sys.time() - time), 1), "minutes\n\n")
      
    }
    
  }
  
  cat("Ended:", as.character(Sys.time()), "\n")
  sink()

}

if(!file.exists("networks-se.rda") | update) {
  
  a = lapply(dir(pattern = "-se\\d{2}.csv"), read.csv, stringsAsFactors = FALSE)
  a = rbind.fill(a)
  
  a$uid = paste0(a$legislature, "K", sprintf("%04.0f", a$dossier))
  a = ddply(a, .(dossier), transform, n = length(name))
  a = subset(a, n > 1)
  a$name = paste(scrubber(a$name), a$sid)
  
  # scrape only sponsors, not full list of identified senators
  
  if(!file.exists("senateurs.csv")) {
    
    b = sort(unique(a$sid))
    b = lapply(rev(b), function(x) {
      
      cat("Scraping senator", x, "... ")
      h = htmlParse(paste0("http://www.senate.be/www/?MIval=/showSenator&ID=", x, "&LANG=fr"))
      h = data.frame(sid = x,
                     nom = xpathSApply(h, "//title", xmlValue),
                     parti = scrubber(xpathSApply(h, "//table/tr[1]", xmlValue)),
                     stringsAsFactors = FALSE)
      h$parti = gsub(paste0(h$nom, " - "), "", h$parti)
      h$parti[ grepl("Correspondance", h$parti) ] = "CD&V" # Pieter de Crem bugfix
      h$parti[ grepl("Bureau", h$parti) ] = "PS" # Patrick Moriau bugfix
      h$parti[ grepl("Privé", h$parti) ] = "sp.a" # Inga Verhaert bugfix
      h$parti[ h$nom == "Herman De Croo" ] = "VLD" # since 2009 (?)
      cat(h$nom, "[", h$parti, "]", "\n")
      return(h)
      
    })
    b = rbind.fill(b)
    
    write.csv(b, "senateurs.csv", row.names = FALSE)
    
  }
  
  b = read.csv("senateurs.csv", stringsAsFactors = FALSE)
  
  # match lower chamber
  
  b$parti[ b$parti %in% c("Vlaams Belang", "VL. BLOK") ] = "VLAAMS"
  b$parti[ b$parti %in% c("Agalev", "Agalev-Ecolo", "Groen", "Groen!", "Ecolo-Groen", "Ecolo") ] = "ECOLO"
  b$parti[ b$parti== "PS" ] = "SOC-F"
  b$parti[ b$parti %in% c("SP", "SP.A-SPIRIT", "sp.a") ] = "SOC-V"
  b$parti[ b$parti %in% c("PRL", "PRL-FDF", "MR") ] = "LIB-F"
  b$parti[ b$parti %in% c("PVV", "Open Vld", "VLD") ] = "LIB-V"
  b$parti[ b$parti %in% c("PSC", "cdH") ] = "C-DEM-F"
  b$parti[ b$parti %in% c("CVP", "CD&V") ] = "C-DEM-V"
  b$parti[ b$parti %in% c("VU", "VU-ID", "N-VA") ] = "VOLKS"
  b$parti[ b$parti == "Indépendant" ] = "INDEP"
  
  for(k in unique(a$legislature)) {
    
    cat("\nParsing Senate, legislature", k, "...\n")
    data = subset(a, legislature == k)
    
    edges = lapply(unique(data$uid), function(i) {
      
      d = subset(data, uid == i)
      d = expand.grid(d$name, d$name)
      d = subset(d, Var1 != Var2)
      d$uid = apply(d, 1, function(x) paste0(sort(x), collapse = "_"))
      d = unique(d$uid)
      if(length(d)) {
        d = data.frame(i = gsub("(.*)_(.*)", "\\1", d),
                       j = gsub("(.*)_(.*)", "\\2", d),
                       w = length(d))
        return(d)
      } else {
        return(data.frame())
      }
      
    })
    
    edges = rbind.fill(edges)
    edges$uid = apply(edges, 1, function(x) paste0(sort(x[ 1:2 ]), collapse = "_"))

    # using raw counts as weights
    edges = aggregate(w ~ uid, length, data = edges)
    
    edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                       j = gsub("(.*)_(.*)", "\\2", edges$uid),
                       w = edges$w)
    
    # network
    rownames(b) = paste(b$nom, b$sid)
    
    n = network(edges[, 1:2 ], directed = FALSE)
    n %n% "title" = paste("Sénat, législature", k)
    
    # modularity
    
    nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
    E(nn)$weight = edges[, 3]
    
    i = b[ V(nn)$name, "parti" ]
    i[ i %in% c("ROSSEM", "LDD", "FN", "", "INDEP") ] = NA # very small groups
    
    nn = nn - which(is.na(i))
    i = as.numeric(factor(i[ !is.na(i) ]))
        
    n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)
    
    walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
    
    # max. partition
    maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
    walktrap = walktrap[[ maxwalks ]]
    
    n %n% "modularity_walktrap" = modularity(walktrap)
    
    louvain = multilevel.community(nn)
    
    n %n% "modularity_louvain" = modularity(louvain)
    
    network::set.edge.attribute(n, "source", as.character(edges[, 1]))
    network::set.edge.attribute(n, "target", as.character(edges[, 2]))
    network::set.edge.attribute(n, "weight", edges[, 3])
    
    network::set.edge.attribute(n, "source", b[ n %e% "source", "nom" ])
    network::set.edge.attribute(n, "target", b[ n %e% "target", "nom" ])
    
    n %v% "party" = b[ network.vertex.names(n), "parti" ]
    network.vertex.names(n) = b[ network.vertex.names(n), "nom" ]
    
    party = n %v% "party"
    names(party) = network.vertex.names(n)
    
    i = colors[ party[ n %e% "source" ] ]
    j = colors[ party[ n %e% "target" ] ]
    
    party = as.vector(i)
    party[ i != j ] = "#AAAAAA"
    
    print(table(n %v% "party", exclude = NULL))
    g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2,
                               segment.color = party) +
                           geom_point(size = 9, alpha = 1/3, aes(color = n %v% "party")) +
                           geom_point(size = 6, alpha = 1/2, aes(color = n %v% "party")) +
                           scale_color_manual("", values = colors, breaks = order))
    
    ggsave(paste0("plots/network-se", k, ".pdf"), g, width = 12, height = 9)
    ggsave(paste0("plots/network-se", k, ".jpg"), g + theme(legend.position = "none"),
           width = 9, height = 9, dpi = 72)
    
    assign(paste0("net_se", k), n)
    
  }
  
  save(list = ls(pattern = "net_se\\d{2}"), file = "networks-se.rda")
  
}

load("networks-se.rda")

# job's done
