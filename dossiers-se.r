# scraper

if(!file.exists("data/dossiers-se.log")) {
  
  root = "http://www.senate.be"
  cat("Scraping raw upper chamber data (be patient, takes a few hours)...\n")

  sink("data/dossiers-se.log")
  cat("Launched:", as.character(Sys.time()), "\n\n")
  
  for(k in 1:5) {
    
    file = paste0("data/dossiers-se", k + 48, ".csv")
    if(!file.exists(file)) {
      
      cat("Scraping legislature", k + 48, "... ")
      h = htmlParse("http://www.senate.be/www/?MIval=/Dossiers/NieuweDossiers&LANG=fr")
      
      h = xpathSApply(h, paste0("//a[contains(@href, 'LEG=", k, "&')]/@href"))
      cat(length(h), "pages\n\n")
      
      time = Sys.time()
      d = data.frame()
      for(i in h) {
        
        cat("Page", sprintf("%3.0f", which(i == h)), "scraping ")
        
        hh = htmlParse(paste0(root, i))
        hh = xpathSApply(hh, "//a[contains(@href, 'NR=')]/@href")
        
        cat(sprintf("%3.0f", length(hh)), "dossiers\n")
        r = data.frame()
        
        for(j in hh) {
          
          t = try(htmlParse(paste0(root, j)))
          if(!"try-error" %in% class(t)) {
            
            amd = which(readHTMLTable(t)[[3]]$Titre == "Amendements")
            
            if(length(amd)) {
              
              amd = xpathSApply(t, "//table[3]/tr/td[1]/a[1]/@href")[ amd ]
              amd = paste0(amd[ !grepl("lachambre", amd) ], collapse = ";")
              
            } else {
              
              amd = NA
              
            }
            
            type = readHTMLTable(t)[[3]]$Titre[1]
            if(!length(type)) type = NA
            
            status = as.character(readHTMLTable(t)[[5]][2, 2])
            if(!length(status)) status = NA

            title = scrubber(xpathSApply(t, "//table[1]/tr/td", xmlValue)[4])
            
            topic = gsub("(  )+( )?", ",", xpathSApply(t, "//table[2]/tr/td", xmlValue))
            topic = toupper(gsub(",$", "", topic))
            
            u = xpathSApply(t, "//a[contains(@href, 'showSenator')]/@href")
            
            if(length(u))
              r = rbind(r, data.frame(legislature = 48 + as.numeric(gsub("(.*)LEG=(\\d+)(.*)", "\\2", j)),
                                      dossier = sprintf("%04.0f", as.numeric(gsub("(.*)NR=(\\d+)(.*)", "\\2", j))),
                                      type, status, title, topic, 
                                      authors = paste0(paste(
                                        xpathSApply(t, "//a[contains(@href, 'showSenator')]", xmlValue),
                                        "[", gsub("(.*)ID=(\\d+)(.*)", "\\2", u), "]"),
                                      collapse = ";"),
                                      amendments = amd, stringsAsFactors = FALSE))
            
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

dir = dir(pattern = "data/dossiers-se\\d+.csv")
for(k in dir) {
  
  file = gsub("dossiers", "sponsors", k)
  
  if(!file.exists(file)) {
    
    d = read.csv(k, stringsAsFactors = FALSE)
    cat("Building", file, ":", nrow(d), "dossiers\n")
    
    sp = data.frame()
    for(j in nrow(d):1) {
      
      dd = d[ j, ]
      r = unlist(strsplit(na.omit(dd$amendments), ";"))
      
      dd = data.frame(uid = paste0(dd$legislature, "K", sprintf("%04.0f", dd$dossier)),
                      dossier = paste0(dd$legislature, "K", sprintf("%04.0f", dd$dossier)),
                      document = NA,
                      type = dd$type,
                      topic = dd$topic,
                      status = dd$status,
                      n_au = 1 + str_count(dd$authors, ";"),
                      authors = paste0(unlist(str_extract_all(dd$authors, "\\d+|;")), collapse = ""),
                      stringsAsFactors = FALSE)
      
      if(length(r)) {
        
        amdts = c()
        for(i in r) {
          
          h = htmlParse(paste0(root, i))
          h = xpathSApply(h, "//frame[@name='pub_doc']/@src")
          h = htmlParse(paste0(root, h))
          
          h = sapply(xpathSApply(h, "//td"), function(x) {
            y = gsub("(.*)ID=(\\d+)(.*)", "\\2", xpathApply(x, "a/@href"))
            paste0(y, collapse = ";")
          })
          h = h[ h != "" ]
          amdts = c(amdts, h)
          
        }
        
        if(length(amdts)) {
          
          dd = rbind(dd, data.frame(uid = dd$uid,
                                    dossier = dd$dossier,
                                    document = NA,
                                    type = "AMENDEMENT",
                                    topic = dd$topic,
                                    status = NA,
                                    n_au = 1 + str_count(amdts, ";"),
                                    authors = amdts,
                                    stringsAsFactors = FALSE))
          
          dd$uid = paste0(dd$uid, "-", 1:nrow(dd))
          dd$document = 1:nrow(dd)
          sp = rbind(sp, dd)
          
        }
        
      } else {
        
        dd$uid = paste0(dd$uid, "-1")
        dd$document = 1
        sp = rbind(sp, dd)
        
      }
      
    }
    
    sp$type = toupper(sp$type)
    sp$status = toupper(sp$status)
    
    write.csv(sp, file, row.names = FALSE)
    
  }
  
  sp = read.csv(file, stringsAsFactors = FALSE)
    
}

bills = lapply(dir("data", pattern = "sponsors-se\\d{2}.csv", full.names = TRUE), read.csv, stringsAsFactors = FALSE)
bills = rbind.fill(bills)

bills$type[ grepl("AMENDEMENT", bills$type) ] = "AMENDEMENTS"
bills$type[ grepl("AUTRES|NOTE|PROJET|RAPPORT|RÉVISION|COMMISSION SPÉCIALE|TEXTE", bills$type) ] = "AUTRES"
bills$type[ grepl("PROPOSITION", bills$type) ] = "PROPOSITIONS"  

bills$status[ grepl("ADOPTÉ|PUBLIÉ|TERMINÉ|PROMULGUÉ", bills$status) ] = "ADOPTE"
bills$status[ grepl("CADUC|REJETÉ|NON PRIS", bills$status) ] = "REJETE"
bills$status[ grepl("SANS OBJET|RETIRÉ|COMMUNIQUÉ|A L'EXAMEN", bills$status) ] = NA

print(table(bills$type, bills$status, exclude = NULL))

if(!file.exists("data/senateurs.csv")) {
  
  # scrape only sponsors, not full list of identified senators (link below, i = 1:5)
  # htmlParse(paste0(root, "/www/?MIval=/WieIsWie/LijstDerSenatoren&LEG=", i, "&LANG=fr"))
  
  b = sort(unique(unlist(strsplit(a$authors, ";"))))
  b = lapply(b[ !b %in% c("4078", "43496") ], function(x) {
    
    cat("Scraping senator", x, "... ")
    
    h = htmlParse(paste0("http://www.senate.be/www/?MIval=/showSenator&ID=", x, "&LANG=fr"))
    sex = na.omit(str_extract(xpathSApply(h, "//table/tr", xmlValue), "Sénateur|Sénatrice"))
    man = xpathSApply(h, "//table/tr", xmlValue)
    man = unique(unlist(str_extract_all(man[ grepl("Législature", man) ], "[0-9]{4}-[0-9]{4}")))
    if(is.null(man)) man = "-"
    
    h = data.frame(sid = x,
                   nom = xpathSApply(h, "//title", xmlValue),
                   sex = ifelse(length(sex), sex, NA),
                   from = min(as.numeric(unlist(strsplit(man, "-")))),
                   to = max(as.numeric(unlist(strsplit(man, "-")))),
                   parti = scrubber(xpathSApply(h, "//table/tr[1]", xmlValue)),
                   stringsAsFactors = FALSE)
    
    h$from[ is.infinite(h$from) ] = NA
    h$to[ is.infinite(h$to) ] = NA
    h$nyears = h$to - h$from + 1
    h$parti = gsub(paste0(h$nom, " - "), "", h$parti)
    h$parti[ grepl("Correspondance", h$parti) ] = "CD&V" # Pieter de Crem bugfix
    h$parti[ grepl("Bureau", h$parti) ] = "PS"     # Patrick Moriau bugfix
    h$parti[ grepl("Privé", h$parti) ] = "sp.a"    # Inga Verhaert bugfix
    h$parti[ h$nom == "Herman De Croo" ] = "VLD"   # since 2009 (?)
    h$parti[ h$nom == "Michiel Aerbeydt" ] = "PSC" # Parti Catholique
    h$parti[ h$parti == "" ] = NA # unspecified
    
    cat(h$nom, "[", h$parti, "]", "\n")
    return(h)
    
  })
  b = rbind.fill(b)
  
  write.csv(b, "data/senateurs.csv", row.names = FALSE)
  
}

b = read.csv("data/senateurs.csv", stringsAsFactors = FALSE)

# download photos
b$photo = b$sid
for(i in unique(b$photo)) {
  photo = paste0("photos_se/", i, ".gif")
  if(!file.exists(photo) | !file.info(photo)$size)
    try(download.file(paste0("http://www.senate.be/www/?MItabObj=persoon&MIcolObj=foto&MInamObj=persoonid&MIvalObj=",
                             i, "&MItypeObj=image/gif"), photo, mode = "wb", quiet = TRUE), silent = TRUE)
  if(!file.exists(photo) | !file.info(photo)$size) {
    file.remove(photo)
    b$photo[ b$photo == i ] = NA
  } else {
    b$photo[ b$photo == i ] = gsub("photos_se/|.gif$", "", photo)
  }
}

if(!file.exists("data/net_se.rda") | update) {
  
  a = subset(bills, type == "PROPOSITIONS" & n_au > 1)

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

  b$name = paste(b$nom, "[", b$parti, "]")
  rownames(b) = b$name
  
  for(k in unique(substr(a$uid, 1, 2))) {
    
    cat("\nParsing Senate, legislature", k, "... ")

    data = subset(a, grepl(paste0("^", k), uid))
    cat(nrow(data), "dossiers\n")
    
    edges = rbind.fill(lapply(unique(data$uid), function(i) {
      
      d = unlist(strsplit(data$authors[ data$uid == i ], ";"))
      e = b$name[ b$sid %in% d ]
      
      e = subset(expand.grid(e, e), Var1 != Var2)
      e = unique(apply(e, 1, function(x) paste0(sort(x), collapse = "_")))
      
      if(length(e))
        return(data.frame(e, w = length(d) - 1)) # number of cosponsors
      else
        return(data.frame())
      
    }))
    
    # raw edge counts
    count = table(edges$uid)
    
    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ uid, function(x) sum(1 / x), data = edges)
    
    # raw counts
    edges$count = as.vector(count[ edges$uid ])
    
    e = unlist(strsplit(gsub("(.*) \\[ (.*) \\]_(.*) \\[ (.*) \\]",
                             "\\2;\\4", edges$uid), ";"))
    
    if(any(!e %in% names(colors))) {
      
      cat("Unrecognized party codes:\n")
      print(table(e[ !e %in% names(colors) ]))
      
    }

    edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                       j = gsub("(.*)_(.*)", "\\2", edges$uid),
                       w = edges$w, n = edges[, 3])
    
    # network
    
    n = network(edges[, 1:2 ], directed = FALSE)
    n %n% "title" = paste("Sénat, législature", k)
    
    n %n% "n_bills" = nrow(data)
    n %n% "n_total" = nrow(subset(bills, type == "PROPOSITIONS" & grepl(paste0("^", k), uid)))
    n %n% "n_sponsors" = table(subset(bills, type == "PROPOSITIONS" & grepl(paste0("^", k), uid))$n_au)
    
    n %v% "url" = as.character(b[ network.vertex.names(n), "sid" ])
    n %v% "photo" = as.character(b[ network.vertex.names(n), "photo" ])
    
    n %v% "party" = b[ network.vertex.names(n), "parti" ]

    network.vertex.names(n) = b[ network.vertex.names(n), "nom" ]
    
    network::set.edge.attribute(n, "source", as.character(edges[, 1]))
    network::set.edge.attribute(n, "target", as.character(edges[, 2]))
    
    network::set.edge.attribute(n, "weight", edges[, 3])
    network::set.edge.attribute(n, "count", edges[, 4])
    network::set.edge.attribute(n, "alpha",
                                as.numeric(cut(n %e% "count", c(1:4, Inf),
                                               include.lowest = TRUE)) / 5)

    # subset
    
    found = network.vertex.names(n)
    known = unique(b$nom)

    cat("\nUnrecognized nodes:", sum(!found %in% known), "out of", network.size(n), "\n")
    
    if(sum(!found %in% known) > 0) {
      
      cat(paste0(found[ !found %in% known ], collapse = ", "), "\n")
      network::delete.vertices(n, which(!found %in% known))
      
    }

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
    
    # weighted adjacency matrix to tnet
    tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
    
    # weighted degree and distance
    wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
    dist = distance_w(tnet)
    wdeg$distance = NA
    wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
    wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
    names(wdeg) = c("node", "degree", "distance", "clustering")
    
    n %v% "degree" = wdeg$degree
    n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
    
    n %v% "distance" = wdeg$distance
    n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
    
    n %v% "clustering" = wdeg$clustering    # local
    n %n% "clustering" = clustering_w(tnet) # global
    
    i = colors[ b[ n %e% "source", "parti" ] ]
    j = colors[ b[ n %e% "target", "parti" ] ]
    
    party = as.vector(i)
    party[ i != j ] = "#AAAAAA"
    
    print(table(n %v% "party", exclude = NULL))

    # number of bills (co)sponsored
    nb = sapply(n %v% "url", function(x) {
      sum(unlist(strsplit(data$authors, ";")) == x)
    })
    n %v% "n_bills" = as.vector(nb)

    n %v% "size" = as.numeric(cut(n %v% "degree", quantile(n %v% "degree"), include.lowest = TRUE))
    g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2,
                               segment.color = party) +
                           geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                           geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                           scale_size_continuous(range = c(6, 12)) +
                           scale_color_manual("", values = colors, breaks = order) +
                           theme(legend.key.size = unit(1, "cm"),
                                 legend.text = element_text(size = 16)) +
                           guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
    
    ggsave(paste0("plots/net_se", k, ".pdf"), g, width = 12, height = 9)
    ggsave(paste0("plots/net_se", k, ".jpg"), g + theme(legend.position = "none"),
           width = 9, height = 9, dpi = 72)
    
    assign(paste0("net_se", k), n)
    assign(paste0("edges_se", k), edges)
    assign(paste0("bills_se", k), subset(bills, type == "PROPOSITIONS" & grepl(paste0("^", k), uid)))
    
    # gexf
    
    gexf = paste0("net_se", k, ".gexf")
    if(!file.exists(gexf) & export) {
            
      rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
      mode = "fruchtermanreingold"
      meta = list(creator = "rgexf",
                  description = paste(mode, "placement", nrow(data), "bills"),
                  keywords = "parliament, belgium")
      
      node.att = data.frame(party = n %v% "party",
                            bills = n %v% "n_bills",
                            distance = round(n %v% "distance", 1),
                            url = n %v% "url",
                            photo = n %v% "photo",
                            stringsAsFactors = FALSE)
      
      people = paste(network.vertex.names(n), "[", n %v% "party", "]")
      people = data.frame(id = as.numeric(factor(people)), label = people, stringsAsFactors = FALSE)
      
      relations = data.frame(
        source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
        target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
        weight = round(n %e% "weight", 2), count = n %e% "count"
      )
      relations = na.omit(relations)
      
      # check all weights are positive after rounding
      stopifnot(all(relations$weight > 0))
      
      nodecolors = lapply(node.att$party, function(x)
        data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
      nodecolors = as.matrix(rbind.fill(nodecolors))
      
      # node placement
      position = do.call(paste0("gplot.layout.", mode),
                         list(as.matrix.network.adjacency(n), NULL))
      position = as.matrix(cbind(round(position, 1), 1))
      colnames(position) = c("x", "y", "z")
      
      # clean up vertex names
      people$label = gsub("(.*) \\[ (.*) \\]", "\\1", people$label)
      
      write.gexf(nodes = people, nodesAtt = node.att,
                 edges = relations[, 1:2 ], edgesWeight = relations[, 3],
                 nodesVizAtt = list(position = position, color = nodecolors,
                                    size = round(n %v% "degree", 1)),
                 # edgesVizAtt = list(size = relations[, 4]),
                 defaultedgetype = "undirected", meta = meta, output = gexf)
      
    }
    
  }
  
  save(list = ls(pattern = "net_se\\d{2}"), file = "data/net_se.rda")
  
  if(export)
    zip("net_se.zip", files = dir(pattern = "net_se\\d{2}.gexf"))
  
}

load("data/net_se.rda")

# job's done
