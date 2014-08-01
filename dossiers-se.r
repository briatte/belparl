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

dir = dir(pattern = "dossiers-se\\d+.csv")
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

a = lapply(dir(pattern = "sponsors-se\\d{2}.csv"), read.csv, stringsAsFactors = FALSE)
a = rbind.fill(a)

a$type[ grepl("AMENDEMENT", a$type) ] = "AMENDEMENTS"
a$type[ grepl("AUTRES|NOTE|PROJET|RAPPORT|RÉVISION|COMMISSION SPÉCIALE|TEXTE", a$type) ] = "AUTRES"
a$type[ grepl("PROPOSITION", a$type) ] = "PROPOSITIONS"  

a$status[ grepl("ADOPTÉ|PUBLIÉ|TERMINÉ|PROMULGUÉ", a$status) ] = "ADOPTE"
a$status[ grepl("CADUC|REJETÉ|NON PRIS", a$status) ] = "REJETE"
a$status[ grepl("SANS OBJET|RETIRÉ|COMMUNIQUÉ|A L'EXAMEN", a$status) ] = NA

print(table(a$type, a$status, exclude = NULL))

if(!file.exists("senateurs.csv")) {
  
  # scrape only sponsors, not full list of identified senators (link below, i = 1:5)
  # htmlParse(paste0(root, "/www/?MIval=/WieIsWie/LijstDerSenatoren&LEG=", i, "&LANG=fr"))
  
  b = sort(unique(unlist(strsplit(a$authors, ";"))))
  b = lapply(b[ !b %in% c("4078", "43496") ], function(x) {
    
    cat("Scraping senator", x, "... ")
    
    h = htmlParse(paste0("http://www.senate.be/www/?MIval=/showSenator&ID=", x, "&LANG=fr"))
    h = data.frame(sid = x,
                   nom = xpathSApply(h, "//title", xmlValue),
                   parti = scrubber(xpathSApply(h, "//table/tr[1]", xmlValue)),
                   stringsAsFactors = FALSE)
    
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
  
  write.csv(b, "senateurs.csv", row.names = FALSE)
  
}

if(!file.exists("networks-se.rda") | update) {
  
  a = subset(a, type == "PROPOSITIONS" & n_au > 1)
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

  b$name = paste(b$nom, "[", b$parti, "]")
  rownames(b) = b$name
  
  for(k in unique(substr(a$uid, 1, 2))) {
    
    cat("\nParsing Senate, legislature", k, "... ")

    data = subset(a, grepl(paste0("^", k), uid))
    cat(nrow(data), "dossiers\n")
    
    edges = lapply(unique(data$uid), function(i) {
      
      d = subset(data, uid == i)
      d = unlist(strsplit(d$authors, ";"))
      d = b$name[ b$sid %in% d ]
      d = expand.grid(d, d)
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
    
    n %v% "sid" = b[ network.vertex.names(n), "sid" ]
    n %v% "name" = b[ network.vertex.names(n), "name" ]
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
        
    party = n %v% "party"
    names(party) = network.vertex.names(n)
    
    i = colors[ b[ n %e% "source", "parti" ] ]
    j = colors[ b[ n %e% "target", "parti" ] ]
    
    party = as.vector(i)
    party[ i != j ] = "#AAAAAA"
    
    print(table(n %v% "party", exclude = NULL))

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
    
    ggsave(paste0("plots/network-se", k, ".pdf"), g, width = 12, height = 9)
    ggsave(paste0("plots/network-se", k, ".jpg"), g + theme(legend.position = "none"),
           width = 9, height = 9, dpi = 72)
    
    assign(paste0("net_se", k), n)
    assign(paste0("edges_se", k), edges)
    assign(paste0("bills_se", k), a)
    
    # gexf
    
    gexf = paste0("net_se", k, ".gexf")
    if(!file.exists(gexf) & export) {
            
      rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
      mode = "fruchtermanreingold"
      meta = list(creator = "rgexf",
                  description = paste0(mode, " placement"),
                  keywords = "Parliament, Belgium")
      
      people = data.frame(sid = n %v% "sid",
                          name = n %v% "name",
                          nom = network.vertex.names(n), 
                          party = n %v% "party", 
                          degree = n %v% "degree",
                          distance = n %v% "distance",
                          stringsAsFactors = FALSE)
      
      node.att = c("nom", "party", "sid", "name", "degree", "distance")
      node.att = cbind(label = people$name, people[, node.att ])
      
      people = data.frame(id = as.numeric(factor(people$name)),
                          label = people$name,
                          stringsAsFactors = FALSE)
      
      relations = data.frame(
        source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
        target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
        weight = n %e% "weight"
      )
      relations = na.omit(relations)
      
      nodecolors = lapply(node.att$party, function(x)
        data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .3 ))
      nodecolors = as.matrix(rbind.fill(nodecolors))
      
      net = as.matrix.network.adjacency(n)
      
      # placement method (Kamada-Kawai best at separating at reasonable distances)
      position = paste0("gplot.layout.", mode)
      if(!exists(position)) stop("Unsupported placement method '", position, "'")
      
      position = do.call(position, list(net, NULL))
      position = as.matrix(cbind(position, 1))
      colnames(position) = c("x", "y", "z")
      
      # compress floats
      position[, "x"] = round(position[, "x"], 2)
      position[, "y"] = round(position[, "y"], 2)
      
      write.gexf(nodes = people,
                 edges = relations[, -3],
                 edgesWeight = relations[, 3],
                 nodesAtt = data.frame(label = as.character(node.att$label),
                                       name = node.att$nom,
                                       party = node.att$party,
                                       sid = gsub("\\D", "", node.att$sid),
                                       distance = node.att$distance,
                                       stringsAsFactors = FALSE),
                 nodesVizAtt = list(position = position,
                                    color = nodecolors,
                                    size = round(node.att$degree)),
                 # edgesVizAtt = list(size = relations[, 3]),
                 defaultedgetype = "undirected", meta = meta,
                 output = gexf)
      
    }
    
  }
  
  save(list = ls(pattern = "net_se\\d{2}"), file = "networks-se.rda")
  
}

# load("networks-se.rda")

# job's done
