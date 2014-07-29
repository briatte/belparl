# scrape full MP listing

if(!file.exists("deputes.csv")) {
  
  deputes = data.frame()
  for(i in 48:54) {
    
    cat("Parsing legislature", i, "... ")
    h = htmlParse(paste0("http://www.lachambre.be/kvvcr/showpage.cfm?section=/depute&language=fr&rightmenu=right_depute&cfm=cvlist54.cfm?legis=", i, "&today=n"))
    h = xpathSApply(h, "//a[contains(@href, 'key=')]/@href")
    cat(length(h), "pages\n")
    
    for(j in h) {
      
      hh = htmlParse(paste0("http://www.lachambre.be/kvvcr/", j))
      hh = data.frame(legislature = i,
                      nom = scrubber(xpathSApply(hh, "//div[@id='story']/*/h2", xmlValue)),
                      photo = xpathSApply(hh, "//img[contains(@src, 'cv/')]/@src"),
                      url = j,
                      bio = xpathSApply(hh, "//div[@id='story']/table[2]/tr[@valign='top'][2]/td/p", xmlValue))
      deputes = rbind(deputes, hh)
      cat(".")
      
    }
    
    cat("\n")
    
  }
  
  write.csv(deputes, "deputes.csv", row.names = FALSE)
  
}

deputes = read.csv("deputes.csv", stringsAsFactors = FALSE)
deputes$bio = scrubber(gsub("▀ ", "", deputes$bio))

# scraper

if(!file.exists("dossiers-ch.log")) {
  
  root = "http://www.dekamer.be/kvvcr/showpage.cfm?section=/flwb&language=fr&cfm=Listdocument.cfm?legislat="
  cat("Scraping raw lower chamber data (be patient, takes a few hours)...\n")
  
  sink("dossiers-ch.log")
  cat("Launched:", as.character(Sys.time()), "\n\n")
  
  # do not add 54 just yet (not enough data) -- 2014-07-25
  for(i in 47:53) {
    
    file = paste0("dossiers-ch", i, ".csv")
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

if(!file.exists("networks-ch.rda") | update) {
  
  dir = dir(pattern = "dossiers-ch\\d{2}.csv")
  for(k in dir) {
    
    file = gsub("dossiers", "sponsors", k)
    cat("\nParsing Chamber, legislature", gsub("\\D", "", k), "...\n")
    
    if(!file.exists(file)) {
      
      cat("Building", file)
      
      d = read.csv(k, stringsAsFactors = FALSE)  
      d$uid = paste0(d$legislature, "K", sprintf("%04.0f", d$dossier))

      uids = unique(d$uid)
      authors = data.frame()

      cat(" :", length(uids), "dossiers\n")
      # sink(gsub("csv$", "log", file))
      for(i in uids) {
        
        dd = subset(d, uid == i & !grepl("^NL|NL$", d$variable))
        # cat("Parsing dossier", i)
        
        topic = unique(dd$value[which(grepl("Eurovoc", dd$variable)) + 2])
        topic = paste0(topic, collapse = ",")
        
        sd = which(dd$variable == "Document principal")
        dd = dd[ min(sd):nrow(dd), ]
        
        sd = c(1, which(dd$variable == "Document(s) suivant(s)"))
        if(length(sd) < 2)
          sd = c(sd, nrow(dd))
          
          for(n in 1:(length(sd) - 1)) {
            
            ddd = dd[ sd[ n ]:sd[ n + 1 ], ]
            type = ddd$value[ which(ddd$variable == "Type")[1] + 1 ]
            
            au = which(ddd$value == "AUTEUR")
            if(length(au)) {
              au = sapply(au, function(x) paste(ddd$value[ x + 2 ], ddd$value[ x + 1 ], "[", ddd$value[ x + 3 ], "]"))
              au = unique(au)
            } else {
              au = NA
            }
            
            cs = which(ddd$value == "SIGNATAIRE")
            if(length(cs)) {
              cs = sapply(cs, function(x) paste(ddd$value[ x + 2 ], ddd$value[ x + 1 ], "[", ddd$value[ x + 3 ], "]"))
              cs = unique(cs)
            } else {
              cs = NA
            }
            
            a = na.omit(c(au, cs))
            a = a[ !grepl("ZZZ|0", a) ]
            
            # subset to cosponsored legislation
            if(length(a) > 1) {

              # cat("\n -", type, ":", length(au), "author(s)",
              #     ifelse(!length(na.omit(cs)), "",
              #            paste(length(na.omit(cs)), "cosponsor(s)")))

              authors = rbind(authors,
                              cbind(uid = paste0(i, "-", n),
                                    dossier = i, document = n,
                                    type, topic,
                                    authors = paste0(na.omit(au[ !grepl("ZZZ|0", au) ]), collapse = ";"),
                                    cosponsors = paste0(na.omit(cs[ !grepl("ZZZ|0", cs) ]), collapse = ";")))
              
            } else if(length(a) == 1) {
              
              # cat("\n -", type, ":", a)
              
            } else {
              
              # cat("\n -", type, ":", gsub("\\d|\\[|\\]|ZZZ|\\s", "", au))
              
            }
            
          }
        
        # cat("\n\n")
        
      }
      # sink()
      
      write.csv(authors, file, row.names = FALSE)
      
    }
    
    a = read.csv(file, stringsAsFactors = FALSE)
    
    a$type[ grepl("AMENDEMENT|DECISION DE NE PAS AMENDER", a$type) ] = "AMENDEMENTS"
    a$type[ grepl("AUTRES|AVIS|CORRIGE|CONCLUSIONS|DECISION|ERRATA|PROJET|RAPPORT|ANNEXE|TEXTE", a$type)
            & a$type != "PROPOSITION DE LOI" ] = "AUTRES"
    a$type[ grepl("PROPOSITION", a$type) ] = "PROPOSITIONS"

    print(table(a$type, exclude = NULL))

    a = subset(a, type == "PROPOSITIONS")

    # remove buggy dossier(s) [ 52 ]
    a = subset(a, uid != "52K1939")

    # edge list
    
    edges = lapply(unique(a$uid), function(i) {
      
      d = na.omit(c(a$authors[ a$uid == i ], a$cosponsors[ a$uid == i ]))
      d = unlist(strsplit(d, ";"))
      e = expand.grid(d, d)
      e = subset(e, Var1 != Var2)
      e = unique(apply(e, 1, function(x) paste0(sort(x), collapse = "_")))
      if(length(e)) {
        d = data.frame(i = gsub("(.*)_(.*)", "\\1", e),
                       j = gsub("(.*)_(.*)", "\\2", e),
                       w = length(d) - 1,
                       stringsAsFactors = FALSE) # number of cosponsors
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

#     # raw party values on edges
#     print(table(unlist(strsplit(gsub("(.*) \\[ (.*) \\]_(.*) \\[ (.*) \\]",
#                                      "\\2;\\4", edges$uid), ";")),
#                 exclude = NULL))

    # Emile Coulonvaux, PLP [ 47 ]
    edges$uid = gsub("\\[ PLP \\]", "[ PVV ]", edges$uid)
    
    # Arthur Honoré Buysse, libéral flamand [ 49-50 ]
    edges$uid = gsub("\\[ LIB \\]", "[ PVV ]", edges$uid)

    # Karin(e) Jiroflée [ 51 ]
    edges$uid = gsub("Karine Jiroflée", "Karin Jiroflée", edges$uid)

    # Sabien (Lahaye-)Battheu [ 51-54 ]
    edges$uid = gsub("Sabien Lahaye-Battheu", "Sabien Battheu", edges$uid)

    # party code fixes [53]
    edges$uid = gsub("Els Van Hoof \\[ 0 \\]", "Els Van Hoof [ CD&V - N-VA ]", edges$uid)
    edges$uid = gsub("Stefaan De Clercq \\[ 0 \\]", "Stefaan De Clercq [ CD&V ]", edges$uid)
    edges$uid = gsub("Myriam Delacroix-Rolin \\[ 0 \\]", "Myriam Delacroix-Rolin [ cdH ]", edges$uid)
    edges$uid = gsub("Fatma Pehlivan \\[ 0 \\]", "Fatma Pehlivan [ sp.a ]", edges$uid)

    edges$uid = gsub("!", "", edges$uid)
    edges$uid = gsub("\\[ (Agalev-)?(ECOLO|Ecolo)(-Groen)? \\]", "[ ECOLO ]", edges$uid)
    # edges$uid = gsub("\\[ (PS|SP|sp.a)(-spirit)?(\\+Vl\\.Pro)? \\]", "[ SOC ]", edges$uid)
    edges$uid = gsub("\\[ PS \\]", "[ SOC-F ]", edges$uid)
    edges$uid = gsub("\\[ (SP|sp.a)(-spirit)?(\\+Vl\\.Pro)? \\]", "[ SOC-V ]", edges$uid)
    edges$uid = gsub("\\[ (PVV|Open Vld|VLD) \\]", "[ LIB-V ]", edges$uid)
    edges$uid = gsub("\\[ (MR|FDF|PRL|FDFPPW|PRLFDF) \\]", "[ LIB-F ]", edges$uid)
    edges$uid = gsub("\\[ (VU|VU-ID|N-VA) \\]", "[ VOLKS ]", edges$uid)
    edges$uid = gsub("\\[ (CVP|CD&V) \\]", "[ C-DEM-V ]", edges$uid)
    edges$uid = gsub("\\[ (PSC|cdH) \\]", "[ C-DEM-F ]", edges$uid)
    edges$uid = gsub("\\[ CD&V - N-VA \\]", "[ C-DEM-V/VOLKS ]", edges$uid)
    edges$uid = gsub("\\[ VB \\]", "[ VLAAMS ]", edges$uid)
    edges$uid = gsub("\\[ ONAFH \\]", "[ INDEP ]", edges$uid)

    e = unlist(strsplit(gsub("(.*) \\[ (.*) \\]_(.*) \\[ (.*) \\]",
                             "\\2;\\4", edges$uid), ";"))
    
    if(any(!e %in% names(colors))) {

      cat("Unrecognized party codes:\n")
      print(table(e[ !e %in% names(colors) ]))

    }
    
    edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                       j = gsub("(.*)_(.*)", "\\2", edges$uid),
                       w = edges[, 2], n = edges[, 3],
                       stringsAsFactors = FALSE)
    
    # network
    
    n = network(edges[, 1:2 ], directed = FALSE)
    n %n% "title" = paste("Chambre, législature", gsub("\\D", "", k))
    
    n %v% "sid" = network.vertex.names(n)
    n %v% "party" = gsub("(.*) \\[ (.*) \\]", "\\2", network.vertex.names(n))

    network.vertex.names(n) = gsub("(.*) \\[ (.*) \\]", "\\1", network.vertex.names(n))
    
    network::set.edge.attribute(n, "source", edges[, 1])
    network::set.edge.attribute(n, "target", edges[, 2])
    
    network::set.edge.attribute(n, "weight", edges[, 3])
    network::set.edge.attribute(n, "count", edges[, 4])
    network::set.edge.attribute(n, "alpha",
                                as.numeric(cut(n %e% "count", c(1:4, Inf),
                                               include.lowest = TRUE)) / 5)
    
    # subset
    
    found = gsub("(.*) \\[ (.*) \\]", "\\1", network.vertex.names(n))
    
    known = unique(deputes$nom[ deputes$legislature == gsub("\\D", "", k) ])
    if(length(known)) {
      
      cat("\nUnrecognized nodes:", sum(!found %in% known), "out of", network.size(n), "\n")
      
      if(sum(!found %in% known) > 0) {
        
        cat(paste0(found[ !found %in% known ], collapse = ", "), "\n")
        network::delete.vertices(n, which(!found %in% known))
        
      }
      
    }
    
    # modularity
    
    nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
    E(nn)$weight = edges[, 3]
    
    i = gsub("(.*) \\[ (.*) \\]", "\\2", V(nn)$name)
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
    
    i = colors[ gsub("(.*) \\[ (.*) \\]", "\\2", n %e% "source") ]
    j = colors[ gsub("(.*) \\[ (.*) \\]", "\\2", n %e% "target") ]
    
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
    
    ggsave(gsub("csv", "pdf", gsub("sponsors", "plots/network", file)), g, width = 12, height = 9)
    ggsave(gsub("csv", "jpg", gsub("sponsors", "plots/network", file)), g + theme(legend.position = "none"),
           width = 9, height = 9, dpi = 72)
    
    assign(paste0("net_ch", gsub("\\D", "", k)), n)
    
    # gexf
    
    gexf = paste0("net_ch", gsub("\\D", "", k), ".gexf")
    if(!file.exists(gexf) & gsub("\\D", "", k) > 47 & export) {
            
      rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
      mode = "fruchtermanreingold"
      meta = list(creator = "rgexf",
                  description = paste0(mode, " placement"),
                  keywords = "Parliament, Belgium")
      
      people = merge(data.frame(sid = paste(network.vertex.names(n), "[", n %v% "party", "]"),
                                nom = network.vertex.names(n), 
                                party = n %v% "party", 
                                degree = n %v% "degree",
                                distance = n %v% "distance",
                                stringsAsFactors = FALSE),
                     subset(deputes,
                            legislature == as.numeric(gsub("\\D", "", k)) &
                              nom %in% network.vertex.names(n)))
      
      node.att = c("nom", "party", "photo", "url", "bio", "degree", "distance")
      node.att = cbind(label = people$sid, people[, node.att ])
      
      people = data.frame(id = as.numeric(factor(people$sid)),
                          label = people$sid,
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
                                       photo = node.att$photo,
                                       link = gsub("(.*)key=(\\d+)(.*)", "\\2", node.att$url),
                                       distance = node.att$distance,
                                       # bio = node.att$bio,
                                       stringsAsFactors = FALSE),
                 nodesVizAtt = list(position = position,
                                    color = nodecolors,
                                    size = round(node.att$degree)),
                 # edgesVizAtt = list(size = relations[, 3]),
                 defaultedgetype = "undirected", meta = meta,
                 output = gexf)
      
    }
    
  }
  
  save(list = ls(pattern = "net_ch\\d{2}"), file = "networks-ch.rda")
  
}

load("networks-ch.rda")

# job's done
