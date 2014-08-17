# scrape full MP listing

if(!file.exists("data/deputes.csv")) {
  
  deputes = data.frame()
  for(i in 48:54) {
    
    cat("Parsing MPs in legislature", i, "... ")

    h = htmlParse(paste0("http://www.lachambre.be/kvvcr/showpage.cfm?section=/depute&language=fr&rightmenu=right_depute&cfm=cvlist54.cfm?legis=", i, "&today=n"))
    h = xpathSApply(h, "//a[contains(@href, 'key=')]/@href")

    cat(length(h), "pages\n")
    
    for(j in h) {
      
      hh = htmlParse(paste0("http://www.lachambre.be/kvvcr/", j))
      hh = data.frame(legislature = i,
                      nom = scrubber(xpathSApply(hh, "//div[@id='story']/*/h2", xmlValue)),
                      mandate = paste0(xpathSApply(hh, "//div[@id='story']/*/a[contains(@href, 'lactivity=')]",
                                                   xmlValue), collapse = ";"),
                      photo = xpathSApply(hh, "//img[contains(@src, 'cv/')]/@src"),
                      url = j,
                      bio = xpathSApply(hh, "//div[@id='story']/table[2]/tr[@valign='top'][2]/td/p", xmlValue))
      deputes = rbind(deputes, hh)
      cat(".")
      
    }
    
    cat("\n")
    
  }
  
  write.csv(deputes, "data/deputes.csv", row.names = FALSE)
  
}

deputes = read.csv("data/deputes.csv", stringsAsFactors = FALSE)

# download photos
for(i in unique(deputes$photo)) {
  photo = gsub("/site/wwwroot/images/cv", "photos_ch", i)
  if(!file.exists(photo) | !file.info(photo)$size)
    try(download.file(paste0("http://www.lachambre.be", i), photo, mode = "wb", quiet = TRUE), silent = TRUE)
  if(!file.exists(photo) | !file.info(photo)$size) {
    file.remove(photo)
    deputes$photo[ deputes$photo == i ] = NA
  } else {
    deputes$photo[ deputes$photo == i ] = gsub("photos_ch/|.gif$", "", photo)
  }
}

deputes$bio = scrubber(gsub("▀ ", "", deputes$bio))

deputes$sexe = str_extract(deputes$bio, "Député(e)?")
deputes$sexe[ deputes$nom == "Juliette Boulet" ] = "Députée" # Flemish text
deputes$sexe[ deputes$nom == "Paul Meeus" ] = "Député" # typo
deputes$sexe[ deputes$sexe == "Députée" ] = "F"
deputes$sexe[ deputes$sexe == "Député" ] = "M"

deputes$from = as.numeric(sapply(str_extract_all(deputes$mandate, "[0-9]{4}"), min))
deputes$to = as.numeric(sapply(str_extract_all(deputes$mandate, "[0-9]{4}"), max))
deputes$nyears = deputes$to - deputes$from + 1

deputes$annee_naissance = str_extract(deputes$bio,
                                      "(N|n)é(e)? (a|à|te) (\\w|,|\\.|'|-|\\(|\\)|\\s)+ le \\d+(er)? \\w+ \\d{4}")
deputes$annee_naissance = as.numeric(str_extract(deputes$annee_naissance, "[0-9]{4}"))
deputes$annee_naissance[ deputes$nom == "Juliette Boulet" ] = 1981 # Flemish text
deputes$annee_naissance[ deputes$nom == "Magda Raemaekers" ] = 1947 # Wikipedia additions
deputes$annee_naissance[ deputes$nom == "Frédéric Daerden" ] = 1970
deputes$annee_naissance[ deputes$nom == "Georges Dallemagne" ] = 1958
deputes$annee_naissance[ deputes$nom == "Frank Wilrycx" ] = 1965
deputes$url = gsub("(.*)key=(\\d+)&lactivity=(\\d+)", "\\2", deputes$url)

# scraper

if(!file.exists("data/dossiers-ch.log")) {
  
  root = "http://www.dekamer.be/kvvcr/showpage.cfm?section=/flwb&language=fr&cfm=Listdocument.cfm?legislat="
  cat("Scraping raw lower chamber data (be patient, takes a few hours)...\n")
  
  sink("data/dossiers-ch.log")
  cat("Launched:", as.character(Sys.time()), "\n\n")
  
  # do not add 54 just yet (not enough data) -- 2014-07-25
  for(i in 47:53) {
    
    file = paste0("data/dossiers-ch", i, ".csv")
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
        hh = gsub("(.*)&dossisink()
                  erID=(.*)", "\\2", as.character(hh))
        
        cat("Page", sprintf("%3.0f", which(h == j)),
            "scraping", sprintf("%3.0f", length(hh)), "dossiers\n")
        
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

if(!file.exists("data/net_ch.rda") | update) {
  
  dir = dir("data", pattern = "dossiers-ch\\d{2}.csv", full.names = TRUE)
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
        
        status = dd$value[which(grepl("Etat d'avancement", dd$variable)) + 1]
        if(!length(status))
          status = NA

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
                                    type, topic, status,
                                    n_au = length(a),
                                    authors = paste0(na.omit(au[ !grepl("ZZZ|0", au) ]), collapse = ";"),
                                    cosponsors = paste0(na.omit(cs[ !grepl("ZZZ|0", cs) ]), collapse = ";")))
              
            } else if(length(a) == 1) {
              
              # cat("\n -", type, ":", a)
              authors = rbind(authors,
                              cbind(uid = paste0(i, "-", n),
                                    dossier = i, document = n,
                                    type, topic, status, n_au = 1,
                                    authors = a,
                                    cosponsors = ""))
              
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
    a$type[ grepl("AVIS|CORRIGE|CONCLUSIONS|DECISION|ERRATA|PROJET|RAPPORT|ANNEXE|TEXTE", a$type) ] = "AUTRES"
    a$type[ grepl("PROPOSITION", a$type) ] = "PROPOSITIONS"

    a$status[ grepl("ADOPTE|TERMINE", a$status) ] = "ADOPTE"
    a$status[ grepl("CADUQUE|REJETE", a$status) ] = "REJETE"
    a$status[ grepl("SANS OBJET|RETIRE", a$status) ] = NA
    
    print(table(a$type, a$status, exclude = NULL))

    b = subset(a, type == "PROPOSITIONS" & n_au > 1)
    b = subset(b, uid != "52K1939") # remove buggy dossier [ 52 ]

    cat("Subsetting to", nrow(b), "cosponsored bills\n")
    
    # edge list
    edges = rbind.fill(lapply(unique(b$uid), function(i) {
      
      d = na.omit(c(b$authors[ b$uid == i ], b$cosponsors[ b$uid == i ]))
      d = unlist(strsplit(d, ";"))
      
      e = subset(expand.grid(d, d), Var1 != Var2)
      e = unique(apply(e, 1, function(x) paste0(sort(x), collapse = "_")))
      
      if(length(e))
        return(data.frame(e, w = length(d) - 1))
      else
        return(data.frame())
      
    }))
    
    # raw edge counts
    count = table(edges$e)
    
    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ e, function(x) sum(1 / x), data = edges)

    # raw counts of ties
    edges$count = as.vector(count[ edges$e ])
    
    #     # raw party values on edges
    #     print(table(unlist(strsplit(gsub("(.*) \\[ (.*) \\]_(.*) \\[ (.*) \\]",
    #                                      "\\2;\\4", edges$uid), ";")),
    #                 exclude = NULL))
    
    # Emile Coulonvaux, PLP [ 47 ]
    edges$e = gsub("\\[ PLP \\]", "[ PVV ]", edges$e)
    
    # Jan Decorte INDEP/ROSSEM [ 48 ]
    edges$e = gsub("Jan Decorte \\[ INDEP \\]", "Jan Decorte [ ROSSEM ]", edges$e)
    
    # Arthur Honoré Buysse, libéral flamand [ 49-50 ]
    edges$e = gsub("\\[ LIB \\]", "[ PVV ]", edges$e)
    
    # Karin(e) Jiroflée [ 51 ]
    edges$e = gsub("Karine Jiroflée", "Karin Jiroflée", edges$e)
    
    # Sabien (Lahaye-)Battheu [ 51-54 ]
    edges$e = gsub("Sabien Lahaye-Battheu", "Sabien Battheu", edges$e)
    
    # party code fixes [53]
    edges$e = gsub("Els Van Hoof \\[ 0 \\]", "Els Van Hoof [ CD&V - N-VA ]", edges$e)
    edges$e = gsub("Stefaan De Clercq \\[ 0 \\]", "Stefaan De Clercq [ CD&V ]", edges$e)
    edges$e = gsub("Myriam Delacroix-Rolin \\[ 0 \\]", "Myriam Delacroix-Rolin [ cdH ]", edges$e)
    edges$e = gsub("Fatma Pehlivan \\[ 0 \\]", "Fatma Pehlivan [ sp.a ]", edges$e)
    
    edges$e = gsub("!", "", edges$e)
    edges$e = gsub("\\[ (Agalev-)?(ECOLO|Ecolo)(-Groen)? \\]", "[ ECOLO ]", edges$e)
    # edges$e = gsub("\\[ (PS|SP|sp.a)(-spirit)?(\\+Vl\\.Pro)? \\]", "[ SOC ]", edges$e)
    edges$e = gsub("\\[ PS \\]", "[ SOC-F ]", edges$e)
    edges$e = gsub("\\[ (SP|sp.a)(-spirit)?(\\+Vl\\.Pro)? \\]", "[ SOC-V ]", edges$e)
    edges$e = gsub("\\[ (PVV|Open Vld|VLD) \\]", "[ LIB-V ]", edges$e)
    edges$e = gsub("\\[ (MR|FDF|PRL|FDFPPW|PRLFDF) \\]", "[ LIB-F ]", edges$e)
    edges$e = gsub("\\[ (VU|VU-ID|N-VA) \\]", "[ VOLKS ]", edges$e)
    edges$e = gsub("\\[ (CVP|CD&V) \\]", "[ C-DEM-V ]", edges$e)
    edges$e = gsub("\\[ (PSC|cdH) \\]", "[ C-DEM-F ]", edges$e)
    edges$e = gsub("\\[ CD&V - N-VA \\]", "[ C-DEM-V/VOLKS ]", edges$e)
    
    # special case of party coalition [ 52 ]
    if(grepl("52", k)) {
      edges$e = gsub("\\[ VOLKS \\]", "[ C-DEM-V/VOLKS ]", edges$e)
      edges$e = gsub("\\[ C-DEM-V \\]", "[ C-DEM-V/VOLKS ]", edges$e)
      edges$e = gsub("Luc Sevenhans \\[ C-DEM-V/VOLKS \\]", "Luc Sevenhans [ VLAAMS ]", edges$e)
    }
    edges$e = gsub("\\[ VB \\]", "[ VLAAMS ]", edges$e)
    edges$e = gsub("\\[ ONAFH \\]", "[ INDEP ]", edges$e)
    
    # last fixes (leave at end)
    edges$e = gsub("Lisette Nelis-Van Liedekerke \\[ LIB-V \\]",
                   "Lisette Nelis-Van Liedekerke [ C-DEM-V ]", edges$e)
    edges$e = gsub("Philippe Dallons \\[ SOC-F \\]", "Philippe Dallons [ ECOLO ]", edges$e)
    edges$e = gsub("Pierrette Cahay-André \\[ LIB-F \\]", "Pierrette Cahay-André [ C-DEM-F ]", edges$e)
    edges$e = gsub("Richard Fournaux \\[ C-DEM-F \\]", "Richard Fournaux [ LIB-F ]", edges$e)
    edges$e = gsub("Vincent Decroly \\[ INDEP \\]", "Vincent Decroly [ ECOLO ]", edges$e)
    
    e = unlist(strsplit(gsub("(.*) \\[ (.*) \\]_(.*) \\[ (.*) \\]",
                             "\\2;\\4", edges$e), ";"))
    
    if(any(!e %in% names(colors))) {
      
      cat("Unrecognized party codes:\n")
      print(table(e[ !e %in% names(colors) ]))
      
    }
    
    edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$e),
                       j = gsub("(.*)_(.*)", "\\2", edges$e),
                       w = edges[, 2], n = edges[, 3],
                       stringsAsFactors = FALSE)
    
    # network
    
    n = network(edges[, 1:2 ], directed = FALSE)
    n %n% "title" = paste("Chambre, législature", gsub("\\D", "", k))

    n %n% "n_bills" = nrow(b)
    n %n% "n_total" = nrow(subset(a, type == "PROPOSITIONS"))
    n %n% "n_sponsors" = table(subset(a, type == "PROPOSITIONS")$n_au)
    
    n %v% "party" = gsub("(.*) \\[ (.*) \\]", "\\2", network.vertex.names(n))
    
    network.vertex.names(n) = gsub("(.*) \\[ (.*) \\]", "\\1", network.vertex.names(n))

    # append to sponsors data
    s = merge(data.frame(nom = network.vertex.names(n), party = n%v% "party"),
              unique(deputes[ deputes$legislature == gsub("\\D", "", k), ]))
    rownames(s) = s$nom

    n %v% "born" = s[ network.vertex.names(n), "annee_naissance" ]
    n %v% "sex" = s[ network.vertex.names(n), "sexe" ]
    n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]
    n %v% "url" = s[ network.vertex.names(n), "url" ]
    n %v% "photo" = as.character(gsub("/site/wwwroot/images/cv/|.gif$", "", s[ network.vertex.names(n), "photo" ]))
    
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
    
    i = colors[ gsub("(.*) \\[ (.*) \\]", "\\2", n %e% "source") ]
    j = colors[ gsub("(.*) \\[ (.*) \\]", "\\2", n %e% "target") ]
    
    party = as.vector(i)
    party[ i != j ] = "#AAAAAA"
    
    print(table(n %v% "party", exclude = NULL))
    
    # number of bills cosponsored
    nb = sapply(network.vertex.names(n), function(x) {
      nrow(subset(b, grepl(x, authors) | grepl(x, cosponsors)))
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
    
    ggsave(gsub("csv", "pdf", gsub("data/sponsors-ch", "plots/net_ch", file)), g, width = 12, height = 9)
    ggsave(gsub("csv", "jpg", gsub("data/sponsors-ch", "plots/net_ch", file)), g + theme(legend.position = "none"),
           width = 9, height = 9, dpi = 72)
        
    assign(paste0("net_ch", gsub("\\D", "", k)), n)
    assign(paste0("edges_ch", gsub("\\D", "", k)), edges)
    assign(paste0("bills_ch", gsub("\\D", "", k)), a)
    
    # gexf
    
    gexf = paste0("net_ch", gsub("\\D", "", k), ".gexf")
    if(!file.exists(gexf) & gsub("\\D", "", k) > 47 & export) {
            
      rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
      mode = "fruchtermanreingold"
      meta = list(creator = "rgexf",
                  description = paste(mode, "placement", nrow(b), "bills"),
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
      
      # save with compressed floats
      write.gexf(nodes = people, nodesAtt = node.att,
                 edges = relations[, 1:2 ], edgesWeight = relations[, 3],
                 nodesVizAtt = list(position = position, color = nodecolors,
                                    size = round(n %v% "degree", 1)),
                 # edgesVizAtt = list(size = relations[, 4]),
                 defaultedgetype = "undirected", meta = meta, output = gexf)
      
    }
    
  }
  
  save(list = ls(pattern = "(net_ch|edges_ch|bills_ch)\\d{2}"), file = "data/net_ch.rda")
  
  if(export)
    zip("net_ch.zip", files = dir(pattern = "net_ch\\d{2}.gexf"))
  
}

load("data/net_ch.rda")

# job's done
