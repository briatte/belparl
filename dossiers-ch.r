# scraper

if(!file.exists("dossiers-ch.log")) {
 
  root = "http://www.dekamer.be/kvvcr/showpage.cfm?section=/flwb&language=fr&cfm=Listdocument.cfm?legislat="
  cat("Scraping raw lower chamber data (be patient, takes ~ 3 hours)...\n")

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

dir = dir(pattern = "dossiers-ch\\d{2}.csv")
for(j in dir) {
  
  file = gsub("dossiers", "sponsors", j)
  cat("Parsing legislature", gsub("\\D", "", j), "...\n")
  
  if(!file.exists(file)) {
    
    cat("Building", file, "...\n")
    
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
  edges$uid = gsub("\\[ sp.a(-spirit)?(+Vl.Pro)? \\]", "[ sp.a ]", edges$uid)
  edges$uid = gsub("\\[ Open Vld \\]", "[ VLD ]", edges$uid)
  
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

save(list = ls(pattern = "net\\d{2}"), file = "networks-ch.rda")

# job's done
