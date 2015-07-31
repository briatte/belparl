for(k in 54:48) { # excluding C. 47
  
  cat("Chambre, législature", k,
      years[ as.character(k) ], "to",
      years[ as.character(k + 1) ])
  
  data = subset(b, legislature == k & type == "PROPOSITION DE LOI" & n_a > 1)
  
  # special case of party coalition [ 52 ]
  if(k == 52) {
    data$sponsors = gsub("\\[ VOLKS \\]", "[ CDEM-V/VOLKS ]", data$sponsors)
    data$sponsors = gsub("\\[ CDEM-V \\]", "[ CDEM-V/VOLKS ]", data$sponsors)
    data$sponsors = gsub("Luc Sevenhans \\[ CDEM-V/VOLKS \\]", "Luc Sevenhans [ VLAAMS ]", data$sponsors)
  }
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  #
  # directed edge list
  #
  
  edges = bind_rows(lapply(data$sponsors, function(d) {
    
    w = unlist(strsplit(d, ";"))
    
    d = expand.grid(i = w, j = w[ 1 ], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))
  
  #
  # edge weights
  #
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # sanity check
  stopifnot(edges$gsw <= 1)
  
  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[1]
  n %n% "title" = paste("Chambre",
                        years[ as.character(k) ], "to",
                        years[ as.character(k + 1) ])

  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(b, legislature == k & type == "PROPOSITION DE LOI")$n_a)

  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")

  # assign party codes to nodes
  n %v% "party" = gsub("(.*)\\s\\[\\s(.*)\\s\\]", "\\2", network.vertex.names(n))
  network.vertex.names(n) = gsub("(.*)\\s\\[\\s(.*)\\s\\]", "\\1", network.vertex.names(n))
  
  # remove party codes from source/target edges
  edges[, 1] = gsub("(.*)\\s\\[\\s(.*)\\s\\]", "\\1", edges[, 1])
  edges[, 2] = gsub("(.*)\\s\\[\\s(.*)\\s\\]", "\\1", edges[, 2])

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
    
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  # append party id to sponsors data
  s = merge(data.frame(nom = network.vertex.names(n),
                       party = n%v% "party", stringsAsFactors = FALSE),
            unique(deputes[ deputes$legislature == k, ]))

  #
  # remove a small number of unidentified sponsors
  #

  if(network.size(n) != nrow(s)) {

    cat("Missing", network.size(n) - nrow(s), "sponsor(s):",
        paste0(network.vertex.names(n)[ !network.vertex.names(n) %in% s$nom ], 
               collapse = ", "), 
        "\n")

    # trim edge list
    m = network.vertex.names(n)[ !network.vertex.names(n) %in% s$nom ]
    m = paste0("^(", paste0(m, collapse = "|"), ")")
    edges = subset(edges, !grepl(m, i) & !grepl(m, j))

    # trim network
    network::delete.vertices(n, which(!network.vertex.names(n) %in% s$nom))

  }

  rownames(s) = s$nom
  
  n %v% "url" = as.character(s[ network.vertex.names(n), "url" ])  
  n %v% "sex" = as.character(s[ network.vertex.names(n), "sexe" ])
  n %v% "born" = as.numeric(substr(s[ network.vertex.names(n), "annee_naissance" ], 1, 4)) # safer
  n %v% "constituency" = as.character(s[ network.vertex.names(n), "constituency" ])
  n %v% "party" = as.character(s[ network.vertex.names(n), "party" ])
  n %v% "partyname" = as.character(groups[ s[ network.vertex.names(n), "party" ] ])
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  n %v% "photo" = as.character(s[ network.vertex.names(n), "photo" ])
  # mandate years done up to start year of legislature
  s$nyears = sapply(s$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(years[ as.character(k) ]))
  })
  n %v% "nyears" = as.numeric(s[ network.vertex.names(n), "nyears" ])
  
  # unweighted degree
  n %v% "degree" = degree(n)
  q = n %v% "degree"
  q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
  
  #
  # network plot
  #
  
  if(plot) {
        
    save_plot(n, file = paste0("plots/net_be_ch", k),
              i = colors[ s[ n %e% "source", "party" ] ],
              j = colors[ s[ n %e% "target", "party" ] ],
              q, colors, order)
    
  }
  
  #
  # save objects
  #
  
  assign(paste0("net_be_ch", k), n)
  assign(paste0("edges_be_ch", k), edges)
  assign(paste0("bills_be_ch", k), data)
  
  #
  # export gexf
  #
  
  if(gexf)
    save_gexf(paste0("net_be_ch", k), n, c(meta[1], "Chambre"), mode, colors, extra = "constituency")
  
}

save(list = ls(pattern = "^(net|edges|bills)_be_ch\\d{2}$"), file = "data/net_be_ch.rda")

if(gexf)
  zip("net_be_ch.zip", files = dir(pattern = "^net_be_ch\\d{2}\\.gexf$"))

# kthxbye
