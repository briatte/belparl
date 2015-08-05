for (k in 54:48) { # excluding C. 47
  
  cat("Chambre, législature", k,
      years[ as.character(k) ], "to",
      years[ as.character(k + 1) ])
  
  data = subset(b, legislature == k & type == "PROPOSITION DE LOI" & n_a > 1)
  
  # special case of party coalition [ 52 ]
  if (k == 52) {
    data$sponsors = gsub("\\[ VOLKS \\]", "[ CDEM-V/VOLKS ]", data$sponsors)
    data$sponsors = gsub("\\[ CDEM-V \\]", "[ CDEM-V/VOLKS ]", data$sponsors)
    data$sponsors = gsub("Luc Sevenhans \\[ CDEM-V/VOLKS \\]", "Luc Sevenhans [ VLAAMS ]", data$sponsors)
  }
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  #
  # directed edge list
  #
  
  edges = lapply(data$sponsors, function(d) {
    
    w = unlist(strsplit(d, ";"))
    
    d = expand.grid(i = w, j = w[ 1 ], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }) %>% bind_rows
  
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
  edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w)
  
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

  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = paste0(years[ as.character(k) ], "-", years[ as.character(k + 1) ])
  n %n% "legislature" = as.character(k)
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type-ch" ] %>% as.character
  n %n% "ipu" = meta[ "ipu-ch" ] %>% as.integer
  n %n% "seats" = meta[ "seats-ch" ] %>% as.integer
  
  n %n% "n_cosponsored" = nrow(data)
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
  s = merge(data_frame(nom = network.vertex.names(n), party = n%v% "party"),
            unique(deputes[ deputes$legislature == k, ]))

  #
  # remove a small number of unidentified sponsors
  #

  if (network.size(n) != nrow(s)) {

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
  
  n %v% "url" = s[ network.vertex.names(n), "url" ]
  n %v% "sex" = s[ network.vertex.names(n), "sexe" ]
  n %v% "born" = s[ network.vertex.names(n), "annee_naissance" ]
  n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  # mandate years done up to start year of legislature
  s$nyears = sapply(s$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(years[ as.character(k) ]))
  })
  n %v% "nyears" = s[ network.vertex.names(n), "nyears" ] %>% as.integer
  
  #
  # network plot
  #
  
  if (plot) {
        
    save_plot(n, paste0("plots/net_be_ch", years[ as.character(k) ], "-", years[ as.character(k + 1) ]),
              i = colors[ s[ n %e% "source", "party" ] ],
              j = colors[ s[ n %e% "target", "party" ] ],
              mode, colors)
    
  }
  
  #
  # save objects
  #
  
  assign(paste0("net_be_ch", years[ as.character(k) ]), n)
  assign(paste0("edges_be_ch", years[ as.character(k) ]), edges)
  assign(paste0("bills_be_ch", years[ as.character(k) ]), data)
  
  #
  # export gexf
  #
  
  if (gexf)
    save_gexf(n, paste0("net_be_ch", years[ as.character(k) ], "-", years[ as.character(k + 1) ]),
              mode, colors)
  
}

if (gexf)
  zip("net_be_ch.zip", files = dir(pattern = "^net_be_ch\\d{4}-\\d{4}\\.gexf$"))

# kthxbye
