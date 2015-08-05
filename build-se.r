bills = subset(bills, type == "PROPOSITION DE LOI")

for (k in 53:49) { # too few bills in S. 54
  
  cat("Sénat, législature", k,
      years[ as.character(k) ], "to",
      years[ as.character(k + 1) ])
  
  data = subset(bills, legislature == k & n_au > 1)
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  #
  # directed edge list
  #
  
  edges = lapply(data$authors, function(d) {
    
    w = unlist(strsplit(d, ";")) %>% as.integer

    # remove missing sponsors; if first author is missing, first nonmissing
    # cosponsor is used in replacement (happens only in very few cases)
    w = w[ w %in% b$sid ]

    if (length(w) > 0) {
      
      e = expand.grid(i = b$nom[ b$sid %in% w ],
                      j = b$nom[ b$sid == w[1]], stringsAsFactors = FALSE)
      
      return(data.frame(e, w = length(w) - 1)) # number of cosponsors

    } else {
      
      return(NULL)
      
    }
    
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
  
  n %n% "country" = meta[ "cty" ]
  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = paste0(years[ as.character(k) ], "-", years[ as.character(k + 1) ])
  n %n% "legislature" = as.character(k)
  n %n% "chamber" = meta[ "se" ] %>% as.character
  n %n% "type" = meta[ "type-se" ] %>% as.character
  n %n% "ipu" = meta[ "ipu-se" ] %>% as.integer
  n %n% "seats" = meta[ "seats-se" ] %>% as.integer

  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(subset(bills, legislature == k)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(b) = b$nom
  
  n %v% "url" = b[ network.vertex.names(n), "url" ]
  n %v% "sex" = b[ network.vertex.names(n), "sex" ]
  n %v% "born" = b[ network.vertex.names(n), "born" ]
  n %v% "party" = b[ network.vertex.names(n), "parti" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "photo" = b[ network.vertex.names(n), "photo" ]
  # mandate years done up to start year of legislature
  b$nyears = sapply(b$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(years[ as.character(k) ]))
  })
  n %v% "nyears" = b[ network.vertex.names(n), "nyears" ] %>% as.integer
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
  #
  # network plot
  #
  
  if (plot) {
    
    save_plot(n, paste0("plots/net_be_se", years[ as.character(k) ], "-", years[ as.character(k + 1) ]),
              i = colors[ b[ n %e% "source", "parti" ] ],
              j = colors[ b[ n %e% "target", "parti" ] ],
              mode, colors)
    
  }
  
  #
  # save objects
  #
  
  assign(paste0("net_be_se", years[ as.character(k) ]), n)
  assign(paste0("edges_be_se", years[ as.character(k) ]), edges)
  assign(paste0("bills_be_se", years[ as.character(k) ]), data)
  
  #
  # export gexf
  #
  
  if (gexf)
    save_gexf(n, paste0("net_be_se", years[ as.character(k) ], "-", years[ as.character(k + 1) ]),
              mode, colors)
  
}

if (gexf)
  zip("net_be_se.zip", files = dir(pattern = "^net_be_se\\d{4}-\\d{4}\\.gexf$"))

# kthxbye
