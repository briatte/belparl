# get committees for Chambre l. 54 (only one available)

deputes = subset(deputes, legislature == 54)
deputes$uid = str_extract(deputes$url, "key=\\w?\\d+")

f = "raw/comm-ch-54.html"
if (!file.exists(f))
  download.file("http://www.lachambre.be/kvvcr/showpage.cfm?section=/none&language=fr&cfm=/site/wwwcfm/comm/LstCom.cfm", f, mode = "wb", quiet = TRUE)

h = htmlParse(f)

comm = data.frame(
  y = 54,
  n = xpathSApply(h, "//a[contains(@href, '?com=')]", xmlValue),
  l = xpathSApply(h, "//a[contains(@href, '?com=')]/@href"),
  stringsAsFactors = FALSE
)

for (i in deputes$uid)
  comm[, i ] = 0

for (i in comm$l) {
  cat(comm$n[ comm$l == i ])
  h = htmlParse(paste0("http://www.lachambre.be/kvvcr/", i))
  k = xpathSApply(h, "//a[contains(@href, '.cfm?key')]/@href")
  k = str_extract(k, "key=\\w?\\d+")
  cat(":", length(k), "members\n")
  comm[ comm$l == i, names(comm) %in% k ] = 1
}

# save flat list

names(comm)[1:3] = c("legislature", "committee", "url")
write.csv(cbind(comm[, 1:3 ], members = rowSums(comm[, -1:-3 ])), 
          "data/committees.csv", row.names = FALSE)

# colnames(comm) = gsub("key=", "", colnames(comm))

# assign co-memberships to networks
for (i in 54) {
  
  n = get(paste0("net_be_ch", years[ as.character(i) ]))
  
  sp = network.vertex.names(n)
  names(sp) = str_extract(n %v% "url", "key=\\w?\\d+")
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ comm$legislature == i, names(sp) ]
  
  cat(i, ":", network.size(n), "nodes", nrow(m), "committees", ncol(m), "MPs")
  
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  set.edge.attribute(nn, "committee", e$committee)
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))
  
  set.edge.attribute(n, "committee", e$committee)
  assign(paste0("net_be_ch", years[ as.character(i) ]), n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("conet_be_ch", years[ as.character(i) ]), nn)
  
}
