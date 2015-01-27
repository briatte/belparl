# get committees for Chambre l. 54 (only one available)

load("data/net_be_ch.rda")

deputes = read.csv("data/deputes.csv", stringsAsFactors = FALSE)
deputes = subset(deputes, legislature == 54)
deputes$uid = str_extract(deputes$url, "key=\\w?\\d+")

h = htmlParse("http://www.lachambre.be/kvvcr/showpage.cfm?section=/none&language=fr&cfm=/site/wwwcfm/comm/LstCom.cfm")
comm = data.frame(
  y = 54,
  n = xpathSApply(h, "//a[contains(@href, '?com=')]", xmlValue),
  l = xpathSApply(h, "//a[contains(@href, '?com=')]/@href"),
  stringsAsFactors = FALSE
)

for(i in deputes$uid)
  comm[, i ] = 0

for(i in comm$l) {
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
