
deputes = data.frame()
for(i in 48:54) {
  
  cat("Parsing legislature", i, "... ")
  h = htmlParse(paste0("http://www.lachambre.be/kvvcr/showpage.cfm?section=/depute&language=fr&rightmenu=right_depute&cfm=cvlist54.cfm?legis=", i, "&today=n"))
  h = xpathSApply(h, "//a[contains(@href, 'key=')]/@href")
  cat(length(h), "pages\n")

  for(j in h) {
    
    hh = htmlParse(paste0("http://www.lachambre.be/kvvcr/", j))
    hh = data.frame(legislature = i,
                    nom = xpathSApply(hh, "//div[@id='story']/*/h2", xmlValue),
                    photo = xpathSApply(hh, "//img[contains(@src, 'cv/')]/@src"),
                    url = j,
                    bio = scrubber(xpathSApply(hh, "//div[@id='story']/table[2]/tr[@valign='top'][2]/td/p", xmlValue)))
    deputes = rbind(deputes, hh)
    cat(".")
    
  }
  
  cat("\n")
  
}
