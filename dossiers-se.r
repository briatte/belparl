# scraper

if(!file.exists("dossiers-se.log")) {
  
  root = "http://www.senate.be"
  cat("Scraping raw upper chamber data (be patient, takes ~ 13 hours)...\n")

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
        
        cat("Parsing", i, "... ")
        
        hh = htmlParse(paste0(root, i))
        hh = xpathSApply(hh, "//a[contains(@href, 'NR=')]/@href")
        
        cat(length(hh), "records\n")
        r = data.frame()
        
        for(j in hh) {
          
          t = htmlParse(paste0(root j))
          u = xpathSApply(t, "//a[contains(@href, 'showSenator')]/@href")
          
          if(length(u))
            r = rbind(r, data.frame(legislature = 48 + as.numeric(gsub("(.*)LEG=(\\d+)(.*)", "\\2", j)),
                                    dossier = sprintf("%04.0f", as.numeric(gsub("(.*)NR=(\\d+)(.*)", "\\2", j))),
                                    sid = gsub("(.*)ID=(\\d+)(.*)", "\\2", u),
                                    name = xpathSApply(t, "//a[contains(@href, 'showSenator')]", xmlValue),
                                    stringsAsFactors = FALSE))
          
        }
        
        d = rbind(r, d)
        
      }
      
      write.csv(d, file, row.names = FALSE)
      
      cat("Scraped", nrow(d), "rows over",
          length(unique(d$dossier)), "dossiers in",
          round(as.numeric(Sys.time() - time), 1), "minutes\n\n")
      
    }
    
  }
  
  cat("\nEnded:", as.character(Sys.time()), "\n")
  sink()

}

# job's done
