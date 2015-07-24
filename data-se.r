# scrape bills

root = "http://www.senate.be"
cat("Scraping raw upper chamber data...\n")

for(k in 6:1) {
  
  file = paste0("data/dossiers-se", k + 48, ".csv")
  if(!file.exists(file)) {
    
    cat("Scraping legislature", k + 48, "... ")
    h = htmlParse("http://www.senate.be/www/?MIval=/Dossiers/NieuweDossiers&LANG=fr")
    
    h = xpathSApply(h, paste0("//a[contains(@href, 'LEG=", k, "&')]/@href"))
    cat(length(h), "pages\n\n")
    
    d = data.frame()
    for(i in h) {
      
      cat("Page", sprintf("%3.0f", which(i == h)), "scraping ")
      
      hh = htmlParse(paste0(root, i))
      hh = xpathSApply(hh, "//a[contains(@href, 'NR=')]/@href")
      
      cat(sprintf("%3.0f", length(hh)), "dossiers\n")
      r = data.frame()
      
      for(j in hh) {
        
        t = try(htmlParse(paste0(root, j)))
        if(!"try-error" %in% class(t)) {
          
          amd = which(readHTMLTable(t)[[3]]$Titre == "Amendements")
          
          if(length(amd)) {
            
            amd = xpathSApply(t, "//table[3]/tr/td[1]/a[1]/@href")[ amd ]
            amd = paste0(amd[ !grepl("lachambre", amd) ], collapse = ";")
            
          } else {
            
            amd = NA
            
          }
          
          type = readHTMLTable(t)[[3]]$Titre[1]
          if(!length(type)) type = NA
          
          status = as.character(readHTMLTable(t)[[5]][2, 2])
          if(!length(status)) status = NA
          
          title = str_clean(xpathSApply(t, "//table[1]/tr/td", xmlValue)[4])
          
          topic = gsub("(  )+( )?", ",", xpathSApply(t, "//table[2]/tr/td", xmlValue))
          topic = toupper(gsub(",$", "", topic))
          
          u = xpathSApply(t, "//a[contains(@href, 'showSenator')]/@href")
          
          if(length(u))
            r = rbind(r, data.frame(legislature = 48 + as.numeric(gsub("(.*)LEG=(\\d+)(.*)", "\\2", j)),
                                    dossier = sprintf("%04.0f", as.numeric(gsub("(.*)NR=(\\d+)(.*)", "\\2", j))),
                                    type, status, title, topic, 
                                    authors = paste0(paste(
                                      xpathSApply(t, "//a[contains(@href, 'showSenator')]", xmlValue),
                                      "[", gsub("(.*)ID=(\\d+)(.*)", "\\2", u), "]"),
                                      collapse = ";"),
                                    amendments = amd, stringsAsFactors = FALSE))
          
        }
        
      }
      
      d = rbind(r, d)
      
    }
    
    write.csv(d, file, row.names = FALSE)
    
    cat("Scraped", nrow(d), "rows over", n_distinct(d$dossier), "dossiers\n")
    
  }
  
}

# parse bills

dir = list.files("data", pattern = "dossiers-se\\d+\\.csv", full.names = TRUE)
for(k in dir) {
  
  file = gsub("dossiers", "sponsors", k)
  
  if(!file.exists(file)) {
    
    d = read.csv(k, stringsAsFactors = FALSE)
    cat("Building", file, ":", nrow(d), "dossiers\n")
    
    sp = data.frame()
    for(j in nrow(d):1) {
      
      dd = d[ j, ]
      r = unlist(strsplit(na.omit(dd$amendments), ";"))
      
      dd = data.frame(uid = paste0(dd$legislature, "K", sprintf("%04.0f", dd$dossier)),
                      dossier = paste0(dd$legislature, "K", sprintf("%04.0f", dd$dossier)),
                      document = NA,
                      type = dd$type,
                      topic = dd$topic,
                      status = dd$status,
                      n_au = 1 + str_count(dd$authors, ";"),
                      authors = paste0(unlist(str_extract_all(dd$authors, "\\d+|;")), collapse = ""),
                      stringsAsFactors = FALSE)
      
      if(length(r)) {
        
        amdts = c()
        for(i in r) {
          
          h = htmlParse(paste0(root, i))
          h = xpathSApply(h, "//frame[@name='pub_doc']/@src")
          h = htmlParse(paste0(root, h))
          
          h = sapply(xpathSApply(h, "//td"), function(x) {
            y = gsub("(.*)ID=(\\d+)(.*)", "\\2", xpathApply(x, "a/@href"))
            paste0(y, collapse = ";")
          })
          h = h[ h != "" ]
          amdts = c(amdts, h)
          
        }
        
        if(length(amdts)) {
          
          dd = rbind(dd, data.frame(uid = dd$uid,
                                    dossier = dd$dossier,
                                    document = NA,
                                    type = "AMENDEMENT",
                                    topic = dd$topic,
                                    status = NA,
                                    n_au = 1 + str_count(amdts, ";"),
                                    authors = amdts,
                                    stringsAsFactors = FALSE))
          
          dd$uid = paste0(dd$uid, "-", 1:nrow(dd))
          dd$document = 1:nrow(dd)
          sp = rbind(sp, dd)
          
        }
        
      } else {
        
        dd$uid = paste0(dd$uid, "-1")
        dd$document = 1
        sp = rbind(sp, dd)
        
      }
      
    }
    
    sp$type = toupper(sp$type)
    sp$status = toupper(sp$status)
    
    write.csv(sp, file, row.names = FALSE)
    
  }
  
  sp = read.csv(file, stringsAsFactors = FALSE)
  
}

bills = lapply(dir("data", pattern = "sponsors-se\\d{2}.csv", full.names = TRUE), read.csv,
               stringsAsFactors = FALSE)
bills = bind_rows(bills)

bills$legislature = substr(bills$uid, 1, 2)

# no need to rewrite that attribute
# table(bills$n_au == 1 + str_count(bills$authors, ";"), exclude = NULL)
# table(bills$authors == "" | is.na(bills$authors), exclude = NULL)
# bills$n_au[ is.na(bills$authors) ] = NA

bills$type[ grepl("AMENDEMENT", bills$type) ] = "AMENDEMENT"
bills$type[ grepl("AUTRES|NOTE|PROJET|RAPPORT|RÉVISION|COMMISSION SPÉCIALE|TEXTE", bills$type) ] = "AUTRE"
bills$type[ grepl("PROPOSITION DE LOI", bills$type) ] = "PROPOSITION DE LOI"
bills$type[ grepl("PROPOSITION", bills$type) & !grepl("PROPOSITION DE LOI", bills$type) ] = "PROPOSITION"

#print(table(bills$type, bills$legislature, exclude = NULL))

bills$status[ grepl("ADOPTÉ|PUBLIÉ|TERMINÉ|PROMULGUÉ", bills$status) ] = "ADOPTE"
bills$status[ grepl("CADUC|REJETÉ|NON PRIS", bills$status) ] = "REJETE"
bills$status[ grepl("SANS OBJET|RETIRÉ|COMMUNIQUÉ|A L'EXAMEN", bills$status) ] = NA

#print(table(bills$type, bills$status, exclude = NULL))

# full list of sponsors
a = sort(unique(unlist(strsplit(bills$authors, ";"))))

# ignore sponsors with empty pages
a = a[ !a %in% c("4078", "43496", "4624", "4595", "4388", "4346",
                 "4297", "4289", "4193", "4118", "397", "3968",
                 "3952", "3940", "351", "250") ]

if(!file.exists("data/senateurs.csv")) {
  
  # scrape only sponsors, not full list of identified senators (link below, i = 1:5)
  # htmlParse(paste0(root, "/www/?MIval=/WieIsWie/LijstDerSenatoren&LEG=", i, "&LANG=fr"))
  
  b = data.frame()

  for(x in rev(a)) {
    
    cat(sprintf("%3.0f", which(a == x)), "Scraping senator", str_pad(x, 4, "right"))
    
    f = paste0("raw/senateur-", x, ".html")
    if(!file.exists(f))
      download.file(paste0("http://www.senate.be/www/?MIval=/showSenator&ID=", x, "&LANG=fr"),
                    f, mode = "wb", quiet = TRUE)
      
    h = htmlParse(f, encoding = "UTF-8")
    sex = na.omit(str_extract(xpathSApply(h, "//table/tr", xmlValue), "Sénateur|Sénatrice"))
    man = xpathSApply(h, "//table/tr", xmlValue)
    man = man[ (which(man == "Travail parlementaire") + 1):length(man) ]
    man = unique(unlist(str_extract_all(man[ grepl("gislature", man) ], "[0-9]{4}-[0-9]{4}")))
    if(is.null(man)) man = "-"
    
    h = data.frame(sid = as.character(x),
                   nom = xpathSApply(h, "//title", xmlValue, encoding = "iso-8859-1"),
                   sex = ifelse(length(sex), sex, NA),
                   from = min(as.numeric(unlist(strsplit(man, "-"))), na.rm = TRUE),
                   to = max(as.numeric(unlist(strsplit(man, "-"))), na.rm = TRUE),
                   mandate = paste0(sort(man), collapse = ";"),
                   parti = str_clean(xpathSApply(h, "//table/tr[1]", xmlValue)),
                   stringsAsFactors = FALSE)
    
    h$from[ is.infinite(h$from) ] = NA
    h$to[ is.infinite(h$to) ] = NA
    h$nyears = h$to - h$from + 1
    h$parti = gsub("(.*)\\s-\\s(.*)", "\\2", h$parti)
    
    cat(":", h$nom, h$parti, h$from, "-", h$to, "\n")
    b = rbind(b, h)
    
  }
  
  write.csv(b, "data/senateurs.csv", row.names = FALSE)
  
}

b = read.csv("data/senateurs.csv", stringsAsFactors = FALSE)
b$sex = NULL

# senators data identify all sponsors
stopifnot(as.integer(a) %in% b$sid)

b$parti[ b$nom == "Lode Vereeck" ] = "Open Vld" =  # duplicated row
b$parti[ b$nom == "Anne Lambelin" ] = "PS" # duplicated row
b$parti[ grepl("Correspondance", b$parti, useBytes = TRUE) ] = "CD&V" # Pieter de Crem bugfix
b$parti[ grepl("Bureau", b$parti, useBytes = TRUE) ] = "PS"     # Patrick Moriau bugfix
b$parti[ grepl("^Priv", b$parti, useBytes = TRUE) ] = "sp.a"    # Inga Verhaert bugfix
b$parti[ b$nom == "Herman De Croo" ] = "VLD"   # since 2009 (?)
b$parti[ b$nom == "Michiel Aerbeydt" ] = "PSC" # Parti Catholique
b$parti[ grepl("^Ind", b$parti, useBytes = TRUE) ] = "IND"

# match lower chamber
b$parti[ b$parti %in% c("Vlaams Belang", "VL. BLOK") ] = "VLAAMS"
b$parti[ b$parti %in% c("Agalev", "Agalev-Ecolo", "Groen", "Groen!", "Ecolo-Groen", "Ecolo") ] = "ECOLO"
b$parti[ b$parti == "PS" ] = "SOC-F"
b$parti[ b$parti %in% c("SP", "SP.A-SPIRIT", "sp.a") ] = "SOC-V"
b$parti[ b$parti %in% c("PRL", "PRL-FDF", "MR") ] = "LIB-F"
b$parti[ b$parti %in% c("PVV", "Open Vld", "VLD") ] = "LIB-V"
b$parti[ b$parti %in% c("PSC", "cdH") ] = "CDEM-F"
b$parti[ b$parti %in% c("CVP", "CD&V") ] = "CDEM-V"
b$parti[ b$parti %in% c("VU", "VU-ID", "N-VA") ] = "VOLKS"

table(b$parti, exclude = NULL)

# fix names
b$nom = gsub("Ã¨", "è", b$nom)
b$nom = gsub("Ã©", "é", b$nom)
b$nom = gsub("Ãª", "ê", b$nom)
b$nom[ b$nom == "Hermes Sanctorum - Vandevoorde" ] = "Hermes Sanctorum-Vandevoorde"

# remove duplicates
b = unique(b)
stopifnot(!duplicated(b$sid))

# fill with lots of manually imputed sex codes
s = read.csv("data/senateurs-details.csv", stringsAsFactors = FALSE)
b = left_join(b, s, by = "sid")

# recode mandate length
b$mandate = sapply(b$mandate, function(x) {
  x = as.numeric(unlist(str_extract_all(x, "[0-9]{4}")))
  if(!length(x))
    NA
  else
    paste0(seq(min(x), max(x)), collapse = ";")
})
# hist(1 + str_count(b$mandate, ","))

# check for duplicates
rownames(b) = b$nom

# download photos
b$photo = b$sid
for(i in b$photo) {
  photo = paste0("photos_se/", i, ".gif")
  if(!file.exists(photo))
    try(download.file(paste0("http://www.senate.be/www/?MItabObj=persoon&MIcolObj=foto&MInamObj=persoonid&MIvalObj=",
                             i, "&MItypeObj=image/gif"), photo, mode = "wb", quiet = TRUE), silent = TRUE)
  if(!file.info(photo)$size) {
    file.remove(photo) # will warn if missing
    b$photo[ b$photo == i ] = NA
  } else {
    b$photo[ b$photo == i ] = gsub("photos_se/|.gif$", "", photo)
  }
}

## all keywords
# kw = unlist(strsplit(bills$topic, ","))
# table(kw)[ table(kw) > 300 ]

write.csv(bills, "data/bills-se.csv", row.names = FALSE)
