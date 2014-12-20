#
# scrape full MP listing
#

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
                      nom = str_clean(xpathSApply(hh, "//div[@id='story']/*/h2", xmlValue)),
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
  
  if(!file.exists(photo))
    try(download.file(paste0("http://www.lachambre.be", i), photo, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(!file.info(photo)$size) {
  
    file.remove(photo) # will warn if missing
    deputes$photo[ deputes$photo == i ] = NA
  
  } else {
    
    deputes$photo[ deputes$photo == i ] = gsub("photos_ch/|.gif$", "", photo)
  
  }

}

deputes$bio = str_clean(gsub("▀ ", "", deputes$bio))

deputes$sexe = str_extract(deputes$bio, "Député(e)?")
deputes$sexe[ deputes$nom == "Juliette Boulet" ] = "Députée" # Flemish text
deputes$sexe[ deputes$nom == "Paul Meeus" ] = "Député" # typo
deputes$sexe[ deputes$sexe == "Députée" ] = "F"
deputes$sexe[ deputes$sexe == "Député" ] = "M"

# mandate years (rough approximation)
#deputes$from = as.numeric(sapply(str_extract_all(deputes$mandate, "[0-9]{4}"), min))
#deputes$to = as.numeric(sapply(str_extract_all(deputes$mandate, "[0-9]{4}"), max))
#deputes$nyears = deputes$to - deputes$from + 1

# mandate years (detailed)
deputes$mandate = sapply(deputes$mandate, function(x) {
  x = as.numeric(unlist(str_extract_all(x, "[0-9]{4}")))
  paste0(seq(min(x), max(x)), collapse = ";")
})

deputes$annee_naissance = str_extract(deputes$bio,
                                      "(N|n)é(e)? (a|à|te) (\\w|,|\\.|'|-|\\(|\\)|\\s)+ le \\d+(er)? \\w+ \\d{4}")
deputes$annee_naissance = as.numeric(str_extract(deputes$annee_naissance, "[0-9]{4}"))
deputes$annee_naissance[ deputes$nom == "Juliette Boulet" ] = 1981 # Flemish text
deputes$annee_naissance[ deputes$nom == "Magda Raemaekers" ] = 1947 # Wikipedia additions
deputes$annee_naissance[ deputes$nom == "Frédéric Daerden" ] = 1970
deputes$annee_naissance[ deputes$nom == "Georges Dallemagne" ] = 1958
deputes$annee_naissance[ deputes$nom == "Frank Wilrycx" ] = 1965

deputes$url = gsub("(.*)key=(.*)", "\\2", deputes$url)
deputes$url = gsub("(\\d+)&(.*)", "\\1", deputes$url)

#
# scrape dossiers in legislatures 47-53
#

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
            t$variable = str_clean(t$variable)
            t$value = str_clean(t$value)
            
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

# parse sponsors from raw dossiers

dir = dir("data", pattern = "dossiers-ch\\d{2}.csv", full.names = TRUE)
for(k in dir) {
  
  file = gsub("dossiers", "sponsors", k)
  
  if(!file.exists(file)) {
    
    cat("\nParsing Chamber, legislature", gsub("\\D", "", k), "...\n")
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
  
}

b = rbind_all(lapply(dir("data", pattern = "sponsors-ch", full.names = TRUE),
                     read.csv, stringsAsFactors = FALSE))

b$legislature = substr(b$uid, 1, 2)

b$n_a = b$n_au # total
b$n_au = 1 + str_count(b$authors, ";")
b$n_au[ is.na(b$authors) ] = NA

b$n_co = 1 + str_count(b$cosponsors, ";")
b$n_co[ b$cosponsors == "" ] = 0
b$n_co[ is.na(b$cosponsors) ] = 0

b = subset(b, authors != "") # removes only one row -- dossier 52K1939 [ 52 ]

b$type[ grepl("AMENDEMENT|DECISION DE NE PAS AMENDER", b$type) ] = "AMENDEMENT"
b$type[ grepl("ADDENDUM|AVIS|BUDGET|CORRIGE|CONCLUSIONS|DECISION|ERRATA|PROJET|RAPPORT|ANNEXE|TEXTE", b$type) ] = "AUTRE"
b$type[ grepl("PROPOSITION DE LOI", b$type) ] = "PROPOSITION DE LOI"
b$type[ grepl("PROPOSITION", b$type) & !grepl("PROPOSITION DE LOI", b$type) ] = "PROPOSITION"

# print(table(b$type, b$legislature, exclude = NULL))

b$status[ grepl("ADOPTE|TERMINE", b$status) ] = "ADOPTE"
b$status[ grepl("CADUQUE|REJETE", b$status) ] = "REJETE"
b$status[ grepl("SANS OBJET|RETIRE", b$status) ] = NA

# print(table(b$type, b$status, exclude = NULL))

# join authors and cosponsors

b$sponsors = paste(b$authors, b$cosponsors, sep = ";")
b$sponsors = gsub(";(NA)?$", "", b$sponsors)

#
# name and party fixes
#

# Sabien (Lahaye-)Battheu [ 51-54 ]
b$sponsors = gsub("Sabien Battheu", "Sabien Lahaye-Battheu", b$sponsors)
b$sponsors = gsub("Karine Jiroflée", "Karin Jiroflée", b$sponsors)

# Emile Coulonvaux, PLP [ 47 ]
b$sponsors = gsub("\\[ PLP \\]", "[ PVV ]", b$sponsors)

# Jan Decorte INDEP/ROSSEM [ 48 ]
b$sponsors = gsub("Jan Decorte \\[ INDEP \\]", "Jan Decorte [ ROSSEM ]", b$sponsors)

# Arthur Honoré Buysse, libéral flamand [ 49-50 ]
b$sponsors = gsub("\\[ LIB \\]", "[ PVV ]", b$sponsors)

# party code fixes [53]
b$sponsors = gsub("Els Van Hoof \\[ 0 \\]", "Els Van Hoof [ CD&V - N-VA ]", b$sponsors)
b$sponsors = gsub("Stefaan De Clercq \\[ 0 \\]", "Stefaan De Clercq [ CD&V ]", b$sponsors)
b$sponsors = gsub("Myriam Delacroix-Rolin \\[ 0 \\]", "Myriam Delacroix-Rolin [ cdH ]", b$sponsors)
b$sponsors = gsub("Fatma Pehlivan \\[ 0 \\]", "Fatma Pehlivan [ sp.a ]", b$sponsors)

#
# party simplifications
#

b$sponsors = gsub("\\[ Groen \\]", "[ ECOLO ]", b$sponsors)
b$sponsors = gsub("\\[ (Agalev-)?(ECOLO|Ecolo)(-Groen)?(!)? \\]", "[ ECOLO ]", b$sponsors)
# b$sponsors = gsub("\\[ (PS|SP|sp.a)(-spirit)?(\\+Vl\\.Pro)? \\]", "[ SOC ]", b$sponsors)
b$sponsors = gsub("\\[ PS \\]", "[ SOC-F ]", b$sponsors)
b$sponsors = gsub("\\[ (SP|sp.a)(-spirit)?(\\+Vl\\.Pro)? \\]", "[ SOC-V ]", b$sponsors)
b$sponsors = gsub("\\[ (PVV|Open Vld|VLD) \\]", "[ LIB-V ]", b$sponsors)
b$sponsors = gsub("\\[ (MR|FDF|PRL|FDFPPW|PRLFDF) \\]", "[ LIB-F ]", b$sponsors)
b$sponsors = gsub("\\[ (VU|VU-ID|N-VA) \\]", "[ VOLKS ]", b$sponsors)
b$sponsors = gsub("\\[ (CVP|CD&V) \\]", "[ CDEM-V ]", b$sponsors)
b$sponsors = gsub("\\[ (PSC|cdH) \\]", "[ CDEM-F ]", b$sponsors)
b$sponsors = gsub("\\[ CD&V - N-VA \\]", "[ CDEM-V/VOLKS ]", b$sponsors)
b$sponsors = gsub("\\[ VB \\]", "[ VLAAMS ]", b$sponsors)
b$sponsors = gsub("\\[ (ONAFH|INDEP) \\]", "[ IND ]", b$sponsors)

# last fixes on duplicates (leave at end)
b$sponsors = gsub("Lisette Nelis-Van Liedekerke \\[ LIB-V \\]",
                  "Lisette Nelis-Van Liedekerke [ CDEM-V ]", b$sponsors) # l. 48
b$sponsors = gsub("Philippe Dallons \\[ SOC-F \\]", "Philippe Dallons [ ECOLO ]", b$sponsors) # l. 49
b$sponsors = gsub("Pierrette Cahay-André \\[ LIB-F \\]", "Pierrette Cahay-André [ CDEM-F ]", b$sponsors) # l. 49
b$sponsors = gsub("Richard Fournaux \\[ CDEM-F \\]", "Richard Fournaux [ LIB-F ]", b$sponsors) # l. 51
b$sponsors = gsub("Vincent Decroly \\[ IND \\]", "Vincent Decroly [ ECOLO ]", b$sponsors) # l. 50

# note: fixes do not cover sponsors from legislature 47,
# for which there is no sponsor details (age, sex, etc.)

## all keywords
# kw = unlist(strsplit(b$topic, ","))
# table(kw)[ table(kw) > 600 ]

write.csv(b, "data/bills-ch.csv", row.names = FALSE)
