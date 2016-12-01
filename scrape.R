library(RCurl)
library(XML)
setwd("C:\\Users\\n633164\\Documents\\R\\ssbkom")

fakta <- function(x){
url <- paste0("http://www.ssb.no/kommunefakta/",x)
kommune <- getURL(url)
tabeller <- readHTMLTable(kommune)
  alder <- tabeller[1]
  folkevekst <- tabeller[2]
  utdanning <- tabeller[3]
  yrke <- tabeller[4]
  inntekt <- tabeller[5]
  driftsutgifter <- tabeller[6]
  
kommune.parse <- htmlParse(kommune)
navn <- as.character(lapply(kommune.parse['//li[@class="kommunenavn"]'],xmlValue))
  navn <- unlist(strsplit(navn," - "))[1]
kommunetall <- lapply(kommune.parse['//p[@class="tall"]'],xmlValue)
  folketall <- sub("innbyggere", "", kommunetall[1])
  #Skilletegn "." for kommuner > 10.000 innb. 
  folketall <- ifelse(nchar(folketall) > 5, sub(" ", ".", folketall), sub (" ","", folketall))
  fodte.hittil.i.aar <- sub("personer", " ", kommunetall[2])
  dode.hittil.i.aar <- sub("personer", " ", kommunetall[3])
  
  #Utskrift
  cat(paste0(navn, " har ", folketall, " innbyggere.\n"))
  cat(paste0("Så langt i år er det født ", fodte.hittil.i.aar, " i kommunen, mens ", dode.hittil.i.aar, " er døde."))
  }