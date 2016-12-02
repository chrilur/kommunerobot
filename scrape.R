library(RCurl)
library(XML)
##setwd("C:\\Users\\n633164\\Documents\\R\\ssbkom\\kommunerobot")

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
  folketall <- skilletegn(folketall)
  #Fødte og døde
  fodte <- as.character(kommunetall[2])
  fodte.hittil.i.aar <- unlist(strsplit(fodte, "pers"))[1]
  per <- unlist(strsplit(fodte, "\\["))[2]
  per <- sub("\\]", "", per)
  fodte.hittil.i.aar <- skilletegn(fodte.hittil.i.aar)
  dode.hittil.i.aar <- as.character(kommunetall[3])
  dode.hittil.i.aar <- unlist(strsplit(dode.hittil.i.aar, "pers"))[1]
  dode.hittil.i.aar <- skilletegn(dode.hittil.i.aar)
  #Størrelse
  areal <- as.character(kommunetall[9])
  areal <- unlist(strsplit(areal, "km2"))[1]
  areal <- skilletegn(areal)
  #Innvandrere
  innv <- as.character(kommunetall[7])
  innv <- unlist(strsplit(innv, "pers"))[1]
  innv <- skilletegn(innv)
  #Lånegjeld
  laan <- as.character(kommunetall[22])
  laan <- unlist(strsplit(laan, "NOK"))[1]
  laan <- skilletegn(laan)
  
  #Utskrift
  cat(paste0("Fakta om ", navn, "\n"))
  cat(paste0(navn, " har ", folketall, " innbyggere.\n")) 
  cat(paste0("Så langt i år, ", per, ", er det født ", fodte.hittil.i.aar, " barn i kommunen, mens ",
                      dode.hittil.i.aar, " mennesker døde.","\n"))
  cat(paste0("Størrelse: ", areal, " kvadratkilometer.", "\n"))
  cat(paste0("I ", navn, " bor det ", innv, " personer med innvandrerbakgrunn.", "\n"))
  cat(paste0("Kommunens lånegjeld er ", laan, " kroner per innbygger.", "\n\n"))
  cat("Kilde: SSB")
  
  
}

#Funksjon som fikser tusenskilletegn i tall
skilletegn <- function(x){
  ifelse(nchar(x) > 5, sub(" ", ".", x), sub (" ","", x))
}