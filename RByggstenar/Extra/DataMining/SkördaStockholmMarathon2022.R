# OBS! Kör inte detta script. Om många kör det samtidigt kan det överbelasta
# Stockholm marathons hemsida.
# Den skördade datan finns redan i mappen DataFiler, i de två filerna:
# SthmMara22Damer.csv och SthlmMara22Herrar.csv
# Jag har enbart gjort det tillgänglig som ett exempel för den som vill
# skörda data från någon annan websida (kanske något annat år för Stholm marathon)
# Räkna dock med att det kan komma att krävas att du studerar och förstår
# vad de olika funktionerna i scriptet gör och att du får 
# experimentera en del för att få det att fungera.
# Om det är mycket data du vill läsa in är det klokt och vänligt mot den hemsida 
# du läser från att först experimentera på en liten del, för att se att det 
# fungerar, innan du läser in hela materialet.

# Här läser jag resultaten från stockholm marathon 2022
# och gör två csv datfiler av dem (en för herrar och en för damer)

# Om du själv vill göra något liknande bör du vara försiktig med att inte 
# överbelasta websidor. Testa din kod noga på en nedladdad sida innan du 
# kör den på alla önskade sidor

#Packet för att samla data från websidor
# https://rvest.tidyverse.org/
library(rvest)

# Laddar hjälpfunktioner från filen kdText.R
source("Verktyg/kdText.R")

# URL till resultat för stopckholm marathon 2022
url <- "http://results.marathon.se/2022/?event=STHM"

# Läs HTML sidan 
html <- read_html(url)

# För att se HTML på en given websida i Chorme
# Högerklicka och välj "View Page Source"
# Där ser jag att länkarna till resultatsidor ligger i element med namnen a
# adresserna anges av attributet href
adresser <- html %>% 
  html_elements("a") %>% 
  html_attr("href")

#HERRAR

# för att bara ta ut länkar till männens resultat
# väljer jag adresser med texten "=M"
mAdresser <- grep("=M",adresser, value=TRUE)
# Tar bort länkar till sidor med åldersindelning (som innehåller texten age_class)
mAdresser <- grep("age_class",mAdresser, value=TRUE, invert=TRUE)

# Skapa tom Data Frame med kolumnamn
cNamn <- c("Tid", "Ålder")
menMaraResult <- data.frame(matrix(nrow = 0, ncol = length(cNamn))) 
colnames(menMaraResult) <- cNamn

# Läs data från dessa sidor
# Referenserna ersätter ? och allt efter från url (http://results.marathon.se/2022/?event=STHM)

for(pNr in 1:length(mAdresser)){
  #Print iterationssteg
  cat(pNr,":",length(mAdresser),"\n")
  
  # url till sida
  sidUrl <- paste("http://results.marathon.se/2022/", mAdresser[pNr], sep="")
  
  # Läs rader från sida och extrahera texten
   rader <- read_html(sidUrl) %>% 
    html_elements("li")  %>% 
     html_text2()
   
   # Välj rader med ordet Finish i (det är resultatrader)
   rader <- grep("Finish",rader, value=TRUE)
   
  # rad 1 är headings
  # Så här ser en typisk rad ut
  # "1\nKirwa, Felix (KEN)\nNumber\n8\nTeam/Country\nKenya\nYOB\n95\nFinish\n02:11:08"
  for(nr in 2:length(rader)){
    values <- unlist(strsplit(rader[nr],"\n"))
    # Sluttid är index 10 och födelselär index 8
    tid <- kdTidTextTillNumSekunder(values[10])
    år <- as.numeric(values[8])
    ålder <- if(år>22) 122-år else 22-år
    menMaraResult[nrow(menMaraResult)+1,] <- c(tid,ålder)
  }
    
}

write.csv2(menMaraResult, "DataFiler/SthlmMara22Herrar.csv", row.names = FALSE)

#DAMER

# för att bara ta ut länkar till kvinnornas resultat
# väljer jag adresser med texten "=F"
fAdresser <- grep("=W",adresser, value=TRUE)
# Tar bort länkar till sidor med åldersindelning (som innehåller texten age_class)
fAdresser <- grep("age_class",fAdresser, value=TRUE, invert=TRUE)

# Skapa tom Data Frame med kolumnamn
cNamn <- c("Tid", "Ålder")
womenMaraResult <- data.frame(matrix(nrow = 0, ncol = length(cNamn))) 
colnames(womenMaraResult) <- cNamn

# Läs data från dessa sidor
# Referenserna ersätter ? och allt efter från url (http://results.marathon.se/2022/?event=STHM)

for(pNr in 1:3){
  #Print iterationssteg
  cat(pNr,":",length(fAdresser),"\n")
  
  # url till sida
  sidUrl <- paste("http://results.marathon.se/2022/", fAdresser[pNr], sep="")
  
  # Läs rader från sida och extrahera texten
  rader <- read_html(sidUrl) %>% 
    html_elements("li")  %>% 
    html_text2()
  
  # Välj rader med ordet Finish i (det är resultatrader)
  rader <- grep("Finish",rader, value=TRUE)
  
  # rad 1 är headings
  # Så här ser en typisk rad ut
  # "1\nKirwa, Felix (KEN)\nNumber\n8\nTeam/Country\nKenya\nYOB\n95\nFinish\n02:11:08"
  for(nr in 2:length(rader)){
    values <- unlist(strsplit(rader[nr],"\n"))
    # Sluttid är index 10 och födelselär index 8
    tid <- kdTidTextTillNumSekunder(values[10])
    år <- as.numeric(values[8])
    if(is.na(år)){
      ålder <- 0  # Om födelseår saknas
    }
    else{
      ålder <- if(år>22) 122-år else 22-år
    }
    womenMaraResult[nrow(womenMaraResult)+1,] <- c(tid,ålder)
  }
  
}

write.csv2(womenMaraResult, "DataFiler/SthlmMara22Damer.csv", row.names = FALSE)



