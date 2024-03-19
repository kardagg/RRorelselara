# Här läser jag information om världsrekordet i stavhopp
# från två websidor i wikipedia
# Den skördade datan finns redan i filen: DataFiler/Simrekord100m.csv
# Detta är tänkt som ett hjälpande exempel för den som vill skörda data
# från någon annan tabell på någon websida
# Räkna dock med att det kan komma att krävas att du studerar och förstår
# vad de olika funktionerna i scriptet gör och att du får 
# experimentera en del för att få det att fungera

#Packet för att samla data från websidor
# https://rvest.tidyverse.org/
library(rvest)

# Laddar hjälpfunktioner från filen kdText.R
source("Verktyg/kdText.R")

#MÄN
# URL to wanted web page
url <- "https://en.wikipedia.org/wiki/Men%27s_pole_vault_world_record_progression"

# Read HTML page 
html <- read_html(url)
# För att se HTML på en given websida i Chorme
# Högerklicka och välj "View Page Source"

# Find all table elements on the pages
tables <- html %>% 
  html_nodes("table") %>% 
  html_table()

rekord <- tables[[2]]
print(colnames(men))

# Ta fram Höjd som numerisk från text
height <- as.numeric(unlist(lapply(unlist(lapply(lapply(lapply(rekord$Mark,strsplit, split="m"),
                               unlist),head,n=1)),substr,start=1,stop=4)))


# Ta fram årtal från text
year <- as.numeric(unlist(lapply(unlist(lapply(lapply(lapply(rekord$Date,strsplit, split=","),
                      unlist),tail,n=1)), substr, start=2, stop=5)))


# Create av Data Frame named män, med Tid, År och Kön (M=Male)
män <- data.frame(Höjd = unlist(height), År = unlist(year), Kön="M")

#Samma för KVINNOR

# URL to wanted web page
url <- "https://en.wikipedia.org/wiki/Women%27s_pole_vault_world_record_progression"

# Read HTML page 
html <- read_html(url)
# För att se HTML på en given websida i Chorme
# Högerklicka och välj "View Page Source"

# Find all table elements on the pages
tables <- html %>% 
  html_nodes("table") %>% 
  html_table()

rekord <- tables[[2]]
print(colnames(rekord))

# Ta fram Höjd som numerisk från text
height <- as.numeric(unlist(lapply(unlist(lapply(unlist(lapply(lapply(lapply(rekord$Mark,strsplit, split="m"),
                  unlist),head,n=1)),function(x) gsub("i ", "", x))),substr,start=1,stop=4)))

height
# Ta fram årtal från text
year <- as.numeric(unlist(lapply(unlist(lapply(lapply(lapply(rekord$Date,strsplit, split=" "),
                                                      unlist),tail,n=1)), substr, start=1, stop=4)))


# Create av Data Frame named män, med Tid, År och Kön (F=Female)
kvinnor <- data.frame(Höjd = unlist(height), År = unlist(year), Kön="F")


# SPARA CSV i en fil
# Slå ihop data frames män o kvinnor
stavResultat <- rbind(kvinnor,män)
write.csv2(stavResultat,"DataFiler/Stavrekord.csv", row.names = FALSE)

#PLOTTA
plot(män$År,män$Höjd,col="blue", xlab="År", ylab="Höjd (m)",pch = 19,
     xlim=c(min(stavResultat$År),max(stavResultat$År)),
     ylim=c(min(stavResultat$Höjd),max(stavResultat$Höjd)),
     main="Världsrekord i stavhopp")
points(kvinnor$År,kvinnor$Höjd,col="red",pch = 19)
grid (NULL,NULL) 
legend("topleft", c("Män", "Kvinnor"), pch = c(19, 19), col = c("blue", "red"))

# The introduction in the early 1950s of flexible vaulting poles made from 
# composites such as fiberglass or carbon fiber allowed vaulters to achieve 
# greater height.
