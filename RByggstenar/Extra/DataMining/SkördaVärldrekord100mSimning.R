# Här läser jag information om världsrekordet i 100m frisim (långbana)
# från en websida i wikipedia
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

# URL to wanted web page
url <- "https://en.wikipedia.org/wiki/World_record_progression_100_metres_freestyle"

# Read HTML page 
html <- read_html(url)
# För att se HTML på en given websida i Chorme
# Högerklicka och välj "View Page Source"

# Find all table elements on the pages
tables <- html %>% 
  html_nodes("table") %>% 
  html_table()

#MEN

# I look on the web page to see which of the tables is which
# I will only use long course data
# Men long course is the first
men <- tables[[1]]
## tables[1] gives a list with the first table as the only item
## tables[[1]] gives the first table ([[i]] gives item i)

# Look at the names of the columns
print(colnames(men))

# Transform Time to numeric values
tid <-list()
for(str in men$Time){ 
  # transform text hour:minute:second to numerical seconds
  t <- kdTidTextTillNumSekunder(str)
  tid <- append(tid,t)
  
}

# Transform Date to numerical year
år <- list()
for(str in men$Date){
  y <- unlist(strsplit(str," "))
  y <- as.numeric(y[[length(y)]])
  år <- append(år,y)
}

# Create av Data Frame named män, med Tid, År och Kön (M=Male)
män <- data.frame(Tid = unlist(tid), År = unlist(år), Kön="M")

#Same for WOMEN

# Women long course is the first
women <- tables[[3]]

# Transform Time to numeric values
tid <-list()
for(str in women$Time){ 
  # transform text hour:minute:second to numerical seconds
  t <- kdTidTextTillNumSekunder(str)
  tid <- append(tid,t)
  
}

# Transform Date to numerical year
år <- list()
for(str in women$Date){
  y <- unlist(strsplit(str," "))
  y <- as.numeric(y[[length(y)]])
  år <- append(år,y)
}

# Create av Data Frame named kvinnor, med Tid, År och Kön (F=Female)
kvinnor <- data.frame(Tid = unlist(tid), År = unlist(år), Kön="F")

# SPARA CSV i en fil
# Slå ihop data frames män o kvinnor
simResultat <- rbind(kvinnor,män)
write.csv2(simResultat,"DataFiler/Simrekord100m.csv", row.names = FALSE)

#PLOTTA
plot(män$År,males$Tid,col="blue", xlab="År", ylab="Tid (s)",pch = 19,
     xlim=c(min(simResultat$År),max(simResultat$År)),
     ylim=c(min(simResultat$Tid),max(simResultat$Tid)),
     main="Världsrekord frisim 100m")
points(kvinnor$År,females$Tid,col="red",pch = 19)
grid (NULL,NULL) 
legend("topright", c("Män", "Kvinnor"), pch = c(19, 19), col = c("blue", "red"))
