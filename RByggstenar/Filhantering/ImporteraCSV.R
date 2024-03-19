# Importera en csv fil som har rubriker (headars) på första raden
# och använder ; som avgränsare 
# MuskelKraftHastighet.csv innehåller maximal normaliserad kraft 
# vs normaliserad förkortningshastighet
# beräknat från Hills model (se referens längst ned i skriptet)

fv <- read.csv2("DataFiler/MuskelKraftHastighet.csv", header=TRUE)

## skriv ut fv
print(fv)

## Plotta fv
plot(fv, col="red", pch=19)

# Om man ska importera en csv fil där punkt används som decimaltecken, 
# istället för komma (vilket är standard exempelvis i England), 
# använder man read.csv istället för read.csv2
# Om du undrar hur din fil ser ut (vilka decimaltecken och 
# avgränsare som används) går det bra att öppna csv filer i
# en vanlig text editor (som exempelvis notepad i windows).

### Hittar ej filen

# Alla sökvägar (sem exempelvis "DataFiler/import.csv") 
# utgår från working direcotry, vilket, förutsatt att projektet
# R_Rörelselära är öppet är R_Rörelselära mappen
# där projekfilen R_Rörelselära.Rproj finns

# Om man vill kolla kan man skriv ut working directory
getwd()
# Är det inte R_Rörelselära som är working directory beror det antagligen
# på att du inte arbetar inom det projektet. 
# För att göra det måste du gå till File->Open Projekt och öppna projektfilen
# Eller srtarta RStudio genom att dubbelklicka på projektfilen (R_Rörelselära.Rproj)

### Info om vilken data typ fv är

# Kontrollera data typen
class(fv)
# När man läser in en CSV fil får man en Data Frame 

# Skriv ut namnen på kolumnerna
colnames(fv)

# För att referera enbart till en kolumn i en Data Frame används $ tecknet
print(fv$Kraft)

### Referens till använd artikel med muskelmodell
# Hill, A. V. (1938). The heat of shortening and the dynamic constants 
# of muscle. Proceedings of the Royal Society of London. 
# Series B-Biological Sciences, 126(843), 136-195.
