# Exempel på hur man kan använda kdIntegral funktionen 
# för att bestämma integralen av en vektor med data 
# samlat med en given samplingsfrekvens.
# Resulatet blir en vektor av samma längd som den ursprungliga datan
# Varje värde i resultatvektorn är integralen från start till respektive "tid" 

# Laddar in använda hjälpfunktioner från filen kdAnalys.R
source("Verktyg/kdAnalys.R")

# Samplingsfrekvens
sf <- 100

# Skapa tidsvariabel från 0 till 2 med samplingsfrekvens sf
t <- seq(from = 0, to = 2, by = 1/sf)

# Tyngdacceleration
g <- 10

# Skapa en vektor a som ger den konstanta accelerationen (g) vid fritt fall
# och som är lika lång som t 
a <- rep(g,length(t))

# Plotta accelerationen
plot(t, a, col="green", type="l", ylab="", lwd =2, ylim=c(0,20))
## ylim ställer in intervallet på y-axeln


# Beräkna integralen av accelerationen = förändring av hastighet (Δv)
Δv <- kdIntegral(a,t)

# Beräkna hastigheten (v) som starthastighten v0 plus hastighetsförändringen Δv
# Starthastigheten sätt till noll
v0 <- 0
v <- Δv+v0

# Plotta hastiheten
lines(t, v, col="blue", type="l", lwd =2)

# Addera symbolförklaring
legend("topleft", c("acceleration", "hastighet"), lwd = 2, col = c("green", "blue"))
