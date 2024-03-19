# Exempel på hur man kan använda kdDerivata funktionen 
# för att bestämma derivatan av en vektor v i varje tidpunkt

# Laddar in använda hjälpfunktioner från filen kdAnalys.R
source("Verktyg/kdAnalys.R")

# Samplingsfrekvens
sf <- 100

# Skapa tidsvariabel från 0 till 2 med samplingsfrekvens sf
t <- seq(from = 0, to = 2, by = 1/sf)

# Tyngdacceleration
g <- 10

# Skapa en funktion s som beskriver sträckan ett föremål faller på tiden t 
# vid fritt fal, om det startar med hastigheten v0. Positiv riktning är nedåt
v0 <- 0
s <- (g*t*t/2+v0*t)

# Plotta s
plot(t,s, col="red", type="l", ylab="", lwd=2)

# beräkna derivatan av s (hastigheten)
# Genom att derivera sträckan (s) får du hastigheten (v). 
v <- kdDerivata(s,t)

## Plotta hastiheten
lines(t, v, col="blue", type="l", lwd=2)
length(t)

## Addera symbolförklaring
legend("topleft", c("sträcka", "hastighet"), lty = 1, col = c("red", "blue"))
