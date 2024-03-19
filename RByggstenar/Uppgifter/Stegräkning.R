# Laddar in använda hjälpfunktioner
source("Verktyg/kdAnalys.R")
source("Verktyg/kdFilter.R")

# Importera csv fil med vertikal acceleration från löpning
acc <- read.csv2( "DataFiler/VertikalAccLöpning25s.csv")

plot(acc, type ="l", col="red",lwd = 2)

# Lågpass filtrera så att man får ett max per stegcykel
lowPassCoefficients <- kdLowPassFilterCoefficients(samplingFrequency = 100, 
                                                   cutOffFrequency = 6, 
                                                   transitionBandWidth = 5 )

accLowPass <- kdFilter(acc$acceleration, coefficients = lowPassCoefficients)

lines(acc$tid,accLowPass, type = "l", col = "blue", lwd = 2) 

# Hitta index för lokala maxima
fotKontaktIndex <- kdLokalaMaximaIndex(accLowPass)
points(acc$tid[fotKontaktIndex],accLowPass[fotKontaktIndex], 
       col = "green", pch=19)

# Beräkna stegfrekvens (= 1/stegtid) i Hz (steg /sekund)
stegFrekvens <- 1/diff(acc$tid[fotKontaktIndex])

# Beräkna index för varje steg mitt emellan fotkontakter
stegIndex <-  head(fotKontaktIndex,-1) + round(diff(fotKontaktIndex)/2)

# Plotta stegfrekvens som steg/minut
plot(acc$tid[stegIndex], stegFrekvens*60,col = "red", pch=19, 
     ylab = "Stegfrekvens (steg/minut)", xlab = "tid (s)")
