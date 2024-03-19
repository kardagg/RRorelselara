#Analyserar hastighet från gps data
source("Verktyg/kdAnalys.R")
source("Verktyg/kdKartor.R")
source("Verktyg/kdFilter.R")

#  Läser in GPS filen
gps <- read.csv("DataFiler/Kärrtorp400m/Location.csv", header = TRUE)

# Plotta hastigheten
plot(gps$seconds_elapsed,gps$speed, type="l", col="red" )

# Omvandla lat och long till x och y koordinater (x åt öst och y åt norr) i meter
mData <- kdXYFromLatLong(gps$latitude,gps$longitude,
                                 mean(gps$latitude), mean(gps$longitude))

# Derivera x och y positonerna för att få hastighet
# Hastighet
mData$vx <- kdDerivata(mData$x, gps$seconds_elapsed)
mData$vy <- kdDerivata(mData$y, gps$seconds_elapsed)

# Beräkna storleken på hastigheten
mData$vTot <- sqrt( mData$vx^2+mData$vy^2)

# Addera den beräknade farten till plotten
lines(gps$seconds_elapsed, mData$vTot, col="blue")

# Eftersom den har större omfång blir det snyggare att plotta den först
plot(gps$seconds_elapsed, mData$vTot, col="blue", type="l", lwd=2)
lines(gps$seconds_elapsed, gps$speed, col="red", lwd=2)

# Filtera den beräknade hastigheten
lowpassKeoff <- kdLowPassFilterCoefficients(samplingFrequency = 1, 
                                            cutOffFrequency = .12, transitionBandWidth = .02 )
filtFart <- kdFilter(mData$vTot,lowpassKeoff)

# Addera filterad fart
lines(gps$seconds_elapsed, filtFart, col="green", lwd=2)

# Integrera den beräknade farten. Integralen borde bli 400m
sträcka1 <- kdIntegral(filtFart,gps$seconds_elapsed)
plot(gps$seconds_elapsed,sträcka1,col="blue", type="l")
print(sträcka1)

# Fart given av GPS
sträcka2 <- kdIntegral(gps$speed,gps$seconds_elapsed)
lines(gps$seconds_elapsed,sträcka2,col="red")
print(sträcka2)

# Hur stort fel?
diff <- 400-tail(sträcka2,1)
relDiff <- diff/400
relDiff
