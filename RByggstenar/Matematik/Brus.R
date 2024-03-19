# Här visas hur man kan skapa en brussignal (white noice=alla frekvenser)
# Det kan vara användbart för att simulera det brus
# som alltid förekommer vid mätningar

# Laddar in använda hjälpfunktioner från filen kdFourier.R
source("Verktyg/kdFourier.R")

# Samplingsfrekvens
sf <- 1000

# Skapa tidsvariabel från 0 till 2 med samplingsfrekvens sf
t <- seq(from = 0, to = 2, by = 1/sf)

# Skapa en vector med vitt brus av samma längd som t (sd=0.1)
brus <- kdVittBrus(längd=length(t), standardDeviation=0.1)

## Plotta bruset
plot.ts(brus, col="blue", main="Vitt brus")

## Medlevärde och standardavvikelse
print(mean(brus))
print(sd(brus))
## Som synes ligger medelvärdet kring 0 och 
## standardavvikelsen kring det angivna 0.1


### Plottar en sinuskurva och en sinuskurva med brus adderat

# Skapa en ren sinuskurva (f) och en med brus (bf), där bruset har en sd=0.1
f <- sin(2*pi*10*t)
fb <- f+brus

# Plotta f med med addereat brus (omskalat till sd=0.1) 
plot(t, fb, type="l", col="blue")

# Addera en ren sinuskurva utan brus
lines(t, f, type = "l", col="red", lwd=2)
# lwd ställer in tjockleken på linjen

### Plotta den spektrala effekttätheten (effekt=amplitud^2)
#Enhet: 1/Hz
freEffekt <-  kdFrekvensanalysEffekt(fb,sf)
plot(freEffekt,type="h", xlim = c(0,50))

# Med effekten ovandlad till decibell (Enhet: dB/Hz)
freEffektDb <-  kdFrekvensanalysEffektDecibell(fb,sf)
plot(freEffektDb,type="l")


