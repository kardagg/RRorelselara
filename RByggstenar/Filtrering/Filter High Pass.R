# Laddar in använda hjälpfunktioner från filen kdFunctions.R
source("Verktyg/kdFilter.R")

# Ange samplingsfrekvens (fs) till 1000
fs <- 1000

# Skapa en tidsvariabel t från tiden 0 till 1s, med tidssteget 1/fs
t <- seq(from = 0, to = 1, by = 1/fs )

# Skapa en funktion av summan av två sinussignaler
# En med frekvensen 2 Hz och en med 30 Hz
f <- sin(2*2*pi*t)+sin(30*2*pi*t)

# High pass filter koefficienter
# för ett filter med cutOff frekvens på 15 
# och ett transition band på 10
# Ju mindre man väljer transitionbandet, desto fler koefficienter behövs
# Gör man det för litet kan det därför gå trögt att filtrera,
# eller man kan få problem med minneshantering
highPassCoefficients <- kdHighPassFilterCoefficients(samplingFrequency = fs, 
cutOffFrequency = 15, transitionBandWidth = 10 )


# Plotta filteregenskaper kvalitativt
# Se "freqz" längre ner för plott av de precisa filteregenskaperna
kdPlotHighPassFilterQualitativeCharacteristics(cutOffFrequency = 15, 
                                              transitionBandWidth = 10,
                                              maxPlotFrequency = 50)

# Filtrera f med ett High pass filter
fFiltrerad <- kdFilter(f, coefficients = highPassCoefficients)

# Plotta f och filtrerad f
plot(t, f, type = "l")
lines(t,fFiltrerad, col="red")


## Plotta coefficienter
plot(highPassCoefficients)
# Om filtret har n koefficienter blir de (n-1)/2 första och (n-1)/2 sista samplen mindre exakta
# Om man samlar data är det alltså bra att ha en liten buffer på minst n/2 samples
# både före och efter de intressanta värdena

## Plotta filteregenskaper
freqz(highPassCoefficients, fs=fs)
# Cutoff frekvensen ligger mitt i transition bandet (övergången mellan pass och stop band). 
# Fullt stoppband har man alltså vid cutoffFrequency - transitionBandWidth/2
