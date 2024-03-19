# Funktioner för att filtrera data

# paketet (package) gsignal används och laddas därför
if (!require('gsignal', quietly=TRUE)) install.packages('gsignal')
library('gsignal')

#FIR low pass filterkoefficienter med Hamming window
# cuttOffFrequency ligger mitt i transition band
kdLowPassFilterCoefficients <- function(samplingFrequency,  cutOffFrequency, transitionBandWidth) {
  n <- round(3.3/(transitionBandWidth/samplingFrequency))
  # add 1 to n if n even
  if((n%%2)==0){
    n=n+1
  }
  # filterkoefficienter
  taps=gsignal::fir1(n-1, cutOffFrequency/(samplingFrequency / 2), type="low", window = hamming(n));
  # För att koefficienterna inte ska ändra medelvärdet på signalen
  # skalar man om dem så att summan av dem blir ett
  taps <- taps/sum(taps)
 
  return(taps)
}

#FIR high pass filterkoefficienter med Hamming window
# cuttOffFrequency ligger mitt i transition band
kdHighPassFilterCoefficients <- function(samplingFrequency,  cutOffFrequency, transitionBandWidth) {
  n <- round(3.3/(transitionBandWidth/samplingFrequency))
  # add 1 to n if n even
  if((n%%2)==0){
    n=n+1
  }
  # filterkoefficienter
  taps=gsignal::fir1(n-1, cutOffFrequency/(samplingFrequency / 2), type="high", window = hamming(n), scale = TRUE);
  return(taps)
}

# Filter (konvolution/faltning av koefficienter)
# med tidskorrektion och adderade buffrar före och efter för att minska 
# start och stop fel
kdFilter <- function(data, coefficients) {
  n <- length(coefficients)
  # Addera starvärde till början och slutvärde till slutet av data
  padData <- c(rep(head(data,1),(n-1)/2),data,rep(tail(data,1),(n-1)/2))
  # Filtrera data
  filteredData <- filter(coefficients, padData)
  # Korrigera så att den filtrerade sinalen synkar med den ofiltrerade
  timeCorrectedFilteredData <- filteredData[n:length(padData)]
  return(timeCorrectedFilteredData)
}


# Beräkknar moving average av given data

kdMovingAverage <- function(data, width){
  # width måste vara ojämt
  if((width %% 2) == 0) {
    print("Avbryter, parametern width måste vara ojämn")
  }
  # Addera medelvärden till slutet av data
  medel <- mean(data)
  padData <- c(rep(medel,(width-1)/2),data,rep(medel,(width-1)/2))
  # medelvärde
  meanData <- data
  for(i in 1:length(data)){
    meanData[i] = mean(padData[i:(i+width-1)])
  }
  return(meanData)
}

# Plotta kvalitativt effekt av filter
# Svenska: Aritmetisk mittfrekvens = cutOffFrequency,
# Övergångsbandbredd = transitionBaNDwIDTH
kdPlotLowPassFilterQualitativeCharacteristics <- function( cutOffFrequency,
                                                transitionBandWidth, 
                                                maxPlotFrequency){
  # maxPlotFrequency bör inte vara större än 
  # nyqvstfrekvensen=samplingsfrekvens/2
  fre <- seq(from = 0, to = maxPlotFrequency, by=0.1)
  kvalitativEffekt <- rep(0,length(fre))
  
  for(i in 1:length(fre)){
    if(fre[i]<(cutOffFrequency-transitionBandWidth/2)){
      kvalitativEffekt[i]=1
    } else if(fre[i]<(cutOffFrequency+transitionBandWidth/2)){
      kvalitativEffekt[i]=1-
        (fre[i]-cutOffFrequency+transitionBandWidth/2)/transitionBandWidth
    }
  }
  return(plot(fre,kvalitativEffekt, col="blue", lwd=2,
              xlab="Frekvens (Hz)", type="l",
       ylab="", main="Kvalitativ effekt av lågpassfiltret",
       yaxt="n")+
         axis(2, at = c(0, 1),
              labels = c("Stopp", "Passera"))+
         abline(v=cutOffFrequency-transitionBandWidth/2, col="lightgray")+
         abline(v=cutOffFrequency+transitionBandWidth/2, col="lightgray")+
         abline(v=cutOffFrequency, col="red")+
         grid()+
         text(cutOffFrequency,1,"cutOffFrequency",col="red", adj=c(1,1), srt=90))
}

kdPlotHighPassFilterQualitativeCharacteristics <- function( cutOffFrequency,
                                                           transitionBandWidth, 
                                                           maxPlotFrequency){
  # maxPlotFrequency bör inte vara större än 
  # nyqvstfrekvensen=samplingsfrekvens/2
  fre <- seq(from = 0, to = maxPlotFrequency, by=0.1)
  kvalitativEffekt <- rep(1,length(fre))
  
  for(i in 1:length(fre)){
    if(fre[i]<(cutOffFrequency-transitionBandWidth/2)){
      kvalitativEffekt[i]=0
    } else if(fre[i]<(cutOffFrequency+transitionBandWidth/2)){
      kvalitativEffekt[i]=
        (fre[i]-cutOffFrequency+transitionBandWidth/2)/transitionBandWidth
    }
  }
  
  return(plot(fre,kvalitativEffekt, col="blue", lwd=2,
              xlab="Frekvens (Hz)", type="l",
              ylab="", main="Kvalitativ effekt av högpassfiltret",
              yaxt="n")+
           axis(2, at = c(0, 1),
                labels = c("Stopp", "Passera"))+
           abline(v=cutOffFrequency-transitionBandWidth/2, col="lightgray")+
           abline(v=cutOffFrequency+transitionBandWidth/2, col="lightgray")+
           abline(v=cutOffFrequency, col="red")+
           grid()+
           text(cutOffFrequency,1,"cutOffFrequency",col="red", adj=c(1,0), srt=90))
}
