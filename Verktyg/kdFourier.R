
#Skapar en vektor med vitt brus med en given standardavvikelse
kdVittBrus <- function(längd, standardDeviation=1){
  brus <- as.vector(arima.sim(model = list(order = c(0, 0, 0)), n = längd))
  brus=brus*standardDeviation
  return(brus)
}

# Spektraltäthet (Power spectrum) för en given tidsserie
kdFrekvensanalysEffekt <- function(tidSerie, samplingsFrekvens) {
  n <- length(tidSerie)
  frek <- seq(from = 0, to = samplingsFrekvens/2, by = samplingsFrekvens/n)
  fourier <- fft(tidSerie)
  effekt  <- Mod(fourier) # complex modulus= normen av z
  fourier <- fourier[1:(floor(n/2)+1)]
  set <- (1/(samplingsFrekvens*n))*(Mod(fourier)^2)
  set[2:length(set)] <- 2*set[2:length(set)]
  resultat <- data.frame(frekvens=frek, spektralEffektTäthet=set )
  return(resultat)
}

# Spektraltäthet (Power spectrum) i decibell för en given tidsserie
kdFrekvensanalysEffektDecibell <- function(tidSerie, samplingsFrekvens) {
  svar <- kdFrekvensanalysEffekt(tidSerie,samplingsFrekvens)
  # adderar litet värde (10^-12) för att log inte kan hantera 0
  setDb <- 10*log10(svar$spektralEffektTäthet+10^-12)
  resultat <- data.frame(frekvens=svar$frekvens, spektralEffektTäthetDecibell=setDb )
  return(resultat)
}


