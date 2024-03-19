# Funktion för att derivera och integrera data numerskt

# Funktion som avrundar väldigt små tal i en lista till noll
chop <- function(numVektor, limit=1e-6){
  result <- numVektor
  for(i in 1:length(numVektor)){
    if(numVektor[i]<limit){
      result[i] <- 0.0
    }
  }
  return(result)
}

# Derivata df(t)/dt för varje tidpunkt
# Vektorerna f och t måste vara samma längd
kdDerivata <- function(f, t){
  if(length(f)!=length(t)){
    print("Avbryter, eftersom vektorerna f och t inte har samma längd.")
    return()
  }
  # Skapa derivata vector med samma längd som data
  derivata <- rep(0.0,length(t))
  for(s in 2:(length(t)-1)){
    #Använder central skillnad
    Δf1 <- (f[s]-f[s-1])
    Δf2 <- (f[s+1]-f[s])
    Δt1 <- (t[s]-t[s-1])
    Δt2 <- (t[s+1]-t[s])
    derivata[s] <- (Δt2*Δf1/Δt1+Δt1*Δf2/Δt2)/(Δt1+Δt2)
  }
  # Interpolerar start och slutvärden
  len <- length(t)
  derivata[1] <- derivata[2]-(derivata[3]-derivata[2])*(t[2]-t[1])/(t[3]-t[2])
  derivata[len] <- derivata[len-1]+
    (derivata[len-1]-derivata[len-2])*(t[len]-t[len-1])/(t[len-1]-t[len-2])
  return(derivata)
}

# Integral Sf(t)dt från start till varje tidpunkt i f
# Vektorerna f och t måste vara samma längd
kdIntegral <- function(f, t){
  if(length(f)!=length(t)){
    print("Avbryter, eftersom vektorerna f och t inte har samma längd.")
    return()
  }
  # Skapa integral vector med samma längd som data
  integral <- rep(0,length(t))
  for(s in 2:length(t)){
    Δt <- (t[s]-t[s-1])
    integral[s] <- integral[s-1]+Δt*(f[s-1]+f[s])/2
  }
  return(integral)
}

#Ger en vector med index för lokala maxima i en numerisk vector
kdLokalaMaximaIndex <- function(vektor){
  if(!is.numeric(vektor)){
    print("Avbryter, eftersom argumentet inte är en numerisk vektor")
    return()
  }
  return(which(diff(sign(diff(vektor)))==-2)+1)
}

#Ger en vector med index för lokala minima i en numerisk vector
kdLokalaMinimaIndex <- function(vektor){
  if(!is.numeric(vektor)){
    print("Avbryter, eftersom argumentet inte är en numerisk vektor")
    return()
  }
  return(which(diff(sign(diff(vektor)))==2)+1)
}

# Gör en numerisk dataFrame med fix samplingsfrekvens från en dataFrame samplad enligt tid
# Enbart numeriska columner överförs
kdSkapaFixSamplingsfrekvensDataFrame <- function(dataFrame, tid, samplingsfrekvens){
  Δt <- 1/samplingsfrekvens
  tid0 <- tid-tid[1]
  
  #Behåll namn men omvandla alla numeriska vectorer till fix samplingsfrekvens
  columnNames <- names(dataFrame)
  
  for(col in columnNames){
    if(is.numeric(unlist(dataFrame[col]))){   # Om numerisk => omvandling
      data <- unname(unlist(dataFrame[col]))
      dataFix <- c()  
      currentIndx <- 2
      for(s in seq(from=0 , to = tid0[length(tid0)], by = Δt)){
        while(currentIndx<length(tid0) && tid0[currentIndx]<s){
          currentIndx = currentIndx+1;
        }
        Δt1 <- (s-tid0[currentIndx-1])
        Δt2 <- (tid0[currentIndx]-s)
        dataFix <- append(dataFix, 
                          (Δt2*data[currentIndx-1]+Δt1*data[currentIndx])/(Δt1+Δt2))
      }
      if(exists("resultatDataFrame")){
        resultatDataFrame[col] = dataFix
      } else{
        resultatDataFrame <- data.frame(col = dataFix)
      }
     
    } else {  # Om ej numerisk 
      
    }
  }
 
  return(resultatDataFrame)
}
