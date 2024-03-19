# Här är verktyg för att bearbeta data från Sensor Logger

# paketet deSolve används och laddas därför
if (!require('deSolve', quietly=TRUE)) install.packages('deSolve')
library(deSolve)

# Laddarin använda hjälpfunktioner från filen kdVektor.R
source("Verktyg/kdVektor.R")

# Räknar om accelerationer från mobiltelefonens koordinatsystem
# (x=höger, y=upp, z= mot dig)
# till ett globalt med x= Öster, y=Nord och z= upp
# Rigtningen för de två väderstrecken mot precis norr och öster 
# blir ofta inte helt exakta (pga kompassens osäkerhet) och kan 
# ändras något pga gyroskopets drift

kdAccelerationTillGlobalaKoordinater <- function(ax, ay, az, roll, pitch, yaw){

   #Använd minvärdet av längderna (orientati0n kan vara en mindre än acceleration)
  len=min(c(length(ax), length(roll)))
  
 #globala acceleratione av samma dimension som ax 
  gax <- rep(0,len)
  gay <- rep(0,len)
  gaz <- rep(0,len)
  
  for(i in 1:length(gax)){
    gAcc <- kdEulerRotationZXY(c(ax[i],ay[i],az[i]),
                               -pitch[i], roll[i], -yaw[i])
    gax[i] <- gAcc[1]
    gay[i] <- gAcc[2]
    gaz[i] <- gAcc[3]
  }
  
  
 resultAcc <- data.frame(ax = gax, ay = gay, az = gaz)
 return(resultAcc)
}

# Hitta medel-riktning (enhetsvektor) för pendlad mobiltelefon i X-Y planet
kdHittaPendelRiktningXYPlan <- function(globalAccX,globalAccY){
  vinklar <- rep(0,length(globalAccX))
  for(i in 1:length(globalAccX)){
    vinklar[i]=atan2(globalAccY[i],globalAccX[i])
    if(vinklar[i]<0){
      vinklar[i]=vinklar[i]+pi
    }
  }
  medelVinkel <- mean(vinklar)
  return(c(cos(medelVinkel),sin(medelVinkel)))
}

# Räkna fram den totala horisontella accelerationen
kdHorisontellPendelAcceleration <- function(globalAccX,globalAccY){
  pendelRiktning <- kdHittaPendelRiktningXYPlan(globalAccX, globalAccY)
  horisontellAcc <- rep(0,length(globalAccX))
  for(i in 1:length(globalAccX)){
    # dotprodukt bestämmer bara tecken
    sign <-  kdDot(pendelRiktning,c(globalAccX[i],globalAccY[i]))
    sign <- -sign/abs(sign)
    horisontellAcc[i] <-sqrt(globalAccX[i]^2+globalAccY[i]^2)*sign
  }
  return(horisontellAcc)
}

# Lös differentialekvation för en dämpad pendel
kdSimuleraPendelAccelerationer <- function(length, startAngle, g, damping, 
                                           time, samplingFrequency){
  # Parametrar   
  θ0 <- startAngle
  ω0 <- 0
  initialValues <- c(θ = θ0, ω = ω0)
  times <- seq(from = 0, to = time, by = 1/samplingFrequency) 
  
  # DIfferential equations
  dFunktioner <-  function(t, y, parms) {
    with (as.list(y),
          list(c(ω, 
                 -sin(θ)*g/length-damping*ω))
    )
  }
  
  #Solve using implicit runge kutta from deSolve
  solution <- as.data.frame(radau(y=initialValues, func = dFunktioner, 
                                  parms= NULL, time=times))
  radialAcc <- length*solution$ω^2
  tangentialAcc <- -g*sin(solution$θ)
  verticalAcc <- radialAcc*cos(solution$θ)+tangentialAcc*sin(solution$θ)
  horisontalAcc <- -radialAcc*sin(solution$θ)+tangentialAcc*cos(solution$θ)
  result <- data.frame(VertikalAcceleration = verticalAcc, 
                       HorisontellAcceleration = horisontalAcc,
                       Tid=times)
  return(result)
}



kdHittaIndexLuftfasHopp <- function(acc, luftNivå = -5){
  # Hitta index för maxvärde
  index2 <- which(acc==max(acc))
  # Gå bakåt till luftnivå
  while(acc[index2]>luftNivå && index2>1){
    index2=index2-1;
  }
  # Gå Bak till acc är större än noll
  index1 <- index2
  while(acc[index1]<0 && index1>1){
    index1=index1-1
  }
  # Gå fram  till luftnivå
  while(acc[index1]>luftNivå && index1<length(acc)){
    index1=index1+1
  }
  return(c(index1,index2))
}


