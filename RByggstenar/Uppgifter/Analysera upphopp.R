hopp <- read.csv2("DataFiler/Hopp/hopp.csv", header=TRUE)

plot(hopp$Tid,hopp$Acc, type="l", col="red", lwd=2)

accFrånF <- (hopp$Kraft-96*9.82)/96

lines(hopp$Tid, accFrånF, col="blue", lwd=2)
grid()

source("Verktyg/kdSensorLoggerTools.R")

# Beräkning av hopphöjd från tid
#1= acc från kraftplatta
indices1 <- kdHittaIndexLuftfasHopp(accFrånF,-9)
points(c(hopp$Tid[indices1[1]],hopp$Tid[indices1[2]]),
       c(accFrånF[indices1[1]],accFrånF[indices1[2]]), col="blue", pch=19)
       
print(hopp$Tid)
sf <- 100
g <- 9.82

tidLuft1 <- (indices1[2]-indices1[1])/sf
h1 <- (g*(tidLuft1/2)^2)/2 

#2= acc från accelerometer
indices2 <- kdHittaIndexLuftfasHopp(hopp$Acc,-5)
points(c(hopp$Tid[indices2[1]],hopp$Tid[indices2[2]]),
       c(hopp$Acc[indices2[1]],hopp$Acc[indices2[2]]), col="red", pch=19)


tidLuft2 <- (indices2[2]-indices2[1])/sf
h2 <- (g*(tidLuft2/2)^2)/2 

# Beräkning av hopphöjd från integralen av accelerationen => hastighet
#1
source("Verktyg/kdAnalys.R")
v1 <- tail(kdIntegral(accFrånF[1:indices1[1]], hopp$Tid[1:indices1[1]]),1)
hh1 <- v1^2/(2*g)

#2
v2 <- tail(kdIntegral(hopp$Acc[1:indices2[1]], hopp$Tid[1:indices2[1]]),1)
hh2 <- v2^2/(2*g)

hopp$Acc