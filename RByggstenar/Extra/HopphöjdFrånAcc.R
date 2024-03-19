source("Verktyg/kdSensorLoggerTools.R")


# Editera namnen på filerna som läses in
# Läs data från dina csv filer från sensor logger
acceleration <- read.csv("DataFiler/HoppSensorLogger/Accelerometer.csv")
orientation <- read.csv("DataFiler/HoppSensorLogger/Orientation.csv")

#Omvandla till globala koordinater
accGlobal <- kdAccelerationTillGlobalaKoordinater(
  acceleration$x, acceleration$y, acceleration$z,
  orientation$roll,orientation$pitch, orientation$yaw)

# Om dina värden blir konstiga och du använder IPhone
# beror det antagligen på att koordinaterna är deffinierade som 
# vänsterhänta för IPhone.
# Prova då att ersätta föregående funktion med nedanstående
# Ta då bort alla 5 # före nedanstående 5 rader
# accGlobal <- kdAccelerationTillGlobalaKoordinater(
#  acceleration$x, -acceleration$y, acceleration$z,
#  orientation$roll,orientation$pitch, orientation$yaw)
# accGlobal$az <- -accGlobal$az 
# accGlobal$ay <- -accGlobal$ay 

# Eftersom acceleration och orientering kan skilja sig ett värde i längd
# skapar jag en tidsvariabel från den minsta längden (som används för global)
if(length(acceleration$seconds_elapsed)<length(acceleration$seconds_elapsed)){
  tid <- acceleration$seconds_elapsed
  } else {
  tid <- orientation$seconds_elapsed
  }

#Plotta hela datan
plot(tid, accGlobal$az, col="red", xlab ="Tid", 
     ylab = "Acc", type="l", main="Vertikal acceleration (m/s^2)" )

# Beskär runt maxvärdet
before <- 50
after <- 20
maxIndex <- which(accGlobal$az==max(accGlobal$az))
accGlobalCropped <- accGlobal[(maxIndex-before):(maxIndex+after),]
tidCropped <- tid[(maxIndex-before):(maxIndex+after)]
tidCropped <- tidCropped-tidCropped[1]

#Plotta beskuren data
plot(tidCropped, accGlobalCropped$az, col="red", xlab ="Tid", 
     ylab = "Acc", type="l", main="Vertikal acceleration (m/s^2)" )

# Hitta indices för luftfas
indices <- kdHittaIndexLuftfasHopp(accGlobalCropped$az,-5)
points(c(tidCropped[indices[1]],tidCropped[indices[2]]),
       c(accGlobalCropped$az[indices[1]],accGlobalCropped$az[indices[2]]), 
       col="red", pch=19)

# Beräkna hopphöjd
g <- 9.82
tidILuft <- tidCropped[indices[2]] - tidCropped[indices[1]]
hoppHöjd <- (g*(tidILuft/2)^2)/2 

# Addera text till plot
mtext(paste("Hopphöjd = ", toString(round(hoppHöjd, digits=3)),"m"),side=3)

                                               