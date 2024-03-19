source("Verktyg/kdSensorLoggerTools.R")

acceleration <- read.csv("DataFiler/PendelMobil/Accelerometer.csv")
orientation <- read.csv("DataFiler/PendelMobil/Orientation.csv")

# y till minus för att göra högerhänt koordinatsystem 
# (eftersom accelerationser från sensor logger i IPhone 
# verkar vara vänsterhänta koordinater)
accGlobal <- kdAccelerationTillGlobalaKoordinater(
  acceleration$x, -acceleration$y, acceleration$z,
  orientation$roll,orientation$pitch, orientation$yaw)

# z och y till minus för att vrida z uppåt 
# (och y för att det fortfarande ska vara ett högerhänt koordinatsystem)
accGlobal$az <- -accGlobal$az 
accGlobal$ay <- -accGlobal$ay 

diffTid <- acceleration$seconds_elapsed-orientation$seconds_elapsed
min(diffTid)
max(diffTid)