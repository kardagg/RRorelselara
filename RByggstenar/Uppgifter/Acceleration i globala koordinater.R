source("Verktyg/kdSensorLoggerTools.R")

acceleration <- read.csv("DataFiler/PendelMobil/Accelerometer.csv")
orientation <- read.csv("DataFiler/PendelMobil/Orientation.csv")

accGlobal <- kdAccelerationTillGlobalaKoordinater(
  acceleration$x, acceleration$y, acceleration$z,
  orientation$roll,orientation$pitch, orientation$yaw)

diffTid <- acceleration$seconds_elapsed-orientation$seconds_elapsed
min(diffTid)
max(diffTid)
