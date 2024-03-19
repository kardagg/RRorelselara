# Kod för att det även ska gå att söka filer från en shiny app
rotDir <- getwd()
while(length(unlist(strsplit(rotDir,"/")))>1 && 
      tail(unlist(strsplit(rotDir,"/")),n=1)!="R_Rörelselära"){
  rotDir <- dirname(rotDir)
}

#Verktyg för att hantera kartdata
source(paste(rotDir,"Verktyg/kdVektor.R",sep="/"))


# omvandla lat long data till kartesiska koordinater med x åt öst och y åt nord
# med origo deffinerat av latLongOrigo
# utgår från en sfärisk modell av jorden (fel i stockholm under 20cm)
kdXYFromLatLong <- function(latitudes,longitudes, latOrigo, longOrigo){
  earthRadius <-  6371009
  zG <- earthRadius*sin(latitudes*pi/180)
  latRadius <- earthRadius*cos(latitudes*pi/180)
  xG <- latRadius*cos(longitudes*pi/180)
  yG <- latRadius*sin(longitudes*pi/180)
  xyKoordinater <- data.frame(matrix(nrow=0,ncol=2))
  colnames(xyKoordinater) <- c("x","y")
  for(i in 1:length(latitudes)){
    koord <- kdRotMatrixY(-(90-latOrigo)*pi/180) %*% 
                   kdRotMatrixZ(-longOrigo*pi/180) %*% 
                   c(xG[i],yG[i],zG[i])
    xyKoordinater[i,] <- c(koord[2],-koord[1])
  }
  return(xyKoordinater)
}


