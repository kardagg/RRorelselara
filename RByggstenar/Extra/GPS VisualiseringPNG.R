# Visualisering av GPS fil skapad med appen "Sensor Logger"
# Testad med Androidversion av appen, men borde funka även för iphone versionen
# Allt du behöver göra för att använda denna fil är att installera 
# de 3 nedanstående paketen (från Tools->Install packages)
# och sedan ändra nament på den csv fil som läses in (till din egen fil)

# Läs in paket
library(shiny)    
library(ggplot2)
library(png)


# Om man kör rad för rad används R_Rörelselära som rot
# men om man använder kanppen "Run App" ovan använder shiny
# den mapp R koden ligger i som rot 
# därför behövs nedanstående kod för att hitta R_Rörelselära mappen
# oavsett hur man aktiverar appen
rotDir <- getwd()
if(tail(unlist(strsplit(rotDir,"/")),n=1)!="R_Rörelselära"){
  rotDir <- dirname(dirname(rotDir))
} 
 
# Laddar in använda hjälpfunktioner från filen kdFunctions.R
source(paste(rotDir,"Verktyg/kdText.R",sep="/"))
source(paste(rotDir,"Verktyg/kdAnalys.R",sep="/"))
source(paste(rotDir,"Verktyg/kdKartor.R",sep="/"))
source(paste(rotDir,"Verktyg/kdFilter.R",sep="/"))

# HÄR BYTER MAN NAMN TILL DEN FIL MAN VILL VISUALISERA
# Läs csv med GPS
gpsData <- read.csv(paste(rotDir,"DataFiler/Kärrtorp400m/Location.csv",sep="/"))
bild <- readPNG(paste(rotDir,"DataFiler/Kärrtorp400m/kartBild.png",sep="/"))
bildPosition <- read.csv2(
  paste(rotDir,"DataFiler/Kärrtorp400m/bildPosition.csv",sep="/"),header =TRUE)
colnames(gpsData)

# Avör om filen är från "GPS Logger" eller "Sensor Logger"
if("date.time" %in% colnames(gpsData)){ #GPS Logger
  # Tidsvektor
  tid <- sapply(gpsData$date.time,function(x) gsub(".* ","",as.character(x)))
  tid <- sapply(tid, kdTidTextTillNumSekunder )
  tid <- tid - tid[1]
  # Byt namn på kolumner (ta bort .m och .m.s)
  colnames(gpsData)[5] <- "accuracy"
  colnames(gpsData)[6] <- "altitude"
  colnames(gpsData)[8] <- "speed"
} else{ #Sensor Logger
  # Tidsvektor
  tid <- gpsData$seconds_elapsed- gpsData$seconds_elapsed[1]
  # Slå samman horizontal och vertikal accuracy
  accuracy <- sapply(gpsData$horizontalAccuracy^2+gpsData$verticalAccuracy^2,sqrt)
  gpsData$accuracy <- accuracy
}

# Addera tid till gpsData tid i sekunder och tidMinuer
gpsData$tid <- tid
gpsData$tidMinuter <- tid/60

# Omvandla lat och long till x och y koordinater (x åt öst och y åt norr) i meter
xyKoordinater <- kdXYFromLatLong(gpsData$latitude,gpsData$longitude,
                mean(gpsData$latitude), mean(gpsData$longitude))
gpsData$x <- xyKoordinater$x
gpsData$y <- xyKoordinater$y

# Hastighet
gpsData$vx <- kdDerivata(gpsData$x, tid)
gpsData$vy <- kdDerivata(gpsData$y, tid)
# Skalar storleken på hastighetspilar i grafiken
hastighetsSkala <- 10

# Acceleration
lowpassKeoff <- kdLowPassFilterCoefficients(samplingFrequency = 1, 
                                            cutOffFrequency = .1, transitionBandWidth = .05 )
gpsData$ax <- kdFilter(kdDerivata(gpsData$vx, tid),lowpassKeoff)
gpsData$ay <- kdFilter(kdDerivata(gpsData$vy, tid),lowpassKeoff)
# Skalar storleken på accelerationsspilar i grafiken
accelerationSkala <- 100


# Omvandla även lat long för bildhörnen till x och y i meter
bildHörn <- kdXYFromLatLong(bildPosition$lat,bildPosition$long,
                                 mean(gpsData$latitude), mean(gpsData$longitude))

#Image plot
par(mar = c(0, 0, 0, 0))
plot(1:2, type='n', xlab="", ylab="", asp = 1, axes=TRUE,
     xlim=c(bildHörn[1,1],bildHörn[2,1]), ylim=c(bildHörn[1,2],bildHörn[2,2])) +
  rasterImage(bild, bildHörn[1,1],bildHörn[1,2], bildHörn[2,1],bildHörn[2,2]) +
  points(gpsData$x,gpsData$y, col="blue", pch=19) 

 imagePlot <- recordPlot()

# Aspect ratio x/y for image
aspectRatio <- (bildHörn[2,1]-bildHörn[1,1])/
  (bildHörn[2,2]-bildHörn[1,2])


# Map height in shiny
if(aspectRatio < 1){
  mapHeight <- 780
  } else {
  mapHeight <- 780*aspectRatio
}

# Hastighetsplott aes= eastethics
velocityPlot <- ggplot(data = gpsData, 
                       mapping = aes(x = tidMinuter,
                                     y = speed,
                                     col = accuracy))+
  geom_line(show.legend = FALSE)+
  scale_color_gradientn(colours =c("red","green", "darkgreen","black"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  xlab("")+
  ylab("Hastighet (m/s)")



# Altitud-plott 
altitudePlot <- ggplot(data = gpsData, 
                       mapping = aes(x = tidMinuter,
                                     y = altitude,
                                     col = accuracy))+
  geom_line()+
  scale_color_gradientn(colours = c("red","green","darkgreen", "black"), 
                        name="Noggrannhet (m)")+
  theme(legend.position="bottom", 
        legend.direction = "horizontal",
        legend.key.width= unit(1.8, 'cm'))+
  xlab("Tid (min)")+
  ylab("Höjd (m)")+
  labs( "Noggrannhet (m)")


#Shiny UI
ui = fluidPage(
  fluidRow(
    column(6,
           sliderInput(inputId = "position", "Tid(s)", 0, 
                       length(gpsData$latitude)-1, value = 0, width="100%"),
           plotOutput(outputId = "velocity", height=320), 
           plotOutput(outputId = "altitude", height=350)
           ),
    column(6, 
           h4("GPS Visualisering"),
           plotOutput(outputId = "map", height=mapHeight)
           )
  )
)


# shiny server
server = function(input, output, session) {
  output$map = renderPlot({
    plot(1:2, type='n', xlab="", ylab="", asp = 1, axes=FALSE,
         xlim=c(bildHörn[1,1],bildHörn[2,1]), ylim=c(bildHörn[1,2],bildHörn[2,2])) +
      rasterImage(bild, bildHörn[1,1],bildHörn[1,2], bildHörn[2,1],bildHörn[2,2]) +
      points(gpsData$x,gpsData$y, col="blue", pch=19, cex=.5) +
    points(gpsData$x[input$position+1], gpsData$y[input$position+1], col="red", pch=19, cex=1.5)+
      arrows(gpsData$x[input$position+1], gpsData$y[input$position+1],
             gpsData$x[input$position+1]+ hastighetsSkala*gpsData$vx[input$position+1], 
             gpsData$y[input$position+1]+ hastighetsSkala*gpsData$vy[input$position+1],
             col="red",lwd=2)+
      arrows(gpsData$x[input$position+1], gpsData$y[input$position+1],
             gpsData$x[input$position+1]+ accelerationSkala*gpsData$ax[input$position+1], 
             gpsData$y[input$position+1]+ accelerationSkala*gpsData$ay[input$position+1],
             col="blue",lwd=2)
    })
  output$velocity = renderPlot({
    velocityPlot +geom_point(aes(x=tidMinuter[input$position+1],y=speed[input$position+1]),size=3, colour="blue")
  })
  output$altitude = renderPlot({
    altitudePlot +
      geom_point(aes(x=tidMinuter[input$position+1],y=altitude[input$position+1]),size=3, colour="blue")
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Kör shiny
shinyApp(ui, server)

# För att se så bra som möjligt bör man välja full screen för det fönster som öppnas 

