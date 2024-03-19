# Visualisering av GPS fil skapad med appen "Sensor Logger"
# Testad med Androidversion av appen, men borde funka även för iphone versionen
# Allt du behöver göra för att använda denna fil är att installera 
# de 3 nedanstående paketen (från Tools->Install packages)
# och sedan ändra nament på den csv fil som läses in (till din egen fil)

# För att kunna använda kartorna måste man skaffa en personlig API nyckel
# Och aktivera den i register_staidamaps nedan.
# 1. Här skapar du ett konto: 
# https://stadiamaps.com/stamen/onboarding/create-account
# 2. Välja Stadia Free (inget kontokort behövs)
# 3. Välj Mobile/Native App -> Continue with API Key
# 4. För att se din API key: Öppna Dashboard -> Manage Properties
# 5. Skriv in din API kod i register_Stadiamaps nedan
# behåll citationsteckning runt API koden
# 6. Ta bort # framför funktionen och kör den 
# (det räcker att göra en gång på datorn, 
# om du inte vill ha din kod synlig kan du nu alltså ta bort funktionen)
# register_stadiamaps("Din API-Nyckel kopieras hit", write = TRUE)


# Läs in paket
library(shiny)    
library(ggplot2)
library(ggmap)


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

# HÄR BYTER MAN NAMN TILL DEN FIL MAN VILL VISUALISERA
# Läs csv med GPS
gpsData <- read.csv(paste(rotDir,"DataFiler/CykelJobb/Location.csv",sep="/"))
gpsData <- read.csv(paste(rotDir,"DataFiler/Kärrtorp400m/Location.csv",sep="/"))
colnames(gpsData)

# Avgör om filen är från "GPS Logger" eller "Sensor Logger"
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
  # Slå samman horizontal oc vertikal accuracy
  accuracy <- sapply(gpsData$horizontalAccuracy^2+gpsData$verticalAccuracy^2,sqrt)
  gpsData$accuracy <- accuracy
}

# Addera tid till gpsData tid i sekunder och tidMinuer
gpsData$tid <- tid
gpsData$tidMinuter <- tid/60


# Geografisk boundingbox för banan
boundingBox <- unname(make_bbox(lon = gpsData$longitude, 
                                lat = gpsData$latitude, f=0.07 ))

# Om någon sida i bounding box är för liten misslyckas kartanropet
# Detta ser därför till att ingen sida är mindre än ett minimum (ca 333 meter)
# Om detblir problem med att göra kartan kan man prova att öka minSide
minSide <- 0.003
longDiff <- boundingBox[3]-boundingBox[1]
if(longDiff < minSide/cos(boundingBox[2]*pi/180)){
  boundingBox[1]=boundingBox[1]-(minSide/cos(boundingBox[2]*pi/180)-longDiff)/2
  boundingBox[3]=boundingBox[3]+(minSide/cos(boundingBox[2]*pi/180)-longDiff)/2
}
latDiff <- boundingBox[4]-boundingBox[2]
if(latDiff<minSide){
  boundingBox[2]=boundingBox[2]-(minSide-latDiff)/2
  boundingBox[4]=boundingBox[4]+(minSide-latDiff)/2
}



# Karta
karta <- get_map(
  location = boundingBox, #  left/bottom/right/top
  maptype = "stamen_toner",
  source = "stadia"           
)


mapPlot <- ggmap(karta)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('') + 
  geom_path(data = gpsData, 
                           aes(x = longitude, y = latitude), 
                           linewidth = 2, lineend = "round", col="red")


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
           plotOutput(outputId = "velocity", height="40vh"), 
           plotOutput(outputId = "altitude", height="40vh")
           ),
    column(6, 
           h4("GPS Visualisering"),
           plotOutput(outputId = "map", height="90vh")
           )
  )
)

# shiny server
server = function(input, output, session) {
  output$map = renderPlot({
    mapPlot + 
      geom_point( aes(x=gpsData$longitude[input$position+1], y=gpsData$latitude[input$position+1]), color="blue", size=5)
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

