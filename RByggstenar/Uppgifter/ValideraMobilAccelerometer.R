# Testa mobiltelefonens acceelerometer och gyroskop genom att 
# se hur väl mätvärden då den pendlar i en lina stämmer med teoretiska värden


# NAMNGE DINA DATAFILER-------------------------------------
namnAccelerometerFil="DataFiler/PendelMobil/Accelerometer.csv"
namnOrienteringsFil="DataFiler/PendelMobil/Orientation.csv"
#________________________________________________________________

# Man kan köra vardera sektion med tangentbordsgenvägen: Ctrl + Alt + T
# På Mac: Cmd + Option + T

# Använda parametrar
tyngdAcceleration=9.82
samplingsfrekvens=100 # inställt värde i SensorLogger
simuleringstid=10 # tid i sekunder för pendelsimulering
# Man måste alltså samla in minst lika mycket pendeltid i mätningen

# Ladda använda verktyg
source("Verktyg/kdSensorLoggerTools.R")

# Läs in Accelerometer och Orientation data från Sensor Logger 
# från en mätning av mobiltelefonen då den penlat
acceleration <- read.csv(namnAccelerometerFil)
orientation <- read.csv(namnOrienteringsFil)

# Transformera accelerationerna till ett globalt koordinatsystem
# med x=Öster, y=Norr och z=Upp 
# Väderstrecken  kan vara oexakta pga osäkerhet i kompassen

accGlobal <- kdAccelerationTillGlobalaKoordinater(
  acceleration$x, acceleration$y, acceleration$z,
  orientation$roll,orientation$pitch, orientation$yaw)

# Beräkna den sammanlagda horisontella accelerationen i pendelplanet
horisontellAcceleration<- kdHorisontellPendelAcceleration(accGlobal$ax,accGlobal$ay)



# Visualisering av resultat---------------------------------------------------

# OBS nedanstående paket måste först installeras: Använd knappen "Tools",
# välj "Install packages" och skriv namnet på paketet (ex: Directional) och "Install"
library(shiny)    

#Shiny UI
ui = fluidPage(
  
  # Application title
  titlePanel("Pendel-validering för mobil-mätning av acceleration och orientering"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      sliderInput(inputId = "start", 
                  "Passa in", 1, simuleringstid*samplingsfrekvens, 
                  value = 1, step=1, width="100%"),
      sliderInput(inputId = "startAngle", 
                  "Startvinkel (◦)", 45, 90, 
                  value = 75, step=0.5, width="100%"),
      sliderInput(inputId = "length", 
                  "Pendellängd (cm)", 50, 100, 
                  value = 75, step=0.5,  width="100%"),
      sliderInput(inputId = "damping", 
                  "Dämpning", 0, 0.2, 
                  value = 0,  width="100%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "pltVertikal"), 
      plotOutput(outputId = "pltHorizontal")
    )
  )
  


)

# shiny server
server = function(input, output, session) {
  
  simulatedAcc <- reactive({kdSimuleraPendelAccelerationer(
    length=input$length/100, startAngle=input$startAngle*pi/180, g=tyngdAcceleration, 
    damping=input$damping , time=10,samplingFrequency=samplingsfrekvens)})
  

  output$pltVertikal = renderPlot({
    acc <- simulatedAcc();
    par(mar = c(0, 6, 2, 2))
    plot(acc$Tid,acc$VertikalAcceleration,  
         type ="l", col="green", lwd=8, xlab = "", 
         ylab=expression("Vertikal acceleration (m/s"^2*")"),
         cex.axis=1.2, cex.lab=1.4)+grid()+
      lines(acc$Tid,
            accGlobal$az[input$start:(input$start+
                                        length(acc$Tid)-1)],
            col="blue")+ 
      mtext(paste("Korrelation:", 
                  toString(cor(acc$VertikalAcceleration,
                               accGlobal$az[input$start:(input$start+
                                        length(acc$Tid)-1)]))), 
                               side=3, font=2, cex=1.2 )
  })
  
  output$pltHorizontal = renderPlot({
    acc <- simulatedAcc();
    par(mar = c(4, 6, 2, 2))
    plot(acc$Tid,acc$HorisontellAcceleration,  
         type ="l", col="green", lwd=8, 
         xlab = "Tid (s)", ylab=expression("Horisontell acceleration (m/s"^2*")"), 
         cex.axis=1.2, cex.lab=1.4 )+grid()+
      lines(acc$Tid,
            horisontellAcceleration[input$start:(input$start+
                                        length(acc$Tid)-1)],
            col="blue")+
      mtext(paste("Korrelation:", 
                  toString(cor(acc$HorisontellAcceleration,
                  horisontellAcceleration[input$start:(input$start+
                                          length(acc$Tid)-1)]))), 
            side=3, font=2, cex=1.2 )
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Kör shiny
shinyApp(ui, server)

