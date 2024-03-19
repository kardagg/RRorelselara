# Läs in paket
library(shiny)    
library(ggplot2)

# Laddar in använda hjälpfunktioner från filen kdFunctions.R
source("Verktyg/kdFilter.R")

# Ange samplingsfrekvens (sf) till 1000
sf <- 100

# Skapa en tidsvariabel t från tiden 0 till 1s, med tidssteget 1/sf
t <- seq(from = 0, to = 1, by = 1/sf )

#Shiny UI
ui <-  fluidPage(
  titlePanel("Visualisera Filter"),
  
  sidebarLayout(
   
     sidebarPanel(
      
       
       sliderInput("f1",
                  "Låg frekvens:",
                  value = 2,
                  min = 1,
                  max = 5),
      
       sliderInput("f2",
                  "Hög frekvens:",
                  value = 10,
                  min = 5,
                  max = 30),
       plotOutput(outputId = "pltLegend")
  ), #sidebarPanel slut

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Medelvärde",
                         sliderInput("width", "Antal:",
                                     value = 11, min = 3, max = 51, step=2),
                         plotOutput(outputId = "pltMedel"),
                         sliderInput("t1", "Tid:",
                                     value = 0.5, min = 0, max = 1, step=0.01,
                                     width="100%")),
                tabPanel("Lågpass filter",
                         sliderInput("cutFre", "Brytfrekvens:",
                                     value = 5, min = 1, max = 30, step=0.5),
                         sliderInput("transWidth", "Övergångsbredd:",
                                     value = 5, min = 1, max = 30, step=0.5),
                         plotOutput(outputId = "pltLow"),
                         sliderInput("tL", "Tid:",
                                     value = 0.5, min = 0, max = 1, step=0.01,
                                     width="100%")),
                tabPanel("Högpass filter", 
                         sliderInput("cutFreH", "Brytfrekvens:",
                                     value = 10, min = 5, max = 50, step=0.5),
                         sliderInput("transWidthH", "Övergångsbredd:",
                                     value = 5, min = 1, max = 30, step=0.5),
                         plotOutput(outputId = "pltHigh"),
                         sliderInput("tH", "Tid:",
                                     value = 0.5, min = 0, max = 1, step=0.01,
                                     width="100%"))
    ) # tabsetPanel slut
    
 
  ) # mainPanel slut 
 ) # sidebarLayout slut 
)
  


# shiny server
server  <-  function(input, output, session) {
  
  funk <- reactive(sin(input$f1*2*pi*t)+sin(input$f2*2*pi*t))

  output$pltLegend = renderPlot({
    par(mar = c(0, 0, 0, 0)) # sätter marginaler
  legend("top", c("Data", "Filtrerad data", "Filterkoefficienter"), 
         col=c("blue", "red", "green"), lty=c(1,1,1), pch=c(NA,NA,1))  
  })
  
  output$pltMedel = renderPlot({
    funkA <- kdMovingAverage(funk(),input$width)
    par(mar = c(2, 1, 2, 1)) # sätter marginaler
    plot(t,funk(),  type ="l", 
         col="blue", ylab="", yaxt='n', xlab="", xaxt='n')
    lines(t,funkA,
          col="red")
    points(seq(from = input$t1-(input$width-1)/200, 
               to = input$t1+(input$width-1)/200, 
               by=0.01),
           rep(6/input$width,input$width), col="green")
    points(input$t1,funkA[round(input$t1*100)+1],col="red", pch=19)
    grid(0,2, lty=1, col="black")
  })
  
  output$pltLow = renderPlot({
    coeffL <- kdLowPassFilterCoefficients(samplingFrequency = 100, 
                                          cutOffFrequency = input$cutFre, 
                                          transitionBandWidth = input$transWidth)
    funkL <- kdFilter(funk(), coefficients = coeffL)
    coeffTimesL <- seq(from = input$tL-(length(coeffL)-1)/200,
        to = input$tL+(length(coeffL)-1)/200, 
        by=0.01)
    par(mar = c(2, 1, 2, 1)) # sätter marginaler
    plot(t,funk(),  type ="l", 
         col="blue", ylab="", yaxt='n', xlab="", xaxt='n')
    lines(t,funkL, col="red")
    lines(coeffTimesL, coeffL*10, col="green")
    points(coeffTimesL, coeffL*10, col="green")
    points(input$tL,funkL[round(input$tL*100)+1],col="red", pch=19)
    grid(0,2, lty=1, col="black")
  })
  
  output$pltHigh = renderPlot({
    coeffH <- kdHighPassFilterCoefficients(samplingFrequency = 100, 
                                          cutOffFrequency = input$cutFreH, 
                                          transitionBandWidth = input$transWidthH)
    funkH <- kdFilter(funk(), coefficients = coeffH)
    coeffTimesH <- seq(from = input$tH-(length(coeffH)-1)/200,
                       to = input$tH+(length(coeffH)-1)/200, 
                       by=0.01)
    par(mar = c(2, 1, 2, 1)) # sätter marginaler
    plot(t,funk(),  type ="l", 
         col="blue", ylab="", yaxt='n', xlab="", xaxt='n')
    lines(t,funkH, col="red")
    lines(coeffTimesH, coeffH*3, col="green")
    points(coeffTimesH, coeffH*3, col="green")
    points(input$tH,funkH[round(input$tH*100)+1],col="red", pch=19)
    grid(0,2, lty=1, col="black")
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}


# Kör shiny
shinyApp(ui, server)
