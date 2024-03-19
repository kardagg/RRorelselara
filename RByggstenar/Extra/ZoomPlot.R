# För att kunna zooma in och titta närmare på specifika delar av en stor
# vector y plottad mot x (bägge måsta ha samma längd)

# Läs in paket
library(shiny)    
library(ggplot2)

# Testvärden 
# man måste köra detta rad för rad (ej med "Run App"), annars hittas ej filen
testvärden<- read.csv2("DataFiler/VertikalAccLöpning25s.csv", header=TRUE)

# Byt ut värdena från testvärdena till några värden ni är intresserrade av
# vektorerna x och y måste bägge ha samma längd.
y <- testvärden$acceleration
x <- testvärden$tid

#Shiny UI
ui = fluidPage(
 
           sliderInput(inputId = "range", "Zoom", 0, tail(x,1), 
                       value = c(0,tail(x,1)), width="100%"),
           plotOutput(outputId = "plt"), 
 
)

# shiny server
server = function(input, output, session) {
  output$plt = renderPlot({
       plot(x,y,  type ="l", col="blue", xlim=c(input$range[1],input$range[2]))
    grid(nx = NULL, ny = NULL,
         lty = 1,      # Grid line type
         col = "gray", # Grid line color
         lwd = 1)      # Grid line width
    })

  session$onSessionEnded(function() {
    stopApp()
  })
}

# Kör shiny
shinyApp(ui, server)



