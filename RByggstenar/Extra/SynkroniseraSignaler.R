# För att kunna passa in vektor2 mot vektor1, 
# och därigenom kunna synkronisera dem

# Läs in paket
library(shiny)    
library(ggplot2)

# Testvärden 
t <- seq(from=0, to=1, by=0.01)
s <- sin(2*pi*t)
c <- cos(2*pi*t)

# Byt ut  testvärdena (s och c) till de värden ni är intresserrade av
vektor1 <- s
vektor2 <- c


#Shiny UI
ui = fluidPage(
  titlePanel("Synkronisera data"),
  sliderInput(inputId = "index", "Translatera Vektor2", -length(vektor2)+1, length(vektor2)-1, 
              value = 0, width="100%"),
  plotOutput(outputId = "plt", height=600),
  h5("Om Vektor2 måste flyttas i positiv riktning för att synkroniseras med 
     Vektor1 tar man bort lika många steg i början av Vektor1 
     för att synkronisera den med Vektor2, 
     om Vektor2 måste flyttas i negativ riktning tar man bort
     lika många steg från Vektor2 för att synkronisera den med
     Vektor1.")
  
)

# shiny server
server = function(input, output, session) {
  output$plt = renderPlot({
    plot(1:length(vektor1),vektor1,  type ="l", col="blue", 
         xlab="Index", ylab="", lwd=2)
    lines(seq(from = input$index, to = (input$index+length(vektor2)-1), by = 1),
        vektor2,  col="red", lwd=2)
    grid(nx = NULL, ny = NULL,
         lty = 1,      # Grid line type
         col = "gray", # Grid line color
         lwd = 1)      # Grid line width
    legend("topleft", c("Vektor1", "Vektor2"), lty = 1, col = c("blue", "red"), lwd=2)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}


# Kör shiny
shinyApp(ui, server)

