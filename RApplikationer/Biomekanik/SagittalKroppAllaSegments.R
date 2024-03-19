# Om man kör rad för rad används R_Rörelselära som rot
# men om man använder knappen "Run App" ovan använder shiny
# den mapp R koden ligger i som rot 
# därför behövsnedanstående kod för att hitta R_Rörelselära mappen
# oavsett hur man aktiverar appen
rotDir <- getwd()
if(tail(unlist(strsplit(rotDir,"/")),n=1)!="R_Rörelselära"){
  rotDir <- dirname(dirname(rotDir))
} 

# Laddar in använda hjälpfunktioner från filen kdSagittalMan.R
source(paste(rotDir,"Verktyg/kdKroppSagittal.R",sep="/"))

#Testa funktionen
kdDrawSagittalBodyAllSegments(c(0,0,0,0,0,0,0,0,0,0,0,0,0),70,180,fixCenterOfMass = TRUE)

# Shiny app ---------------------------
# Läs in paket
library(shiny)   

#Shiny UI
ui = fluidPage(
  # Custom CSS to make the numeric input shorter
  tags$head(
    tags$style(HTML("
            .shiny-input-container .form-control[type='number'] {
                width: 80px; /* Adjust the width as needed */
            }
        "))
  ),
  fluidRow(
    column(4,
           
           sliderInput(inputId = "vinkelHead", "Huvud(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelTrunk", "Bål(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelPelvis", "Bäcken(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelRUpperArm", "Höger överarm(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelLUpperArm", "Vänster överarm(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelRLowerArm", "Höger underarm(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelLLowerArm", "Vänster underarm(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelRThigh", "Höger lår(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelLThigh", "Vänster lår(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelRShank", "Höger underben(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelLShank", "Vänster underben(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelRFot", "Höger fot(°)", -180, 
                       180, value = 0, width="100%"),
           sliderInput(inputId = "vinkelLFot", "Vänster fot(°)", -180, 
                       180, value = 0, width="100%"),
           fluidRow(
             column(4,checkboxInput("showCenterOfGravity", "Tyngdpunkt", TRUE)),
             column(4,checkboxInput("showGravityForce", "Tyngdkraft", TRUE)),
             column(4,checkboxInput("showMomentOfInertia", "Tröghetsmoment", TRUE))
           ),
           fluidRow(
             column(3,numericInput("weight", 
                                   label = "Vikt(kg)", 
                                   value = 70, 
                                   min = 1, 
                                   max = 150, 
                                   step = 1)),
             column(3,numericInput("length",
                                   label = "Längd(cm)", 
                                   value = 180, 
                                   min = 1, 
                                   max = 250, 
                                   step = 1)),
             column(6,checkboxInput("lockCM", "Fix tyngdpunkt", TRUE))
           )
    ),
    column(8, 
           plotOutput(outputId = "sagittalMan", height="95vh") 
           #vw=% viewport height
    )
  )
)

# shiny server
server = function(input, output, session) {
  output$sagittalMan = renderPlot({
    kdDrawSagittalBodyAllSegments(c(input$vinkelRFot,input$vinkelRShank,input$vinkelRThigh,
                            input$vinkelPelvis,input$vinkelTrunk,input$vinkelHead,
                            input$vinkelRUpperArm,input$vinkelRLowerArm,
                            input$vinkelLThigh,input$vinkelLShank,input$vinkelLFot,
                            input$vinkelLUpperArm,input$vinkelLLowerArm),
                            input$weight,input$length,
                            input$lockCM, input$showCenterOfGravity, 
                            input$showGravityForce,
                            input$showMomentOfInertia)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Kör shiny
shinyApp(ui, server)

