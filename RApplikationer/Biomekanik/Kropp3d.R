library(shiny)
library(rgl)

# Om man kör rad för rad används R_Rörelselära som rot
# men om man använder knappen "Run App" ovan använder shiny
# den mapp R koden ligger isom rot 
# därför behövsnedanstående kod för att hitta R_Rörelselära mappen
# oavsett hur man aktiverar appen
rotDir <- getwd()
if(tail(unlist(strsplit(rotDir,"/")),n=1)!="R_Rörelselära"){
  rotDir <- dirname(dirname(rotDir))
} 

# Laddar in använda hjälpfunktioner från filen kdSagittalMan.R
source(paste(rotDir,"Verktyg/kdKropp3D.R",sep="/"))


#kdDrawBody3D(c(0,0,0,0,45,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), inShiny=FALSE)

# Shiny app ---------------------------


# Define the UI
ui <- fluidPage(

  
  #UI
 # titlePanel("Kropp 3D"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("objectSelector", "Rotera kroppsdel:",
                  choices = c("Huvud"=5,
                              "Bål"=4,
                              "Vänster överarm"=9, 
                              "Höger överarm"=12,
                              "Vänster underarm"=10,
                              "Höger underarm"=13,
                              "Vänster hand"=11,
                              "Höger hand"=14,
                              "Vänster lår"=3, 
                              "Höger lår"=6,
                              "Vänster underben"=2, 
                              "Höger underben"=7,
                              "Vänster fot"=1, 
                              "Höger fot"=8), selected = "Huvud"),
      sliderInput("alphaSlider", "Kring x-axel (G1, L3)", min = -180, max = 180, value = 0, ticks = FALSE),
      sliderInput("betaSlider", "Kring y-axel (G2, L2)", min = -180, max = 180, value = 0, ticks = FALSE),
      sliderInput("gammaSlider", "Kring z-axel (G3, L1)", min = -180, max = 180, value = 0, ticks = FALSE),
 
     fluidRow(column(4,checkboxInput("showCM", "TP", TRUE)),
              column(4,checkboxInput("showF", "Fg", FALSE)),
              column(4,checkboxInput("showJ", "J", FALSE))
              ),
     fluidRow(column(4,h5("Visa axlar:")),
              column(4,checkboxInput("globalAxes", "Globala", TRUE)),
              column(4,checkboxInput("localAxes", "Lokala", FALSE))
              ),
     sliderInput("opacity", "Transparens", min = 0, max = 1, value = 0, ticks = FALSE),
     fluidRow(
       column(6,numericInput("mass", 
                             label = "Vikt(kg)", 
                             value = 90, 
                             min = 1, 
                             max = 250, 
                             step = 1)),
       column(6,numericInput("length",
                             label = "Längd(cm)", 
                             value = 180, 
                             min = 1, 
                             max = 250, 
                             step = 1))
     ),
     selectInput("positionSelector", "Position:",
                 choices = c("Anatomisk grundposition",
                             "Som vald kroppsdel",
                             "Löpsteg",
                             "Sittande"), 
                 selected = "Anatomisk grundposition"),
     fluidRow(column(6,actionButton("savePosition", "Spara position")),
              column(6,actionButton("zeroSegment", "Nollställ kroppsdel"))
              )

    ),
    mainPanel(
      div(style = "width: 100%; height: 90vh; margin: auto;", # Container to control size and alignment
          rglwidgetOutput("rglPlot", width = "100%", height = "100%")) # Use 100% of the container's size
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Reactive values for angles
  angles <- reactiveValues(alphaList = rep(0, 14), betaList = rep(0, 14), gammaList = rep(0, 14))
  
  # Update the slider values when a new object is selected
  observeEvent(input$objectSelector, {
    idx <- as.numeric(input$objectSelector)
    updateSliderInput(session, "alphaSlider", value = angles$alphaList[idx])
    updateSliderInput(session, "betaSlider", value = angles$betaList[idx])
    updateSliderInput(session, "gammaSlider", value = angles$gammaList[idx])
  })
  
  # Update the all angle values when a new position is selected
  observeEvent(input$positionSelector, {
    if(input$positionSelector =="Anatomisk grundposition")
    {
      angles$alphaList <- rep(0, 14)
      angles$betaList <- rep(0, 14)
      angles$gammaList <- rep(0, 14)
    } 
    else if(input$positionSelector=="Som vald kroppsdel")
    {
      idx <- as.numeric(input$objectSelector)
      angles$alphaList <- rep(angles$alphaList[idx], 14)
      angles$betaList <- rep(angles$betaList[idx], 14)
      angles$gammaList <- rep(angles$gammaList[idx], 14)
    }
    else if(input$positionSelector=="Löpsteg")
    {
      angles$alphaList <- c(-2.9, -5, -35.3, 0, 0, 23.8, 97.9, -87.8, 58, -11, 0, 166, -96, -71)
      angles$betaList <- c(0, 0, 0, 0, 0, 0, 0, -180, 0, -6, 0, -180, 0, 60)
      angles$gammaList <- c(0, 0, 0, 0, 0, 0, 0, -180, 0, 0, 272.9, 180, 0, 10)
    }
    else if(input$positionSelector=="Sittande")
    {
      angles$alphaList <- c(0, 2, -88, 0, 0, -86.4, 1, 1, -20, -21, 0, -20, 0, 0)
      angles$betaList <- c(0, 0, 0, 0, 0, -360, -1, 0, 0, 72, 0, 0, -75, 50)
      angles$gammaList <- c(0, 0, 0, 0, 0, 0, 180, 0, 0, 0, 180, 0, -27, -180)
    }

    idx <- as.numeric(input$objectSelector)
    updateSliderInput(session, "alphaSlider", value = angles$alphaList[idx])
    updateSliderInput(session, "betaSlider", value = angles$betaList[idx])
    updateSliderInput(session, "gammaSlider", value = angles$gammaList[idx])
  })
  
  # Print the current angles when save button is pressed
  observeEvent(input$savePosition, {
    alphaString <- paste("angles$alphaList <- c(",toString(angles$alphaList),")", sep="")
    betaString <- paste("angles$betaList <- c(",toString(angles$betaList),")", sep="")
    gammaString <- paste("angles$gammaList <- c(",toString(angles$gammaList),")", sep="")
    print(alphaString)
    print(betaString)
    print(gammaString)
   
    writeClipboard(paste(alphaString,betaString,gammaString,sep="\n"))
  })
  
  observeEvent(input$zeroSegment, {
    updateSliderInput(session, "alphaSlider", value = 0)
    updateSliderInput(session, "betaSlider", value = 0)
    updateSliderInput(session, "gammaSlider", value = 0)
  })
  
  # Update angles when sliders change
  observe({
    idx <-  as.numeric(input$objectSelector)
    angles$alphaList[idx] <- input$alphaSlider
    angles$betaList[idx] <- input$betaSlider
    angles$gammaList[idx] <- input$gammaSlider
  })
  
  # Render the 3D plot
  output$rglPlot <- renderRglwidget({

    # Clear the scene before drawing
    clear3d()
  
    # Call the draw function using the specific angles
    kdDrawBody3D(angles$alphaList, 
                 angles$betaList, 
                 angles$gammaList, 
                 mass=input$mass,
                 length=input$length,
                 inShiny = TRUE,
                 showGlobalAxes = input$globalAxes,
                 showLocalAxes = input$localAxes,
                 selectedSegment=as.numeric(input$objectSelector),
                 alphaChannel = 1-input$opacity,
                 showCenterOfGravity=input$showCM,
                 showForce = input$showF,
                 showJ = input$showJ
                 )
    
    # Return the rgl widget
    rglwidget()
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application
shinyApp(ui, server)
