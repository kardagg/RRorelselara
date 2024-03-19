library(shiny)
library(colourpicker) 

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
source(paste(rotDir,"Verktyg/kdUtility.R",sep="/"))



# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Skapa linjal"),
  
  # Controls to specify the ruler options
  fluidRow(
    column(4, numericInput("antalSteg", "Antal skalsteg:", 
                           15, min = 1, step = 1),
           
          numericInput("magnitude", "Tiopotens:", 
                                  0, min = -3, max= 3, step = 1),
           numericInput("aspect", "Bildförhållande:", 
                        20, min = 2, step = 1),
           colourInput("col", "Färg", "lightgreen")
   ),
    column(4,  selectInput(
      inputId = "prefix", 
      label = "Prefix:", 
      choices = c("", "G", "M", "k", "h", "d", "c", "m", "mu", "n", "p"), 
      selected = NULL, 
      multiple = FALSE, 
      selectize = TRUE
    ),
    selectInput(
      inputId = "unit", 
      label = "Enhet:", 
      choices = c("m", "N", "m/s", "Ns", "Nm", "m/s\ub2"), 
      selected = NULL, 
      multiple = FALSE, 
      selectize = TRUE
    ),
    numericInput("fontSize", "Teckenstorlek:", 
                 .8, min = .2, step = .1),
    HTML('<p style="font-size: 20px; color: gray; font-weight: bold; font-style: italic;
         ">Högerklicka på linjalen för att kopiera eller spara den som en fil.</p>')
    )

  ),
  
  # Main panel to display the ruler taking the full width of the browser window
  fluidRow(
    column(8, align = "center",
           uiOutput("dynamicPlotOutput")
           )
    #xxvw xx% of viewport width
  )
)

# Server logic to draw the ruler
server <- function(input, output) {
  
  # Reactive expression to calculate height based on input
  plotHeight <- reactive({
    ceiling(96 / input$aspect)  
  })
  
  # Dynamically generate plotOutput with calculated height
  output$dynamicPlotOutput <- renderUI({
    plotOutput("rulerPlot", width = "96vw", height = paste0(plotHeight(), "vw"))
  })
  
  output$rulerPlot <- renderPlot({
  #  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    kdRitaLinjal(nr_major_ticks = input$antalSteg, 
                 majorIntervall = 10^input$magnitude , 
                 unit=input$unit, 
                 prefix=input$prefix, 
                 bg_color=input$col, 
                 tick_color = "black", 
                 aspectRatio=input$aspect, 
                 fontSize = input$fontSize)
  }, res = 96)
  
}


# Run the application 
shinyApp(ui = ui, server = server)

