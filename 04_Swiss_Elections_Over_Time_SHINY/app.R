

# load packages -----------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(magrittr)
library(glue)

theme_set(theme_minimal())





# User interface (aka front end) ------------------------------------------

ui <- fluidPage(
  
  ### select a theme
  theme = shinytheme("paper"),
  
  # Application title
  br(),
  br(),
  titlePanel(
    
    windowTitle = "Nationalratswahlen 1971 bis 2019", 
    title = p(strong("Nationalratswahlen in der Schweiz von 1971 bis 2019"), br(), "Parteistärke auf Gemeindeebene")
    
  ),
  
  
  br(),
  br(),
  
  ###### 1. SIDEBAR PANEL ###### 
  
  sidebarLayout(
    
    selectInput("party",
                strong("Partei:"),
                choices = c("SVP", "FDP", "Mitte", 
                            "GLP", "Grüne", "SP"),
                multiple = FALSE,
                selected = "SVP"),
    
    # Show a plot of the generated distribution
    mainPanel(
      title = "Grafik",
      imageOutput(outputId = "gif_output"),
      
      # unfortunately something like rep(glue("{br()}"), 27) does not work...
      br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(),
      br(), br(), br(),
      hr(),
      
      helpText("Die Daten wurden am 10.10.2023 von der Webseite des Bundesamtes für Statistik (BfS) heruntergeladen:", br(),
               "https://www.bfs.admin.ch/bfs/de/home/statistiken/politik/wahlen.assetdetail.12967001.html"),
      helpText("Shiny-App by ©Luca Keiser | Oktober, 2023", br(),
               "Code: https://github.com/LucaKeiser/Swiss_Elections_Over_Time")
      
    )
  )
)





# Server (aka back end) ---------------------------------------------------

server <- function(input, output) {
  
  output$gif_output <- renderImage({
    
    if(input$party == "SVP") {
      list(src = "Data/map_SVP.gif",
           contentType = 'image/gif',
           height = 1000,
           width = 1500)  
    } else if(input$party == "FDP") {
      list(src = "Data/map_FDP.gif",
           contentType = 'image/gif',
           height = 1000,
           width = 1500)
    } else if(input$party == "Mitte") {
      list(src = "Data/map_Mitte.gif",
           contentType = 'image/gif',         
           height = 1000,
           width = 1500)  
    } else if(input$party == "GLP") {
      list(src = "Data/map_GLP.gif",
           contentType = 'image/gif',
           height = 1000,
           width = 1500)  
    } else if(input$party == "Grüne") {
      list(src = "Data/map_GPS.gif",
           contentType = 'image/gif',
           height = 1000,
           width = 1500)  
    } else {
      list(src = "Data/map_SP.gif",
           contentType = 'image/gif',         
           height = 1000,
           width = 1500)   
    }
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
