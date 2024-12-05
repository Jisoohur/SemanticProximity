#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: Specilized Science with BERTopic 
##  goal : Extract data source from WoS
##  Data set: WoS
##  Time Span: 
##  Variables
##      Input: 
##      Output:  
##  Methodology: BERTopic
##  Time-stamp: :  
##  Notice :

# Data generated from SemanticProximity_R

library(shiny)
library(dplyr)
library(magrittr)
library(tmap)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(wordcloud)
library(DT)

# Define UI for application that draws a histogram
qsp_shiny_ui <- shinyUI(fluidPage(
  
  titlePanel("Semantic Proximity: Quantum Technology in EU"),
  
  sidebarLayout(
    sidebarPanel(
      # map
      leafletOutput(outputId = "map", width = "100%"), # "width="100%", height = "100%" width = "100vh"
      h6("*skyblue: Top 100 Qunatum Tech Science REgions, orange: selected regions"),
      
      selectInput(inputId = "NUTS_ID", 
                  label = "Choose a Region",
                  choices = c("UKM34", "PL633", "ITI43", "DE212", "DE21H", "DK042", "FR716", "UKE32", "ES511", "BE242", "CH040", "NL414",
                              "DE712", "CH031", "DE144", "FR101", "FR104", "ITH55", "LT00A", "PT170", "ITH36", "ITC11", "ITI17", "DED21",
                              "DEA26", "NL310", "DK011", "ES300", "PL415", "PL213", "ITI14", "FR714", "UKJ25", "DE300", "PL127", "HU101",
                              "UKD33", "DEA23", "DE600", "DE232", "UKK11", "FR301", "DE711", "PL514", "FI1B1", "UKI31", "AT332", "ES213",
                              "SE224", "DEA2D", "UKI32", "SE121", "CZ010", "DE111", "FR623", "DEG03", "UKM25", "DE911", "NL337", "NL333",
                              "BE100", "UKH12", "DE131", "CH011", "SE110", "UKE42", "ITC33", "DEA51", "IE021", "UKF14", "ITC4C", "DK012",
                              "DE252", "CZ071", "ITH20", "DEC01", "DE929", "UKJ14", "DE263", "UKE21", "BE211", "CH033", "DE125", "DED2C",
                              "DE122", "FR824", "AT130", "ES523", "CH013", "UKJ32", "SE232", "NL326", "ES212", "UKM22", "CZ064", "DEB35",
                              "ITF33", "ITH44", "DED51", "FR421")),
      sliderInput("period",
                  "Choose a period",
                  min=1, max=6, value=1, step=1),

      h5("Presented by"),
      tags$a(href="https://awekim.github.io/portfolio/", "Keungoui Kim (Ph. D.)"),tags$br(),
      tags$a(href="", "Jisoo Hur (Ph. D.)"),tags$br(),
      tags$a(href="https://www.linkedin.com/in/dieterfkogler/", "Dieter F. Kogler (Ph. D.)"),tags$br(),
      
      tags$br(),tags$br(),
      tags$span("All analytic sources are available "),
      tags$a(href="https://github.com/awekim/SemanticProximity", "here")
      
    ),
    
    mainPanel(
      
      h4("Publication Topic Modelling (BERTopic + ChatGPT)"),
      tableOutput(outputId = "pubtmtble"),
      
      h4("Quantum Techn Publication Topic Modelling (BERTopic + ChatGPT)"),
      tableOutput(outputId = "qpubtmtble"),#, # "width="100%", height = "100%" 

      h4("Period-trend of BERT Similarities"),
      plotOutput(outputId = "sptrend")

    )
  )
))

# Define server logic required to draw a histogram
qsp_shiny_server <- function(input, output) {
  
  # Map
  datasetInput_map <- reactive({
    load(file="ind_sf.RData")
    load(file="eu.reg.map.RData")
    eu.reg.map <- eu.reg.map +
      tm_shape(ind_sf %>% filter(NUTS_ID==input$NUTS_ID)) +
      # tm_compass() + tm_scale_bar() +  tmap_options(check.and.fix = TRUE) +
      # tm_layout(frame = FALSE, legend.outside = FALSE, legend.show = FALSE) +
      # tmap_options(output.size = 10) +
      tm_fill("orange") + tm_borders("grey25", alpha=.5)
    # tm_view(set.zoom.limits = c(10, 20))
  })
  
  output$map <- renderLeaflet({
    datasetInput_map() %>% tmap_leaflet() %>%
      setView(lat=55, lng=15, zoom=3)# setView(lat=57.5, lng=45, zoom=3)
  })
  
  # Publication table
  datasetInput_pubtmtble <- reactive({
    load(file="qsp_pub_bertopic.RData")
    qsp_pub_bertopic %<>%  
      filter(NUTS_ID==input$NUTS_ID & period == input$period) %>%
      rename('Generated Topic'=content)
  })
  
  output$pubtmtble <- renderTable({
    datasetInput_pubtmtble() 
  })
  
  # BERTopic table
  datasetInput_qpubtmtble <- reactive({
    load(file="qsp_quant_pub_bertopic.RData")
    qsp_quant_pub_bertopic %<>%  
      filter(NUTS_ID==input$NUTS_ID & period == input$period) %>%
      rename('Generated Topic'=content)
  })
  
  output$qpubtmtble <- renderTable({
    datasetInput_qpubtmtble() 
  })
  
  # Semantic Proximity Trend line plot
  datasetInput_sptrend <- reactive({
    load(file="qsp_bert_similarity.RData")
    qsp_bert_similarity %<>%  
      filter(NUTS_ID==input$NUTS_ID) 
  })
  
  output$sptrend <- renderPlot({
    datasetInput_sptrend() %>%
      ggplot(aes(x=period, y=bert_similarity, group=1)) +
      geom_line() + 
      labs(x = "Period", y = "BERT Similarities") +
      theme_bw()
  })
  
}

# Run the application 
shinyApp(ui = qsp_shiny_ui, server = qsp_shiny_server)