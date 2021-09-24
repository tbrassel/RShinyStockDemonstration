# Load packages
library(shiny)
library(tidyverse)
library(tidyquant)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)

display.mode = "showcase"

# Load data

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
          tags$head(
            tags$style(HTML("hr {border-top: 1px solid #000000;}"))
          ),
                titlePanel("Yahoo Finance Stock Viewer"),
                sidebarLayout(
                  sidebarPanel(
                    textInput(inputId = "symb", label = h3("Stock Ticker Symbol"), value = "ZS=F"),
                  
                    hr(),
                    
                    fluidRow(column(3, verbatimTextOutput("value"))),
                    
                    # Select date range to be plotted
                    dateRangeInput(inputId = "dates", strong("Date range"), start = "2008-01-01", end = NULL,
                                   min = "2001-01-01", max = NULL)
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "candleplot", height = "800px"),
                    #textOutput(outputId = "desc"),
                    tags$a(href = "https://finance.yahoo.com/lookup", "Source: Yahoo Finance", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  chartTitle <- reactive({
    req(input$symb)
    paste(input$symb, "Candlestick Chart", sep = " ")
  })
  
  dataInput <- reactive({ 
    req(input$symb)
    req(input$dates)
    validate(need(!is.na(input$dates[1]) & !is.na(input$dates[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$dates[1] < input$dates[2], "Error: Start date should be earlier than end date."))
    df <- tq_get(input$symb, get = "stock.prices", from = input$dates[1], to = input$dates[2])
  })
  
  output$candleplot <- renderPlot({
    dataInput() %>%
      ggplot(aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      geom_hline(yintercept = top_n(dataInput(), 1, wt=dataInput()$date)[[6]]) +
      labs(title =  chartTitle(), y = "Closing Price", x = "") + 
      theme_tq()
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)