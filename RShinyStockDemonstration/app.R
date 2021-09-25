# Load packages
library(shiny)
library(tidyverse)
library(tidyquant)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
          #Make horizontal rules show up in the UI
          tags$head(
            tags$style(HTML("hr {border-top: 1px solid #000000;}"))
          ),
                titlePanel("Yahoo Finance Stock Viewer"),
                sidebarLayout(
                  sidebarPanel(
                    #Get the user's desired stock symbol, defaults to Soybean Futures
                    textInput(inputId = "symb", label = h3("Stock Ticker Symbol"), value = "ZS=F"),
                  
                    #Add a divider
                    hr(),
                    
                    
                    # Select date range to be plotted, set a boundary for the limit's of Yahoo finances data
                    dateRangeInput(inputId = "dates", strong("Date range"), start = "2021-08-01", end = NULL,
                                   min = "2001-01-01", max = NULL),
                    
                    hr(),
                    
                    # Add a checkbox to toggle the horiozontal line
                    checkboxInput(inputId = "href",
                                  label = strong("Show a horizotal reference line at today's closing price."),
                                  value = FALSE),
                    
                  ),
                  
                  # Output: Candleplot and reference
                  mainPanel(
                    plotOutput(outputId = "candleplot", height = "800px"),
                    #textOutput(outputId = "desc"),
                    tags$a(href = "https://finance.yahoo.com/lookup", "Source: Yahoo Finance", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  # Change the chart title dynamically based in the ticker symbol input
  chartTitle <- reactive({
    req(input$symb)
    paste(input$symb, "Candlestick Chart", sep = " ")
  })
  
  # Change the data input dynamically based on user input, and check if that input makes sense
  dataInput <- reactive({ 
    req(input$symb)
    req(input$dates)
    validate(need(!is.na(input$dates[1]) & !is.na(input$dates[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$dates[1] < input$dates[2], "Error: Start date should be earlier than end date."))
    df <- tq_get(input$symb, get = "stock.prices", from = input$dates[1], to = input$dates[2])
  })
  
  # Render a candlestick chart dynamically, and draw a line at the current price. 
  # To do: hover to see date information
  output$candleplot <- renderPlot({
    # Different plot depending on a toggled button
    if(input$href){
      dataInput() %>%
        ggplot(aes(x = date, y = close)) +
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        geom_hline(yintercept = top_n(dataInput(), 1, wt=dataInput()$date)[[6]]) +
        labs(title =  chartTitle(), y = "Closing Price", x = "") + 
        theme_tq()
    } else {
      dataInput() %>%
        ggplot(aes(x = date, y = close)) +
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        labs(title =  chartTitle(), y = "Closing Price", x = "") + 
        theme_tq()
    }
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)