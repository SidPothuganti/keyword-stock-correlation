library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(gtrendsR)
library(lmtest)
library(tseries)


# -----------------------------------------------------
# UI
#-------------------------------------------------------


ui <- fluidPage(theme = shinytheme("slate"),
  
  
  titlePanel("Word and Market Correlation"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stock", "Enter Stock Symbol:", value = "AAPL"),
      textInput("keyword", "Enter Keyword:", value = "Technology"),
      dateRangeInput("dateRange", "Date Range:", 
                     start = Sys.Date() - years(1), 
                     end = Sys.Date()),
      actionButton("goButton", "Go")
    ),
    mainPanel(
      plotlyOutput("stockPlot"),
      plotlyOutput("keywordPlot")
    )
  ),
  verbatimTextOutput("grangerResult")
)

# -----------------------------------------------------
# SERVER
#-------------------------------------------------------

server <- function(input, output) {
  
  observeEvent(input$goButton, {
    stock_data <- tq_get(input$stock, 
                         from = format(input$dateRange[1]), 
                         to = format(input$dateRange[2])) %>%
      select(date = date, stock_close = adjusted)
    
    keyword_data <- gtrends(keyword = input$keyword, 
                            time = paste(format(input$dateRange[1]), format(input$dateRange[2]), sep = " ")) %>%
      pluck("interest_over_time") %>%
      select(date = date, keyword_hits = hits)
    
    # Combine data into one data frame for Granger causality test
    combined_data <- merge(stock_data, keyword_data, by = "date", all = TRUE)

    # Check if combined_data has at least 3 columns (date, stock, keyword)
    if(ncol(combined_data) < 3) {
      output$grangerResult <- renderText({"Not enough data for Granger causality test"})
      return()
    }

    # Convert to time series
    combined_ts <- ts(combined_data[, -1], start = c(year(min(combined_data$date)), month(min(combined_data$date))), frequency = 12)

    # Perform Granger Causality Test
    granger_result <- grangertest(combined_ts[, 1] ~ combined_ts[, 2], order = 1)
    
    output$stockPlot <- renderPlotly({q
      plot_ly(data = stock_data, x = ~date, y = ~stock_close, type = 'scatter', mode = 'lines', 
              line = list(color = 'blue')) %>%
        layout(title = "Stock Price Over Time",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Stock Price (USD)"))
    })
    
    output$keywordPlot <- renderPlotly({
      plot_ly(data = keyword_data, x = ~date, y = ~keyword_hits, type = 'scatter', mode = 'lines', 
              line = list(color = 'red')) %>%
        layout(title = "Keyword Popularity Over Time",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Keyword Hits"))
    })
    
    output$grangerResult <- renderText({
      req(granger_result)  # make sure granger_result is available
      summary(granger_result)
    })
  })


}

# Run the application
shinyApp(ui = ui, server = server)




# Testing

# 
#   stock_data <- tq_get("AAPL", 
#                        from = Sys.Date() - years(1), 
#                        to = Sys.Date()) %>%
#     select(date = date, stock_close = adjusted)
#   
#   keyword_data <- gtrends(keyword = "Technology", 
#                           time = paste(format(input$dateRange[1]), format(input$dateRange[2]), sep = " ")) %>%
#     pluck("interest_over_time") %>%
#     select(date = date, keyword_hits = hits)

 # x = gtrends(keyword = "economics",
 #             time = "today 1-m")$interest_over_time
 # 
 # y = tq_get("^GSPC",
 #            get  = "stock.prices",
 #            from = today()-months(1),
 #            to   = today()) %>%
 #   select(symbol,date,close)
 # 
 # tsData <- EuStockMarkets[, 1:2]
 # 
 # comb = inner_join(x,y) |>
 #   select(date,hits, close)
 # 
 # adf.test(x$hits)
 # adf.test(y$close)
 # 
 # 
 # grangertest(hits ~ close, order = 3, data = comb)
 # 
 # grangertest(close ~ hits, order = 3, data = comb)


# # Combine data into one data frame for Granger causality test
# combined_data <- merge(stock_data, keyword_data, by = "date", all = TRUE)
# 
# # Check if combined_data has at least 3 columns (date, stock, keyword)
# if(ncol(combined_data) < 3) {
#   output$grangerResult <- renderText({"Not enough data for Granger causality test"})
#   return()
# }
# 
# # Convert to time series
# combined_ts <- ts(combined_data[, -1], start = c(year(min(combined_data$date)), month(min(combined_data$date))), frequency = 12)
# 
# # Perform Granger Causality Test
# granger_result <- grangertest(combined_ts[, 1] ~ combined_ts[, 2], order = 1)
