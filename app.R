library(shiny)
library(dygraphs)
library(xts)
library(tidyverse)
library(DT)

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(12,
             mainPanel(
               tabsetPanel(
                 tabPanel("Tweets",
                          DT::dataTableOutput("datatable", 
                                              width = "100%",
                                              height = "600px")
                 ),
                 tabPanel("Stats",
                          dygraphOutput("dg", 
                                        width = "100%", 
                                        height = "500")
                 )
                 )
               )
             )
    )
  ),
  
  server = function(input, output) {
    
    twdata <- read.csv("to_app_data.csv",
                     stringsAsFactors = FALSE)

    twdata$date <- as.Date(twdata$date, "%Y-%m-%d")
    
    # Because emojis are broken (IFTTT cannot handle them),
    # replacing all <.*> with empty string
    twdata$text <- gsub("<.*>", "", twdata$text)
    
    # Prepare dygraph object
    # Fill in missing dates
    # http://stackoverflow.com/a/31484550
    ts <- seq.POSIXt(as.POSIXct(min(twdata$date),'%Y-%m-%d', tz="Europe/Helsinki"), 
                     as.POSIXct(max(twdata$date),'%Y-%m-%d', tz="Europe/Helsinki"), 
                     by="day")
    d <- data.frame(timestamp=ts)
    d$timestamp <- as.Date(d$timestamp)
    twdata$date <- as.Date(twdata$date)
    data_with_missing_dates <- full_join(d,twdata,by=c("timestamp"="date"))
  
    dailysums <- data_with_missing_dates %>%
      mutate(day = format(timestamp, "%d"), 
             month = format(timestamp, "%m"), 
             year = format(timestamp, "%Y")) %>%
      group_by(day, month, year) %>%
      summarise(favs = n())
    
    stats_sum <- dailysums %>% 
      mutate(date = as.POSIXct(paste(day, month, year, sep = "-"), 
                               format = "%d-%m-%Y" ), tz=Sys.timezone(),
             weekday = weekdays(date)) 
    
    stats <- stats_sum[,c("date", "favs")]
    
    rownames(stats) <- stats[[1]]

    stats.xts <- as.xts(stats, dateFormat = "Date")
    
    dygraph_plot <- dygraph(stats.xts) %>% 
      dyOptions(stepPlot=TRUE,
                pointSize=7,
                fillGraph = TRUE,
                colors = RColorBrewer::brewer.pal(3, "Set1"),
                strokeWidth = 3, 
                strokePattern = "dashed") %>% 
      dyLegend(width = 200,
               show = "follow",
               hideOnMouseOut = FALSE)
    
    
    output$dg <- renderDygraph({
      
      dygraph_plot
      
      })
    
    
    # Prepare tweet status data for DT table rendering
    twdata$avatar <- sapply(twdata$tweet, function(x) strsplit(x, "/")[[1]][4])
    
    twdata <- twdata %>% 
      mutate(Link = paste0('<a href="', tweet, '">', avatar, '</a>')) %>% 
      rename(Date = date,
             Status = text) %>% 
      select(Link, Date, Status)
  
      
    output$datatable <- DT::renderDataTable({
      
      twdata
      
      }, escape = FALSE)
    
  }
)