library(shiny)
library(forecast)
library(magrittr)
library(timeDate)


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("XAUUSD Data Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("upload_file", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a column ----
      selectInput("ohlcv", "Select a column:",
                  choices = c('select','open','high','low','close','volume'),
                  selected = 'select'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a graph ----
      selectInput("graphs", "Select a graph:",
                  choices = c('Select','TimeSeries','DecomposedSeries'),
                  selected = 'Select'),
      
      # Horizontal line ----
      tags$hr(),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("dataTable"),
      plotOutput("graphs"),
      
    )
    
  )
  
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  getContents <- reactive({
    # input$upload_file will be NULL initially. After the user selects and uploads a file
    req(input$upload_file)
    uf <<- read.csv(input$upload_file$datapath)
    uf[is.na(uf)] <- 0
    uf$date #<- as.Date(uf$date, format = "%Y-%m-%d")
    return(uf)
  })
  
  output$dataTable <- renderTable({		
    head(getContents())
  })
  
  output$graphs <- renderPlot({
    prices <<- create_ts(which(colnames(getContents()) == getColOpt(input$ohlcv)),getWorkingdates())
    actualSeries <<- getContents()[,which(colnames(getContents()) == getColOpt(input$ohlcv))]
    
    if(input$graphs == 'TimeSeries'){
      plot.ts(prices, main = "Time-Series plot", col = "blue")
    } else if(input$graphs == 'DecomposedSeries'){
      stldecomp = stl(prices, s.window="periodic")
      plot(stldecomp)
    }
  })
  
  error <- function(){
    print("enter valid input")
  }
  
  
  ## other functions
  
  getWorkingdates <- function(){
    dates <- seq(as.Date("2019-01-01"),as.Date("2021-12-06"),by = "day")
    week_days <- dates[!is.weekend(dates)]
    workingdates <- week_days[!getUSHolidays(week_days)]
    return(workingdates)
  }
  
  is.weekend <- function(x) {
    ((as.numeric(x)-2) %% 7) < 2
  }
  
  getUSHolidays <- function(x) {
    years = as.POSIXlt(x)$year+1900
    years = unique(years)
    holidays <- NULL
    for (y in years) {
      if (y >= 1885)
        holidays <- c(holidays, as.character(USNewYearsDay(y)))
      if (y >= 1885)
        holidays <- c(holidays, as.character(USIndependenceDay(y)))
      if (y >= 1885)
        holidays <- c(holidays, as.character(USThanksgivingDay(y)))
      if (y >= 1885)
        holidays <- c(holidays, as.character(USChristmasDay(y)))
    }
    holidays = as.Date(holidays,format="%Y-%m-%d")
    ans = x %in% holidays
    return(ans)
  }
  
  getColOpt <- function(cv){
    if(cv == 'select'){
      print("Choose a column")
    } else if(cv == 'open'){
      return("open")
    } else if(cv == 'high'){
      return("high")
    } else if(cv == 'low'){
      return("low")
    } else if(cv == 'close'){
      return("close")
    } else if(cv == 'volume'){
      return("volume")
    }
  }
  
  create_ts <- function(col_idx, wd){
    wd <- getWorkingdates()
    ## Create a time series object
    if (input$ohlcv == 'select'){
      print("select please")
    } else {
      i_ts <- as.numeric(getContents()[,col_idx]) %>%
        tsclean(replace.missing = TRUE, lambda = NULL) %>%
        ts(start = c(2019, as.numeric(format(wd[1], "%j"))),
           frequency = 257)
      return(i_ts)
    }
  }
  
}

# Create Shiny app ----
shinyApp(ui, server)





















#library(dplyr)
#library(zoo)
#library(tseries)
