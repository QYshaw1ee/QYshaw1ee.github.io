library(shiny)
library(dplyr)
library(ggplot2)

#read data
Tidy_calls_monthly<-read.csv("Tidy_calls_monthly.csv")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Friendship Calls"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h5("Total number of calls per each county in California"),
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "County",
                  label = "Choose a county:",
                  choices = Tidy_calls_monthly %>% distinct(Counties))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( 
      h3("April to July 2020"),
      
      # Output: Histogram ----
          plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    Tidy_calls_monthly%>%filter(Counties==input$County) %>%
    ggplot(aes(x=Month,y=`Number.of.Calls`,group=Counties)) + geom_point() + geom_line() + theme_bw()
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
