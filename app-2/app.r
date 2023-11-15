# Install required packages if not already installed
# install.packages(c("shiny", "dplyr"))

# Load required libraries
library(shiny)
library(dplyr)
library(DT)


Tidy_calls_monthly<-read.csv("Tidy_calls_monthly.csv")

# Define UI
ui <- fluidPage( 
  tags$html(lang = "en",
                           
  titlePanel("Calls Summary by County"),  
  tabsetPanel(
    tabPanel("Overview", 
             mainPanel(DTOutput("summary_table"))
             ),
    tabPanel("Top 10 Counties",mainPanel(DTOutput("Most_calls"))
             ),
    tabPanel("Bottom 10 Counties", mainPanel(DTOutput("Least_calls"))
             )
  )
)
)

# Define server
server <- function(input, output) {
  # Calculate summary statistics for each county
  summary_data <- reactive({
    Tidy_calls_monthly %>%
      group_by(Counties) %>%
      summarize(
        Most = max(`Number.of.Calls`),
        Least = min(`Number.of.Calls`),
        Medium = median(`Number.of.Calls`),
        Average = mean(`Number.of.Calls`)
      )
  })
  
  # Calculate the top 10 counties
  top10<- reactive({
    Tidy_calls_monthly %>%
      group_by(Counties) %>%
      summarise(Total_Calls = sum(`Number.of.Calls`)) %>%
      arrange(desc(Total_Calls)) %>%
      head(10)
  })
  
  bottom10_data<- reactive({
    Tidy_calls_monthly %>%
      group_by(Counties) %>%
      summarise(Total_Calls = sum(`Number.of.Calls`)) %>%
      arrange(Total_Calls) %>%
      head(10)
  })
  
  # Render the summary table
  output$summary_table <- renderDT({
    datatable(summary_data(),options = list(scrollX = TRUE))
  })
  
  output$Most_calls <- renderDT({
    datatable(top10(),options = list(scrollX = TRUE))
  })
  
  output$Least_calls<- renderDT({
    datatable(bottom10_data(),options = list(scrollX = TRUE))
  })

  }

# Run the Shiny app
shinyApp(ui, server)
