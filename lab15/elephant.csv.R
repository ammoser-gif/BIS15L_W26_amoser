library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

elephants <- read_csv("data/elephants_data/elephants.csv") %>% clean_names()

library(shiny)

ui <- dashboardPage(
  
  dashboardHeader(title = "Elephant Variables by Sex"),
  
  dashboardSidebar(
    selectInput(
      "x",
      "Select Age or Height",
      choices = c("age", "height"),
      selected = "age"
    )
  ),
  
  dashboardBody(
    plotOutput("plot")
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    x_label <- if (input$x == "age") {
      "Age (yrs)"
    } else {
      "Height (cm)"
    }
    
    ggplot(data = elephants, aes(x = .data[[input$x]], y = sex, fill = sex)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        x = x_label,
        y = "Sex"
      ) +
      guides(fill = "none") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)