# Load libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)

# Shiny App proper
ui <- fluidPage(
  selectInput(
    "x",
    "Select Sleep Variable",
    choices = c(
      "sleep_total",
      "sleep_cycle",
      "awake"
    ),
    selected = "sleep_total"
  ),
  plotOutput("plot",
             width = "600px",
             height = "500px"
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    msleep %>% 
      filter(vore != "NA") %>% 
      ggplot(aes(y = vore, x = .data[[input$x]], fill = vore)) +
      geom_boxplot(alpha = 0.75) +
      labs(
        title = "Sleep Variables by Vore Type",
        x = "Total Sleep Time (hrs)",
        y = "Vore Type"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)