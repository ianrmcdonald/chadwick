library(shiny)
library(tidyverse)
#source("setup.R")

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("team_1", "Team 1", choices = franch_list$franchID, selected="SEA"),
           selectInput("team_2", "Team 2", choices = franch_list$franchID), selected="SFG")
           
  ),
  fluidRow(
            column(12, tableOutput("players")),
    
  )
  
)





server <- function(input, output, session) {
  
    
  output$players <- renderTable({
    find_all_two_teams(input$team_1, input_team_2) 

  })
    
}

shinyApp(ui, server)