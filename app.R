library(shiny)
library(tidyverse)
source("setup.R", local=TRUE)

ui <- fluidPage(
  
  titlePanel("Immaculate Grid Cheatmobile"),
  
      tabsetPanel(
          id="tabset",
          tabPanel("Two Team Players",
            fluidRow(
                column(6,
                  selectInput("team_1", "Team 1", choices = franch_list$franchName, selected="New York Mets"),
                  selectInput("team_2", "Team 2", choices = franch_list$franchName, selected="Boston Red Sox"),
                ),
            ),
    
            fluidRow(
                  column(12, dataTableOutput("players")),
    
                )
              )  
            )
)

server <- function(input, output, session) {
  
  find_them <- reactive( {
     t_1 <- lookup_franchise_id(input$team_1)
     t_2 <- lookup_franchise_id(input$team_2)
     find_all_two_teams(t_1, t_2)
   })
  
  output$players <- renderDataTable(find_them())

}

shinyApp(ui, server)