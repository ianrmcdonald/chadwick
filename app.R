library(shiny)
#library(tidyverse)
#source("setup.R", local=TRUE)

ui <- fluidPage(

    titlePanel("Immaculate Grid Cheatmobile"),
    
    tabsetPanel(
        id="tabset",
        
        
        tabPanel("Two Team Players",  ##Panel 1
                 
            fluidRow(
                column(6,
                    selectInput("team_1", "Team 1", choices = franch_list$franchName, selected="New York Mets"),
                    selectInput("team_2", "Team 2", choices = franch_list$franchName, selected="Boston Red Sox"),
                ),
            ),
            
            
            
            fluidRow(
                column(12, 
                    dataTableOutput("players")),
                ),
            ),
        
        
        
        tabPanel("Team and Season Goal", ##Panel 2
                 
                 
            fluidRow(
                column(6,
                    selectInput("team", "Team", choices = franch_list$franchName, selected="New York Mets"),
                    selectInput("stat", "Stat Category", choices = stat_categories, selected="H"),
                    numericInput("milestone", "Milestone", value = 20, min = 0),
                )
            ),
            
            fluidRow(
              column(12, 
                     dataTableOutput("players_goal")),
            )
        
                 
        ) 
                 
        
    )  
)





server <- function(input, output, session) {
  
    find_them <- reactive({
        t_1 <- lookup_franchise_id(input$team_1)
        t_2 <- lookup_franchise_id(input$team_2)
        find_all_two_teams(t_1, t_2) %>% 
            select(-playerID)
    })
    
    find_them_goal <- reactive({
      if(input$stat %in% pitching_stat_categories) class <- "pitching"
      if(input$stat %in% batting_stat_categories) class <- "batting"
      
      
      t_1 <- lookup_franchise_id(input$team)
      
      threshhold_team(t_1, input$milestone, input$stat, class) 
      
    })
    
    output$players <- renderDataTable(find_them())
    output$players_goal <- renderDataTable(find_them_goal())

}

shinyApp(ui, server)