library(shiny)
#library(tidyverse)
source("setup.R", local=TRUE)

ui <- fluidPage(

    titlePanel("Immaculate Grid Cheatmobile"),
    
    tabsetPanel(
        id="tabset",
        
        tabPanel("Two Team Players",  ##Panel 1
                 
            fluidRow(
                column(6,
                    selectInput("team_1", "Team 1", choices = franch_list$franchName, selected="New York Mets"),
                    selectInput("team_2", "Team 2", choices = franch_list$franchName, selected="New York Yankees"),
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
                    selectInput("team", "Team", choices = franch_list$franchName, selected="New York Yankees"),
                    selectInput("stat", "Stat Category", choices = stat_categories, selected="H"),
                    numericInput("milestone", "Milestone", value = 200, min = 0),
                )
            ),
            
            fluidRow(
                column(12, 
                    dataTableOutput("players_goal")),
            )
        
                 
        ), 
        
        tabPanel("Team and Career Goal",  #Panel 3
            fluidRow(
                column(6,
                    selectInput("team_c", "Team", choices = franch_list$franchName, selected="New York Yankees"),
                    selectInput("stat_c", "Stat Category", choices = stat_categories, selected="H"),
                    numericInput("milestone_c", "Milestone", value = 3000, min = 0),
                )
            ),
    
            fluidRow(
                column(12, 
                    dataTableOutput("players_career")),
            )
    
        ),         
        
        tabPanel("Any Team: Career Goal", #Panel 4
                 fluidRow(
                   column(6,
                          selectInput("stat_cat", "Stat Category", choices = stat_categories, selected="H"),
                          numericInput("milestone_cat", "Milestone", value = 3000, min = 0),
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          dataTableOutput("players_career_any_team")),
                 )
                 
        ),      
        
        tabPanel("Team and Award", #Panel 5
                 fluidRow(
                   column(6,
                          selectInput("award_taa", "Award", choices = awards_categories, selected="Most Valuable Player"),
                          selectInput("team_taa", "Team", choices = franch_list$franchName, selected="New York Yankees"),
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          dataTableOutput("team_and_award")),
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
    
    find_them_career <- reactive({
      if(input$stat_c %in% pitching_stat_categories) class <- "pitching"
      if(input$stat_c %in% batting_stat_categories) class <- "batting"
      
      
      t_1 <- lookup_franchise_id(input$team_c)
      
      threshhold_career(t_1, input$milestone_c, input$stat_c, class) 
      
    })
    
    find_them_career_any_team <- reactive({
      if(input$stat_cat %in% pitching_stat_categories) class <- "pitching"
      if(input$stat_cat %in% batting_stat_categories) class <- "batting"
      
      
      threshhold_career_any_team(input$milestone_cat, input$stat_cat, class) 
      
    })
    
    find_team_and_award <- reactive({
      
      t_1 <- lookup_franchise_id(input$team_taa)
      find_award_winners(t_1, input$award_taa) 
      
    })
    
    output$players <- renderDataTable(find_them())
    output$players_goal <- renderDataTable(find_them_goal())
    output$players_career <- renderDataTable(find_them_career())
    output$players_career_any_team <- renderDataTable(find_them_career_any_team())
    output$team_and_award <- renderDataTable(find_team_and_award())
    
    

}

shinyApp(ui, server)