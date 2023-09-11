library(shiny)
#library(tidyverse)
source("setup.R", local=TRUE)
library(bslib)


ui <- fluidPage(
  
    theme = bs_theme(
      bootswatch = "minty"
    ),
    
    shinyUI(fluidPage(
      # Application title
      titlePanel("Immaculate Grid Cheatmobile"),
      p("11-Sep-2023"), 
      p("Ian McDonald ianrmcdonald at gmail.com"),
      p("Data through end of 2022")
      )
    ),

    #titlePanel("Immaculate Grid Cheatmobile"),
    
    tabsetPanel(
        id="tabset",
        
        tabPanel("Two Team Players",  ##Panel 1
                 
            fluidRow(
                column(6,
                    selectInput("team_1", "Team 1", choices = franch_list$franchName, selected="Los Angeles Dodgers"),
                    selectInput("team_2", "Team 2", choices = franch_list$franchName, selected="New York Yankees"),
                    checkboxInput("two_team_checkbox_HOF", "Hall of Fame?", value = FALSE),
                    checkboxInput("two_team_checkbox_all_star", "All Star?", value = FALSE),
                    
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
                    selectInput("stat", "Stat Category", choices = stat_categories, selected="HR"),
                    numericInput("milestone", "Milestone", value = 50, min = 0),
                )
            ),
            
            fluidRow(
                column(12, 
                    dataTableOutput("players_goal")),
            )
        
                 
        ), 
        
        tabPanel("Season Goal: Any Team", ##Panel 2a
                 
                 
                 fluidRow(
                   column(6,
                          selectInput("stat_any_team", "Stat Category", choices = stat_categories, selected="HR"),
                          numericInput("milestone_any_team", "Milestone", value = 50, min = 0),
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          dataTableOutput("players_goal_any_team")),
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
        
        tabPanel("Career Goal: Any Team", #Panel 4
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
                 
        ),    
        
        tabPanel("Two Awards Any Year", #Panel 6
                 fluidRow(
                   column(6,
                          selectInput("award_t1aay", "Award 1", choices = awards_categories, selected="Most Valuable Player"),
                          selectInput("award_t2aay", "Award 2", choices = awards_categories, selected="Silver Slugger"),
                          checkboxInput("checkbox_HOF", "Hall of Fame?", value = FALSE),
                          checkboxInput("same_year", "Same Year?", value = FALSE)
                          
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          dataTableOutput("two_awards_any_year")),
                 )
                
                 
        
                 
                 
        ),
        
        tabPanel("Names and Teams", #Panel 7
                 
                 fluidRow(
                   column(6,
                          selectizeInput("names_and_teams", "Names", 
                                         choices = name_whole_list$nameWholeYears, selected="Felix Hernandez: 2005-2019"),
                          
                          
                   )
                 ),
                 
                 actionButton(inputId = "pull_teams", label = "Pull Teams"),
                 
                 fluidRow(
                   column(12, 
                          dataTableOutput("names_teams")),
                 ) 
                 
                 
                 
                 
        ),
        tabPanel("Players and Number of Teams", #Panel 8
                 
                 
                 fluidRow(
                   column(12, 
                          dataTableOutput("teams_number")),
                 ) 
                 
                 
                 
                 
        )  
    )  

)


server <- function(input, output, session) {
  

  
    find_them <- reactive({
        t_1 <- lookup_franchise_id(input$team_1)
        t_2 <- lookup_franchise_id(input$team_2)
        
        if(input$two_team_checkbox_HOF) {
          
          find_all_two_teams(t_1, t_2) |> 
            inner_join(playerID_number_of_teams, by="playerID") |> 
            inner_join(select(hof_raw, playerID), by="playerID", relationship =
                         "many-to-many") |> 
            select(-playerID) |> 
            distinct() |> 
            arrange(desc(career_teams))
          
        } else if(input$two_team_checkbox_all_star) {
          
          find_all_two_teams(t_1, t_2) |> 
            inner_join(playerID_number_of_teams, by="playerID") |> 
            inner_join(all_star_any_year, by="playerID", relationship =
                         "many-to-many") |> 
            select(-playerID) |> 
            distinct() |> 
            arrange(desc(career_teams))
          
        } else {
          
        find_all_two_teams(t_1, t_2) |> 
            inner_join(playerID_number_of_teams, by="playerID") |> 
            select(-playerID) |> 
            distinct() |> 
            arrange(desc(career_teams))
        }
    })
    
    find_them_goal <- reactive({
      if(input$stat %in% pitching_stat_categories) class <- "pitching"
      if(input$stat %in% batting_stat_categories) class <- "batting"
      
      
      t_1 <- lookup_franchise_id(input$team)
      
      threshhold_team(t_1, input$milestone, input$stat, class) |> 
        select(-playerID)
      
    })
    
    find_them_goal_any <- reactive({
      if(input$stat_any_team %in% pitching_stat_categories) class <- "pitching"
      if(input$stat_any_team %in% batting_stat_categories) class <- "batting"
      
      
      t_1 <- lookup_franchise_id(input$team)
      
      threshhold_team_any(input$milestone_any_team, input$stat_any_team, class) |> 
        select(-playerID)
      
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
    
    two_awards_any_year <- reactive({
      
      t_6 <- find_two_award_winners_any_team(input$award_t1aay, input$award_t2aay, same_year = input$same_year)
      
      if(input$checkbox_HOF & input$same_year) {
        
        t_6 |> 
          inner_join(select(hof_raw, playerID), by="playerID", relationship =
                       "many-to-many") |> 
          select(nameWhole, yearID) |> 
          distinct() 
        
      } else if(input$checkbox_HOF & !input$same_year) { 
        t_6 |> 
          inner_join(select(hof_raw, playerID), by="playerID", relationship =
                       "many-to-many") |> 
          select(nameWhole) |> 
          distinct() 
        
      }  else if(!input$checkbox_HOF & input$same_year) { 
        t_6 |> 
          select(nameWhole, yearID) |> 
          distinct() 
        
      }  else { 
        t_6 |> 
          select(nameWhole) |> 
          distinct() 
      }
    })
    
    find_teams <- eventReactive(input$pull_teams,
                                
                                
        {
          
          pid <- name_whole_list |> filter(nameWholeYears == input$names_and_teams) 
          
          pid_1 <- players_and_teams(pid$playerID) |> 
            inner_join(number_of_games_tbl, by=c("playerID", "franchID")) |> 
            select(-franchID, -playerID) |> 
            arrange(desc(games))
          
          
        })
    
      
 
    

    output$players <- renderDataTable(find_them(), options = list(pageLength = 100))
    output$players_goal <- renderDataTable(find_them_goal(), options = list(pageLength = 100))
    output$players_goal_any_team <- renderDataTable(find_them_goal_any(), options = list(pageLength = 100))
    
    output$players_career <- renderDataTable(find_them_career() |> select(-playerID), options = list(pageLength = 100))
    output$players_career_any_team <- renderDataTable(find_them_career_any_team() |> select(-playerID),
                                                      options = list(pageLength = 100))
    output$team_and_award <- renderDataTable(find_team_and_award() |> select(-playerID) |> select(-franchID),
                                             options = list(pageLength = 100))
    output$two_awards_any_year <- renderDataTable(two_awards_any_year(),
                                                  options = list(pageLength = 100))
    
    output$names_teams <- renderDataTable(find_teams(),
                                          options = list(pageLength = 100))
    
    output$teams_number <- renderDataTable(playerID_number_of_teams_names |> select(-playerID),
                                           options = list(pageLength = 100))
    
   

}

shinyApp(ui, server)

