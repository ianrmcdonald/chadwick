library(tidyverse)

#https://chadwick.readthedocs.io/en/latest/

#https://github.com/chadwickbureau/baseballdatabank

#https://github.com/chadwickbureau

#https://github.com/chadwickbureau/baseballdatabank/tree/master/core

batting_raw <- read_csv("data/Batting.csv")
pitching_raw <- read_csv("data/pitching.csv")
people_raw <- read_csv("data/people.csv") #modified people.csv per 7/30/2023 update
awards_raw <- read_csv("data/AwardsPlayers.csv")
team_raw <- read_csv("data/Teams.csv")
franch_raw <- read_csv("data/TeamsFranchises.csv")
games_with_team("boutoji01", "NYY")

#teem table has a faulty WAS franchid for Nationals causing dupes at row 31921
#this fixes it
team_franch <- team_raw %>% 
  select(teamID, franchID, lgID) %>% 
  filter(!(teamID == "WAS" & franchID == "WAS")) %>%
  inner_join(., franch_raw, by="franchID") %>% 
  filter(active == "Y") %>% 
  distinct()

batting_id_teams <- inner_join(batting_raw, team_franch, by = "teamID", 
                              relationship = "many-to-many") %>% 
  select(playerID, franchID) %>% 
  distinct()

pitching_id_teams <- inner_join(pitching_raw, team_franch, by = "teamID",
                              relationship = "many-to-many") %>% 
  select(playerID, franchID) %>% 
  distinct()

all_playerID_teams <- rbind(batting_id_teams, pitching_id_teams) %>% 
  distinct()

batting_id_teams_year <- inner_join(batting_raw, team_franch, by = "teamID",
                               relationship = "many-to-many") %>% 
  select(playerID, franchID, yearID) %>% 
  distinct()

pitching_id_teams_year <- inner_join(pitching_raw, team_franch, by = "teamID",
                                relationship = "many-to-many") %>% 
  select(playerID, franchID, yearID) %>% 
  distinct()

all_playerID_teams_year <- rbind(batting_id_teams_year, pitching_id_teams_year) %>% 
  distinct()







playerID_number_of_teams <- all_playerID_teams %>% 
  count(playerID)

playerID_number_of_teams <- inner_join(playerID_number_of_teams, people_raw, 
                                       by = "playerID") %>% 
  select(teams_n = n, playerID, nameFirst, nameLast, debut, finalGame)







find_all_two_teams <- function(t1, t2) {
    two_teams <- all_playerID_teams %>% 
    filter(franchID == t1 | franchID == t2) %>%
    mutate(team_count = ifelse(franchID == t1, 1, 2)) %>%
    group_by(playerID) %>% 
    summarise(team_count = sum(team_count)) %>% 
    filter(team_count == 3) %>% 
    inner_join(., people_raw, by="playerID") %>%
    arrange(desc(debut)) %>% 
    mutate(days = finalGame - debut) %>% 
    mutate(nameWhole = str_c(nameFirst, " ", nameLast)) %>% 
    mutate_if(is.Date,~format(.,"%Y-%m-%d")) %>% 
    select(nameWhole, debut, days)

}

threshhold_team <- function(team, threshhold_i, stat, type="batting") {

  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  threshhold_list <- table_data %>%
        inner_join(., team_franch, by = "teamID", relationship = "many-to-many") %>% 
        select(playerID, franchID, yearID, !!as.name(stat)) %>%
         filter(franchID == team, !!as.name(stat) >= threshhold_i) %>% 
         inner_join(., people_raw, by="playerID") %>% 
         select(playerID, nameFirst, nameLast, yearID, debut, finalGame, !!as.name(stat))
  }

threshhold_team_any <- function(threshhold_i, stat, type="batting") {
  
  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  threshhold_list <- table_data %>%
    inner_join(., team_franch, by = "teamID", relationship = "many-to-many") %>% 
    select(playerID, franchID, yearID, !!as.name(stat)) %>%
    filter(!!as.name(stat) >= threshhold_i) %>% 
    inner_join(., people_raw, by="playerID") %>% 
    select(playerID, nameFirst, nameLast, yearID, franchID, debut, finalGame, !!as.name(stat))
}

threshhold_career <- function(team, threshhold_i, stat, type="batting") {
  
  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  threshhold_list <- table_data %>% 
    select(playerID, !!as.name(stat)) %>%
    group_by(playerID) %>% 
    summarise(stat_i = sum(!!as.name(stat))) %>% 
    inner_join(., all_playerID_teams, by="playerID",
                relationship = "many-to-many") %>% 
     filter(franchID == team & stat_i >= threshhold_i) %>% 
     distinct() %>% 
    inner_join(., people_raw, by="playerID",
                relationship = "many-to-many") %>% 
    select(playerID, nameFirst, nameLast, debut, finalGame, stat_i) 

  
}

threshhold_career_any_team <- function(threshhold_i, stat, type="batting") {
  
  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  threshhold_list <- table_data %>% 
    select(playerID, !!as.name(stat)) %>%
    group_by(playerID) %>% 
    summarise(stat_i = sum(!!as.name(stat))) %>% 
  inner_join(., all_playerID_teams, by="playerID",
             relationship = "many-to-many") %>%
  filter(stat_i >= threshhold_i) %>%
  distinct() %>%
  inner_join(., people_raw, by="playerID",
             relationship = "many-to-many") %>%
    select(playerID, nameFirst, nameLast, debut, finalGame, stat_i) %>%
  distinct()

  
}

find_award_winners <- function(t1, award_name = "Most Valuable Player") {
  
    winner <- awards_raw %>% 
      filter(awardID == award_name) %>% 
      inner_join(., all_playerID_teams_year, by=c("playerID", "yearID")) %>%
      inner_join(., people_raw, by="playerID") %>%
      select(playerID, nameFirst, nameLast, yearID, franchID, notes) %>%
      filter(franchID == t1)
}


find_award_winners_any_team <- function(award_name = "Most Valuable Player") {
  
  winner <- awards_raw %>% 
    filter(awardID == award_name) %>% 
    inner_join(., all_playerID_teams_year, by=c("playerID", "yearID"),
               relationship = "many-to-many") %>%
    inner_join(., people_raw, by="playerID") %>%
    select(playerID, nameFirst, nameLast, yearID, franchID, notes) 
}

find_two_award_winners_any_team <- function(a1, a2, same_year = FALSE) {
  

  
  a1_list <- find_award_winners_any_team(a1) %>% 
    select(playerID, yearID, nameFirst, nameLast) %>% 
    distinct()
  
  a2_list <- find_award_winners_any_team(a2) %>% 
    select(playerID, yearID, nameFirst, nameLast) %>% 
    distinct()

  if(same_year) {
    a_both_list <- inner_join(a1_list, a2_list, by=c("playerID", "nameFirst", "nameLast", "yearID"))
    
  } else {
    a_both_list <- inner_join(a1_list, a2_list, by=c("playerID", "nameFirst", "nameLast")) %>% 
      select(playerID, nameFirst, nameLast) %>% 
      distinct()
  }
  
}


franch_list <- franch_raw %>% 
  filter(active == "Y") %>% 
  select(franchID, franchName) %>% 
  arrange(franchName)

  
find_award_winners_season_threshold <- function(award, t_number, t_stat, t_type="batting") {
  award_list <- find_award_winners_any_team(award) %>% 
    select(playerID, nameFirst, nameLast) %>% 
    distinct()
  
  threshhold_list <- threshhold_team_any(t_number, t_stat, t_type) %>% 
    select(playerID, nameFirst, nameLast) %>% 
    distinct()
  
  a_t <- inner_join(award_list, threshhold_list, by=c("playerID", "nameFirst", "nameLast"))
  
}

find_award_winners_season_threshold_same_year <- function(award, t_number, t_stat, t_type="batting") {
  
  award_list <- find_award_winners_any_team(award) %>% 
    select(playerID, nameFirst, nameLast, yearID) %>% 
    distinct()
  
  threshhold_list <- threshhold_team_any(t_number, t_stat, t_type) %>% 
    select(playerID, nameFirst, nameLast, yearID) %>% 
    distinct()
  
  a_t <- inner_join(award_list, threshhold_list, by=c("playerID", "nameFirst", "nameLast", "yearID"))
  
}



lookup_franchise_id <- function(franchise_name) {
  
  franch_ID <- franch_list %>% 
    filter(franchName == franchise_name)
  
    as.character(franch_ID$franchID)
    
}

batting_stat_categories <- colnames(batting_raw[6:ncol(batting_raw)])
pitching_stat_categories <- colnames(pitching_raw[6:ncol(pitching_raw)])


games_with_team <- function(player, franch, teams = "all", type="batting") {
  
  
  if(type == "batting") source_tbl <- "batting_raw"
  if(type == "pitching") source_tbl <- "pitching_raw"
  
  
  all_games <- 
    inner_join(!!as.name(source_tbl), team_franch, by="teamID", relationship = "many-to-many") |> 
    filter(active == "Y") |> 
    select(playerID, franchID, G) |> 
    group_by(playerID, franchID) |> 
    summarise(games = sum(G), .groups = "keep")
  
    if (teams == "all") {
      all_games <- all_games |> filter(playerID == player)
    } else {
      all_games <- all_games |> filter(playerID == player, franchID == franch)  
    }
  
}


games_with_team("boutoji01", "NYY")