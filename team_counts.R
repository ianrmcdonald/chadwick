library(tidyverse)


batting_raw <- read_csv("data/Batting.csv")
pitching_raw <- read_csv("data/Pitching.csv")
people_raw <- read_csv("data/People.csv")
awards_raw <- read_csv("data/AwardsPlayers.csv")
team_raw <- read_csv("data/Teams.csv")
franch_raw <- read_csv("data/TeamsFranchises.csv")

team_franch <- team_raw %>% 
  select(teamID, franchID) %>% 
  filter(!(teamID == "WAS" & franchID == "WAS")) %>% 
  distinct()
#teem table has a faulty WAS franchid for Nationals causing dupes at row 31921
#this fixes it


batting_id_teams <- inner_join(batting_raw, team_franch, by = "teamID", relationship = "many-to-many") %>% 
  select(playerID, franchID) %>% 
  distinct()
###

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
            filter(franchID == t1  | franchID == t2 ) %>%
            mutate(team_count = ifelse(franchID == t1, 1, 2)) %>%
            group_by(playerID) %>% 
            summarise(team_count = sum(team_count)) %>% 
            filter(team_count == 3) %>% 
            inner_join(., people_raw) %>%
            select(playerID, nameFirst, nameLast, debut, finalGame) %>% 
            mutate(days = finalGame - debut)

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
         select(nameFirst, nameLast, yearID, debut, finalGame, !!as.name(stat))
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
    select(nameFirst, nameLast, yearID, franchID, debut, finalGame, !!as.name(stat))
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
    inner_join(., all_playerID_teams_year, by=c("playerID", "yearID")) %>%
    inner_join(., people_raw, by="playerID") %>%
    select(playerID, nameFirst, nameLast, yearID, franchID, notes) 
}








award_winner_list <- find_award_winners("PHI")
award_winner_list_any <- find_award_winners_any_team()

two_team_list <- find_all_two_teams("ANA", "MIL")
award_winner_list <- find_award_winners("PHI")
award_winner_list_any <- find_award_winners_any_team()
career_list <- threshhold_career("CHC", 3000, "H")
career_list_any <- threshhold_career_any_team(3000, "H")
threshhold_list_any <- threshhold_team_any(50, "SV", type="pitching")

threshhold_list <- threshhold_team("ANA", 20, "SB", type="batting")
jul31 <- inner_join(award_winner_list_any, career_list_any, by="playerID")



