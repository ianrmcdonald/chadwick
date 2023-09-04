library(tidyverse)
library(DT)
library(bslib)

#https://chadwick.readthedocs.io/en/latest/
#https://github.com/chadwickbureau/baseballdatabank
#https://github.com/chadwickbureau
#https://github.com/chadwickbureau/baseballdatabank/tree/master/core
#https://www.baseball-reference.com/awards/hof_batting.shtml



## Read Data Files
batting_raw <- read_csv("data/Batting.csv") |> 
  mutate(SF = ifelse(is.na(SF), 0, SF)) |> 
  mutate(BA = ifelse(AB + SF + SH + BB + HBP >= 502, round(H / AB * 1000, 
                                           digits=0), 0))

pitching_raw <- read_csv("data/Pitching.csv")
people_raw <- read_csv("data/People.csv") #modified people.csv per 7/30/2023 update
awards_raw <- read_csv("data/AwardsPlayers.csv")
team_raw <- read_csv("data/Teams.csv")
franch_raw <- read_csv("data/TeamsFranchises.csv")
hof_b_raw <- read_csv("data/hof_b.csv") |> 
  select(yearID = Inducted, playerID = `Name-additional`) |>
  mutate(awardID = "Hall of Fame")
hof_p_raw <- read_csv("data/hof_p.csv") |> 
  select(yearID = Inducted, playerID = `Name-additional`) |>
  mutate(awardID = "Hall of Fame")
hof_raw <- bind_rows(hof_b_raw, hof_p_raw)


#team table has a faulty WAS franchid for Nationals causing dupes at row 31921
#this fixes it
#I dropped lgID because Houston and Milwaukee are double counted




team_franch <- team_raw  |>
  select(teamID, franchID) |> 
  filter(!(teamID == "WAS" & franchID == "WAS")) |> 
  filter(!(teamID == "BLA" & franchID == "NYY")) |> #baseball ref doesn't match these (1901 & 1902)
  inner_join(franch_raw, by="franchID")  |>  
  filter(active == "Y") |>  
  distinct()

batting_id_teams <- inner_join(batting_raw, team_franch, by = "teamID", 
                              relationship = "many-to-many") |> 
  select(playerID, franchID) |> 
  distinct()

pitching_id_teams <- inner_join(pitching_raw, team_franch, by = "teamID",
                              relationship = "many-to-many") |> 
  select(playerID, franchID) |> 
  distinct()

all_playerID_teams <- rbind(batting_id_teams, pitching_id_teams) |> 
  distinct()

batting_id_teams_year <- inner_join(batting_raw, team_franch, by = "teamID",
                               relationship = "many-to-many") |> 
  select(playerID, franchID, yearID) |> 
  distinct()

pitching_id_teams_year <- inner_join(pitching_raw, team_franch, by = "teamID",
                                relationship = "many-to-many") |> 
  select(playerID, franchID, yearID) |> 
  distinct()

all_playerID_teams_year <- rbind(batting_id_teams_year, pitching_id_teams_year) |> 
  distinct()

playerID_number_of_teams <- all_playerID_teams |> 
  count(playerID) |> 
  rename(career_teams = n)|> 
  select(playerID, career_teams)

playerID_number_of_teams_names <- all_playerID_teams |> 
  count(playerID) |> 
  rename(career_teams = n)|> 
  inner_join(people_raw, by="playerID") |> 
  mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |>
  arrange(desc(career_teams)) |> 
  select(playerID, nameWhole, career_teams) 
  

players_and_teams <- function(player, name_complete = FALSE) {

  if(!name_complete) {
    
  p_and_t <- all_playerID_teams |>
    inner_join(people_raw, by="playerID") |> 
    mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |>
    filter(playerID == player) |>
    inner_join(franch_list, by="franchID")  |> 
    select(playerID, nameWhole, franchID, franchName)
  }
  
  else {
    p_and_t <- all_playerID_teams |>
      inner_join(people_raw, by="playerID") |> 
      mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |>
      filter(nameWhole == player) |> 
      inner_join(franch_list, by="franchID") |> 
      select(playerID, nameWhole, franchID, franchName)
  }
  
}

name_whole_list <- people_raw |> 
  mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |>
  mutate(y_text_1 = format(debut,"%Y")) |> 
  mutate(y_text_2 = format(finalGame,"%Y")) |> 
  mutate(ifelse (y_text_2 == "2023", "", y_text_2)) |> 
  mutate(nameWholeYears = str_c(nameWhole, ": ", y_text_1,"-",y_text_2)) |> 
  select(playerID, nameWholeYears)






games_with_team <- function(type="batting") {

  all_games <- batting_raw |> 
    select(-lgID) |> 
    inner_join(team_franch, by="teamID", relationship = "many-to-many")   |>
    filter(active == "Y") |> 
    select(playerID, franchID, G) |>
    group_by(playerID, franchID) |>
    summarise(games = sum(G), .groups = "keep") |> 
    select(playerID, franchID, games)
}

number_of_games_tbl <- games_with_team()

team_ngames <- function(franch) {
  number_of_games_tbl |> 
    filter(franchID == franch)
}


find_all_two_teams <- function(t1, t2) {
  
  t1_name <- str_c(t1," games")
  t2_name <- str_c(t2," games")
  
  two_teams <- all_playerID_teams |> 
        filter(franchID == t1 | franchID == t2) |>
        mutate(team_count = ifelse(franchID == t1, 1, 2)) |>
        group_by(playerID) |> 
        summarise(team_count = sum(team_count)) |> 
        filter(team_count == 3) |> 
        inner_join(people_raw, by="playerID") |>
        arrange(desc(debut)) |> 
        mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |> 
        mutate_if(is.Date,~format(.,"%Y")) |> 
        select(playerID, nameWhole, debut, finalGame) |> 
        inner_join(team_ngames(t1), by="playerID") |> 
        rename("{t1_name}" := games) |> 
        select(-franchID) |> 
        inner_join(team_ngames(t2), by="playerID") |> 
        rename("{t2_name}" := games) |> 
        select(-franchID)
  
}

threshhold_team <- function(team, threshhold_i, stat, type="batting") {

  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  threshhold_list <- table_data |>
        inner_join(team_franch, by = "teamID", relationship = "many-to-many") |> 
        select(playerID, franchID, yearID, !!as.name(stat)) |>
         filter(franchID == team, !!as.name(stat) >= threshhold_i) |> 
         inner_join(people_raw, by="playerID") |> 
         mutate(player = str_c(nameFirst, " ", nameLast)) |>
         relocate(.before = player) |>
         mutate_if(is.Date,~format(.,"%Y")) |> 
         select(playerID, player, yearID, debut, finalGame, !!as.name(stat))
         
  }

threshhold_team_any <- function(threshhold_i, stat, type="batting") {
  
  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  threshhold_list <- table_data |>
    inner_join(team_franch, by = "teamID", relationship = "many-to-many") |> 
    select(playerID, franchID, yearID, !!as.name(stat)) |>
    filter(!!as.name(stat) >= threshhold_i) |> 
    inner_join(people_raw, by="playerID") |>
    mutate_if(is.Date,~format(.,"%Y")) |>
    mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |> 
    select(playerID, nameWhole, yearID, franchID, debut, finalGame, !!as.name(stat))
}

threshhold_career <- function(team, threshhold_i, stat, type="batting") {
  
  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  stat_i <- stat
  threshhold_list <- table_data |> 
    select(playerID, !!as.name(stat)) |>
    group_by(playerID) |> 
    summarise(stat_i = sum(!!as.name(stat))) |> 
    inner_join(all_playerID_teams, by="playerID",
                relationship = "many-to-many") |> 
     filter(franchID == team & stat_i >= threshhold_i) |> 
     distinct() |>  
    inner_join(people_raw, by="playerID",
                relationship = "many-to-many") |>
    mutate_if(is.Date,~format(.,"%Y")) |> 
    mutate(player = str_c(nameFirst, " ", nameLast)) |>
    rename("{stat_i}" := stat_i) |> 
    select(playerID, player, debut, finalGame, !!as.name(stat_i)) |> 
    distinct()
  
}

#ought to combine these two functions. 

threshhold_career_any_team <- function(threshhold_i, stat, type="batting") {
  
  if(type == "pitching") {
    table_data = pitching_raw
  } else {
    table_data = batting_raw
  }
  
  stat_i <- stat
  threshhold_list <- table_data |> 
    select(playerID, !!as.name(stat)) |>
    group_by(playerID) |> 
    summarise(stat_i = sum(!!as.name(stat))) |> 
    inner_join(all_playerID_teams, by="playerID",
               relationship = "many-to-many") |> 
    filter(stat_i >= threshhold_i) |> 
    distinct() |> 
    inner_join(people_raw, by="playerID",
               relationship = "many-to-many") |>
    mutate_if(is.Date,~format(.,"%Y")) |> 
    mutate(player = str_c(nameFirst, " ", nameLast)) |>
    rename("{stat_i}" := stat_i) |> 
    select(playerID, player, debut, finalGame, !!as.name(stat_i)) |> 
    distinct()
  
  
}


awards_and_hof <- awards_raw |> 
  bind_rows(hof_raw)
  
find_award_winners <- function(t1, award_name = "Most Valuable Player") {
  
    winner <- awards_raw |> 
      filter(awardID == award_name) |> 
      inner_join(all_playerID_teams_year, by=c("playerID", "yearID")) |>
      inner_join(people_raw, by="playerID") |>
      mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |> 
      select(playerID, nameWhole, yearID, franchID, notes) |>
      filter(franchID == t1)
    
}

find_award_winners_any_team <- function(award_name = "Most Valuable Player") {
  
  winner <- awards_raw |> 
    filter(awardID == award_name) |> 
    inner_join(all_playerID_teams_year, by=c("playerID", "yearID"),
               relationship = "many-to-many") |>
    inner_join(people_raw, by="playerID") |>
    select(playerID, nameFirst, nameLast, yearID, franchID, notes) 
}

find_two_award_winners_any_team <- function(a1, a2, same_year = TRUE) {
  
  a1_list <- find_award_winners_any_team(a1) |> 
    mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |>
    select(playerID, yearID, nameWhole) |>
    distinct()
  
  a2_list <- find_award_winners_any_team(a2) |>
    mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |>
    select(playerID, yearID, nameWhole) |>
    distinct()

  if(same_year) {
    a_both_list <- inner_join(a1_list, a2_list, by=c("playerID", "nameWhole", "yearID"), relationship =
                                                       "many-to-many")

  } else {
    a_both_list <- inner_join(a1_list, a2_list, by=c("playerID", "nameWhole"), relationship =
                                                       "many-to-many") |>
      select(playerID, nameWhole) |>
      distinct()
  }
  
}

find_award_winners_and_HOF <- function(a1) {
  
  a1_list <- find_award_winners_any_team(a1) |> 
    select(playerID) |> 
    distinct()
  
  a2_list <- hof_raw |> 
    select(playerID) |> 
    distinct()
  
    a_both_list <- inner_join(a1_list, a2_list, by=c("playerID")) |> 
      select(playerID) |> 
      inner_join(people_raw, by="playerID",
                 relationship = "many-to-many") |>
      select(playerID, nameFirst, nameLast) |> 
      inner_join(hof_raw, by ="playerID") |> 
      distinct()
      
}

franch_list <- franch_raw |> 
  filter(active == "Y") |> 
  select(franchID, franchName) |> 
  arrange(franchName)

find_award_winners_season_threshold <- function(award, t_number, t_stat, t_type="batting") {
  award_list <- find_award_winners_any_team(award) |> 
    select(playerID, nameFirst, nameLast) |> 
    distinct()
  
  threshhold_list <- threshhold_team_any(t_number, t_stat, t_type) |> 
    select(playerID, nameFirst, nameLast) |> 
    distinct()
  
  a_t <- inner_join(award_list, threshhold_list, by=c("playerID", "nameFirst", "nameLast"))
  
}

find_award_winners_season_threshold_same_year <- function(award, t_number, t_stat, t_type="batting") {
  
  award_list <- find_award_winners_any_team(award) |> 
    select(playerID, nameFirst, nameLast, yearID) |> 
    distinct()
  
  threshhold_list <- threshhold_team_any(t_number, t_stat, t_type) |> 
    select(playerID, nameFirst, nameLast, yearID) |> 
    distinct()
  
  a_t <- inner_join(award_list, threshhold_list, by=c("playerID", "nameFirst", "nameLast", "yearID"))
  
}

lookup_franchise_id <- function(franchise_name) {
  
  franch_ID <- franch_list |> 
    filter(franchName == franchise_name)
  
    as.character(franch_ID$franchID)
    
}

batting_stat_categories <- colnames(batting_raw[6:ncol(batting_raw)])
pitching_stat_categories <- colnames(pitching_raw[6:ncol(pitching_raw)])
pitching_stat_categories <- pitching_stat_categories[pitching_stat_categories!="G"]
awards_categories <- unique(awards_raw$awardID) |> sort()
stat_categories <- unique(str_sort(c(batting_stat_categories, pitching_stat_categories)))







  