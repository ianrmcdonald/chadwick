
#compute both a single 30HR/30SB season & a different 200 H season

x <- threshhold_team_any(30, "HR") |> select(playerID, yearID)

y <- threshhold_team_any(30, "SB") |> select(playerID, yearID)
w <- threshhold_team_any(200, "H") |> select(playerID, yearID)

z <- inner_join(x, y, by=c("playerID", "yearID"))
v <- inner_join(z, w, by=c("playerID")) |> select(playerID) |> distinct()
t <- v |> 
  inner_join(people_raw, by="playerID") |> 
  mutate(nameWhole = str_c(nameFirst," ", nameLast)) |> 
  arrange(playerID) |> 
  select(nameWhole)


#award plus season goal
#threshhold_team_any <- function(threshhold_i, stat, type="batting") {
#find_award_winners_season_threshold <- function(award, t_number, t_stat, t_type="batting") {

gg <- find_award_winners_any_team("Gold Glove")
tl <- threshhold_team_any(200, "H")
gg_tl <- inner_join(gg, tl, by="playerID") |> select(nameWhole) |> distinct()

hr_40 <- threshhold_team_any(40, "HR")
gg_hr40 <- inner_join(gg, hr_40, by="playerID") |> 
  arrange(playerID) |> 
  select(nameWhole) |> distinct()


#the 2022 awards problem

awards_list <- awards_raw |> 
  filter(yearID == 2021) |> 
  select(awardID) |> 
  distinct()

awards_2021 <- awards_raw |> 
  filter(yearID == 2021) |> 
  inner_join(people_raw, by="playerID") |> 
  mutate(nameWhole = str_c(nameFirst," ", nameLast)) |> 
  select(playerID, nameWhole, awardID, lgID)
           
#seattle pilots

pilots_batting_id_teams <- inner_join(batting_raw, team_franch, by = "teamID", 
                                         relationship = "many-to-many") |>
  filter(teamID == "SE1") |> 
  select(playerID, franchID) |> 
  distinct()

pilots_pitching_id_teams <- inner_join(pitching_raw, team_franch, by = "teamID",
                                relationship = "many-to-many") |> 
  filter(teamID == "SE1") |> 
  select(playerID, franchID) |> 
  distinct()

pilots <- rbind(pilots_batting_id_teams, pilots_pitching_id_teams) |> 
  distinct() |> 
  inner_join(people_raw, by="playerID") |> 
  mutate(nameWhole = str_c(nameFirst, " ", nameLast)) |> 
  select(playerID, nameWhole) |> 
  inner_join(number_of_games_tbl, by=c("playerID")) |> 
  filter(franchID != "MIL") |> 
  select(-playerID) |> 
  arrange(desc(games))

pilots_wide <- pilots |> 
  pivot_wider(id_cols = nameWhole, names_from = franchID, values_from = games)
