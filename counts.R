source("setup.R")

award_winner_list <- find_award_winners("CLE", "Most Valuable Player")
award_winner_list_any <- find_award_winners_any_team("Silver Slugger")

two_team_list <- find_all_two_teams("ARI", "ATL")
award_winner_list <- find_award_winners("NYY")
award_winner_list_any <- find_award_winners_any_team()
career_list <- threshhold_career("ANA", 300, "SV", type="pitching")
career_list_any <- threshhold_career_any_team(3000, "H")
threshhold_list_any <- threshhold_team_any(20, "W", type="pitching")

threshhold_list <- threshhold_team("TEX", 40, "HR", type="batting")



k_w <- inner_join(k,w, by="playerID")
