source("setup.R")

award_winner_list <- find_award_winners("CLE", "Most Valuable Player")
award_winner_list_any <- find_award_winners_any_team("Silver Slugger")

two_team_list <- find_all_two_teams("PIT", "SDP")
award_winner_list <- find_award_winners("NYY", "Cy Young Award")
award_winner_list_any <- find_award_winners_any_team("Cy Young Award")
career_list <- threshhold_career("ANA", 3000, "H", type="batting")
career_list_any <- threshhold_career_any_team(3000, "H")
threshhold_list_any <- threshhold_team_any(200, "H", type="batting")

threshhold_list <- threshhold_team("TEX", 40, "HR", type="batting")



two_winners <- find_two_award_winners_any_team("Most Valuable Player", "Rookie of the Year", same_year = FALSE)


