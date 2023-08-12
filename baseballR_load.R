# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("billpetti/baseballr")


library(baseballr)
library(dplyr)
bref_standings_on_date("2023-08-01", "NL East", from = FALSE)
