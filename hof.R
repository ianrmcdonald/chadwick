

hof_b_raw <- read_csv("data/hof_b.csv") %>% 
  select(name = Name, inducted = Inducted, playerID = `Name-additional`)

hof_p_raw <- read_csv("data/hof_p.csv") %>% 
  select(name = Name, inducted = Inducted, playerID = `Name-additional`)

hof_raw <- bind_rows(hof_b_raw, hof_p_raw)
