#Leaderboard with record holders in each major category and then a table linking a full list for each record.

# Homepage- standings & playoff standings, Power Rankings, Whats Happening? this week visual, owner profile selection
# NFL Players- advanced player analytics like WAR and WAR vs draft dollars, draft history
# BL Team Profile- history for each individual team
# Season Summaries - power rank, standings, results for previous seasons
# Records- Leaderboard with record holders followed by links to full table views
# News - twitter handle? Slack feed?
# Talking Busch Feed


library(tidyverse)

ggl_sheet <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRPAR9n5KuCw3Mw_Ka48khDgK28rrnD4XujJ_tAadcA_kmbYjZF6ZFKX-7WGRHBlY47N8jP6G8JgG1k/pub?gid=0&single=true&output=csv"

games_raw <- read_csv(ggl_sheet)

games_raw$Team <- toupper(games_raw$Team)

games_raw <- games_raw %>%
  mutate_all(~replace(., is.na(.), 0))

write_csv(games_raw, "gamessource.csv")

this_year <- max(games_raw$Year)

games_ty <- games_raw %>%
  filter(Year == this_year) %>%
  arrange(desc(Week))

games_allplay <- games_raw %>%
  group_by(Week, Year) %>%
  mutate(w_rank = ifelse(Year == 2008, rank(PF) / 4 + 1, rank(PF))) %>%
  mutate(APW = as.integer(w_rank - 1)) %>%
  mutate(l_rank = ifelse(Year == 2008, rank(-PF) / 4 + 1, rank(-PF))) %>%
  mutate(APL = as.integer(l_rank - 1)) %>%
  select(-w_rank, -l_rank)

this_week <- games_ty[[1, 2]]

ty_ppg <- mean(games_raw$PF)

divisions <- data.frame("Team" = c("ADAM HARTMAN", 
                                   "TIM LANG", 
                                   "DREW VOGT", 
                                   "JEFF KING", 
                                   "DERRIK HARTMAN", 
                                   "TREVOR SOUPIR", 
                                   "MAX APONTE", 
                                   "TIM GREVE", 
                                   "MATT TRAEN", 
                                   "PHIL BASTRON", 
                                   "JOHN THOMPSON", 
                                   "DAVID PIKUS"), 
                        "Division" = c("Tommie-Johnnie", 
                                       "Tommie-Johnnie", 
                                       "Tommie-Johnnie", 
                                       "Bulldog", 
                                       "Bulldog", 
                                       "Bulldog", 
                                       "Maverick", 
                                       "Maverick", 
                                       "Maverick", 
                                       "Warrior", 
                                       "Warrior", 
                                       "Warrior"))

#Records

at_record <- games_raw %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Win = sum(w_l), Loss = sum(w_l == 0), Pct = as.integer(Win / (Win + Loss) * 1000)) %>%
  arrange(desc(Win))

y12t_record <- games_raw %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  filter(Year > 2012) %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Win = sum(w_l), Loss = sum(w_l == 0), Pct = as.integer(Win / (Win + Loss) * 1000)) %>%
  arrange(desc(Win))

at_playoff_rec <- games_raw %>%
  filter(Type == "Playoffs") %>%
  group_by(Team) %>%
  filter(n() > 2) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Win = sum(w_l), Loss = sum(w_l == 0), Pct = as.integer(Win / (Win + Loss) * 1000)) %>%
  arrange(desc(Win))

y12t_playoff_rec <- games_raw %>%
  filter(Type == "Playoffs") %>%
  filter(Year > 2012) %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Win = sum(w_l), Loss = sum(w_l == 0), Pct = as.integer(Win / (Win + Loss) * 1000)) %>%
  arrange(desc(Win))

at_championships <- games_raw %>%
  filter(Type == "Championship") %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Win = sum(w_l), Loss = sum(w_l == 0), Pct = as.integer(Win / (Win + Loss) * 1000)) %>%
  arrange(desc(Win))

y12t_championships <- games_raw %>%
  filter(Type == "Championship") %>%
  filter(Year > 2012) %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Win = sum(w_l), Loss = sum(w_l == 0), Pct = as.integer(Win / (Win + Loss) * 1000)) %>%
  arrange(desc(Win))

at_po_appear <- games_raw %>%
  select(Year, Team, Type) %>%
  filter(Type == "Playoffs") %>%
  distinct() %>%
  count(Team) %>%
  filter(n > 1) %>%
  arrange(desc(n))

y12t_po_appear <- games_raw %>%
  filter(Year > 2012) %>%
  select(Year, Team, Type) %>%
  filter(Type == "Playoffs") %>%
  distinct() %>%
  count(Team) %>%
  arrange(desc(n))

champions <- games_raw %>%
  filter(Type == "Championship") %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  filter(w_l == 1) %>%
  select(Year, Champion = Team, PF, Opponent, PA)

at_apw <- games_allplay %>%
  filter(Type == "Reg") %>%
  group_by(Team) %>%
  summarise(APW = as.integer(sum(APW)), APL = as.integer(sum(APL))) %>%
  mutate(Pct = as.integer(APW / (APW + APL) * 1000)) %>%
  arrange(desc(APW))

y12t_apw <- games_allplay %>%
  filter(Year > 2012) %>%
  filter(Type == "Reg") %>%
  group_by(Team) %>%
  summarise(APW = as.integer(sum(APW)), APL = as.integer(sum(APL))) %>%
  mutate(Pct = as.integer(APW / (APW + APL) * 1000)) %>%
  arrange(desc(APW))

at_apw_season <- games_allplay %>%
  filter(Type == "Reg") %>%
  group_by(Team, Year) %>%
  summarise(APW = as.integer(sum(APW)), APL = as.integer(sum(APL))) %>%
  mutate(Pct = as.integer(APW / (APW + APL) * 1000)) %>%
  arrange(desc(Pct))

y12t_apw_season <- games_allplay %>%
  filter(Year > 2012) %>%
  filter(Type == "Reg") %>%
  group_by(Team, Year) %>%
  summarise(APW = as.integer(sum(APW)), APL = as.integer(sum(APL))) %>%
  mutate(Pct = as.integer(APW / (APW + APL) * 1000)) %>%
  arrange(desc(Pct))


#Adjusted Points

games_adj <- games_raw %>%
  group_by(Year) %>%
  mutate(ppg = mean(PF)) %>%
  ungroup() %>%
  mutate(Adj_PF = PF / ppg * ty_ppg) %>%
  select(-ppg)

at_ppg <- games_adj %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  group_by(Team) %>%
  summarise(PPG = mean(Adj_PF)) %>%
  arrange(desc(PPG))

y12t_ppg <- games_adj %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  group_by(Team) %>%
  summarise(PPG = mean(Adj_PF)) %>%
  arrange(desc(PPG))

at_ppg_season <- games_adj %>%
  group_by(Team, Year) %>%
  summarise(PPG = mean(Adj_PF)) %>%
  arrange(desc(PPG))

y12t_ppg_season <- games_adj %>%
  filter(Year > 2012) %>%
  group_by(Team, Year) %>%
  summarise(PPG = mean(Adj_PF)) %>%
  arrange(desc(PPG))

at_pts_game <- games_adj %>%
  arrange(desc(Adj_PF)) %>%
  select(-Type)

y12t_pts_game <- games_adj %>%
  filter(Year > 2012) %>%
  arrange(desc(Adj_PF)) %>%
  select(-Type)

#Shame

at_shame_pts_game <- games_adj %>%
  arrange(Adj_PF) %>%
  select(-Type)

y12t_shame_pts_game <- games_adj %>%
  filter(Year > 2012) %>%
  arrange(Adj_PF) %>%
  select(-Type)

at_shame_ppg_season <- games_adj %>%
  group_by(Team, Year) %>%
  summarise(PPG = mean(Adj_PF)) %>%
  arrange(PPG)

y12t_shame_ppg_season <- games_adj %>%
  filter(Year > 2012) %>%
  group_by(Team, Year) %>%
  summarise(PPG = mean(Adj_PF)) %>%
  arrange(PPG)





