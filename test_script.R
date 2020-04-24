

##############################################################################
# FOR SEASON AND GAME RECORDS WE CAN JUST BIND LEADERBOARD AND FILTER FOR 
# ANY RECORDS THIS YEAR WHICH RANK IN THE TOP 2 OR 3
#
# FOR CAREER WE NEED TO COMPARE CAREER RANK FROM ONE WEEK TO THE NEXT
##############################################################################

this_year_week <- this_year + (this_week / 100)

record_test1 <- games_raw %>%
  mutate(year_week = Year + (Week / 100)) %>%
  filter(year_week < this_year_week) %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Record = "Most Wins", 
            n = sum(w_l), 
            Loss = sum(w_l == 0), 
            Pct = as.integer(n / (n + Loss) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  select(Record, n, Team, grp)

record_test2 <- games_raw %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Record = "Most Wins", 
            n = sum(w_l), 
            Loss = sum(w_l == 0), 
            Pct = as.integer(n / (n + Loss) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  select(Record, n, Team, grp)

record_test3 <- full_join(record_test1, record_test2, by = c("Record", "Team"))

record_test3 %>%
  filter(grp.y == 1) %>%
  mutate(new_record_check = as.logical(grp.x != grp.y))

at_wins <- games_raw %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Record = "Most Wins", 
            n = sum(w_l), 
            Loss = sum(w_l == 0), 
            Pct = as.integer(n / (n + Loss) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

at_w_pct <- games_raw %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Record = "Win %", 
            Win = sum(w_l), 
            Loss = sum(w_l == 0), 
            n = as.integer(Win / (Win + Loss) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

at_po_appear <- games_raw %>%
  select(Year, Team, Type) %>%
  filter(Year > 2012) %>%
  filter(Type == "Playoffs") %>%
  distinct() %>%
  count(Team) %>%
  filter(n > 1) %>%
  mutate(Record = "Playoff Appearances") %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

at_playoff_w <- games_raw %>%
  filter(Type != "Reg") %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 2) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Record = "Playoff Wins", 
            n = sum(w_l), 
            Loss = sum(w_l == 0), 
            Pct = as.integer(n / (n + Loss) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)  

at_bb_appear <- games_raw %>%
  select(Year, Team, Type) %>%
  filter(Year > 2012) %>%
  filter(Type == "Championship") %>%
  distinct() %>%
  count(Team) %>%
  filter(n > 1) %>%
  mutate(Record = "Busch Bowl Appearances") %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

at_bb_w <- games_raw %>%
  filter(Type == "Championship") %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  filter(n() > 2) %>%
  ungroup() %>%
  mutate(w_l = as.numeric(as.logical(PF > PA))) %>%
  group_by(Team) %>%
  summarise(Record = "Busch Bowl Wins", 
            n = sum(w_l), 
            Loss = sum(w_l == 0), 
            Pct = as.integer(n / (n + Loss) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team) 

at_apw <- games_allplay %>%
  filter(Type == "Reg") %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  summarise(Record = "All Play Wins", 
            n = as.integer(sum(APW)), 
            APL = as.integer(sum(APL))) %>%
  mutate(Pct = as.integer(n / (n + APL) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

at_apw_pct <- games_allplay %>%
  filter(Type == "Reg") %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  summarise(Record = "All Play W%", 
            APW = as.integer(sum(APW)), 
            APL = as.integer(sum(APL))) %>%
  filter(APW > 150) %>%
  mutate(n = as.integer(APW / (APW + APL) * 1000)) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

at_ppg <- games_adj %>%
  group_by(Team) %>%
  filter(n() > 23) %>%
  ungroup() %>%
  filter(Year > 2012) %>%
  group_by(Team) %>%
  summarise(Record = "PPG", 
            n = as.integer(mean(Adj_PF))) %>%
  mutate(grp = dense_rank(desc(n))) %>%
  filter(grp == 1) %>%
  select(Record, n, Team)

rbind(at_wins, at_w_pct, at_po_appear, at_playoff_w, at_bb_appear, at_bb_w, at_apw, at_apw_pct, at_ppg)


##############################################################################
# SCRAPE PLAYER STATS FROM FANTASYPROS.COM AND CLEAN NAMES SO THEY MATCH W/ ESPN 
##############################################################################



fp_errors <- test1 %>%
  filter(is.na(Team))

fp_data %>% filter(Team == "DET") %>% filter(Position == "WR")
