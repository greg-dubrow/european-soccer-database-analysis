# data available from https://www.kaggle.com/hugomathien/soccer

## note - some fields in match are XML and need further parsing. 
  # see https://www.kaggle.com/hugomathien/soccer/discussion/44871

library(tidyverse)
library(RSQLite)
library(janitor)
library(usethis)

proj_set()
usethis::use_git()

sqlite <- dbDriver("SQLite")

## euro soccer db from kaggle. some field defs http://www.football-data.co.uk/notes.txt
con <- dbConnect(sqlite,"eurosoccer.sqlite") ## will require full path if not in wd

dbListTables(con)
dbListTables(con2)
# create data 
country <- tbl_df(dbGetQuery(con,"SELECT * FROM Country"))
league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
team   <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
matchdb  <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))

glimpse(matchdb)

matchdb <- matchdb %>%
  mutate(stage_chr = str_pad(stage, width=2, side="left", pad="0")) %>%
  select(-stage) %>%
  rename(stage = stage_chr)

match_stg <- matchdb %>%
  tabyl(stage_chr, stage)

matchdb_select <- matchdb %>%
  filter(match_api_id == 483360)
glimpse(matchdb_select)

## create table with country name, league name and all teams
# country id and league id are the same
# merge league & country on id
teamalla <- left_join(country, league, by = "id") %>%
  rename(country = name.x, league = name.y) %>%
  select(country_id, country, league) %>%
  arrange(country_id)
# get team id w/ (league/country) id, keep one record per team
teamallb <- matchdb %>%
  select(country_id, home_team_api_id) %>%
  distinct(home_team_api_id, .keep_all = TRUE) %>%
  rename(team_api_id = home_team_api_id) %>%
  arrange(country_id, team_api_id)
# merge these two together to get team ids with country and league
teamallc <- left_join(teamallb, teamalla, by = "country_id")
# merge with team file arrange by league, team name
teamall <- left_join(team, teamallc, by = "team_api_id") %>%
  select(country_id, country, league, team_api_id, team_fifa_api_id, team_long_name, team_short_name) %>%
  mutate(team_long_name = str_replace_all(team_long_name, "1. ", "")) %>%
  arrange(league, team_long_name )


# improve code for https://www.kaggle.com/stephene/home-and-away-form-compared/comments
# Restrict the objects to columns needed for the analysis
league <- select(league, id, name, country_id) %>% rename(league_id = id, league_name = name)
team   <- select(team, team_api_id, team_long_name, team_short_name) 

glimpse(matchdb)

## create home & away points, total match goals, avg goals per season, stage (by season)
points1  <- matchdb %>%
  mutate(points_home = case_when(home_team_goal > away_team_goal ~ 3,
                                home_team_goal < away_team_goal ~ 0,
                                home_team_goal == away_team_goal ~ 1)) %>%
  mutate(points_away = case_when(home_team_goal < away_team_goal ~ 3,
                                 home_team_goal > away_team_goal ~ 0,
                                 home_team_goal == away_team_goal ~ 1)) %>%
  mutate(total_goals = home_team_goal + away_team_goal) %>%
  mutate(goaldiff_ha = home_team_goal - away_team_goal) %>%
  rename(match_date = date) %>%
  # compute avg home, away & total goals per game per season
  arrange(league_id, season) %>%
  group_by(league_id, season) %>%
  mutate(gpgs_home = round(mean(home_team_goal), 2)) %>%
  mutate(gpgs_away = round(mean(away_team_goal), 2)) %>%
  mutate(gpgs_total = round(mean(total_goals), 2)) %>%
  mutate(gdiff_ha_avg = round(mean(goaldiff_ha), 2)) %>%
  ungroup() %>%
  # compute avg home, away & total goals per game per stage per season
  arrange(league_id, season, match_date) %>%
  group_by(league_id, season, stage) %>%
  mutate(gpgst_home = round(mean(home_team_goal), 2)) %>%
  mutate(gpgst_away = round(mean(away_team_goal), 2)) %>%
  mutate(gpgst_total = mean(total_goals)) %>%
  ungroup() %>%
  
  select(id, country_id, league_id, season, stage, match_api_id, match_date,
         home_team_api_id, away_team_api_id, 
         home_team_goal, away_team_goal, total_goals, goaldiff_ha, gdiff_ha_avg,
         gpgs_home, gpgs_away, gpgs_total, gpgst_home, gpgst_away, gpgst_total,
         points_home, points_away)
glimpse(points1)
## add home team names
points2 <- left_join(points1, teamall, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name_l = team_long_name, home_team_name_s = team_short_name,
         home_team_id_fifa = team_fifa_api_id) %>%
  select(-country, -league, -country_id.x, -country_id.y)

## add away team names
points3 <- left_join(points2, teamall, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name_l = team_long_name, away_team_name_s = team_short_name,
         away_team_id_fifa = team_fifa_api_id) %>%
  select(id, league_id, country, league, season, stage, match_api_id, match_date,
         home_team_api_id, home_team_name_l, home_team_name_s, home_team_id_fifa,
         away_team_api_id, away_team_name_l, away_team_name_s, away_team_id_fifa, 
         home_team_goal, away_team_goal, total_goals, goaldiff_ha, gdiff_ha_avg,
         gpgs_home, gpgs_away, gpgs_total, gpgst_home, gpgst_away, gpgst_total,
         points_home, points_away) %>%
  arrange(league_id, season, stage, match_date)

glimpse(points3)

# line plot of goals per game by season by league
points3 %>%
  select(league, season, gpgs_total) %>%
  distinct(league, season, .keep_all = TRUE) %>%
  #filter(league_id == "1729") %>%
  #distinct(season, stage, .keep_all = TRUE) %>%
  ggplot(aes(x = season, y = gpgs_total)) +
  geom_line(group = 1) +
  geom_point() +
  ylim(0, 4) +
  facet_wrap(~ league)

# line plot gpg home and away, by season by league. note need to make wide to long
points3 %>%
  select(league, season, gpgs_home, gpgs_away) %>%
  distinct(league, season, .keep_all = TRUE) %>%
  gather(key = "home_away", value = "gpg", gpgs_home:gpgs_away) %>%
  ggplot(aes(x = season, y = gpg, group = home_away)) +
  geom_line(aes(color = home_away)) +
  geom_point() +
  ylim(0, 4) +
  facet_wrap(~ league, nrow = 4, ncol = 3)


# Calculate average home team points per game
home_points <- match_points %>%
  select(league_id, team_api_id = home_team_api_id, home_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avg_home_ppg = mean(home_team_points))

# Calculate average away team points per game
away_points <- match_points %>%
  select(league_id, team_api_id = away_team_api_id, away_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avg_away_ppg = mean(away_team_points))

# Combine the data for the average home and away team points per game
all_points <- left_join(home_points, away_points, by = c("league_id", "team_api_id"))

# Add the average points scored per game (home and away)
# It's OK to take a simple average of the home and away averages as each team plays the same number of home and away games
all_points <- all_points %>%
  mutate(avg_ppg = (avg_home_ppg + avg_away_ppg)/2)

# Add the details of the league and the team to each record so that their names can be displayed on the plots
all_points <- left_join(all_points, league, by = "league_id")
all_points <- left_join(all_points, team, by = "team_api_id")