# Load library
library(tidyverse)
library(GGally)
library(PerformanceAnalytics)
library(ggpubr)
library(modelr)
library(broom)

# Read team advanced statistics data
team_adv_stat <- read_csv("data/raw/2018-19_nba_team_statistics_1.csv") 

# Read team basic statistics data
team_bas_stat <- read_csv("data/raw/2018-19_nba_team_statistics_2.csv") 

# Read team payroll data 
team_pay_stat <- read_csv("data/raw/2019-20_nba_team-payroll.csv")

# Read player salaries data 
player_sal_stat <- read_csv("data/raw/2018-19_nba_player-salaries.csv")

# Read player salaries data 
player_stat <- read_csv("data/raw/2018-19_nba_player_statistics.csv")

# Check structures 
str(team_adv_stat)

str(team_bas_stat)

str(player_stat)

str(player_sal_stat)

str(team_pay_stat)

# Combine team data
team_stat <- as_tibble(cbind(team_adv_stat, team_bas_stat[, -c(1:2)]))

# Check idential of the joined tbl
stopifnot(identical(team_stat[, c(1:25)], team_adv_stat[, c(1:25)]),
          identical(team_stat[, c(26:48)], team_bas_stat[, -c(1:2)]))

# check str of the joined tbl
str(team_stat)

# Check for missing values
which(is.na(team_stat), arr.ind = TRUE)

which(is.na(player_stat), arr.ind = TRUE)

which(is.na(player_sal_stat), arr.ind = TRUE)

# Transform joined team data 
team_tbl <- team_stat %>% 
  select_if(!colSums(is.na(.)) == nrow(team_stat)) %>%
  rename(X3PAr = `3PAr`,
         TS_Percent = `TS%`,
         eFG_Percent = `eFG%`,
         TOV_Percent = `TOV%`, 
         ORB_Percent = `ORB%`, 
         FT_FGA = `FT/FGA`,
         DRB_Percent = `DRB%`, 
         FG_Percent = `FG%`,
         X3P = `3P`,
         X3PA = `3PA`,
         X3P_Percent = `3P%`,
         X2P = `2P`,
         X2PA = `2PA`,
         X2P_Percent = `2P%`,
         FT_Percent = `FT%`) %>% 
  mutate(Rk = as.factor(Rk)) 

# Take a look at team_tbl
head(team_tbl)

# Transform player salary data 
player_sal_tbl <- player_sal_stat %>%  
  select_if(!colSums(is.na(.)) == nrow(player_sal_stat)) %>% 
  mutate(player_name = stringi::stri_trans_general(player_name, "Latin-ASCII")) %>% 
  mutate(first_name = player_name,
         last_name = player_name) %>% 
  mutate(first_name = str_replace(first_name, " .*",""), 
         last_name = str_replace(last_name, ".* ","")) %>% 
  select(-salary, salary)

# Transform player data
player_tbl <- player_stat %>% 
  rename(eFG_Percent = `eFG%`,
         FG_Percent = `FG%`,
         X3P = `3P`,
         X3PA = `3PA`,
         X3P_Percent = `3P%`,
         X2P = `2P`,
         X2PA = `2PA`,
         X2P_Percent = `2P%`,
         FT_Percent = `FT%`) %>% 
  mutate(player_name = str_replace_all(player_name, "\\.", ""), 
         player_name = stringi::stri_trans_general(player_name, "Latin-ASCII"),
         Pos = str_replace(Pos, "-.*", ""))

# Transform team payroll data
team_pay_tbl <- team_pay_stat %>% 
  mutate(team_id = as_factor(team_id), 
         salary = parse_number(salary)) 




