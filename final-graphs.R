# Source codes
source("cleaning.R")
source("analysis.R")
source("modelling.R")


# Team payroll 2019-20 
plt_tm_payroll <- team_pay_tbl %>% 
  ggplot(aes(x = reorder(team, salary), y = salary)) +
  geom_bar(aes(fill = salary), stat = "identity") +
  coord_flip() +
  labs(x = "Team") +
  scale_y_continuous(name = "Team Payroll (Milions)", labels = scales::comma) +
  theme(legend.position = "none")

# Choosing players
players_points <- 1:length(players$Pos)

# Players with top pred_W and have salary of less than $20 millon
player_C <- which(players[, "Pos"] == "C" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_PF <- which(players[, "Pos"] == "PF" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_PG <- which(players[, "Pos"] == "PG" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_SF <- which(players[, "Pos"] == "SF" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_SG <- which(players[, "Pos"] == "SG" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]

# Index of chosen players
players_chosen <- c(player_C, player_PF, player_PG, player_SF, player_SG)

# Store their names in a vector
players_points <- if_else(players_points %in% players_chosen, 
                          paste(players$player_name[players_points]),"")

# Players chosen
plt_players <- players %>% 
  ggplot(aes(x = salary/1000000, y = pred_W, color = Pos)) + 
  geom_point() +
  labs(x = "Salary (Millions)", y = "Predicted Win Ratio") +
  xlim(c(0,20)) +
  ylim(c(10,30)) +
  geom_text(aes(label = players_points), nudge_y = 1.0)+
  theme(legend.position = "top")

tbl_players <- ggtexttable(players[players_chosen,], rows = NULL, theme = ttheme("mOrange"))


