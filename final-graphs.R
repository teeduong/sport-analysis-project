source("cleaning.R")
source("analysis.R")
source("modelling.R")

# Choosing players
players_points <- 1:length(players$Pos)

player_C <- which(players[, "Pos"] == "C" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_PF <- which(players[, "Pos"] == "PF" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_PG <- which(players[, "Pos"] == "PG" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_SF <- which(players[, "Pos"] == "SF" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]
player_SG <- which(players[, "Pos"] == "SG" & players[, "salary"] < 20000000, arr.ind = TRUE)[1, "row"]

players_chosen <- c(player_C, player_PF, player_PG, player_SF, player_SG)

players_points <- if_else(players_points %in% players_chosen, 
                          paste(players$player_name[players_points]),"")

# Players chosen
players %>% 
  ggplot(aes(x = salary/1000000, y = pred_W, color = Pos)) + 
  geom_point() +
  xlab("Salary (Millions)") +
  xlim(c(0,20)) +
  ylim(c(10,40)) +
  geom_text(aes(label = players_points), nudge_y = 0.5)