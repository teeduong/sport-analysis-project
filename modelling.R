# Source codes
source("cleaning.R")
source("analysis.R")

# Fitting linear regression for the explanatory variables
fit <- lm(W_per_game ~ X3P_per_game + X2P_per_game + FT_per_game + ORB_per_game + DRB_per_game + TOV_per_game, data = team_tbl_final)

# Descriptive Statistics 
summary(fit)

# Descriptive Statistics in table
broom::tidy(fit, conf.int = TRUE)

# There are evidence of suppression. 

## Check assumptions for multilinear regression model 
# Check independence of observations
car::durbinWatsonTest(fit)

# Durbin-Watson statistic is close to 2 which indicates 
# no correlation between residuals. 

# Check linearity 
car::avPlots(fit)

# There are a linear relationship between the response variable 
# and each explanatory variable
# However, there is evidence of negative confounding in DRB_per_game. 
# Will go back to this issue after checking the rest of assumptions. 

# Check multicollinearity
car::vif(fit)

# There are no evidence of multicollinearity as VIF scores are less than 5.

# Check homoscedasticity
ggplot(data = NULL, aes(x = predict(fit), y = residuals(fit))) +
  geom_point(colour = "dodgerblue") + 
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# There is no evidence of homoscedasticity

# Check normality of residuals
ggplot(data = NULL, aes(x = residuals(fit))) +
  geom_histogram(colour = "black", fill = "dodgerblue", bins = 8)

ggplot(data = NULL, aes(sample = residuals(fit))) +
  stat_qq() + stat_qq_line()

# Fails normality assumption. The reason might be because of small sample size
# Need to transform W_per_game using sqrt

# New model
fit_1 <- lm(sqrt(W_per_game) ~ X3P_per_game + X2P_per_game + FT_per_game + ORB_per_game + DRB_per_game + TOV_per_game, data = team_tbl_final)

ggplot(data = NULL, aes(x = residuals(fit_1))) +
  geom_histogram(colour = "black", fill = "dodgerblue", bins = 8)

ggplot(data = NULL, aes(sample = residuals(fit_1))) +
  stat_qq() + stat_qq_line()

# The distribution of residuals is not a perfectly normal due to small sample size
# However, the normality has been improved significantly. 

# Detecting Outliers 
std_res <- rstandard(fit_1)

points <- 1:length(std_res)

res_labels <- if_else(abs(std_res) >= 2.5, paste(points), "")

ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  geom_text(aes(label = res_labels), nudge_y = 0.3) +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

# No outliers detected.

# Check summary of fit_1 again
summary(fit_1)

broom::tidy(fit_1, conf.int = TRUE)

# To resolve the negative confounding, try ommitting DRB_per_game
fit_2 <- lm(sqrt(W_per_game) ~ X3P_per_game + X2P_per_game + FT_per_game + ORB_per_game + TOV_per_game, data = team_tbl_final)

summary(fit_2)

# Even though multiple R-Square of fit_2 is lower than that of fit_1, 
# the multiple R-square is still high with 92% of the variations of 
# square root Win per game around the mean are explained by
# 3 points per game, 2 points per game, free throws per game, 
# offensive rebounds per game, and turnovers per game values. 
# We will accept this fit_2 model as 
# collecting more sample is beyond the scope of this analysis. 

# Interpretation of fit_2
tbl_fit_2 <- tidy(fit_2, conf.int = TRUE)

# intercept coefficient = when all continuous explanatory variables are equal to 0,
# the estimated sqrt(W_per_game) is -22.9%
# slope coef for X3P_per_game = when X3P_per_game is increased by 1 goal, sqrt(W_per_game) increases by 0.8%, when all other variables remain fixed
# slope coef for X2P_per_game = when X2P_per_game is increased by 1 goal, sqrt(W_per_game) increases by 0.6%, when all other variables remain fixed
# slope coef for FT_per_game = when FT_per_game is increased by 1 goal, sqrt(W_per_game) increases by 0.2%, when all other variables remain fixed
# slope coef for ORB_per_game = when ORB_per_game is increased by 1 rebound, sqrt(W_per_game) increases by 0.04%, when all other variables remain fixed
# # slope coef for TOV_per_game = when TOV_per_game is increased by 1 turnover, sqrt(W_per_game) increases by 0.07%, when all other variables remain fixed.

# Model Testing
team_tbl_final %>% 
  mutate(W_hat = predict(fit_2, newdata = .)**2) %>%
  ggplot(aes(W_hat, W_per_game, label = Rk)) + 
  geom_point(aes(colour = Rk)) +
  geom_text(nudge_x=1, nudge_y = -1.1, cex = 3) + 
  geom_abline() +
  labs(x = "Predicted Win Per Game", y = "Actual Win Per Game")+
  theme(legend.position = "none")

# Create a player metric that comparable with team metric 
# Calculate the average of minutes played per game 
MP_per_game <- team_tbl_final %>% 
  group_by(Team) %>% 
  summarise(MP_per_game = sum(MP)/G) %>% 
  pull(MP_per_game) %>% 
  mean

# Predict player's win ratio
players_pred <- player_tbl %>% 
  group_by(player_name) %>%
  summarize(G = sum(MP)/MP_per_game,
            X3P_per_game = sum(X3P)/G,
            X2P_per_game = sum(X2P)/G,
            FT_per_game = sum(FT)/G, 
            ORB_per_game = sum(ORB)/G,
            TOV_per_game = sum(TOV)/G) %>% 
  select(-G) %>%
  mutate(pred_W = predict(fit_2, newdata = .))

# Remove duplicates of player_name based on max MP
players <- player_tbl %>% 
  group_by(player_name, Pos, Tm) %>% 
  summarise(MP_sum = sum(MP)) %>% 
  filter(MP_sum == max(MP_sum)) %>% 
  filter(MP_sum > 1500)

# Add pred_W to players data
players <- right_join(players, players_pred[, c("player_name", "pred_W")], by = "player_name") %>% 
  filter(!is.na(Pos))

# Add salary to players data
players <- right_join(players, player_sal_tbl[,c("player_name", "salary")], by="player_name") %>% 
  filter(!is.na(Pos)  & !is.na(salary))

# Sort players data by pred
players <- players %>% 
  arrange(desc(pred_W), salary) 



