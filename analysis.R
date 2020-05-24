source("cleaning.R")

library(ggpubr)

## Based on Four Factor model
## Create a metric Win Ratio (W/G) to value players 
## based on successful actions and their relative contribution to Wins 

## How does team 3P values relate to success?
## How does team 3P values relate to success?
## How does team FT values relate to success?
## We can use Win Ratio (W/G) as a measure of success to compare teams

# Data transformation
team_tbl_final <-  team_tbl %>% 
  mutate(X3P_per_game = X3P/G,
         X2P_per_game = X2P/G,
         FT_per_game = FT/G,
         TOV_per_game = TOV/G,
         ORB_per_game = ORB/G,
         DRB_per_game = DRB/G,
         W_per_game = W/G*100)
         
# Explanatory variables
vars <- grep("\\per_game$", names(team_tbl_final), value = TRUE)

# How does team 3P values relate to success?
plt_3P <- team_tbl_final %>% 
  ggplot(aes(x = X3P_per_game, y = W_per_game)) +
  geom_point(alpha = 0.5, colour = "dodgerblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "red")+
  ylim(c(20,80)) 

# How does team 2P values relate to success?
plt_2P <- team_tbl_final %>% 
  ggplot(aes(x = X2P_per_game, y = W_per_game)) +
  geom_point(alpha = 0.5, colour = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  ylim(c(20,80))

# How does team FT values relate to success?
plt_FT <- team_tbl_final %>% 
  ggplot(aes(x = FT_per_game, y = W_per_game)) +
  geom_point(alpha = 0.5, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  ylim(c(20,80))

# How does team TOV values relate to success?
plt_TOV <- team_tbl_final %>% 
  ggplot(aes(x = TOV_per_game, y = W_per_game)) +
  geom_point(alpha = 0.5, colour = "blue") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  ylim(c(20,80)) 

# How does team ORB values relate to success?
plt_ORB <- team_tbl_final %>% 
  ggplot(aes(x = ORB_per_game, y = W_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  ylim(c(20,80))

# How does team DRB values relate to success?
plt_DRB <- team_tbl_final %>% 
  ggplot(aes(x = DRB_per_game, y = W_per_game)) +
  geom_point(alpha = 0.5, colour = "green") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  ylim(c(20,80))

ggarrange(plt_3P, plt_2P, plt_FT, plt_TOV, plt_ORB, plt_DRB,
          ncol = 3, nrow = 2)

# Take a closer look to the those variables with correlation > 0.3,
corr_vol <- team_tbl_final %>% 
  select_if(names(.) %in% vars) %>% 
  cor() %>% 
  .[, "W_per_game"] 

# Correlation coefficient:
corr <- tibble(exp_vars = names(corr_vol),
               corr_W = corr_vol) %>% 
        arrange(desc(corr_W))

# Plot ggpairs of W_per_game against explanatory variables
plt_pairs <- team_tbl_final %>% 
  select_if(names(.) %in% vars) %>% 
  ggpairs(axisLabels = "none")

ggtexttable(corr, rows = NULL, 
            theme = ttheme("mOrange"))



