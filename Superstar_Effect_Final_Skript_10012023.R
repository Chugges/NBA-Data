# Setting right working directory
setwd("C:/Users/klein/Desktop/FH Würzburg/7. Semester/Empirische Oekonomik/working directory")
## Opening libraries for Regressions
# Library for opening excel databases
require(readxl)
# Library tidyverse for functionality of ggplot2 & dplyr
require(stargazer)
## opening database and assigning it to variable X for easier use
X <- read_excel("Final_Projekt_NBA.xlsx", sheet = "Final") 
### First Regression
Att_Regression_start <- lm(formula = percentage_cap ~ superstar_home + rating_home_team_woS, data = subset(X), percentage_cap > 0.5)
summary(Att_Regression_start)
### Second Regression for Attendance with Interaction, Rating without Superstars
Att_Regression_wo_Sup <- lm(formula = percentage_cap ~ superstar_home + rating_home_team_woS + superstar_away + rating_away_team_woS, data = subset(X), percentage_cap > 0.5)
summary(Att_Regression_wo_Sup)
### Third Regression with Interaction Term & Control variable historic capacity of Arena of home team, Rating without Superstars
Att_Regression_control <- lm(formula = percentage_cap ~ superstar_home + rating_home_team_woS + superstar_away + rating_away_team_woS + historic_capacity, data = subset(X), percentage_cap > 0.5)
summary(Att_Regression_control)

# Visualization with stargazer
stargazer(Att_Regression_start, Att_Regression_wo_Sup, Att_Regression_control,
          dep.var.caption  = "Dep. var.: Attendance (cap = 100 %)", dep.var.labels.include = FALSE, 
          model.names = FALSE,
          model.numbers = TRUE, #column.labels = c("x", "x"), 
          decimal.mark=",", digit.separator=".", single.row = FALSE,
          omit.stat = c("rsq", "ser"),
          notes.label = "Notes:", notes.align = "l",
          notes = "Std. errors in parentheses.",
          type="html", out="Attendance.html")

### First WL_Rate
WL_Base <- lm(formula = X$home_team_win ~ X$superstar_home + X$rating_home_team_woS)
summary(WL_Base)
### Regression Win-Loss Rate
WL_Teams <- lm(formula = X$home_team_win ~ X$superstar_home + X$superstar_away + X$rating_home_team_woS + X$rating_away_team_woS)
summary(WL_Teams)
### Dritte Regression
Reg_WL <- lm(formula = X$home_team_win ~ X$home_team_offensiv + X$home_team_defensiv + X$rating_home_team_woS + X$away_team_offensiv + X$away_team_defensiv + X$rating_away_team_woS)
summary(Reg_WL)
# control Variable home & away superstar

# Visualization with Stargazer
stargazer(WL_Base, WL_Teams, Reg_WL,
          dep.var.caption  = "Dep. var.: Win Loss of home team", dep.var.labels.include = FALSE, 
          model.names = FALSE,
          model.numbers = TRUE, #column.labels = c("x", "x"), 
          decimal.mark=",", digit.separator=".", single.row = FALSE,
          omit.stat = c("rsq", "ser"),
          notes.label = "Notes:", notes.align = "l",
          notes = "Std. errors in parentheses.",
          type="html", out="WL_Rate.html")
