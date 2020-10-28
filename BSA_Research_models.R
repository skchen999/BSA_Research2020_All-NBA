library(dplyr)
library(car)
library(caret)
nba_filter2 = read.csv('nba_filtered2.csv')
nba_allnba = nba_filter2 %>% select(-c(FIRST_TEAM,SECOND_TEAM,THIRD_TEAM,ALL_DEFENSE,ALL_STAR))
nba_train =  nba_allnba %>% filter(YEAR == '2009-10' | YEAR == '2010-11' | YEAR == '2013-14' | YEAR == '2014-15' | YEAR == '2016-17' | YEAR == '2018-19')
nba_test = nba_allnba %>% filter(YEAR == '2011-12' | YEAR == '2012-13' | YEAR == '2015-16' | YEAR == '2017-18')


### Best subsets regression (forward) ###
library(leaps)
regfwd = regsubsets(ALL_NBA ~ ., data = nba_train[,c(-1,-2)], method = 'forward', really.big = TRUE, nvmax = 40)
plot(regfwd$rss[1:30]) # rss plot
which(summary(regfwd)[[1]][8,]) # predictors for n-predictor model

# 10 VAR -- FORWARD BEST SUBSETS REGRESSION
regfwd_mod10 = glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + EFG_PCT + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_pred10 = predict(regfwd_mod10, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_pred10_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_pred10)
#regfwd_pred_df %>% arrange(desc(score)) %>% head(60) %>% arrange(PLAYER_NAME)
regfwd_pred10_df = regfwd_pred10_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
regfwd_pred10_df = cbind(regfwd_pred10_df, ALL_NBA_real = nba_test$ALL_NBA)
regfwd_pred10_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred)
table(regfwd_pred10_df$ALL_NBA_pred, regfwd_pred10_df$ALL_NBA_real)
summary(regfwd_mod10)
vif(regfwd_mod10)
AIC(regfwd_mod10)
BIC(regfwd_mod10)

### REFINING ###
findCorrelation(cor(nba_train %>% dplyr::select(c(
  ALL_NBA, MIN, GP, PTS, FTA, TD3, NBA_FANTASY_PTS, PER, WS, EFG_PCT, TS_PCT))), names = TRUE)
## output: NBA_FANTASY_PTS, EFG_PCT

findCorrelation(cor(nba_train %>% dplyr::select(c(
  ALL_NBA, MIN, GP, PTS, FTA, TD3, PER, WS, TS_PCT))), names = TRUE)

###### **  BEST MODEL ** #############
regfwd_modrefine8 = glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + PER + WS + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_predrefine8 = predict(regfwd_modrefine8, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_predrefine8_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_predrefine8)
regfwd_predrefine8_df = regfwd_predrefine8_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
regfwd_predrefine8_df = cbind(regfwd_predrefine8_df, ALL_NBA_real = nba_test$ALL_NBA)
regfwd_predrefine8_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred, ALL_NBA_real, YEAR)
table(regfwd_predrefine8_df$ALL_NBA_pred, regfwd_predrefine8_df$ALL_NBA_real)
summary(regfwd_modrefine8)
vif(regfwd_modrefine8)
AIC(regfwd_modrefine8)
BIC(regfwd_modrefine8)


# 10 VAR -- SELECT K BEST
select10best_mod = glm(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS, data = nba_train[,c(-1,-2)], family = binomial)
select10best_pred = predict(select10best_mod, type = 'response', newdata = nba_test[,c(-1,-2)])
select10best_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = select10best_pred)
select10best_pred_df = select10best_pred_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
select10best_pred_df = cbind(select10best_pred_df, ALL_NBA_real = nba_test$ALL_NBA)
select10best_pred_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred)
table(select10best_pred_df$ALL_NBA_pred, select10best_pred_df$ALL_NBA_real)
summary(select10best_mod)
vif(select10best_mod)
AIC(select10best_mod)
BIC(select10best_mod)

### REFINING
findCorrelation(cor(nba_train %>% dplyr::select(c(
  ALL_NBA, PTS, PER, PIE, DD2, FTM, FTA, NBA_FANTASY_PTS, PFD, OWS, WS))), names = TRUE)

select10best_modrefine = glm(ALL_NBA ~ PTS + PIE + DD2 + FTM + OWS, data = nba_train[,c(-1,-2)], family = binomial)
select10best_predrefine = predict(select10best_modrefine, type = 'response', newdata = nba_test[,c(-1,-2)])
select10best_predrefine_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = select10best_predrefine)
select10best_predrefine_df = select10best_predrefine_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
select10best_predrefine_df = cbind(select10best_predrefine_df, ALL_NBA_real = nba_test$ALL_NBA)
select10best_predrefine_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred)
table(select10best_predrefine_df$ALL_NBA_pred, select10best_predrefine_df$ALL_NBA_real)
summary(select10best_modrefine)
vif(select10best_modrefine)
AIC(select10best_modrefine)
BIC(select10best_modrefine)


# 15 VAR (best subsets + select k-best) COMBO
combo_mod15 = glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + EFG_PCT + TS_PCT + DD2 + PFD + OWS + PIE + FTM, data = nba_train[c(-1,-2)], family = binomial)
combo_pred15 = predict(combo_mod15, type = 'response', newdata = nba_test[,c(-1,-2)])
combo_pred15_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = combo_pred15)
combo_pred15_df = combo_pred15_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
combo_pred15_df = cbind(combo_pred15_df, ALL_NBA_real = nba_test$ALL_NBA)
combo_pred15_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred)
table(combo_pred15_df$ALL_NBA_pred, combo_pred15_df$ALL_NBA_real)
summary(combo_mod15)
vif(combo_mod15)
AIC(combo_mod15)
BIC(combo_mod15)

### REFINING
findCorrelation(cor(nba_train %>% dplyr::select(c(
  ALL_NBA, MIN, GP, PTS, FTA, TD3, NBA_FANTASY_PTS, PER, WS, EFG_PCT, TS_PCT, DD2, PFD, OWS, PIE, FTM))), names = TRUE)

combo_mod15refine = glm(ALL_NBA ~ MIN + GP + PTS + TD3 + TS_PCT + DD2 + OWS + PIE + FTM, data = nba_train[c(-1,-2)], family = binomial)
combo_pred15refine = predict(combo_mod15refine, type = 'response', newdata = nba_test[,c(-1,-2)])
combo_pred15refine_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = combo_pred15refine)
combo_pred15refine_df = combo_pred15refine_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
combo_pred15refine_df = cbind(combo_pred15refine_df, ALL_NBA_real = nba_test$ALL_NBA)
combo_pred15refine_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred)
table(combo_pred15refine_df$ALL_NBA_pred, combo_pred15refine_df$ALL_NBA_real)
summary(combo_mod15refine)
vif(combo_mod15refine)
AIC(combo_mod15refine)
BIC(combo_mod15refine)


findCorrelation(cor(nba_train %>% dplyr::select(c(
  ALL_NBA, MIN, GP, PTS, TD3, TS_PCT, DD2, OWS, PIE, FTM))), names = TRUE)

MIN + GP + PTS + FTA + TD3 + PER + WS + TS_PCT + PIE + DD2 + FTM + OWS
MIN + GP + PTS + TD3 + TS_PCT + PIE + DD2 + FTM + OWS

refinemod1 = glm(ALL_NBA ~ MIN + GP + PTS + TD3 + TS_PCT + PIE + DD2 + FTM + OWS, data = nba_train[c(-1,-2)], family = binomial)
refinepred1 = predict(refinemod1, type = 'response', newdata = nba_test[,c(-1,-2)])
refinepred1_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = refinepred1)
refinepred1_df = refinepred1_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
refinepred1_df = cbind(refinepred1_df, ALL_NBA_real = nba_test$ALL_NBA)
refinepred1_df %>% filter(score > 0.5 | ALL_NBA_real == 1) %>% arrange(ALL_NBA_pred)
table(refinepred1_df$ALL_NBA_pred, refinepred1_df$ALL_NBA_real)
summary(refinemod1)
vif(refinemod1)
AIC(refinemod1)
BIC(refinemod1)



# GRAPHS
library(ggplot2)
library(tidyr)
nba_filter2 %>% dplyr::select(ALL_NBA, MIN, GP, PTS, FTA, TD3, PER, WS, TS_PCT) %>% 
  mutate(ALL_NBA = factor(ALL_NBA, levels = c(0,1), labels = c("No","Yes"))) %>% 
  gather("stat","value",-ALL_NBA) %>% 
  ggplot(aes(x=ALL_NBA,y=value,fill=ALL_NBA)) +
  geom_boxplot() +
  labs(x="",y="Value",fill="All-NBA") +
  facet_wrap(~stat,ncol=4,scales="free")


#### OLD BUSY WORK ##########################
# 7 variable model/predictions
regfwd_mod7 = glm(ALL_NBA ~ MIN + GP + PTS + TD3 + PER + WS + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_pred7 = predict(regfwd_mod7, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_pred7_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_pred7)
regfwd_pred7_df %>% filter(score > 0.5) # all predicted ALL_NBA players
regfwd_pred7_df = regfwd_pred7_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
table(regfwd_pred7_df$ALL_NBA_pred, nba_test$ALL_NBA) # confusion matrix

# 9 variable model/predictions **GOOD**
regfwd_mod9 = glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_pred9 = predict(regfwd_mod9, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_pred9_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_pred9)
regfwd_pred9_df %>% filter(score > 0.5)
regfwd_pred9_df = regfwd_pred9_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
table(regfwd_pred9_df$ALL_NBA_pred, nba_test$ALL_NBA) # confusion matrix

# 9 variable with ridge
library(glmnet)
nba_train_mat9 = model.matrix(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + TS_PCT, data = nba_train[,c(-1,-2)])
nba_test_mat9 = model.matrix(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + TS_PCT, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_9 = glmnet(nba_train_mat9, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_9_cv = cv.glmnet(nba_train_mat9, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_9_cv$lambda.min
ridge9_pred = predict(ridge_9, s = bestL, newx = nba_test_mat9)
ridge9_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge9_pred)))
ridge9_pred_df %>% filter(X1 > 0.5)


# 10 variable with ridge
nba_train_mat10 = model.matrix(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + EFG_PCT + TS_PCT, data = nba_train[,c(-1,-2)])
nba_test_mat10 = model.matrix(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + EFG_PCT + TS_PCT, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_10 = glmnet(nba_train_mat10, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_10_cv = cv.glmnet(nba_train_mat10, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_10_cv$lambda.min
ridge10_pred = predict(ridge_10, s = bestL, newx = nba_test_mat10)
ridge10_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge10_pred)))
ridge10_pred_df %>% filter(X1 > 0.5)

# 15 variable model/predictions
regfwd_mod15 = glm(ALL_NBA ~ MIN + GP + PTS + FG3A + FTA + FT_PCT + OREB + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + EFG_PCT + TS_PCT + POSS, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_pred15 = predict(regfwd_mod15, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_pred15_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_pred15)
regfwd_pred15_df %>% filter(score > 0.5)
regfwd_pred15_df = regfwd_pred15_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
table(regfwd_pred15_df$ALL_NBA_pred, nba_test$ALL_NBA)


# 15 variable with ridge
nba_train_mat15 = model.matrix(ALL_NBA ~ MIN + GP + PTS + FG3A + FTA + FT_PCT + OREB + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + EFG_PCT + TS_PCT + POSS, data = nba_train[,c(-1,-2)])
nba_test_mat15 = model.matrix(ALL_NBA ~ MIN + GP + PTS + FG3A + FTA + FT_PCT + OREB + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + EFG_PCT + TS_PCT + POSS, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_15 = glmnet(nba_train_mat15, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_15_cv = cv.glmnet(nba_train_mat15, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_15_cv$lambda.min
ridge15_pred = predict(ridge_15, s = bestL, newx = nba_test_mat15)
ridge15_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge15_pred)))
ridge15_pred_df %>% filter(X1 > 0.5)

# 20 variable model/predictions **GOOD**
regfwd_mod20 = glm(ALL_NBA ~ MIN + GP + W_PCT + PTS + FG3A + FTA + FT_PCT + OREB + PF + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + PIE + POSS, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_pred20 = predict(regfwd_mod20, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_pred20_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_pred20)
regfwd_pred20_df %>% filter(score > 0.5)
regfwd_pred20_df = regfwd_pred20_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
table(regfwd_pred20_df$ALL_NBA_pred, nba_test$ALL_NBA)

# 20 variables with ridge
nba_train_mat20 = model.matrix(ALL_NBA ~ MIN + GP + W_PCT + PTS + FG3A + FTA + FT_PCT + OREB + PF + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + PIE + POSS, data = nba_train[,c(-1,-2)])
nba_test_mat20 = model.matrix(ALL_NBA ~ MIN + GP + W_PCT + PTS + FG3A + FTA + FT_PCT + OREB + PF + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + PIE + POSS, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_20 = glmnet(nba_train_mat20, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_20_cv = cv.glmnet(nba_train_mat20, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_20_cv$lambda.min
ridge20_pred = predict(ridge_20, s = bestL, newx = nba_test_mat20)
ridge20_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge20_pred)))
ridge20_pred_df = ridge20_pred_df %>% mutate(ALL_NBA_pred = ifelse(X1 > 0.5, 1, 0))
table(ridge20_pred_df$ALL_NBA_pred, nba_test$ALL_NBA)
ridge20_pred_df %>% filter(X1 > 0.5)

# 30 variable model/predictions
regfwd_mod30 = glm(ALL_NBA ~ MIN + GP + W_PCT + PTS + FGA + FG3A + FTA + FT_PCT + OREB + BLK + PF + PFD + PLUS_MINUS + OPP_PTS_PAINT + DD2 + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + E_NET_RATING + NET_RATING + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + USG_PCT + E_USG_PCT + PIE + POSS, data = nba_train[,c(-1,-2)], family = binomial)
regfwd_pred30 = predict(regfwd_mod30, type = 'response', newdata = nba_test[,c(-1,-2)])
regfwd_pred30_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = regfwd_pred30)
regfwd_pred30_df %>% filter(score > 0.5)
regfwd_pred30_df = regfwd_pred30_df %>% mutate(ALL_NBA_pred = ifelse(score > 0.5, 1, 0))
table(regfwd_pred30_df$ALL_NBA_pred, nba_test$ALL_NBA)

# 30 variables with ridge
nba_train_mat30 = model.matrix(ALL_NBA ~ MIN + GP + W_PCT + PTS + FGA + FG3A + FTA + FT_PCT + OREB + BLK + PF + PFD + PLUS_MINUS + OPP_PTS_PAINT + DD2 + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + E_NET_RATING + NET_RATING + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + USG_PCT + E_USG_PCT + PIE + POSS, data = nba_train[,c(-1,-2)])
nba_test_mat30 = model.matrix(ALL_NBA ~ MIN + GP + W_PCT + PTS + FGA + FG3A + FTA + FT_PCT + OREB + BLK + PF + PFD + PLUS_MINUS + OPP_PTS_PAINT + DD2 + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + E_NET_RATING + NET_RATING + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + USG_PCT + E_USG_PCT + PIE + POSS, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_30 = glmnet(nba_train_mat30, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_30_cv = cv.glmnet(nba_train_mat30, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_30_cv$lambda.min
ridge30_pred = predict(ridge_30, s = bestL, newx = nba_test_mat30)
ridge30_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge30_pred)))
ridge30_pred_df = ridge30_pred_df %>% mutate(ALL_NBA_pred = ifelse(X1 > 0.5, 1, 0))
table(ridge30_pred_df$ALL_NBA_pred, nba_test$ALL_NBA)
ridge30_pred_df %>% filter(X1 > 0.5)

### Ridge (all variables) ### **BEST**
library(glmnet)
nba_train_mat = model.matrix(ALL_NBA ~., data = nba_train[,c(-1,-2)])
nba_test_mat = model.matrix(ALL_NBA ~., data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_full = glmnet(nba_train_mat, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_full_cv = cv.glmnet(nba_train_mat, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_full_cv$lambda.min
ridge_pred = predict(ridge_full, s = bestL, newx = nba_test_mat)
ridge_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge_pred)))
ridge_pred_df %>% filter(X1 > 0.5)
ridge_pred_df = ridge_pred_df %>% mutate(ALL_NBA_pred = ifelse(X1 > 0.5, 1, 0))
table(ridge_pred_df$ALL_NBA_pred, nba_test$ALL_NBA)

# 20 variable select k best ridge
library(glmnet)
nba_train_mat20 = model.matrix(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS + MIN + FGM + FGA + DREB + AST + TOV + PLUS_MINUS + DWS + USG_PCT + E_USG_PCT, data = nba_train[,c(-1,-2)])
nba_test_mat20 = model.matrix(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS + MIN + FGM + FGA + DREB + AST + TOV + PLUS_MINUS + DWS + USG_PCT + E_USG_PCT, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_20 = glmnet(nba_train_mat20, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_20_cv = cv.glmnet(nba_train_mat20, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_20_cv$lambda.min
ridge20_pred = predict(ridge_20, s = bestL, newx = nba_test_mat20)
ridge20_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge20_pred)))
ridge20_pred_df = ridge20_pred_df %>% mutate(ALL_NBA_pred = ifelse(X1 > 0.5, 1, 0))
table(ridge20_pred_df$ALL_NBA_pred, nba_test$ALL_NBA)

# 30 variable select k best ridge
library(glmnet)
nba_train_mat30 = model.matrix(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS + MIN + FGM + FGA + DREB + AST + TOV + PLUS_MINUS + DWS + USG_PCT + E_USG_PCT + REB + STL + DEF_WS + OPP_PTS_2ND_CHANCE + OPP_PTS_PAINT + TD3 + OFF_RATING + E_NET_RATING + NET_RATING + POSS, data = nba_train[,c(-1,-2)])
nba_test_mat30 = model.matrix(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS + MIN + FGM + FGA + DREB + AST + TOV + PLUS_MINUS + DWS + USG_PCT + E_USG_PCT + REB + STL + DEF_WS + OPP_PTS_2ND_CHANCE + OPP_PTS_PAINT + TD3 + OFF_RATING + E_NET_RATING + NET_RATING + POSS, data = nba_test[,c(-1,-2)])
grid = 10 ^ seq(10,-2, length = 100)
ridge_30 = glmnet(nba_train_mat30, as.factor(nba_train$ALL_NBA), alpha = 0, family = 'binomial', lambda = grid)
ridge_30_cv = cv.glmnet(nba_train_mat30, (nba_train$ALL_NBA), alpha = 0)
bestL = ridge_30_cv$lambda.min
ridge30_pred = predict(ridge_30, s = bestL, newx = nba_test_mat30)
ridge30_pred_df = data.frame(PLAYER_NAME = nba_test$PLAYER_NAME, YEAR = nba_test$YEAR, score = 1/(1+exp(-ridge30_pred)))
ridge30_pred_df = ridge30_pred_df %>% mutate(ALL_NBA_pred = ifelse(X1 > 0.5, 1, 0))
table(ridge30_pred_df$ALL_NBA_pred, nba_test$ALL_NBA)

### ORDINAL REGRESSION ###
nba_filter3 = nba_filter2 %>% mutate(ALL_NBA_TEAM = ifelse(FIRST_TEAM == 1, 1, ifelse(SECOND_TEAM == 1, 2, ifelse(THIRD_TEAM == 1, 3, 0))))
nba_filter3$ALL_NBA_TEAM = as.factor(nba_filter3$ALL_NBA_TEAM)
allnba = nba_filter3 %>% dplyr::select(-c(ALL_NBA,FIRST_TEAM,SECOND_TEAM,THIRD_TEAM,ALL_DEFENSE,ALL_STAR))
train =  allnba %>% filter(YEAR == '2009-10' | YEAR == '2010-11' | YEAR == '2013-14' | YEAR == '2014-15' | YEAR == '2016-17' | YEAR == '2018-19')
test = allnba %>% filter(YEAR == '2011-12' | YEAR == '2012-13' | YEAR == '2015-16' | YEAR == '2017-18')

library(MASS)

# polr doesnt work :(

ord_regfwd = regsubsets(ALL_NBA_TEAM ~ ., data = nba_filter3 %>% filter(ALL_NBA_TEAM > 0) %>% dplyr::select(-c(PLAYER_NAME,YEAR,ALL_NBA,FIRST_TEAM,SECOND_TEAM,THIRD_TEAM,ALL_DEFENSE,ALL_STAR)), method = 'forward', really.big = TRUE, nvmax = 20)
