library(dplyr)
library(car)
library(caret)
library(tidyr)
nba_final = read.csv('/users/stephenchen/Downloads/nba_filter_final.csv')[,-1]
nba_final = nba_final %>% distinct(PLAYER_NAME,YEAR,.keep_all = TRUE)
write.csv(nba_final,'nba_final_2.csv', row.names = FALSE)
nba_2020 = read.csv('/users/stephenchen/Downloads/full_2020_stats.csv')[,-1]
nba_allnba = nba_final %>% dplyr::select(-c(FIRST_TEAM,SECOND_TEAM,THIRD_TEAM,ALL_DEFENSE,ALL_STAR))
nba_train =  nba_allnba %>% filter(YEAR == '2009-10' | YEAR == '2010-11' | YEAR == '2013-14' | YEAR == '2014-15' | YEAR == '2016-17' | YEAR == '2018-19')
nba_test = nba_allnba %>% filter(YEAR == '2011-12' | YEAR == '2012-13' | YEAR == '2015-16' | YEAR == '2017-18')


### FORWARD STEPWISE

# 7 VAR
regfwd_mod7 = glm(ALL_NBA ~ MIN + GP + PTS + TD3 + PER + WS + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)

# 10 VAR 
regfwd_mod10 = glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + NBA_FANTASY_PTS + PER + WS + EFG_PCT + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)

# 20 VAR
regfwd_mod20 = glm(ALL_NBA ~ MIN + GP + W_PCT + PTS + FG3A + FTA + FT_PCT + OREB + PF + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + PIE + POSS, data = nba_train[,c(-1,-2)], family = binomial)

# 20 VAR W/ RIDGE
library(leaps)
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

# 30 VAR
regfwd_mod30 = glm(ALL_NBA ~ MIN + GP + W_PCT + PTS + FGA + FG3A + FTA + FT_PCT + OREB + BLK + PF + PFD + PLUS_MINUS + OPP_PTS_PAINT + DD2 + TD3 + NBA_FANTASY_PTS + PER + OWS + WS + E_NET_RATING + NET_RATING + AST_TO + E_TOV_PCT + EFG_PCT + TS_PCT + USG_PCT + E_USG_PCT + PIE + POSS, data = nba_train[,c(-1,-2)], family = binomial)

# 30 VAR W/ RIDGE
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


### SELECT K BEST

# 10 VAR
select10best_mod = glm(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS, data = nba_train[,c(-1,-2)], family = binomial)

# 20 VAR
mod20 = glm(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS + MIN + FGM + FGA + DREB + AST + TOV + PLUS_MINUS + DWS + USG_PCT + E_USG_PCT, data = nba_train[,c(-1,-2)], family = binomial)

# 20 VAR W/ RIDGE
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

# 30 VAR
mod30 = glm(ALL_NBA ~ PTS + PER + PIE + DD2 + FTM + FTA + NBA_FANTASY_PTS + PFD + OWS + WS + MIN + FGM + FGA + DREB + AST + TOV + PLUS_MINUS + DWS + USG_PCT + E_USG_PCT + REB + STL + DEF_WS + OPP_PTS_2ND_CHANCE + OPP_PTS_PAINT + TD3 + OFF_RATING + E_NET_RATING + NET_RATING + POSS, data = nba_train[,c(-1,-2)], family = binomial)

# 30 VAR W/ RIDGE
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

### FINAL MODEL - 8 VAR ###
regfwd_modrefine8 = glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + PER + WS + TS_PCT, data = nba_train[,c(-1,-2)], family = binomial)


# DF WITH ALL MODELS' PREDICTION ACCURACY
model_acc = data.frame(
'Model' = c('Forward stepwise, 7-predictor', 'Forward stepwise, 10-predictor', 'Forward stepwise, 20-predictor',
            'Forward stepwise, 20-predictor w/ ridge regression', 'Forward stepwise, 30-predictor',
            'Forward stepwise, 30-predictor w/ ridge regression', 
            'Select k-best, 10-predictor',
            'Select k-best, 20-predictor', 
            'Select k-best, 20-predictor w/ ridge regression', 'Select k-best, 30-predictor',
            'Select k-best, 30-predictor w/ ridge regression', 
            'Final Model: 8-predictor'), 
'Accuracy' = paste(round(c(33,36,41,32,46,41,36,41,35,43,35,34) / 60 * 100, 2), '%', sep = ''))

# new column for CV scores, once you finish running CV for all the models, you can add it to this column. 
# you can use round() to round the numbers to 2 decimal places
model_acc$CV = NA

nba_allnba <- nba_allnba[sample(nrow(nba_allnba)),]
folds <- cut(seq(1,nrow(nba_allnba)), breaks = 5, labels = FALSE)
CV_errors <- rep(0,5)
for(i in 1:5){
  test.i <- which(folds == i, arr.ind = TRUE)
  glm1 <- glm(ALL_NBA ~ MIN + GP + PTS + FTA + TD3 + PER + WS + TS_PCT, data = nba_allnba[-test.i,c(-1,-2)], family = binomial)
  glm.pred <- predict(glm1, newdata = nba_allnba[test.i,c(-1,-2)], type = 'response')
  #print(glm.pred)
  glm.pred.val <- ifelse(glm.pred > .5, 1,0)
  CV_errors[i] <- 1 - mean(glm.pred.val == nba_allnba[test.i,c(-1,-2)]$ALL_NBA)
  print(table(glm.pred.val, nba_allnba[test.i,c(-1,-2)]$ALL_NBA))
}
mean(CV_errors)