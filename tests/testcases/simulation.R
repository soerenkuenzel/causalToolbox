devtools::load_all()
library(ranger)
library(forestry)
library(MASS)
library(Matching)
library(hte)

calculate_e <- function(train_df, test_features) {
  train_df[['tr']] <- as.factor(train_df[['tr']])
  rf <- ranger(formula = tr ~ ., data = droplevels(train_df), probability = T)
  predicted_e <- predict(rf, test_features)$predictions[ ,2]
  return(predicted_e)
}

calculate_y_star <- function(test_df, predicted_e){
  test_df[['tr']] <- as.integer(test_df[['tr']])
  y_star <- test_df[['yobs']] / ((test_df[['tr']]) * predicted_e -
                                   (1 - test_df[['tr']]) * (1 - predicted_e))
  return(y_star)
}

eval_TO <- function(learner_func, y_obs, x_te, w_te, y.diff){
  learner <- learner_func(feat = x_te, tr = w_te, yobs = y_obs)
  estimate <- EstimateCate(learner, x_te)
  gof <- - sum((y.diff - estimate)^2) / nrow(x_te)
  return(gof)
}

#calculate_ydiff <- function(yobs, w_te, x_te){
#  m <- Match(Y = yobs, Tr = w_te, X = x_te)
#  matched_df <- rbind(yobs[m$index.treated],yobs[m$index.control])
#  y.diff <- matched_df[1,] - matched_df[2,]
#  return(y.diff)
#}
eval_M <- function(learner_func, yobs, w_te, x_te) {
  m <- Match(Y = yobs, Tr = w_te, X = x_te)
  matched_df <- rbind(yobs[m$index.treated],yobs[m$index.control])
  y.diff <- matched_df[1,] - matched_df[2,]
  learner_tr <- learner_func(x_te[m$index.treated], tr = rep(1, nrow(matched_df)), 
                        yobs[m$index.treated]) 
  learner_te <- learner_func(x_te[m$index.control], tr = rep(0, nrow(matched_df)), 
                        yobs[m$index.control])
  est_tr <- EstimateCate(learner_tr, x_te[m$index.treated])
  est_te <- EstimateCate(learner_te, x_te[m$index.control])
  gof <- - sum((y.diff - 0.5 * (est_tr + est_te))^2) / nrow(x_te)
  
  
           
}


#Example
set.seed(1423614230)

train_feat <- iris[, c(-1,-5)]
test_feat <- iris3[ , ,2][ ,-2]
train_tr <- rbinom(nrow(iris), 1, .5)
test_tr <- rbinom(nrow(iris3), 1, .5)
train_yobs <- iris[, 1]
test_yobs <- iris3[ , ,2][, 2]
train_df <- data.frame(train_feat, train_yobs, train_tr)
names(train_df)[ncol(train_df)] <- 'tr'
names(train_df)[4] = 'yobs'

test_df <- data.frame(test_feat, test_yobs, test_tr)
names(test_df)[4] = 'yobs'
names(test_df)[ncol(test_df)] <- 'tr'
names(test_df)[1] <- 'Sepal.Width'
names(test_df)[2] <- 'Petal.Length'
names(test_df)[3] <- 'Petal.Width'
propensity_score <- calculate_e(train_df, test_df)
y_star_te <- calculate_y_star(test_df, propensity_score)
gof1 <- eval_TO(S_RF, test_yobs, test_feat, test_tr, y_star_te)
gof2 <- eval_TO(T_RF, test_yobs, test_feat, test_tr, y_star_te)
#gof3 <- eval(X_RF, test_yobs, test_feat, test_tr, y_star_te)

gof_21 <- eval_M(S_RF, test_yobs, test_tr, test_feat)