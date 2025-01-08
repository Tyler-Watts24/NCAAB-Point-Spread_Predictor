library(dplyr)
allData <- read.csv("~your_file_path/FinalDF{startYear}.csv", row.names = 1) #Import allData that we train with
allData <- allData %>% select(-team, -opp) #clean it
#Split data into target and training 
set.seed(123)
train_index <- sample(seq_len(nrow(allData)), size = 0.8 * nrow(allData))
train_data <- allData[train_index, ]
test_data <- allData[-train_index, ]

# Separate variables and what we are predicting (spread)
X_train <- as.matrix(train_data[, -which(names(train_data) == "spread")])
y_train <- train_data$spread

X_test <- as.matrix(test_data[, -which(names(test_data) == "spread")])
y_test <- test_data$spread

dtrain <- lgb.Dataset(data = X_train, label = y_train)

##Bayesian optimization
install.packages("ParBayesianOptimization")
library(ParBayesianOptimization)
library(lightgbm)

#Params for our gradient boosting
objective_function <- function(num_leaves, learning_rate, feature_fraction, bagging_fraction, bagging_freq, lambda_l1, lambda_l2, max_depth) {
  params <- list(
    objective = "regression",
    metric = "rmse",
    boosting = "gbdt",
    num_leaves = as.integer(num_leaves),
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    bagging_fraction = bagging_fraction,
    bagging_freq = bagging_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_depth = as.integer(max_depth)
  )
  
  # Perform cross-validation
  cv_result <- lgb.cv(
    params = params,
    data = dtrain,
    nfold = 5,
    nrounds = 500,
    early_stopping_rounds = 10,
    verbose = -1  # Silence the output
  )
  
  
  rmse_values <- cv_result$record_evals$valid$rmse$eval
  rmse_values <- unlist(rmse_values)
  # Ensure rmse_values are numeric
  rmse_values <- as.numeric(rmse_values)
  
  View(rmse_values)
  # Return the best RMSE 
  best_rmse <- min(rmse_values, na.rm = TRUE)  # Handle NA values
  
  View(best_rmse)
  finalReturn <- list(Score = best_rmse)
  View(finalReturn)
  return(finalReturn)
}
#Check between bounds below for optimization, can change values
bounds <- list(
  num_leaves = c(62L, 70L),               
  learning_rate = c(0.01, 0.015),          
  feature_fraction = c(0.5, 0.6),           
  bagging_fraction = c(0.4, 0.5),          
  bagging_freq = c(7L, 9L),
  lambda_l1 = c(4.0, 5.5),                
  lambda_l2 = c(8.0, 9.0),                
  max_depth = c(9L, 13L)                  
)

#Run bayesion optimization function
opt_results <- bayesOpt(
  FUN = objective_function,  
  bounds = bounds,          
  initPoints = 10,           
  iters.n = 40,              
  acq = "ucb"                
)
best_score <- min(opt_results$scoreSummary$Score, na.rm = TRUE)
best_row <- opt_results$scoreSummary[opt_results$scoreSummary$Score == best_score, ]
print(best_row) #Best results 
# View the optimization score
View(opt_results$scoreSummary)
