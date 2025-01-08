install.packages("lightgbm", repos = "https://cran.r-project.org")
library(lightgbm)
#Import all game data
allData <- read.csv("~your_file_path/FinalDF{startYear}.csv", row.names = 1)
library(dplyr)
#Remove team names, not needed for training model
allData <- allData %>% select(-team, -opp)
#Remove any duplicate rows
allData <- unique(allData)
#Split data into target and training 
set.seed(50)
train_index <- sample(seq_len(nrow(allData)), size = 0.8 * nrow(allData))
train_data <- allData[train_index, ]
test_data <- allData[-train_index, ]

# Separate variables from predictor (spread)
X_train <- train_data %>%
  select(-spread) %>%
  as.matrix()
y_train <- train_data$spread


X_test <- test_data %>%
  select(-spread) %>%
  as.matrix()
y_test <- test_data$spread

dtrain <- lgb.Dataset(data = X_train, label = y_train)

#Hyperparamaters
params <- list(
  objective = "regression_l1",   #Using l1 because score is based on mae not rmse  
  metric = "mae",                
  boosting = "gbdt",              
  num_leaves = 69,                 
  learning_rate = .01,            
  feature_fraction = .55,          
  bagging_fraction =.449,          
  bagging_freq = 8,                
  lambda_l1 = 4.33,                
  lambda_l2 = 8.36,
  max_depth = 11
)


#Baseline model (less efficient than cross validation)
model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,                  # Number of  rounds
  valids = list(test = lgb.Dataset(X_test, label = y_test)),  # test dataset
  early_stopping_rounds = 10      # Stop if no improvement after 10 rounds
)

# Predict on the test set and calculate rmse and mae
predictions <- predict(model, X_test)
valid_rows <- !is.na(y_test)
predictions_clean <- predictions[valid_rows]
y_test_clean <- y_test[valid_rows]
# Now calculate RMSE and MAE
rmse <- sqrt(mean((predictions_clean - y_test_clean)^2))
cat("RMSE:", rmse, "\n")
mae <- mean(abs(predictions_clean - y_test_clean), na.rm = TRUE)
cat("Overall MAE:", overall_mae, "\n")


#Compare with cross validation (more efficient)
# allData <- subset(allData, year != 2025) if we want to test 2025 prediction specifically
k <- 5 #5 folds
#Seperate variables and predictor 
X<-allData %>%
  select(-spread) %>%
  as.matrix()
y<- allData$spread
folds <- sample(rep(1:k, length.out = nrow(X)))  # Assign each row to a fold

# Initialize predictions
cv_preds <- matrix(NA, nrow = nrow(X), ncol = 1)  # For one base model
rmse_values <- numeric(k)
mae_values <- numeric(k)
base_models <- list() #will fill with each model for the five folds

#Train for 5 folds
for (i in 1:k) {
  cat("Training base model for fold:", i, "\n")
  
  # Split data into training and validation sets
  train_idx <- which(folds != i)
  val_idx <- which(folds == i)
  
  dtrain <- lgb.Dataset(data = X[train_idx, ], label = y[train_idx])
  dval <- lgb.Dataset(data = X[val_idx, ], label = y[val_idx])
  
  # Train model 
  model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    valids = list(validation = dval),
    early_stopping_rounds = 10,
    verbose = 1
  )
  
  # Store each model
  base_models[[i]] <- model
  
  #Predict for this fold
  fold_preds <- predict(model, X[val_idx, ])
  
  # Store predictions on the fold
  cv_preds[val_idx, 1] <- fold_preds
  
  # Calculate RMSE for this fold
  rmse_values[i] <- sqrt(mean((y[val_idx] - fold_preds)^2, na.rm = TRUE))  # Calculate RMSE for the fold
  cat("RMSE for fold", i, ":", rmse_values[i], "\n")
  #Calculate MAE
  mae_values[i] <- mean(abs(y[val_idx] - fold_preds), na.rm = TRUE)
  cat("MAE for fold", i, ":", mae_values[i], "\n")
}

#Predictons for rmse and mae
cv_rmse <- sqrt(mean((y - cv_preds[, 1])^2, na.rm = TRUE))
cat("Out-of-Fold RMSE for Base Models:", cv_rmse, "\n")
cat("RMSE for each fold:\n")
print(rmse_values)
overall_mae <- mean(abs(y - cv_preds), na.rm = TRUE)
cat("Overall MAE:", overall_mae, "\n")

#Check importance
importance <- lgb.importance(model, percentage = TRUE)

# View the top features
View(importance)

# Plot feature importance
lgb.plot.importance(importance, top_n = 20)

#Determine if the model is normal distribution
residuals <- cv_preds
install.packages("nortest")
library(nortest)
ad_result <- ad.test(residuals)
print(ad_result)
lillie_test_result <- lillie.test(residuals)
print(lillie_test_result)
#Both tests suggest the model is not normal, (p value is below .05)

#Create intervals for a 75% confidence interval
residuals_clean <- residuals[!is.na(residuals)]
lower_bound <- quantile(residuals_clean, 0.125, na.rm = TRUE)
upper_bound <- quantile(residuals_clean, 0.875, na.rm = TRUE)
cat("\n75% Confidence Interval based on Quantiles: [", lower_bound, ", ", upper_bound, "]\n")


#Account for the smaller sample size (in the competition I am predicting on 43 games) using t test (not a normal distribution but works as an aproximation to be extra safe)
# Original confidence interval values
original_lower_bound <- lower_bound
original_upper_bound <- upper_bound
original_width <- original_upper_bound - original_lower_bound

# Degrees of freedom for the new sample size (43 rows)
degrees_of_freedom <- 42

# critical value for 75% Confidence Interval and 42 degrees of freedom
t_new <- qt(0.875, df = degrees_of_freedom)  # 75% CI -> 87.5 critical value

# value for 98949 degrees of freedom (what the model is trained on)
t_original <- qt(0.875, df = 98949)  # For larger sample, assuming large df

# Calculate the adjustment factor
adjustment_factor <- t_new / t_original

# Adjust the CI width
adjusted_width <- original_width * adjustment_factor
cat("Adjustment Factor:", adjustment_factor, "\n")
cat("Adjusted CI Width:", adjusted_width, "\n")

# Adjust the original CI bounds
adjusted_lower_bound <- original_lower_bound - (adjusted_width - original_width) / 2
adjusted_upper_bound <- original_upper_bound + (adjusted_width - original_width) / 2

#Final confidence interval for a sample size of only 43
cat("Adjusted CI: [", adjusted_lower_bound, ", ", adjusted_upper_bound, "]\n")
