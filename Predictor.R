install.packages("caret")
library(caret)
library(dplyr)
devtools::install_github("andreweatherman/cbbdata")
devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)
#create account to access api
cbbdata::cbd_create_account("account name", "email", "pass", "pass confirmation")
cbbdata::cbd_login(username = 'your_username', password = ('your_password'))
library(cbbdata)

#Clean games by making dummy variables and removing unecessary info
cleanGames<- function(selectYear) {
  all_games = cbd_torvik_game_stats(year = selectYear)
  #Normally it includes each game twice so we are removing duplicates
  all_games <- all_games %>%
    group_by(game_id) %>%
    slice_sample(n = 1) %>%   # Keep a random row, allowing for both home and away to exist
    ungroup()
  View(all_games)
  all_games$spread <- all_games$pts_scored-all_games$opp_pts #Make our spread column for predictions
  #Take only wanted variables
  gamesCleaned <- subset(all_games, select = c(year, team, opp, type, location, result, spread, conf, opp_conf)) 
  #Make dummy variables for categories
  View(gamesCleaned)
  dmy <- dummyVars(" ~ location + result + conf + opp_conf", data = gamesCleaned)
  newDf <- data.frame(predict(dmy, newdata = gamesCleaned)) 
  # Manually make conf variable dummies because of errors
  newDf$typeconf <- 0
  newDf$typeconf_t <- 0
  newDf$typenc <- 0
  newDf$typepost <- 0
  
  # Set the corresponding column to 1 where the category is true
  newDf$typeconf[gamesCleaned$type == "conf"] <- 1
  newDf$typeconf_t[gamesCleaned$type == "conf_t"] <- 1
  newDf$typenc[gamesCleaned$type == "nc"] <- 1
  newDf$typepost[gamesCleaned$type == "post"] <- 1
  
  newDf
  #Remove the original columns
  gamesCleaned <- subset(gamesCleaned, select = -c(type, location, result, conf, opp_conf))
  #Add the new df
  gamesCleaned <- cbind(gamesCleaned, newDf)
  return(gamesCleaned)
}


#This project was made for a 2025 competition and because of that many 2025 info
#is incomplete or missing, because of that below I had to add some info manually and
#then make predictions on North Carolina, Duke, and NC State games
all_games <- cbd_torvik_team_schedule(year = 2025)
all_games <- all_games %>%  
  filter(team %in% c("North Carolina", "Duke", "North Carolina St."))
all_games<- unique(all_games) #Take only games of the three schools

#Clean games 
gamesCleaned <- as.data.frame(subset(all_games, select = c(year, team, opp, type, location, conf, opp_conf)))
#Encode all categories correctly...
dmy <- dummyVars(" ~ location + opp_conf", data = gamesCleaned)
newDf <- data.frame(predict(dmy, newdata = gamesCleaned))

# Initialize the dummy columns to 0
newDf$typeconf <- 0
newDf$typeconf_t <- 0
newDf$typenc <- 0
newDf$typepost <- 0

# Set the column to 1 where true
newDf$typeconf[gamesCleaned$type == "conf"] <- 1
newDf$typeconf_t[gamesCleaned$type == "conf_t"] <- 1
newDf$typenc[gamesCleaned$type == "nc"] <- 1
newDf$typepost[gamesCleaned$type == "post"] <- 1


newDf
#Remove the original columns
gamesCleaned <- subset(all_games, select = -c(type, location, opp_conf))
#Add the new df
gamesCleaned <- cbind(gamesCleaned, newDf)
# Remove the conf column
gamesCleaned$conf <- NULL

# Add a new column "confACC" with value 1 for every row (because our three schools are all acc)
gamesCleaned$confACC <- 1
gamesCleaned <- subset(gamesCleaned, select = -c(date, neutral, game_id))
View(gamesCleaned)

#Now add team averages (same as before)
team_averages <- tibble(team = character(0))
all_Team_List <- unique(c(gamesCleaned$opp, "North Carolina", "Duke", "North Carolina St."))
x=0
for(teams in all_Team_List) {
  df <- cbd_torvik_game_stats() %>% 
    select(-c(year, min)) %>%
    filter(team == teams) %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "average_.{col}"
    ))
  temp_df <- df %>%
    mutate(team = teams) %>%
    select(team, everything())  # Reorder columns to have team first
  # Append the result to the main data frame
  team_averages <- rbind(team_averages, temp_df)
  x = x+1
  print(x)
}




#Make kenpom into a win/loss ratio instead of total count
kenpom <- read.csv("~your_file_path/kenpom_table_2025.csv")
kenpom$team <- gsub("North Carolina St.", "N.C. State", kenpom$team, ignore.case = TRUE)
## make win loss percentage
library(tidyr)

# Clean the data and compute win_percentage
seasonEnd <- 2025
kenpom <- kenpom %>%
  filter(grepl("^\\d+/\\d+$", w_l)) %>%   # Keep rows with number/number format
  separate(w_l, into = c("wins", "losses"), sep = "/", convert = FALSE) %>%
  mutate(
    wins = as.numeric(wins),
    losses = as.numeric(losses),
    win_percentage = wins / (wins + losses)
  ) %>%
  select(-wins, -losses)  # Remove win and losses columns

torvik <- cbd_torvik_ratings(year = seasonEnd)
ap <- cbd_ap_rankings(year = seasonEnd)
ap$team <- gsub("North Carolina St.", "N.C. State", ap$team, ignore.case = TRUE)
ap<- ap %>% select(-poll_start, -conf)

#Here we will manually add the 2025 rankings bc they aren't updated currently
ap <- rbind(ap, data.frame(rank = 1, week = "final", year = 2025, team = "Tennessee"))
ap <- rbind(ap, data.frame(rank = 2, week = "final", year = 2025, team = "Auburn"))
ap <- rbind(ap, data.frame(rank = 3, week = "final", year = 2025, team = "Iowa State"))
ap <- rbind(ap, data.frame(rank = 4, week = "final", year = 2025, team = "Duke"))
ap <- rbind(ap, data.frame(rank = 5, week = "final", year = 2025, team = "Alabama"))
ap <- rbind(ap, data.frame(rank = 6, week = "final", year = 2025, team = "Florida"))
ap <- rbind(ap, data.frame(rank = 7, week = "final", year = 2025, team = "Kansas"))
ap <- rbind(ap, data.frame(rank = 8, week = "final", year = 2025, team = "Marquette"))
ap <- rbind(ap, data.frame(rank = 9, week = "final", year = 2025, team = "Oregon"))
ap <- rbind(ap, data.frame(rank = 10, week = "final", year = 2025, team = "Kentucky"))
ap <- rbind(ap, data.frame(rank = 11, week = "final", year = 2025, team = "UConn"))
ap <- rbind(ap, data.frame(rank = 12, week = "final", year = 2025, team = "Oklahoma"))
ap <- rbind(ap, data.frame(rank = 13, week = "final", year = 2025, team = "Texas A&M"))
ap <- rbind(ap, data.frame(rank = 14, week = "final", year = 2025, team = "Houston"))
ap <- rbind(ap, data.frame(rank = 15, week = "final", year = 2025, team = "UCLA"))
ap <- rbind(ap, data.frame(rank = 16, week = "final", year = 2025, team = "Cincinnati"))
ap <- rbind(ap, data.frame(rank = 17, week = "final", year = 2025, team = "Mississippi St."))
ap <- rbind(ap, data.frame(rank = 18, week = "final", year = 2025, team = "Michigan St."))
ap <- rbind(ap, data.frame(rank = 19, week = "final", year = 2025, team = "Gonzaga"))
ap <- rbind(ap, data.frame(rank = 20, week = "final", year = 2025, team = "Purdue"))
ap <- rbind(ap, data.frame(rank = 21, week = "final", year = 2025, team = "Memphis"))
ap <- rbind(ap, data.frame(rank = 22, week = "final", year = 2025, team = "Illinois"))
ap <- rbind(ap, data.frame(rank = 23, week = "final", year = 2025, team = "Arkansas"))
ap <- rbind(ap, data.frame(rank = 24, week = "final", year = 2025, team = "Mississippi"))
ap <- rbind(ap, data.frame(rank = 25, week = "final", year = 2025, team = "Baylor"))

ap <- ap %>% filter( week == "final")
#combine all stats
combinedStats <- left_join(torvik, kenpom, by = "team") 
combinedStats<- combinedStats %>% select(-seed, -conf.x, -conf.y, -year.x, -year.y)
combinedStats <- combinedStats %>%
  mutate(
    # Check if the team is ranked by ap
    is_ranked = ifelse(team %in% ap$team, 1, 0),
    is_unranked = ifelse(team %in% ap$team, 0, 1),
    rank = ifelse(team %in% ap$team, ap$rank[match(team, ap$team)], 0)
  )

team_averages <- full_join(team_averages, combinedStats, by = "team")
year <- seasonEnd
View(team_averages)

team_averages<- unique(team_averages)
#Add them together to get our final df
finishedGames <- gamesCleaned %>%
  left_join(team_averages, by = "team")

finishedGames <- finishedGames %>%
  left_join(
    team_averages,
    by = c("opp" = "team"),
    suffix = c("", "_opp") # Add opp suffix
  )


#Check to make sure dataframes contain the same columns
unique_cols_df1 <- setdiff(names(allData), names(finishedGames))  # Columns unique to allData
unique_cols_df2 <- setdiff(names(finishedGames), names(allData))  # Columns unique to finishedGames

# Create dataframes out of the data that is missing
df1_unique <- allData %>% select(all_of(unique_cols_df1)) #Should contain all unused conferences in our prediction dataset
df2_unique <- finishedGames %>% select(all_of(unique_cols_df2)) #Should only contain team and opp

# Return both data frames
list(df1_unique = df1_unique, df2_unique = df2_unique)

#Add the columns that do not exist and initiate as 0 (the conferences the teams are not a part of)
trainset <- read.csv("~your_file_path/FinalDF2008.csv", row.names = 1)
trainset$spread <- NULL #Remove column 
missing_cols <- setdiff(names(trainset), names(finishedGames))
finishedGames <- as.data.frame(finishedGames) #Make it a dataframe
# Add missing columns with default value 0
finishedGames[missing_cols] <- 0

# Ensure column order matches the training data
finishedGames <- finishedGames[names(trainset)]
teamNames <- subset(finishedGames, select = c(team, opp))
finishedGames<- subset(finishedGames, select = c(-team, -opp))

#Now predict with our model
test_preds_matrix <- matrix(NA, nrow = nrow(finishedGames), ncol = length(base_models))
# Collect predictions from each base model
for (i in seq_along(base_models)) {
  test_preds_matrix[, i] <- predict(base_models[[i]], as.matrix(finishedGames))
}
#Equal weight
blended_preds_equal <- rowMeans(test_preds_matrix, na.rm = TRUE)
# Proportional weight
weights <- 1 / rmse_values  # Inverse RMSE to find weight of each model
weights <- weights / sum(weights)  # Normalize weights to sum to 1
# Weighted average
blended_preds_weighted <- rowSums(test_preds_matrix * weights[col(test_preds_matrix)], na.rm = TRUE)
# Check blended predictions
head(blended_preds_equal)   # Predictions from all models being equal
head(blended_preds_weighted) # Predictions from weighted blending
spreadPrediction<- cbind(blended_preds_weighted, teamNames) #Final predictions by game


