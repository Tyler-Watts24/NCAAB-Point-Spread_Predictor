#Combines all season averages from startYear to create a final big df
createFinal <- function(startYear) {
  library(glue)
  currentYear <- startYear
  finalDf <- data.frame()
  while(currentYear <=2025) {
    print(currentYear)
    file_path <- glue("your_file_path/team_averages_{currentYear}.csv")
    print(file_path)
    # Import dataframe
    team_averages <- read.csv(file_path)
    team_averages <- team_averages[, -1]
    print(team_averages)
    
    #Clean Games
    gamesCleaned <- cleanGames(currentYear)
    finishedGames <- gamesCleaned %>%
      left_join(team_averages, by = "team")
    
    #Add specific team averages for team and opp 
    finishedGames <- finishedGames %>%
      left_join(
        team_averages,
        by = c("opp" = "team"),
        suffix = c("", "_opp") # Add suffix opp
      )
    finishedGames <- finishedGames %>% select(-resultW, -resultL)
    
    #When finalDf is empty (during first iteration), initialize it with finishedGames
    if (nrow(finalDf) == 0) {
      finalDf <- finishedGames
    } else {
      #Add missing columns (set to 0) for both dataframes
      finalDf[setdiff(names(finishedGames), names(finalDf))] <- 0
      finishedGames[setdiff(names(finalDf), names(finishedGames))] <- 0
    }
    
    #Reorder columns to match
    finalDf <- finalDf[, names(finishedGames)]
    finishedGames <- finishedGames[, names(finalDf)]
    
    finalDf <- rbind(finalDf, finishedGames, fill = TRUE)
    currentYear <- currentYear+1
  }
  file_path <- glue("your_file_path/SeasonAverages/FinalDF{startYear}.csv")
  # export df
  write.csv(finalDf, file = file_path, row.names = TRUE)
}
