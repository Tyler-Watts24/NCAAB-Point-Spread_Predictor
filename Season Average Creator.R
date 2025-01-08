library(dplyr)
library(tidyr)
#Creates team averages for a given season determined by season end year (year = 2025 would be the 2024-2025 season)
createCSV <- function(seasonEnd) {
  library(glue)
  team_averages <- tibble(team = character(0))
  #Fix N.C State and make new df with it
  stats <- cbd_torvik_game_stats() #All game stats
  stats$team <- gsub("North Carolina St.", "N.C. State", stats$team, ignore.case = TRUE)
  stats$opp <- gsub("North Carolina St.", "N.C. State", stats$opp, ignore.case = TRUE)
  fixed_stats <- stats
  #Lists all teams we have game data on
  all_Team_List <- unique(fixed_stats$team)
  
  
  #Create the averages
  x = 0 #Team Count
  for(teams in all_Team_List) {
    df <- fixed_stats %>% filter(year == seasonEnd)%>% #take game stats for only given year
      select(-c(year, min)) %>%
      filter(team == teams) %>% #Filter to only the team we want to find averages for at the moment
      summarise(across( #Averaging out their stats for each variable
        .cols = where(is.numeric),
        .fns = ~ mean(.x, na.rm = TRUE),
        .names = "average_ {col}"
      )) 
    temp_df <- df %>%
      mutate(team = teams) %>% #Create df and add team column to calculated averages
      select(team, everything())  # Reorder columns to have team first
    # Add this team to our team averages dataframe
    team_averages <- rbind(team_averages, temp_df)
    x = x+1 #Team count
    print(x)
  }
  
  #Other stats to add  
  kenpom <- cbd_kenpom_ratings(year = seasonEnd)
  kenpom$team <- gsub("North Carolina St.", "N.C. State", kenpom$team, ignore.case = TRUE) #Fix NC State
  library(dplyr)
  library(tidyr)
  
  # Compute win_percentage by seperating the record column formated as num-num into wins loss
  kenpom <- kenpom %>%
    filter(grepl("^\\d+-\\d+$", w_l)) %>%   # Only keep rows with num1-num2 format
    separate(w_l, into = c("wins", "losses"), sep = "-", convert = FALSE) %>%
    mutate(
      wins = as.numeric(wins),#Create win column
      losses = as.numeric(losses), #loss column
      win_percentage = wins / (wins + losses) #Win percentage columns
    ) %>%
    select(-wins, -losses)  # Remove wins and losses columns
  
  #add and clean ap/torvik ratings stats
  torvik <- cbd_torvik_ratings(year = seasonEnd)
  ap <- cbd_ap_rankings(year = seasonEnd)
  ap$team <- gsub("North Carolina St.", "N.C. State", ap$team, ignore.case = TRUE)
  ap<- ap %>% select(-poll_start, -conf)
  ap <- ap %>% filter( week == "Final")
  
  #Put all stats together and clean
  combinedStats <- left_join(torvik, kenpom, by = "team") 
  combinedStats<- combinedStats %>% select(-seed, -conf.x, -conf.y, -year.x, -year.y, -ncaa_seed)
  combinedStats <- combinedStats %>%
    mutate(
      # Check if the team in is ranked top 25 by ap and add info accordingly
      is_ranked = ifelse(team %in% ap$team, 1, 0),
      is_unranked = ifelse(team %in% ap$team, 0, 1),
      rank = ifelse(team %in% ap$team, ap$rank[match(team, ap$team)], 0)
    )
  #combined averages with other rankings/stats
  team_averages <- full_join(team_averages, combinedStats, by = "team")
  View(team_averages_final)
  year <- seasonEnd
  #Export the df
  # Save the CSV to device, below is my folder
  file_path <- glue("your_file_path/team_averages_{year}.csv")
  print(file_path)
  # Write the dataframe to this path
  write.csv(team_averages, file = file_path, row.names = TRUE)
}

#Create average for all available data (only goes back to 2008)
for (i in 2008:2024) {
  createCSV(i)
}

#This project was made for a 2025 competition and because of that many 2025 info
#is incomplete or missing, because of that below I had to add some info manually

##Must Create for 2025 bc ap rank and kenpom incomplete/missing
library(glue)
#List of all teams we have data on
team_averages <- tibble(team = character(0))
#Fix NC State
stats <- cbd_torvik_game_stats()
stats$team <- gsub("North Carolina St.", "N.C. State", stats$team, ignore.case = TRUE)
stats$opp <- gsub("North Carolina St.", "N.C. State", stats$opp, ignore.case = TRUE)
# Now assign the entire modified data frame to fixed_stats
fixed_stats <- stats
all_Team_List <- unique(fixed_stats$team)

#Recreate season averages
x = 0
for(teams in all_Team_List) {
  df <- fixed_stats %>% filter(year == seasonEnd)%>%
    select(-c(year, min)) %>%
    filter(team == teams) %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "average_ {col}"
    ))
  temp_df <- df %>%
    mutate(team = teams) %>%
    select(team, everything())  # Reorder columns to have team first
  team_averages <- rbind(team_averages, temp_df)
  x = x+1
  print(x)
}

#Here we must manually add the kenpom table as a csv 
kenpom <- read.csv("~your_file_path/kenpom_table_2025.csv") #What I had the table saved as
kenpom$team <- gsub("North Carolina St.", "N.C. State", kenpom$team, ignore.case = TRUE) #Fix NC State

# Clean the data and compute win_percentage, same as before
kenpom <- kenpom %>%
  filter(grepl("^\\d+/\\d+$", w_l)) %>% # Keep rows with  num/num(different then num-num format)
  separate(w_l, into = c("wins", "losses"), sep = "/", convert = FALSE) %>%
  mutate(
    wins = as.numeric(wins),
    losses = as.numeric(losses),
    win_percentage = wins / (wins + losses)
  ) %>%
  select(-wins, -losses)

#Again get torvik and ap df, this time the ap df is incomplete
torvik <- cbd_torvik_ratings(year = seasonEnd)
ap <- cbd_ap_rankings(year = seasonEnd)
ap$team <- gsub("North Carolina St.", "N.C. State", ap$team, ignore.case = TRUE)
ap<- ap %>% select(-poll_start, -conf)

#Here we will manually add the 2025 rankings bc they arent updated currently
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

#Combine and add ap stats
combinedStats <- left_join(torvik, kenpom, by = "team") 
combinedStats<- combinedStats %>% select(-seed, -conf.x, -conf.y, -year.x, -year.y)
combinedStats <- combinedStats %>%
  mutate(
    # Check if the team in combinedStats exists in ap's "team" column
    is_ranked = ifelse(team %in% ap$team, 1, 0),
    is_unranked = ifelse(team %in% ap$team, 0, 1),
    rank = ifelse(team %in% ap$team, ap$rank[match(team, ap$team)], 0)
  )

team_averages <- full_join(team_averages, combinedStats, by = "team")
year <- seasonEnd

#Save df to folder, my personal folder below
file_path <- glue("your_file_path/team_averages_2025.csv")
print(file_path)
# Write the dataframe to this path
write.csv(team_averages, file = file_path, row.names = TRUE)

