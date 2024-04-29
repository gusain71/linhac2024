data <- read.csv("Linhac24_Sportlogiq.csv")


library(dplyr)
library(tidyverse)
library(ggplot2)

# Step 1: Load data and add row numbers
data <- data %>% mutate(row_number = row_number())

# Step 2: Identify rows with type == "2on1"
one_on_zero_events <- data %>% filter(type == "2on1")
View(one_on_zero_events)

# Step 3: Initialize a list to hold all sequences leading up to "2on1" events
all_sequences <- list()

# Step 4: Iterate through each '2on1' event
for (i in seq_len(nrow(one_on_zero_events))) {
  # Get the row number of the current "2on1" event
  row_num <- one_on_zero_events$row_number[i]
  
  # Determine the range of rows to retrieve (6 events after and the "2on1" event)
  range_start <- row_num
  range_end <- min(nrow(data), row_num + 6) 
  
  # Retrieve the data subset for this range
  data_subset <- data %>% filter(row_number >= range_start & row_number <= range_end)
  
  # Get the team ID of the current "2on1" event
  one_on_zero_team_id <- one_on_zero_events$teamid[i]
  
  # Multiply coordinates by -1 if teaminpossession matches the team ID of the "2on1" event
  data_subset <- data_subset %>% mutate(
    xadjcoord = ifelse(teamid == one_on_zero_team_id, xadjcoord * -1, xadjcoord),
    yadjcoord = ifelse(teamid == one_on_zero_team_id, yadjcoord * -1, yadjcoord)
  )
  
  # Add a sequence identifier to each row in the data subset
  data_subset <- data_subset %>% mutate(sequence_id = i)
  
  # Add the data subset to the list of sequences
  all_sequences[[i]] <- data_subset
}

two_on_one_sequences = bind_rows(all_sequences)
two_on_one_passes = two_on_one_sequences %>% filter(eventname == "pass" & xadjcoord > 0)

#Get avergare passing accuracy in 2v1
two_on_one_passes_success = two_on_one_passes %>% filter(outcome == "successful")
nrow(two_on_one_passes_success)/nrow(two_on_one_passes)

#Get avergare passing accuracy
all_passes = data %>% filter(eventname == "pass")
all_passes_success = all_passes %>% filter(outcome == "successful")
nrow(all_passes_success)/nrow(all_passes)

# All player ids that has made a pass in a 2v1 situation
unique_player_ids <- unique(two_on_one_passes$playerid)



#Dictionary of players and their passing accuracy
acc_dic = list()

for(i in 1:length(unique_player_ids)){
  succesful_passes = nrow(data %>% filter(playerid == unique_player_ids[i] & eventname == "pass" & outcome == "successful"))
  total_passes = nrow(data %>% filter(playerid == unique_player_ids[i] & eventname == "pass"))
  
  pass_accuracy = succesful_passes/total_passes
  acc_dic[[i]] = list(playerid=unique_player_ids[i], acc=pass_accuracy-0.18)
}


#Creates dataframe for player ids och their passing accuracy
acc_df <- do.call(rbind, lapply(acc_dic, function(x) data.frame(playerid = x$playerid, acc = x$acc)))

# Sort the dataframe by the 'acc' column
sorted_acc_df <- arrange(acc_df, desc(acc))



#Dictionary with players weighted pass xg
player_weighted_pass_xg_dict = list()


# Initialize a dataframe to store all shots with xg
shots_with_xg <- bind_rows(all_sequences) %>% 
  filter(eventname == "shot" & !is.na(xg_allattempts)) %>%
  arrange(desc(xg_allattempts))  # Sort by xg, descending


two_on_one_pass_indices <- c()
totalXGcount = 0
contains_pass_count = 0
total_successful_passes = 0

# Iterate over all_sequences to check each sequence
for (i in seq_along(all_sequences)) {
  sequence <- all_sequences[[i]]
  # Check if sequence contains 'pass' and 'shot'
  contains_pass_before_shot <- TRUE
  pass_index = 0
  shot_index = 0
  defending_team = 0
  # Get pass and shot indexes to know if pass comes before shot
  for (idx in 1:nrow(sequence)){
    if (sequence$eventname[idx] == "controlledentryagainst"){
      defending_team = sequence$teamid[idx]
    }
    if (sequence$eventname[idx] == "pass" && pass_index == 0 && defending_team != sequence$teamid[idx]){
      pass_index = idx
    }
    if (sequence$eventname[idx] == "shot" && shot_index == 0 && defending_team != sequence$teamid[idx]){
      shot_index = idx
    }
  }
  
  if (pass_index == 0 || (shot_index != 0 && shot_index < pass_index)){
    contains_pass_before_shot <- FALSE
  }
  
  
  # If the condition is true, store the index and calculate xG
  if (contains_pass_before_shot && !is.na(sequence$teamgoalieoniceid[1])) {
    two_on_one_pass_indices <- c(two_on_one_pass_indices, i)
    contains_pass_count = contains_pass_count + 1 #total pass before shot events
    if (sequence$outcome[pass_index] == "successful"){
      total_successful_passes = total_successful_passes + 1
    }
    if(shot_index > 0){
      totalXGcount = totalXGcount + sum(sequence$xg_allattempts[sequence$eventname == "shot"], na.rm = TRUE)
    }
  }
}

average_pass_xg <- if(contains_pass_count > 0) totalXGcount / contains_pass_count else 0

#Calculate weigthed xg per pass in 2v1, (total xg) / (total tried passes in 2v1)
for(i in 1:length(unique_player_ids)){
  player_pass_xg <- totalXGcount / (total_successful_passes / acc_dic[[i]]$acc)
  player_weighted_pass_xg_dict[[i]] = list(playerid=unique_player_ids[i], xg=player_pass_xg)
}


#Create dataframe for player ids och their xg per pass in 2v1
pass_xg_df <- do.call(rbind, lapply(player_weighted_pass_xg_dict, function(x) data.frame(playerid = x$playerid, xg = x$xg)))

# Sort the dataframe by the 'acc' column
sorted_pass_xg_df <- arrange(pass_xg_df, desc(xg))

# Print or use the sorted dataframe
View(sorted_pass_xg_df)


#NOPASS
two_on_one_NOpass_indices <- c()
totalXGcountNOpass = 0
contains_NOpass_count = 0
player_weighted_shot_xg_dict = list()

two_on_one_shots = two_on_one_sequences %>% filter(eventname == "shot")
unique_player_ids_shots <- unique(two_on_one_shots$playerid)

# Iterate over all_sequences to check each sequence
for (i in seq_along(all_sequences)) {
  sequence <- all_sequences[[i]]
  # Check if sequence contains 'pass' and 'shot'
  contains_shot_before_pass <- TRUE
  pass_index = 0
  shot_index = 0
  defending_team = 0
  #Check that the shot comes before potential passes
  for (idx in 1:nrow(sequence)){
    if (sequence$eventname[idx] == "controlledentryagainst"){
      defending_team = sequence$teamid[idx]
    }
    if (sequence$eventname[idx] == "pass" && pass_index == 0 && defending_team != sequence$teamid[idx]){
      pass_index = idx
    }
    if (sequence$eventname[idx] == "shot" && shot_index == 0 && defending_team != sequence$teamid[idx]){
      shot_index = idx
    }
  }
  
  if (shot_index == 0 || (pass_index != 0 && pass_index < shot_index)){
    contains_shot_before_pass <- FALSE
  }
  
  
  # If the condition is true, store the index and calculate xG
  if (contains_shot_before_pass && !is.na(sequence$teamgoalieoniceid[1])) {
    two_on_one_NOpass_indices <- c(two_on_one_NOpass_indices, i)
    contains_NOpass_count = contains_NOpass_count + 1
    totalXGcountNOpass = totalXGcountNOpass + sum(sequence$xg_allattempts[sequence$eventname == "shot"], na.rm = TRUE)
  }
}

average_NOpass_xg <- if(contains_NOpass_count > 0) totalXGcountNOpass / contains_NOpass_count else 0

for(i in 1:length(unique_player_ids_shots)){
  # Calculates a players performance compared to its expected goals
  # This by deviding actual goals with their expected goals
  # then multiplies it with average xg, more goals than xg => better than average
  player_shot_xg <- sum(data$xg_allattempts[data$eventname == "shot" & data$playerid == unique_player_ids_shots[i]], na.rm = TRUE)
  player_actual_goals <- sum(data$eventname == "goal" & data$playerid == unique_player_ids_shots[i], na.rm = TRUE)
  xg_performance <- player_actual_goals/player_shot_xg
  if(player_actual_goals == 0 && player_shot_xg > 2){
    xg_performance = 0.5
  }
  player_shot_xg <- average_NOpass_xg * xg_performance
  player_weighted_shot_xg_dict[[i]] = list(playerid=unique_player_ids_shots[i], xg=player_shot_xg)
}

#Create dataframe with player id och xg
shot_xg_df <- do.call(rbind, lapply(player_weighted_shot_xg_dict, function(x) data.frame(playerid = x$playerid, xg = x$xg)))

# Sort the dataframe by the 'acc' column
sorted_shot_xg_df <- arrange(shot_xg_df, desc(xg))

# Print or use the sorted dataframe
View(sorted_shot_xg_df)


contains_NOpass_count

contains_pass_count + contains_NOpass_count

# Combines the two dataframse
# Assuming sorted_shot_xg_df and sorted_pass_xg_df have been created
# Merge the two data frames on 'playerid'
merged_df <- merge(sorted_shot_xg_df, sorted_pass_xg_df, by = "playerid", suffixes = c("_shot", "_pass"))

merged_df <- merged_df %>%
  mutate(
    decision = case_when(
      xg_shot == 0 ~ "unknown",
      xg_shot > xg_pass ~ "shoot",
      TRUE ~ "pass"
    )
  )

# View the resulting data frame
View(merged_df)

shoot_count <- merged_df %>%
  filter(decision == "shoot") %>%
  nrow()  # Count the number of rows

# Count the number of "pass" decisions
pass_count <- merged_df %>%
  filter(decision == "pass") %>%
  nrow()  # Count the number of rows

# Print the counts
cat("Number of 'shoot' decisions:", shoot_count, "\n")
cat("Number of 'pass' decisions:", pass_count, "\n")

# Print the counts
cat("shoot percentage:", shoot_count/(shoot_count+pass_count) * 100, "%\n")
cat("pass percentage:", pass_count/(shoot_count+pass_count) * 100, "%\n")





