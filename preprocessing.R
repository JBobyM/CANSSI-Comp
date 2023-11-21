library(dplyr)
library(tidyr)
library(data.table)
library(ROSE)
library(arrow)
library(dplyr)
library(stringr)
library(lubridate)

data_horse = read_parquet("C:\\Users\\User\\Downloads\\trots_2013-2022.parquet")

# Convert FinishPosition to numeric 
data_horse$FinishPosition1 <- as.numeric(data_horse$FinishPosition)

#-- Check horses that had no non-NA positions in their history

# Filter the dataset with only NA values in FinishPosition
na_finish_position_dataset <- data_horse %>% filter(is.na(FinishPosition1))

#--------------------------------------------------------------------------------------------
# REMOVE HORSES WITH NO NUMERIC POSITION IN THEIR HISTORY

# Filter the dataset with only NA values in FinishPosition
na_finish_position_dataset <- data_horse %>% filter(is.na(FinishPosition1))

# Create a dictionary (named list) with the counts of each horse
horse_counts1 <- data_horse %>%
  group_by(HorseID) %>%
  summarize(Appearances = n_distinct(RaceID)) %>%
  ungroup() %>%
  select(HorseID, Appearances) %>%
  distinct()

horse_counts <- na_finish_position_dataset %>%
  group_by(HorseID) %>%
  summarize(Appearances = n_distinct(RaceID)) %>%
  ungroup() %>%
  select(HorseID, Appearances) %>%
  distinct()

# Convert the named lists to a dictionary
horse_counts_dict <- as.list(setNames(horse_counts$Appearances, horse_counts$HorseID))
horse_counts_dict2 <- as.list(setNames(horse_counts1$Appearances, horse_counts1$HorseID))


# Create a new dictionary d3 with common keys and matching values from d1 and d2
common_keys <- intersect(names(horse_counts_dict), names(horse_counts_dict2))
d3 <- list()
for (key in common_keys) {
  if (horse_counts_dict[[key]] == horse_counts_dict2[[key]]) {
    d3[[key]] <- horse_counts_dict[[key]]
  }
}

# View the resulting dictionary
no_nuneric_position_horses <- as.list(names(d3))
no_nuneric_position_horses
#------------------------------------------------------------------------------------

# Remove horses with no numeric position
data_horse1 <- data_horse[!(data_horse$HorseID %in% no_nuneric_position_horses), ]


# Convert "RaceStartTime" to Date and Time format
data_horse1$RaceStartTime <- as.POSIXct(data_horse1$RaceStartTime, format = "%Y-%m-%d %H:%M:%S")
data_horse1$FoalingDate <- as.POSIXct(data_horse1$FoalingDate, format = "%Y-%m-%d %H:%M:%S")

data_horse1$FinishPosition3 <- as.numeric(data_horse1$FinishPosition)


data_horse1 <- data_horse1 %>%
  mutate(HasPosition = ifelse(is.na(FinishPosition3), 0, 1))


# ---------------FinishPosition3-----------------------------
# NAs are filled with the mode position of each horse

# Custom function to calculate mode
calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Arrange the data by HorseID and RaceStartTime
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime)

# Create a new column 'FinishPosition2' to hold the updated positions
data_horse1 <- data_horse1 %>%
  mutate(FinishPosition2 = ifelse(is.na(FinishPosition1), NA, FinishPosition1))

# Replace NA values in 'FinishPosition2' with the mode of positions for each horse
data_horse1 <- data_horse1 %>%
  group_by(HorseID) %>%
  mutate(FinishPosition2 = ifelse(is.na(FinishPosition2), calculate_mode(FinishPosition2), FinishPosition2)) %>%
  ungroup()

# Finally, copy the values from 'FinishPosition2' to 'FinishPosition3
data_horse1$FinishPosition3 <- data_horse1$FinishPosition2

# Remove 'FinishPosition2' if you no longer need it
data_horse1 <- data_horse1 %>%
  select(-FinishPosition2)


#------------ NEW FEATURES RELATED TO HORSES ----------------------------

#----Number of times a horse finished first (prior currrent race)-----

# Sort the dataset by HorseId and RaceStartTime to ensure data is in chronological order
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime)

# Create the PriorPosition1st column
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PriorPosition1st = cumsum(FinishPosition3 == 1) - (FinishPosition3 == 1))


# Create the PriorNumberOfRaces column
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PriorNumberOfRaces = row_number() - 1)



# ---- Create the DelayLastRace column ----
# Put 365 if its the first race, if not, compute the delay between current race and last race
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(
    DelayLastRace = ifelse(row_number() == 1, 365, RaceStartTime - lag(RaceStartTime))
  )

#data_horse1 = subset(data_horse1, select = -c(HasPositionn,FinishPosition1) )

# ---- Create the PosLast3 column ----
# -3 if its the first race; 3* pos in the first race if its the secod race; (pos1+pos2)*1.5 if its the third race, sum last 3 pos if its more than third race

data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(
    PosLast3 = ifelse(row_number() == 1, -3,
                      ifelse(row_number() == 2, lag(FinishPosition3) * 3,
                             ifelse(row_number() == 3, (lag(FinishPosition3) * 1.5) + (lag(lag(FinishPosition3)) * 1.5),
                                    lag(FinishPosition3, 1) + lag(FinishPosition3, 2) + lag(FinishPosition3, 3)
                             )
                      )
    )
  )


# ---- Create the OppsPriorPos1st column ----
# SUm of number of times opponents has finished 1st in previous races
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPos1st = sum(PriorPosition1st) - PriorPosition1st
  ) %>%
  ungroup()


# Create HasPosition column, to differentiate when a horse had a numeric position vs when it's not
data_horse1 <- data_horse1 %>%
  mutate(HasPositionn = ifelse(is.na(FinishPosition1), 0, 1))


# ---- Create the OppsPriorPos1st column ----
# SUm opponents PosLast3 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPosLast3 = sum(PosLast3) - PosLast3
  ) %>%
  ungroup()

# ---- Create the OppsDelayLasRace column ----
# SUm opponents OppsDelayLasRace 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsDelayLasRace = sum(DelayLastRace) - DelayLastRace
  ) %>%
  ungroup()


# Create the number of NA in FinishPosition3 by HorseID
#data_horse1 <- data_horse1 %>%
 # group_by(HorseID) %>%
 # mutate(NumNA_H = sum(is.na(FinishPosition3))) %>%
 # ungroup()

data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(NumNAPrior_H = cumsum(is.na(FinishPosition1)) - is.na(FinishPosition1)) %>%
  ungroup()


data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PrevBeatenMargin = lag(BeatenMargin, order_by = RaceStartTime, default = first(BeatenMargin)))

data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PrevPriceSP = lag(PriceSP, order_by = RaceStartTime, default = first(PriceSP)))



# ---- Create the the dependent variable RaceOutcome ----
data_horse1 <- data_horse1 %>%
  mutate(RaceOutcome = factor(ifelse(FinishPosition3 == 1, "Win", "Not_Win")))

#PIRPosition from last race
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PIRPosition_H = lag(PIRPosition, order_by = RaceStartTime, default = first(PIRPosition)))

#Number of opponents
data_horse1 <- data_horse1 %>%
  group_by(RaceID) %>%
  mutate(n_horses = n_distinct(HorseID)) %>%
  ungroup()

# Sum of money won prior to the race

data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PriorSumMoneyWon_H = cumsum(Prizemoney) - Prizemoney)

# Sum of Positions prior to the race 
data_horse1 <- data_horse1 %>%
  arrange(HorseID, RaceStartTime) %>%
  group_by(HorseID) %>%
  mutate(PriorSumPos_H = cumsum(FinishPosition3) - FinishPosition3)




#------------ NEW FEATURES RELATED TO JOCKEY ----------------------------

#----Number of times a horse finished first (prior currrent race)-----
# Load the dplyr package if not already loaded

# Sort the dataset by JockeyId and RaceStartTime to ensure data is in chronological order
data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime)

# Create the PriorPosition1st column
data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PriorPosition1stJockey = cumsum(FinishPosition3 == 1) - (FinishPosition3 == 1))


# Create the PriorNumberOfRaces column
data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PriorNumberOfRacesJockey = row_number() - 1)


# ---- Create the DelayLastRace column ----
# Put 365 if its the first race, if not, compute the delay between current race and last race
data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(
    DelayLastRaceJockey = ifelse(row_number() == 1, 365, as.numeric(difftime(RaceStartTime, lag(RaceStartTime), units = "days")) )
  )

data_horse1 = subset(data_horse1, select = -c(HasPositionn,FinishPosition1) )

# ---- Create the PosLast3 column ----
# -3 if its the first race; 3* pos in the first race if its the secod race; (pos1+pos2)*1.5 if its the third race, sum last 3 pos if its more than third race

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(
    PosLast3Jockey = ifelse(row_number() == 1, -3,
                      ifelse(row_number() == 2, lag(FinishPosition3) * 3,
                             ifelse(row_number() == 3, (lag(FinishPosition3) * 1.5) + (lag(lag(FinishPosition3)) * 1.5),
                                    lag(FinishPosition3, 1) + lag(FinishPosition3, 2) + lag(FinishPosition3, 3)
                             )
                      )
    )
  )


# ---- Create the OppsPriorPos1st column ----
# SUm of number of times opponents has finished 1st in previous races
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPos1st = sum(PriorPosition1stJockey) - PriorPosition1stJockey
  ) %>%
  ungroup()


# ---- Create the OppsPriorPos1st column ----
# SUm opponents PosLast3 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPosLast3Jockey = sum(PosLast3Jockey) - PosLast3Jockey
  ) %>%
  ungroup()

# ---- Create the OppsDelayLasRace column ----
# SUm opponents OppsDelayLasRace 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsDelayLasRaceJockey = sum(DelayLastRaceJockey) - DelayLastRaceJockey
  ) %>%
  ungroup()

# Create the number of NA in FinishPosition3 by JockeyID

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(NumNAPrior_J = cumsum(is.na(FinishPosition3)) - is.na(FinishPosition3)) %>%
  ungroup()

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PrevBeatenMargin_J = lag(BeatenMargin, order_by = RaceStartTime, default = first(BeatenMargin)))

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PrevPriceSP_J = lag(PriceSP, order_by = RaceStartTime, default = first(PriceSP)))

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PIRPosition_J = lag(PIRPosition, order_by = RaceStartTime, default = first(PIRPosition)))

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PriorSumMoneyWon_J = cumsum(Prizemoney) - Prizemoney)

data_horse1 <- data_horse1 %>%
  arrange(JockeyID, RaceStartTime) %>%
  group_by(JockeyID) %>%
  mutate(PriorSumPos_J = cumsum(FinishPosition3) - FinishPosition3)


#------------ NEW FEATURES RELATED TO TRAINER  ----------------------------

#----Number of times a horse finished first (prior currrent race)-----
# Load the dplyr package if not already loaded

# Sort the dataset by HorseId and RaceStartTime to ensure data is in chronological order
data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime)

# Create the PriorPosition1st column
data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PriorPosition1stTrainer = cumsum(FinishPosition3 == 1) - (FinishPosition3 == 1))


# Create the PriorNumberOfRaces column
data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PriorNumberOfRacesTrainer = row_number() - 1)


# ---- Create the DelayLastRace column ----
# Put 365 if its the first race, if not, compute the delay between current race and last race
data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(
    DelayLastRaceTrainer = ifelse(row_number() == 1, 365, as.numeric(difftime(RaceStartTime, lag(RaceStartTime), units = "days")) )
  )

#data_horse1 = subset(data_horse1, select = -c(NumNA_D,NumNA_S, NumNA_T))

# ---- Create the PosLast3 column ----
# -3 if its the first race; 3* pos in the first race if its the secod race; (pos1+pos2)*1.5 if its the third race, sum last 3 pos if its more than third race

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(
    PosLast3Trainer = ifelse(row_number() == 1, -3,
                            ifelse(row_number() == 2, lag(FinishPosition3) * 3,
                                   ifelse(row_number() == 3, (lag(FinishPosition3) * 1.5) + (lag(lag(FinishPosition3)) * 1.5),
                                          lag(FinishPosition3, 1) + lag(FinishPosition3, 2) + lag(FinishPosition3, 3)
                                   )
                            )
    )
  )


# ---- Create the OppsPriorPos1st column ----
# SUm of number of times opponents has finished 1st in previous races
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPos1stTrainer = sum(PriorPosition1stTrainer) - PriorPosition1stTrainer
  ) %>%
  ungroup()


# ---- Create the OppsPriorPos1st column ----
# SUm opponents PosLast3 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPosLast3Trainer = sum(PosLast3Trainer) - PosLast3Trainer
  ) %>%
  ungroup()

# ---- Create the OppsDelayLasRace column ----
# SUm opponents OppsDelayLasRace 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsDelayLasRaceTrainer = sum(DelayLastRaceTrainer) - DelayLastRaceTrainer
  ) %>%
  ungroup()

# Create the number of NA in FinishPosition3 by TrainerID

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(NumNAPrior_T = cumsum(is.na(FinishPosition3)) - is.na(FinishPosition3)) %>%
  ungroup()

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PrevBeatenMargin_T = lag(BeatenMargin, order_by = RaceStartTime, default = first(BeatenMargin)))

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PrevPriceSP_T = lag(PriceSP, order_by = RaceStartTime, default = first(PriceSP)))

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PIRPosition_T = lag(PIRPosition, order_by = RaceStartTime, default = first(PIRPosition)))

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PriorSumMoneyWon_T = cumsum(Prizemoney) - Prizemoney)

data_horse1 <- data_horse1 %>%
  arrange(TrainerID, RaceStartTime) %>%
  group_by(TrainerID) %>%
  mutate(PriorSumPos_T = cumsum(FinishPosition3) - FinishPosition3)


#------------ NEW FEATURES RELATED TO Sire  ----------------------------

#----Number of times a horse finished first (prior currrent race)-----
# Load the dplyr package if not already loaded

# Sort the dataset by HorseId and RaceStartTime to ensure data is in chronological order
data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime)

# Create the PriorPosition1st column
data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PriorPosition1stSire = cumsum(FinishPosition3 == 1) - (FinishPosition3 == 1))


# Create the PriorNumberOfRaces column
data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PriorNumberOfRacesSire = row_number() - 1)


# ---- Create the DelayLastRace column ----
# Put 365 if its the first race, if not, compute the delay between current race and last race
data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(
    DelayLastRaceSire = ifelse(row_number() == 1, 365, as.numeric(difftime(RaceStartTime, lag(RaceStartTime), units = "days")) )
  )

#data_horse1 = subset(data_horse1, select = -c(HasPositionn,FinishPosition1) )

# ---- Create the PosLast3 column ----
# -3 if its the first race; 3* pos in the first race if its the secod race; (pos1+pos2)*1.5 if its the third race, sum last 3 pos if its more than third race

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(
    PosLast3Sire = ifelse(row_number() == 1, -3,
                             ifelse(row_number() == 2, lag(FinishPosition3) * 3,
                                    ifelse(row_number() == 3, (lag(FinishPosition3) * 1.5) + (lag(lag(FinishPosition3)) * 1.5),
                                           lag(FinishPosition3, 1) + lag(FinishPosition3, 2) + lag(FinishPosition3, 3)
                                    )
                             )
    )
  )


# ---- Create the OppsPriorPos1st column ----
# SUm of number of times opponents has finished 1st in previous races
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPos1stSire = sum(PriorPosition1stSire) - PriorPosition1stSire
  ) %>%
  ungroup()


# ---- Create the OppsPriorPos1st column ----
# SUm opponents PosLast3 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPosLast3Sire = sum(PosLast3Sire) - PosLast3Sire
  ) %>%
  ungroup()

# ---- Create the OppsDelayLasRace column ----
# SUm opponents OppsDelayLasRace 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsDelayLasRaceSire = sum(DelayLastRaceSire) - DelayLastRaceSire
  ) %>%
  ungroup()

# Create the number of NA in FinishPosition3 by SireID

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(NumNAPrior_S = cumsum(is.na(FinishPosition3)) - is.na(FinishPosition3)) %>%
  ungroup()

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PrevBeatenMargin_S = lag(BeatenMargin, order_by = RaceStartTime, default = first(BeatenMargin)))

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PrevPriceSP_S = lag(PriceSP, order_by = RaceStartTime, default = first(PriceSP)))

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PIRPosition_S = lag(PIRPosition, order_by = RaceStartTime, default = first(PIRPosition)))

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PriorSumMoneyWon_S = cumsum(Prizemoney) - Prizemoney)

data_horse1 <- data_horse1 %>%
  arrange(SireID, RaceStartTime) %>%
  group_by(SireID) %>%
  mutate(PriorSumPos_S = cumsum(FinishPosition3) - FinishPosition3)


#------------ NEW FEATURES RELATED TO DamID  ----------------------------

#----Number of times a horse finished first (prior currrent race)-----
# Load the dplyr package if not already loaded

# Sort the dataset by HorseId and RaceStartTime to ensure data is in chronological order
data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime)

# Create the PriorPosition1st column
data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PriorPosition1stDam = cumsum(FinishPosition3 == 1) - (FinishPosition3 == 1))


# Create the PriorNumberOfRaces column
data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PriorNumberOfRacesDam = row_number() - 1)


# ---- Create the DelayLastRace column ----
# Put 365 if its the first race, if not, compute the delay between current race and last race
data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(
    DelayLastRaceDam = ifelse(row_number() == 1, 365, as.numeric(difftime(RaceStartTime, lag(RaceStartTime), units = "days")) )
  )

#data_horse1 = subset(data_horse1, select = -c(HasPositionn,FinishPosition1) )

# ---- Create the PosLast3 column ----
# -3 if its the first race; 3* pos in the first race if its the secod race; (pos1+pos2)*1.5 if its the third race, sum last 3 pos if its more than third race

data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(
    PosLast3Dam = ifelse(row_number() == 1, -3,
                          ifelse(row_number() == 2, lag(FinishPosition3) * 3,
                                 ifelse(row_number() == 3, (lag(FinishPosition3) * 1.5) + (lag(lag(FinishPosition3)) * 1.5),
                                        lag(FinishPosition3, 1) + lag(FinishPosition3, 2) + lag(FinishPosition3, 3)
                                 )
                          )
    )
  )


# ---- Create the OppsPriorPos1st column ----
# SUm of number of times opponents has finished 1st in previous races
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPos1stDam = sum(PriorPosition1stDam) - PriorPosition1stDam
  ) %>%
  ungroup()


# ---- Create the OppsPriorPos1st column ----
# SUm opponents PosLast3 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsPriorPosLast3Dam = sum(PosLast3Dam) - PosLast3Dam
  ) %>%
  ungroup()

# ---- Create the OppsDelayLasRace column ----
# SUm opponents OppsDelayLasRace 
data_horse1 <- data_horse1 %>%
  arrange(RaceID, RaceStartTime) %>%
  group_by(RaceID) %>%
  mutate(
    OppsDelayLasRaceDam = sum(DelayLastRaceDam) - DelayLastRaceDam
  ) %>%
  ungroup()

# Create the number of NA in FinishPosition1 by DamID


data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(NumNAPrior_D = cumsum(is.na(FinishPosition3)) - is.na(FinishPosition3)) %>%
  ungroup()

data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PrevBeatenMargin_D = lag(BeatenMargin, order_by = RaceStartTime, default = first(BeatenMargin)))

data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PrevPriceSP_D = lag(PriceSP, order_by = RaceStartTime, default = first(PriceSP)))

data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PIRPosition_D = lag(PIRPosition, order_by = RaceStartTime, default = first(PIRPosition)))

data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PriorSumMoneyWon_D = cumsum(Prizemoney) - Prizemoney)

data_horse1 <- data_horse1 %>%
  arrange(DamID, RaceStartTime) %>%
  group_by(DamID) %>%
  mutate(PriorSumPos_D = cumsum(FinishPosition3) - FinishPosition3)

selected_columns <- data_horse1 %>%
  select(
    RaceID, HorseID, Barrier, Distance, FoalingCountry, FrontShoes, Gender, GoingAbbrev,
    HandicapDistance, HindShoes, HorseAge, RacePrizemoney, RacingSubType,
    Saddlecloth, StartType, StartingLine, Surface, NoFrontCover, HasPosition,
    WideOffRail, WeightCarried, WetnessScale, RaceStartTime,
    FinishPosition3:PriorSumPos_D
  )

# Create a new dataset with the selected columns
data_model <- as.data.frame(selected_columns)

#data_model <- subset(data_model, select = -c(HorseID) )

###############################################################################################################
#data_model <- read.csv("C:\\Users\\User\\Documents\\Horse Racing\\data_model.csv")
#rs <- read.csv("C:\\Users\\User\\Documents\\Horse Racing\\data_model.csv")
#rs <- subset(rs, select = -c(HorseID) )

# Define the number of categories
num_categories <- 3

# Calculate quantiles to find cut-off points
quantiles <- quantile(data_model$RacePrizemoney, probs = seq(0, 1, length.out = num_categories + 1))

# Extract the cut-off points for your three categories
cut_off_points <- quantiles[-c(1, length(quantiles))]

# Create the categorical variable
data_model$RaceMoney_Cat <- cut(data_model$RacePrizemoney, 
                                          breaks = c(-Inf, cut_off_points, Inf), 
                                          labels = c("Low", "Medium", "High"))

# Calculate quantiles to find cut-off points
quantiles <- quantile(data_model$Distance, probs = seq(0, 1, length.out = num_categories + 1))

# Extract the cut-off points for your three categories
cut_off_points <- quantiles[-c(1, length(quantiles))]

# Create the categorical variable
data_model$Distance_Cat <- cut(data_model$Distance, 
                                breaks = c(-Inf, cut_off_points, Inf), 
                                labels = c("Short", "Medium", "Long"))


# Calculate quantiles to find cut-off points
quantiles <- quantile(as.numeric(data_model$Saddlecloth), probs = seq(0, 1, length.out = num_categories + 1))

# Extract the cut-off points for your three categories
cut_off_points <- quantiles[-c(1, length(quantiles))]

# Create the categorical variable
data_model$Saddlecloth_Cat <- cut(as.numeric(data_model$Saddlecloth), 
                               breaks = c(-Inf, cut_off_points, Inf), 
                               labels = c("Short", "Medium", "Long"))

##############
# Calculate quantiles to find cut-off points
quantiles <- quantile(data_model$WideOffRail, probs = seq(0, 1, length.out = num_categories + 1))

# Extract the cut-off points for your three categories
cut_off_points <- quantiles[-c(1, length(quantiles))]

# Create the categorical variable
data_model$WideOffRail_Cat <- cut(data_model$WideOffRail, 
                                  breaks = c(-Inf, cut_off_points, Inf), 
                                  labels = c("Short", "Medium", "Long"))


data_model <- data_model %>%
  mutate(StartingLine_Cat = recode(StartingLine, `-1` = "F", `1` = "G", `2` = "H"))

data_model <- data_model %>%
  mutate(WetnessScale_Cat = recode(WetnessScale, `3` = "G", `1` = "F", `4` = "G",`7`='H',`9`='H'))
# You're here drop variables you create categprie for


data_model <- data_model %>%
  mutate(NoFrontCover_Cat = recode(NoFrontCover, `-9` = "F", `0` = "G", `1` = "H"))


# Specify the columns to transform into factors
factor_columns <- c(
  "FoalingCountry", "Gender", "GoingAbbrev", "RacingSubType", "StartType", "StartingLine_Cat", "Surface", 
  "NoFrontCover_Cat", "RaceOutcome", "HasPosition","WetnessScale_Cat", "WideOffRail_Cat", "RaceMoney_Cat", "Distance_Cat","Saddlecloth_Cat"
)


# Select and transform factor columns
factored_data <- data_model %>%
  select(all_of(factor_columns)) %>%
  mutate(across(.cols = everything(), .fns = as.factor))


Other<- data_model %>% select(RaceID, HorseID, RaceStartTime)


# Define your continuous variable columns # I'am there***********************8
#continuous_columns <- data_model %>% select(-all_of(factor_columns), -RaceID, -HorseID, -RaceStartTime,-X,-FinishPosition3,-FinishPosition1,-Saddlecloth,
#                                            -NoFrontCover,-WetnessScale,-WideOffRail,-StartingLine,-Distance,-RacePrizemoney)

continuous_columns <- data_model %>% select(-all_of(factor_columns), -RaceID,-HorseID, -RaceStartTime,-FinishPosition3,-Saddlecloth,
                                            -NoFrontCover,-WetnessScale,-WideOffRail,-StartingLine,-Distance,-RacePrizemoney)

# Extract the column names from the data frame
continuous_column_names <- colnames(continuous_columns)

# Function to standardize a column
standardize_column <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    return(0.001)
  } else {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  }
}

# Standardize continuous variables within each "RaceID" group
standardized_data1 <- data_model %>%
  group_by(RaceID) %>%
  mutate_at(.vars = continuous_column_names, .funs = list(~ standardize_column(.)))



#For some reason, R got me non continious variables. Now I retake only the ones i scaled by raceid
standardized_data1 <- standardized_data1[,continuous_column_names]

# Combine factor and continuous variables into a new dataset
data_model1 <- cbind(factored_data, standardized_data1, Other)
#data_model1 <- data_model1 %>% select(-HorseID)

# Convert the factor variable to a character variable
data_model1$FoalingCountry <- trimws(as.character(data_model1$FoalingCountry))
# Create a new binary variable within the dataset
data_model1$FoalingCountry1 <- ifelse(data_model1$FoalingCountry == "FR", 1, 0)
data_model1 <- data_model1 %>% select(-FoalingCountry)


#data_model1 <- data_model1 %>% select(-NoFrontCover)


###################_________#############################################
# Dividing the dataset into train set (before november 1st, 2022) and test set
data_model1$RaceStartTime <- as.Date(data_model1$RaceStartTime)

# Create the training set (before November 1, 2022)
train_data <- subset(data_model1, RaceStartTime < as.Date("2021-11-01"))

#Race_outcome_train <- train_data$RaceOutcome
train_data <- train_data %>% select(-RaceID,-RaceStartTime,-GoingAbbrev) ########################################################################

# Create the test set (from November 1, 2022 to the last date)
test_data <- subset(data_model1, RaceStartTime >= as.Date("2021-11-01")) # Don't know why but GoingAbbrev gets me trouble when sampling (over/under)

#RaceID_To_Norm <- test_data$RaceID

RaceID_To_Norm <- test_data[, c("HorseID","RaceStartTime","RaceID")]

#Race_outcome_test <- test_data$RaceOutcome

test_data <- test_data %>% select(-RaceID,-RaceStartTime, -GoingAbbrev)


formula <- reformulate(termlabels = names(train_data)[names(train_data) != "RaceOutcome"], response = "RaceOutcome")

################################################### One-Hot encoding ################################################################################################

test_data$RaceOutcome <- ifelse(test_data$RaceOutcome == "Win", 1, 0)
train_data$RaceOutcome <- ifelse(train_data$RaceOutcome == "Win", 1, 0)

fac_columns <-c( "Gender",     "RacingSubType"  ,  "StartType"    ,    "StartingLine_Cat", "Surface"  ,        "NoFrontCover_Cat"
                     ,  "WetnessScale_Cat", "WideOffRail_Cat" , "RaceMoney_Cat"  ,  "Distance_Cat"  ,   "Saddlecloth_Cat" )
library(caret)

# One-Hot encoding for test_data
# Create dummy variables using dummyVars with fullRank = TRUE
dummy_variables <- dummyVars( ~ ., data = test_data[, fac_columns], fullRank = TRUE)

# Apply the dummy variables to the original data
under_sampled_data_encoded <- predict(dummy_variables, newdata = test_data)

# Combine the encoded data with the remaining columns
test_data_encoded1 <- cbind(test_data[, setdiff(names(test_data), fac_columns)], under_sampled_data_encoded)

# View the final encoded data
head(test_data_encoded1)


# One-Hot encoding for test_data
# Create dummy variables using dummyVars with fullRank = TRUE
dummy_variables <- dummyVars( ~ ., data = train_data[, fac_columns], fullRank = TRUE)

# Apply the dummy variables to the original data
under_sampled_data_encoded <- predict(dummy_variables, newdata = train_data)

# Combine the encoded data with the remaining columns
train_data_encoded1 <- cbind(train_data[, setdiff(names(train_data), fac_columns)], under_sampled_data_encoded)

# View the final encoded data
head(train_data_encoded1)

write.csv(train_data_encoded1, file = "C:\\Users\\User\\Documents\\Horse Racing\\train_data_encoded1_without_over_under.csv", row.names = TRUE)

##############################################################################################################################################


############################ UNDER SAMPLING to manage imbalance Not_Win: 1028949     Win: 137595 ###################################


# Load the purrr package
library(ROSE)

# Determine the under-sampling ratio to balance the classes
min_class_count <- min(table(train_data_encoded1$RaceOutcome))

# Specify which class to under-sample in the formula
ros_data <- ovun.sample(RaceOutcome ~ ., data = train_data_encoded1, method = "under", 
                        N = 2*min_class_count, seed = 123)$data
under_sampled_train_data <- ros_data  # table(under_sampled_train_data$RaceOutcome) ---> Not_Win : 137595 and Win:137595 


############################## Undersampling and Oversampling. Desired size for each class: (length(train_data$RaceOutcome)/2
#n_rows<-(length(train_data$RaceOutcome))/2
desired_size <- (length(train_data_encoded1$RaceOutcome))/2

# Perform under-sampling on the majority class to the desired size
under_sampled_data <- ovun.sample(RaceOutcome ~ ., data = train_data_encoded1, method = "under", N = desired_size+min_class_count)$data


# Perform over-sampling on the minority class to the desired size
over_sampled_data <- ovun.sample(RaceOutcome ~ ., data = train_data_encoded1, method = "over", N = 3*desired_size-min_class_count)$data

# Filter under_sampled_data where RaceOutcome is "Not_Win"
filtered_under_sampled_data <- under_sampled_data[under_sampled_data$RaceOutcome == 0, ]

# Filter over_sampled_data where RaceOutcome is "Win"
filtered_over_sampled_data <- over_sampled_data[over_sampled_data$RaceOutcome == 1, ]

# Combine the filtered datasets
over_under_sampled_train_data <- rbind(filtered_under_sampled_data, filtered_over_sampled_data)

#write.csv(under_sampled_train_data, file = "C:\\Users\\User\\Documents\\Horse Racing\\under_sampled_train_data_encoded2.csv", row.names = TRUE)
#write.csv(test_data_encoded1, file = "C:\\Users\\User\\Documents\\Horse Racing\\test_data_encoded2.csv", row.names = TRUE)





