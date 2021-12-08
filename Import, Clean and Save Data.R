# Import, Clean and Save Data
#------- Libraries -------
library(tidyverse)
library(readbulk)
library(magrittr)
library(tidyr)

options("digits.secs" = 6)

#------- LOGGED DATA --------

#------- Import and save raw Logged data --------
# Combine all logged data files into one data frame
dfL = readbulk::read_bulk('LoggedData', sep=',', na.strings = 'none', stringsAsFactors=FALSE)

# In R there are two main ways to get a quick overview of the data
str(dfL)

summary(dfL)

#As we can see there is a lot of fucked columns in the data I just want to keep the first 13
dfL <- dfL[c(1:12)]

dfL <- dfL[!(dfL$End=="End!"),]

# See the difference 
str(dfL)

summary(dfL)

#Save the imported files
save(dfL, file='logged_data_Raw.rda', compress=TRUE)

#------- Load Raw Data From rda --------

#Load
load("logged_data_Raw.rda")

#------- Clean: testID, time and names --------

# order data correctly by creating a row number to keep track of things
dfL <- dfL %>%
  arrange(Participant.ID, Test.ID, Timestamp) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

# Make n/a into true NA
dfL[dfL == "n/a"] <- NA

# Rename Columns
dfL %<>% rename(ParticipantID = Participant.ID,
                TestID = Test.ID,
                NumBox = X..Box,
                Range = Within.Range.,
                Collision = Box.Collision.,
                End = End.
                )

# Make variables into factor(s)
dfL %<>%  mutate(Gender = as.factor(Gender),
                 ParticipantID = as.factor(ParticipantID),
                 TestID = as.factor(TestID),
                 PAM = as.factor(PAM),
                 State = as.factor(State),
                 Range = as.factor(Range),
                 Collision = as.factor(Collision))

# Rename the different PAMs into something more telling
dfL$PAM <- recode_factor(dfL$PAM,
                         "0" = "Training",
                         "1" = "Base Game", 
                         "2" = "Input Override", 
                         "3" = "Mitigated Failure",
                         "4" = "Rule Change",
                         "5" = "Augmented Success")

# Make sure all graphs and analysis keeps the same order of pams - just for concistancy
dfL$PAM <- factor(dfL$PAM, 
                  levels=c("Training", 
                           "Base Game",
                           "Input Override",
                           "Mitigated Failure",
                           "Rule Change",
                           "Augmented Success"))

# Rename the different State into something more telling
dfL$State <- recode_factor(dfL$State,
                         "1" = "Baseline",
                         "2" = "PAM", 
                         "3" = "Forced Fail",
                         "4" = "True Fail")


save(dfL, file='logged_data_Clean.rda', compress=TRUE)

#------- PARTICIPANT QUESTIONNAIRE --------
#------- Import and save raw Questionnaire data --------
# Combine all Questionnaire data files into one data frame
dfQ = readbulk::read_bulk('ParticipantQuestionnaire', sep=',', na.strings = 'none', stringsAsFactors=FALSE)

# In R there are two main ways to get a quick overview of the data
str(dfQ)

summary(dfQ)

#Save the imported files
save(dfQ, file='questionnaire_data_Raw.rda', compress=TRUE)

#------- Load Raw Data From rda --------

#Load
load("questionnaire_data_Raw.rda")

#------- Clean: testID, time and names --------

# Rename Columns
dfQ %<>% rename(ParticipantID = Participant.ID,
                Control = 3,
                Frustration = 4)

# Make a Column with PAM
dfQ$PAM <- substr(dfQ$File, 1, 4)

dfQ$PAM <- as.factor(dfQ$PAM)

# Rename the different PAMs into something more telling
dfQ$PAM <- recode_factor(dfQ$PAM,
                         "Pam1" = "Base Game", 
                         "pam2" = "Input Override", 
                         "pam3" = "Mitigated Failure",
                         "pam4" = "Rule Change",
                         "pam5" = "Augmented Success")

#Save the imported files
save(dfQ, file='questionnaire_data_Clean.rda', compress=TRUE)

#------- RANKED LIST FOR GAMES --------
#------- Import and save raw Rank data --------
# Combine all Rank data files into one data frame
dfR = readbulk::read_bulk('RankedListForGames', sep=',', na.strings = 'none', stringsAsFactors=FALSE)

# In R there are two main ways to get a quick overview of the data
str(dfR)

summary(dfR)

#Save the imported files
save(dfR, file='rank_data_Raw.rda', compress=TRUE)

#------- Load Raw Data From rda --------

#Load
load("rank_data_Raw.rda")

#------- Clean: testID, time and names --------

# Rename Columns
dfR %<>% rename(ParticipantID = Participant,
                Input = Input.Override,
                Mitigated = Mitigated.Fail,
                Rule = Rule.Change,
                Augmented = Augmented.Success) 

# Making the data long instead of wide
dfR <- gather(dfR, PAM, Rank, Base:Augmented, factor_key=TRUE)

dfR$PAM <- as.factor(dfQ$PAM)

# Rename the different PAMs into something more telling
dfR$PAM <- recode_factor(dfR$PAM,
                         "Base" = "Base Game", 
                         "Input" = "Input Override", 
                         "Mitigated" = "Mitigated Failure",
                         "Rule" = "Rule Change",
                         "Augmented" = "Augmented Success")

#Save the imported files
save(dfR, file='ranked_data_Clean.rda', compress=TRUE)

#------- MERGE ALL DATA TOGETHER --------

dfT <- merge(dfL, dfQ, by=c("ParticipantID","PAM"), all = TRUE)

dfT <- merge(dfT, dfR, by=c("ParticipantID","PAM"), all = TRUE)

# remove columns you will never use
dfT <- dfT[,-which(names(dfT) %in% c("File", "File.x", "File.y", "End", "Tidsstempel"))]

# Rearrange the columns
col_order <- c("rowNum", "ParticipantID", "Age", "Gender", "TestID", "PAM", "NumBox", "State", "Range", "Collision", "Control", "Frustration", "Rank", "Timestamp")

dfT <- dfT[, col_order]

remove(col_order)

#Save the imported files
save(dfT, file='merged_data_Clean.rda', compress=TRUE)


# Here I make a Data frame that is a bit easier to work with when we are looking at simple graphs and statistic
dfStat <- dfT %>%
  group_by(ParticipantID, TestID, PAM, Gender) %>%
  filter(Control != "NA") %>%
  summarise(Age = mean(Age),
            Control = mean(Control),
            Frustration = mean(Frustration),
            Rank = mean(Rank)) %>%
  ungroup()

#Save the imported files
save(dfStat, file='simple_data_Clean.rda', compress=TRUE)
