# Data Analysis


# Here is a link to a lot of different statistical analyses you can do in R. Start by looking at the BCI papers what type of test do they use? It would make sense to use the same type as them. https://stats.idre.ucla.edu/r/whatstat/what-statistical-analysis-should-i-usestatistical-analyses-using-r/


#------- libraries ------------
library(tidyverse)
library(magrittr)

#------- Load Data ------------
load("simple_data_Clean.rda")
load("merged_data_Clean.rda")

#------- Order of PAMs --------

# Does the test order of the PAM influence the fustration?
TestIDandFrustration <- dfStat %>%
  group_by(TestID) %>%
  summarise(meanFrustration = mean(Frustration),
            medianFrustration = median(Frustration),
            sdFrustration = sd(Frustration))

TestIDandFrustration

summary(lm(Frustration ~ TestID, data = dfStat))

# Does the test order of the PAM influence the Control?
TestIDandControl <- dfStat %>%
  group_by(TestID) %>%
  summarise(meanControl = mean(Control),
            medianControl = median(Control),
            sdControl = sd(Control))

TestIDandControl

summary(lm(Control ~ TestID, data = dfStat))

# Does the test order of the PAM influence the Control
TestIDandRank <- dfStat %>%
  dplyr::group_by(TestID) %>%
  summarise(meanRank = mean(Rank),
            medianRank = median(Rank),
            sdRank = sd(Rank))

TestIDandRank

summary(lm(Rank ~ TestID, data = dfStat))

#------- Frustration --------
FrustrationOverview <- dfStat %>%
  group_by(PAM) %>%
  summarise(meanFrustration = mean(Frustration),
            medianFrustration = median(Frustration),
            sdFrustration = sd(Frustration))

FrustrationOverview

dfStat %>% 
  ggplot(aes(x = PAM, y = Frustration, color = PAM)) +
  geom_violin() + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic()

# PAMs Influence on Fustration
summary(lm(Frustration ~ PAM, data = dfStat))

#------- Control --------

ControlOverview <- dfStat %>%
  group_by(PAM) %>%
  summarise(meanControl = mean(Control),
            medianControl = median(Control),
            sdControl = sd(Control))

ControlOverview

# PAMs Influence on Control
summary(lm(Control ~ PAM, data = dfStat))

#------- Rank --------

RankOverview <- dfStat %>%
  dplyr::group_by(PAM) %>%
  summarise(meanRank = mean(Rank),
            medianRank = median(Rank),
            sdRank = sd(Rank))

RankOverview

dfStat %>% 
  filter(PAM != "Training") %>%
  ggplot(aes(x = PAM, y = Rank, color = PAM)) +
  geom_violin() +
  theme_classic()

#------- Frustration vs Control --------

dfStat %>% 
  ggplot(aes(x = Control, y = Frustration, color = PAM)) +
  geom_jitter() +
  geom_smooth(method = lm, se=F) +
  theme_classic()

# if we look at all the data
summary(lm(Frustration ~ Control, data = dfStat))

# If we split them up 
summary(lm(Frustration ~ Control * PAM, data = dfStat))

#------- Frustration vs Rank --------

dfStat %>% 
  ggplot(aes(x = Rank, y = Frustration, #color = PAM
             )) +
  geom_jitter() +
  geom_smooth(method = lm, se=F) +
  theme_classic()

# if we look at all the data
summary(lm(Frustration ~ Rank, data = dfStat))

# If we split them up 
summary(lm(Frustration ~ PAM, data = dfStat))

# If we split them up 
summary(lm(Frustration ~ Rank * PAM, data = dfStat))

#------- Frustration vs Gender --------

GenderFrustrationOverview <- dfStat %>%
  group_by(Gender) %>%
  summarise(meanFrustration = mean(Frustration, na.rm = TRUE),
            medianFrustration = median(Frustration, na.rm = TRUE),
            sdFrustration = sd(Frustration, na.rm = TRUE))

GenderFrustrationOverview

# Are males and females equal?
summary(lm(Frustration ~ Gender, data = dfStat))

#------- Frustration vs Age  --------

dfStat %>% 
  ggplot(aes(x = Age, y = Frustration)) +
  geom_jitter() +
  geom_smooth(aes(color = Gender), method = lm, se=F) +
  #geom_smooth(aes(color = PAM), method = lm, se=F) +
  theme_classic()

# Are males and females equal?
summary(lm(Frustration ~ Age, data = dfStat))

#------- Control vs Rank --------

dfStat %>% 
  ggplot(aes(x = Rank, y = Control)) +
  #geom_violin(shape=16, position=position_jitter(0.1)) +
  geom_smooth(color = "Black", method = lm, se=F) +
  geom_smooth(aes(color = PAM), method = lm, se=F) +
  theme_classic()

# if we look at all the data
summary(lm(Control ~ Rank, data = dfStat))

# If we split them up 
summary(lm(Control ~ Rank * PAM, data = dfStat))

#------- Control vs Gender --------

GenderControlOverview <- dfStat %>%
  group_by(Gender) %>%
  summarise(meanControl = mean(Control, na.rm = TRUE),
            medianControl = median(Control, na.rm = TRUE),
            sdControl = sd(Control, na.rm = TRUE))

GenderControlOverview

dfStat %>% 
  ggplot(aes(x = Gender, y = Control)) +
  geom_violin(shape=16, position=position_jitter(0.1)) +
  theme_classic()

# Are men and women equal?
summary(lm(Control ~ Gender, data = dfStat))
