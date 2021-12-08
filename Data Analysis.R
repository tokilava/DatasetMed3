# Data Analysis
#------- libraries ------------
library(tidyverse)
library(magrittr)

#------- Load Data ------------
load("logged_data_Clean.rda")
load("questionnaire_data_Clean.rda")
load("ranked_data_Clean.rda")
load("merged_data_Clean.rda")

# Data Overview

#------- Frustration --------
FrustrationOverview <- dfT %>%
  group_by(PAM) %>%
  summarise(meanFrustration = mean(Frustration),
            medianFrustration = median(Frustration),
            sdFrustration = sd(Frustration))

FrustrationOverview

dfT %>% 
  ggplot(aes(x = PAM, y = Frustration, color = PAM)) +
  geom_violin() + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_errorbar(aes(ymin = Frustration - sd, ymax = Frustration + sd)) +
  theme_classic()

#------- Control --------

ControlOverview <- dfT %>%
  group_by(PAM) %>%
  summarise(meanControl = mean(Control),
            medianControl = median(Control),
            sdControl = sd(Control))

ControlOverview

#------- Rank --------

RankOverview <- dfT %>%
  dplyr::group_by(PAM) %>%
  dplyr::filter(PAM != "Training") %>%
  summarise(meanRank = mean(Rank),
            medianRank = median(Rank),
            sdRank = sd(Rank))

RankOverview

dfT %>% 
  filter(PAM != "Training") %>%
  ggplot(aes(x = PAM, y = Rank, color = PAM)) +
  geom_violin() +
  theme_classic()

#------- Frustration vs Control --------

dfT %>% 
  ggplot(aes(x = Control, y = Frustration, color = PAM)) +
  geom_jitter() +
  geom_smooth(method = lm, se=F) +
  theme_classic()

# if we look at all the data
summary(lm(Frustration ~ Control, data = dfT))

# If we split them up 
summary(lm(Frustration ~ Control * PAM, data = dfT))

#------- Frustration vs Rank --------

dfT %>% 
  #filter(Rank = NA) %>%
  ggplot(aes(x = Rank, y = Frustration, #color = PAM
             )) +
  geom_jitter() +
  geom_smooth(method = lm, se=F) +
  theme_classic()

# if we look at all the data
summary(lm(Frustration ~ Rank, data = dfT))

# If we split them up 
summary(lm(Frustration ~ PAM, data = dfT))

# If we split them up 
summary(lm(Frustration ~ Rank * PAM, data = dfT))

#------- Frustration vs Gender --------

GenderFrustrationOverview <- dfT %>%
  group_by(Gender) %>%
  summarise(meanFrustration = mean(Frustration, na.rm = TRUE),
            medianFrustration = median(Frustration, na.rm = TRUE),
            sdFrustration = sd(Frustration, na.rm = TRUE))

GenderFrustrationOverview

# Are males and females equal?
summary(lm(Frustration ~ Gender, data = dfT))

#------- Frustration vs Age  --------

dfT %>% 
  ggplot(aes(x = Age, y = Frustration)) +
  geom_jitter() +
  geom_smooth(aes(color = Gender), method = lm, se=F) +
  #geom_smooth(aes(color = PAM), method = lm, se=F) +
  theme_classic()

# Are males and females equal?
summary(lm(Frustration ~ Age, data = dfT))

#------- Control vs Rank --------

dfT %>% 
  ggplot(aes(x = Rank, y = Control)) +
  #geom_violin(shape=16, position=position_jitter(0.1)) +
  geom_smooth(color = "Black", method = lm, se=F) +
  geom_smooth(aes(color = PAM), method = lm, se=F) +
  theme_classic()

# if we look at all the data
summary(lm(Control ~ Rank, data = dfT))

# If we split them up 
summary(lm(Control ~ PAM, data = dfT))

# If we split them up 
summary(lm(Control ~ Rank * PAM, data = dfT))

#------- Control vs Gender --------

GenderControlOverview <- dfT %>%
  group_by(Gender) %>%
  summarise(meanControl = mean(Control, na.rm = TRUE),
            medianControl = median(Control, na.rm = TRUE),
            sdControl = sd(Control, na.rm = TRUE))

#interstring men feel more in crontrol than women, but women are more frustrated then men
GenderControlOverview

dfT %>% 
  ggplot(aes(x = Gender, y = Control)) +
  geom_violin(shape=16, position=position_jitter(0.1)) +
  theme_classic()

# Are men and women equal?
summary(lm(Control ~ Gender, data = dfT))
