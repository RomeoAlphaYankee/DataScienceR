install.packages("Lahman")

library(Lahman)
library(tidyverse)

data("LahmanData")


nominated <- HallOfFame %>%
  group_by(playerID) %>%
  tally()

nominated_full <- HallOfFame %>%
  left_join(Master, by = "playerID") %>%
  select(playerID, nameFirst, nameLast)

str(nominated)
str(nominated_full)

inducted <- HallOfFame %>%
  distinct(playerID, .keep_all = TRUE) %>%
  filter(inducted == "Y")

#Find players inducted
inducted %>%
  count(vars = playerID) %>%
  summarise(mean(n))
  summary(inducted$inducted)

inducted_full <- inducted %>%
  left_join(Master, by = "playerID") %>%
  select(playerID, nameFirst, nameLast)

#Player awards, total per player
nAwards <- AwardsPlayers %>%
  group_by(playerID) %>%
  tally()

#Avg number of player awards won by inductees
nAwards %>%
  semi_join(inducted, by = "playerID") %>%
  summarize(avg_n = mean(n))

#Average number of player awards won
nAwards %>%
  anti_join(inducted, by = "playerID") %>%
  summarize(avg_n = mean(n))

nAwards %>%
  semi_join(nominated, by = "playerID") %>%
  anti_join(inducted, by = "playerID") %>%
  summarize(avg_n = mean(n))

#Nominated, but not Inducted 
notInducted <- nominated %>% 
  setdiff(inducted)

#Avg max salary of a non-inducted nominee
Salaries %>% 
  inner_join(notInducted, by = "playerID") %>% 
  group_by(playerID) %>% 
  summarize(max_salary = max(salary)) %>% 
  summarize(avg_salary = mean(max_salary))

#Avg max salary of an inductee
Salaries %>% 
  inner_join(inducted) %>% 
  group_by(playerID) %>% 
  summarize(max_salary = max(salary)) %>% 
  summarize(avg_salary = mean(max_salary))

Appearances %>% 
  # Filter Appearances against nominated
  semi_join(nominated, by = "playerID") %>% 
  # Find last year played by player
  group_by(playerID) %>% 
  summarize(last_year = max(yearID)) %>% 
  # Join to full HallOfFame
  left_join(HallOfFame, by = "playerID") %>% 
  # Filter for unusual observations
  filter(last_year >= yearID)

masterInfo <- Master %>%
  select(playerID, nameLast, nameFirst, throws)

pitching1 <- right_join(masterInfo, Pitching, by = "playerID")

winp <- pitching1 %>%
  group_by(yearID, lgID) %>%
  filter(W == max(W)) %>%
  select(nameLast, nameFirst, teamID, W, throws)

filter(winp, W == max(winp$W))
