# Joining data with dplyr
library(dplyr)
library(readr)

# Retreive the data for Stack Overflow questions tags and answers

# The data can be found here
# https://data.stackexchange.com/stackoverflow/query/new

# Or here
questions <- read_rds(url("https://assets.datacamp.com/production/repositories/5284/datasets/89d5a716b4f41dbe4fcda1a7a1190f24f58f0e47/questions.rds"))
tags <- read_rds(url("https://assets.datacamp.com/production/repositories/5284/datasets/207c31b235786e73496fd7e58e416779911a9d98/tags.rds"))
question_tags <- read_rds(url("https://assets.datacamp.com/production/repositories/5284/datasets/966938d665c69bffd87393b345ea2837a94bab97/question_tags.rds"))
answers <- read_rds(url("https://assets.datacamp.com/production/repositories/5284/datasets/6cb9c039aa8326d98de37afefa32e1c458764638/answers.rds"))

# Inspect the data
head(questions)
head(tags)
head(question_tags)
head(answers)

# Join the questions with tag ids and then tag names, then replace NAs
questions_with_tags <- questions %>%
  left_join(question_tags, by = c("id" = "question_id")) %>%
  left_join(tags, by = c("tag_id" = "id")) %>%
  replace_na(list(tag_name = "only-r"))
  
head(questions_with_tags)

# Check the most common tags
questions_with_tags %>%
  count(tag_name, sort = TRUE)

# Compare scores across tags
questions_with_tags %>%
  group_by(tag_name) %>%
  summarize(num_questions = n(), score = mean(score)) %>%
  arrange(desc(num_questions))

# Using a join, filter for tags that are never on an R question
tags %>% 
  anti_join(question_tags, by = c("id" = "tag_id"))

# find gaps between quesitons and answers
questions %>%
  inner_join(answers, by = c("id" = "question_id"), suffix = c("_question", "_answer")) %>%
  mutate(gap = as.integer(creation_date_answer - creation_date_question))

# Count and sort the question id column in the answers table
answer_counts <- answers %>%
  count(question_id, sort = TRUE)

# Combine the answer_counts and questions tables
question_answer_counts <- questions %>%
  left_join(answer_counts, by = c("id" = "question_id")) %>%
  replace_na(list(n = 0))

# Join the tags to the question answer counts
tagged_answers <- question_answer_counts %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id"))

# Calculate average answers per question
tagged_answers %>%
  group_by(tag_name) %>%
  summarize(questions = n(), average_answers = mean(n)) %>%
  arrange(desc(questions))

# Join tags to both the questions and answers in new tables
# Inner join the question_tags and tags tables with the questions table
questions_with_tags <- questions %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id"))

# Inner join the question_tags and tags tables with the answers table
answers_with_tags <- answers %>%
  inner_join(question_tags, by = "question_id") %>%
  inner_join(tags, by = c("tag_id" = "id"))

library(lubridate)

# Combine the two tables into posts_with_tags
posts_with_tags <- bind_rows(questions_with_tags %>% mutate(type = "question"),
                             answers_with_tags %>% mutate(type = "answer"))


# Add year column, ggregate by year and type, then plot
posts_with_tags %>%
  mutate(year = year(creation_date)) %>%
  count(year, type) %>%
  ggplot(aes(x = year, y = n, color = type)) +
  geom_line()

# Add a year column, aggregate by type, year, and tag_name, then filter and plot
posts_with_tags %>%
  mutate(year = year(creation_date)) %>%
  count(type, year, tag_name) %>%
  filter(tag_name %in% c("dplyr", "ggplot2")) %>%
  ggplot(aes(x = year, y = n, color = type)) +
  geom_line() +
  facet_wrap(~tag_name)