
# Header ------------------------------------------------------------------

# title: "Trump Classifier, Assignment 2"
# author: "James Wu, Hongye Wu, Eva Wang"
# date: "October 4, 2018"
# output: R script


# A) Set up ------------------------------------------------------------------

library(tidyverse)
library(twitteR)
library(lubridate)
library(scales)
library(naivebayes)
library(e1071)
library(tidytext)

trump_tweets <- read_tsv("./Data/trump_data.tsv", col_names = c("source", "time_posted", "text"))


# B) Clean/organize dataset --------------------------------------------------

# Create graph to see the relationship between the source of tweets and the hour of day tweeted
trump_tweets %>% 
  count(source, hour = hour(with_tz(time_posted, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
# So Trump usually tweets in the early morning, whereas his staff usually tweets in the afternoon and early evening
# Add the feature "hour" to the trump_tweets data.
trump_tweets <- mutate(trump_tweets, hour = hour(with_tz(time_posted, "EST")))


# After looking at the text more, we realized another identifier might be 
# whether the tweet contains multimedia information (such as pictures), denoting by https://t.co* in text 
trump_tweets %>% count(source,
                        multimedia = ifelse(str_detect(text, "t.co"),
                                  "multimedia", "just text")) %>% 
                 mutate(percent = n / sum(n))
trump_tweets <-mutate(trump_tweets, mutlimedia = ifelse(str_detect(text, "t.co"), "multimedia", "just text"))

# Trump also seems to be more likely to retweet others
trump_tweets %>% count(
  source, retweet = ifelse(str_detect(text, '^"'), "retweet", "not retweet")
) %>% 
  mutate(percent = n / sum(n))
trump_tweets <-mutate(trump_tweets, retweet = ifelse(str_detect(text, '^"'), "yes", "no"))

# Staff seems to like using hashtag much more than Trump
trump_tweets %>% count(
  source, hashtag = ifelse(str_detect(text, "#"), "hastag", "no hastag")
) %>% 
  mutate(percent = n / sum(n))
trump_tweets <-mutate(trump_tweets, hashtag = ifelse(str_detect(text, "#"), "yes", "no"))

trump_tweets %>% count(
  source, hashtag = ifelse(str_detect(text, "#"), "hastag", "no hastag")
) %>% 
  mutate(percent = n / sum(n))
trump_tweets <-mutate(trump_tweets, hashtag = ifelse(str_detect(text, "#"), "yes", "no"))

#Comparison of words: extract key words in each tweet 
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- trump_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))
#add severeal key workds to the model? (whether used or not?)
#check the frequency of keywords?

# C) Divide dataset -------------------------------------------------------

# Randomly select training and testing sets from the original dataset
dt = sort(sample(nrow(trump_tweets), nrow(trump_tweets)*.8))
training <- trump_tweets[dt,]
test <- trump_tweets[-dt,]

  
# Naive Bayes model
nb<- naive_bayes(source~hour+mutlimedia+retweet+hashtag ,data= training,prior = NULL, laplace = 1 )
NB_Predictions=predict(nb,test)
summary(NB_Predictions)


#Load the test dataset trump_hidden_test_set.csv
#test_data<-read_csv("./Data/trump_data.tsv")