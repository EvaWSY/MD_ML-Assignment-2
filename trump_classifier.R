
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
#read in test data
hidden_test_data = read_tsv('./Data/trump_hidden_test_set.tsv',col_names = c('source', 'time_posted','text'), quote='')

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
trump_tweets <-mutate(trump_tweets, multimedia = ifelse(str_detect(text, "t.co"), "multimedia", "just text"))

# Trump also seems to be more likely to retweet others
trump_tweets %>% count(
  source, retweet = ifelse(str_detect(text, '^"'), "retweet", "not retweet")
) %>% 
  mutate(percent = n / sum(n))
trump_tweets <-mutate(trump_tweets, retweet = ifelse(str_detect(text, '^"'), "yes", "no"))

# Staff seems to like using hashtag much more than Trump
trump_tweets %>% count(
  source, hashtag = ifelse(str_detect(text, "#"), "hashtag", "no hashtag")
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
#add several key words to the model? (whether used or not?)
#check the frequency of keywords?

# While not a large predictor, Trump seems to like to mention "Democrats" and "Republican"
trump_tweets %>% count(source,
                       Democrats = ifelse(str_detect(text, "(Democrat)"),
                                           "Democrat", "No Democrat")) %>% 
  mutate(percent = n / sum(n))

trump_tweets %>% count(source,
                       Democrats = ifelse(str_detect(text, "(Republican)"),
                                          "Republican", "No Republican")) %>% 
  mutate(percent = n / sum(n))

trump_tweets <-mutate(trump_tweets, Republican = ifelse(str_detect(text, "(Republican)"), "yes", "no"),
                      Democrat = ifelse(str_detect(text, "(Republican)"), "yes", "no"))


# C) Divide dataset -------------------------------------------------------

# Randomly select training and testing sets from the original dataset
set.seed(123)
dt = sort(sample(nrow(trump_tweets), nrow(trump_tweets)*.8))
training <- trump_tweets[dt,]
test <- trump_tweets[-dt,]
  
# Naive Bayes model
nb_model <- training %>% 
  naive_bayes(source ~ multimedia + retweet + hashtag + Republican + Democrat,laplace = 1,data=.)

pred<- predict(nb_model,test)
# check for performance
accurate_pred <- sum(test$source == pred)/nrow(test)
accurate_pred
precision <-sum(test$source==pred & pred =="Trump")/sum(pred=="Trump")
precision
recall<- sum(test$source==pred & pred == "Trump")/(sum(test$source==pred & pred == "Trump")+sum(test$source!=pred & pred == "Staff"))
recall




# D) Divide dataset and implement the classifier -------------------------------------------------------
hidden_test_data<-hidden_test_data %>% mutate(hour = hour(with_tz(time_posted, "EST")),
                            multimedia = ifelse(str_detect(text, "t.co"), "multimedia", "just text"),
                            retweet = ifelse(str_detect(text, '^"'), "yes", "no"),
                            hashtag = ifelse(str_detect(text, "#"), "yes", "no"),
                            Republican = ifelse(str_detect(text, "(Republican)"), "yes", "no"),
                            Democrat = ifelse(str_detect(text, "(Democrat)"), "yes", "no")
                            )
#Predictions
predt<-predict(nb_model,hidden_test_data)
prediction<-ifelse(predict(nb_model,hidden_test_data)=="Trump",1,0)
write_csv(data.frame(prediction),"prediction.csv")

# check for performance
accurate_predt <- sum(hidden_test_data$source == predt)/nrow(hidden_test_data)
accurate_predt
precisiont <-sum(hidden_test_data$source==predt & predt =="Trump")/sum(predt=="Trump")
precisiont
recallt <- sum(hidden_test_data$source==predt & predt == "Trump")/(sum(hidden_test_data$source =="Trump"))
recallt
