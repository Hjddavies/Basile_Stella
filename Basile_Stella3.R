library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(tidyverse)

consumerKey = "QqstUdQ2yLRUGboueH7wJIsv1"
consumerSecret = "ykRtbBKSarjpQYRBCUBUHaiuW4FnPz2iDLO2TkcEtO9mocYOTN"
accessToken = "240583200-1R98JE0NLVS1CzwngMk5zOywJzd4Ia2MMYX7jMF3"
accessSecret = "SdVrYx1Gdu0lNPBxAw6AkkQBiTcLUHPc6SrPNNPqttKGd"
options(httr_oauth_cache=TRUE)

#fix found on https://stackoverflow.com/questions/29634342/unable-to-use-the-setup-twitter-oauth-function-in-r-to-work-with-twitter
twitteR:::setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

#Scraping replies to Creasy
tweetstostella2 <- searchTwitter("@stellacreasy exclude:retweets", n=3200)
tweetstostella2_df <- tbl_df(map_df(tweetstostella2, as.data.frame))
write.csv(tweetstostella2_df, "tweetstostella2.csv")

tweetstostella2_df <- twListToDF(tweetstostella2)
tweet_words <- tweetstostella2_df %>% select(id, text) %>% unnest_tokens(word,text)


#sorting and simple plot of tweets
word_count <- tweet_words %>% count(word, sort = T) 

wordstheme <- theme(plot.title = element_text(colour = "steelblue", size = 20, hjust = 0.5), 
                    axis.text.x = element_text(angle = 60, hjust = 1))


word_count %>% slice(1:20) %>%
  ggplot(aes(x = reorder(word, n, function(n) - n), y = n)) + 
  geom_bar(stat = "identity") + 
  wordstheme +
  xlab("words") + 
  ylab("word count") + 
  ggtitle("how many times do certain words appear?")

#adding stop words to plotting 
#(largely from http://utstat.toronto.edu/~nathan/teaching/sta4002/Class1/scrapingtwitterinR-NT.html)
my_stop_words <- stop_words %>% select(-lexicon) %>%
  bind_rows(data.frame(
    word = c(
      "https",
      "t.co",
      "stellacreasy",
      "a",
      "to",
      "theresa_may",
      "stella",
      "10downingstreet",
      "it's",
      "mp",
      "amp",
      "is",
      "it",
      "on",
      "that",
      "for",
      "be",
      "not",
      "it's",
      "jessphillips",
      "mrjamesbob",
      "bbcpolitics",
      "labour",
    )
  ))

#secondary plotting after stop words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% tally(sort = TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) - n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,
                                                                                                                             hjust = 1)) + xlab("")
