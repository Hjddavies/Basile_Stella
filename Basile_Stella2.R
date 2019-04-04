Initial ideas from https://www.interhacktives.com/2017/01/25/scrape-tweets-r-journalists/ by Ryan Watts

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

#error in inital set-up - "could not find function "setup_twitter_oauth"
#fix found on https://stackoverflow.com/questions/29634342/unable-to-use-the-setup-twitter-oauth-function-in-r-to-work-with-twitter
twitteR:::setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)
