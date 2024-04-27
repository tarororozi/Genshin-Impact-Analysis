# This purpose of this document is to prepare the Genshin Impact data for analysis 
# Created for APAN 5205 Team 6 Proposal 
# 02/27/2024

#########################Set up the environment ###############################
#Clean up the environment 
rm(list = ls())

#Set up the environment
library(tidyverse) 
library(skimr)
library(stringr)
library(tidytext)
library(magrittr)
library(ggthemes)
library(wordcloud)
library(textdata)
library(lexicon)

##########################Import data #########################################
# Data Source: 
# The data comes from two sources. The first one is manually imputed 
# from paimon.moe, an in-game assistant tool. 
# The second one is from Twitter using web_scraping technique in Python. We have 
# collected all the Twitter comments using the hash tag of #boycottgenshin. 

#1: paimon.moe
#2: #boycottgenshin from Twitter, see attached python file


# Import the dataset 
genshin_reviews <- read.csv("G:/Columbia University Data Projects/APAN 5205/Data/#boycottgenshin.csv")

View(genshin_reviews)

##########################Data Exploration#####################################
# number of rows and columns 
print(list(row = nrow(genshin_reviews), column = ncol(genshin_reviews)))

# check the data 
skim(genshin_reviews)

# Summary of the data
summary(genshin_reviews)

# Check distribution of numeric predictors 
genshin_reviews %>% 
  select_if(is.numeric)%>%
  pivot_longer(cols = 1:4,names_to = 'numeric_predictor', values_to = 'values')%>%
  ggplot(aes(x = values))+
  geom_histogram(binwidth = 30)+
  facet_wrap(numeric_predictor~., scales = 'free')+
  xlim(c(0, 500)) +
  ylim(c(0, 500)) +
  theme_bw()

# Check for missing values
# Show all missing data in columns
missing_data_summary <- data.frame(
  NumMissing = sapply(genshin_reviews, function(x) sum(is.na(x)))
)
print(missing_data_summary)

# Analysis on the reviews
## Sample Review, review 3000
genshin_reviews$TWEET[3000]

## characters 
summary(nchar(genshin_reviews$TWEET))

## words 
summary(str_count(string = genshin_reviews$TWEET,pattern = '\\S+'))

## sentences
summary(str_count(string = genshin_reviews$TWEET,
                  pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))

## shortest reviews 
genshin_reviews$TWEET[which.min(str_count(string = genshin_reviews$TWEET,
                                          pattern = '\\S+'))]

## longest reviews 
genshin_reviews$TWEET[which.max(str_count(string = genshin_reviews$TWEET,
                                          pattern = '\\S+'))]

## common words, excluding stop words
genshin_reviews%>%
  unnest_tokens(input = TWEET, output = word)%>%
  select(word)%>%
  filter(!str_detect(word, "^http|^https|t\\.co")) %>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

################################Tokenize words################################
# One word 
# Create an id column 
genshin_reviews <- genshin_reviews %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

#tokenize based on word
genshin_reviews %>%
  select(id,TWEET)%>%
  group_by(id)%>%
  unnest_tokens(input = TWEET, output = word)%>%
  ungroup()%>%
  group_by(id)%>%
  summarize(count = n())

#Exclude dirty words
#profanity_alvarez
#profanity_arr_bad
#profanity_banned
#profanity_racist
#profanity_zac_anger

genshin_reviews <-genshin_reviews %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = TWEET)%>%
  ungroup()%>%
  select(id, word)%>%
  anti_join(data.frame(word = c(profanity_banned, profanity_racist, profanity_alvarez,
                                profanity_arr_bad, profanity_zac_anger)), 
            by = c('word'='word'))

#Change the column name to TWEET
colnames(genshin_reviews)[2] = "TWEET"

################################Sentiment Analysis###########################

# Part 1: Binary Sentiment 
# Binary Token
genshin_reviews_bin <- genshin_reviews %>% 
  select(id,TWEET) %>% 
  group_by(id)%>%
  unnest_tokens(input = TWEET, output = word)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)
  
# Visualization 1
genshin_reviews_bin %>% 
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+
  geom_col()+
  geom_text(aes(label = n), hjust = 0) +
  theme_economist()+
  guides(fill=F)+
  coord_flip()

# Visualization 2 
total_sentiments <- 7005 + 4792

genshin_reviews_bin %>%
  count() %>%
  mutate(percentage = n / total_sentiments * 100) %>%
  ggplot(aes(x = percentage, y = sentiment, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.5) +
  theme_wsj() +
  guides(fill = FALSE) +
  labs(x = "Percentage", 
       y = "Sentiment",
       title = "Binary Sentiment Results") +
  scale_x_continuous(limits = c(0, 100))

# Word cloud 
word_cloud <- genshin_reviews_bin %>% 
  count(sentiment,word,sort=T)%>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)%>%
  data.frame()

rownames(word_cloud) = word_cloud[,'word']

word_cloud <- word_cloud[,c('positive','negative')]

comparison.cloud(term.matrix = word_cloud,scale = c(2,0.5),max.words = 200, rot.per=0)

# Part 2: nrc emotion 

## install and explore the nrc package 
nrc = get_sentiments('nrc')

nrc %>% 
  group_by(sentiment) %>% 
  count()

##Perform sentiment analysis 
genshin_reviews_nrc_emotion <- genshin_reviews %>% 
  select(id,TWEET) %>% 
  group_by(id)%>%
  unnest_tokens(output = word, input = TWEET)%>%
  inner_join(nrc, relationship = "many-to-many")%>%
  group_by(sentiment)%>%
  count() %>% 
  ungroup()

#show the result in descending order
genshin_reviews_nrc_emotion %>% 
  arrange(desc(n))

#visualize the result 
total_nrc <- genshin_reviews_nrc_emotion %>%
  summarise(total = sum(n))

genshin_reviews_nrc_emotion %>%
  mutate(percentage = n / total_nrc$total * 100) %>%
  ggplot(aes(x = reorder(sentiment, percentage), y = percentage, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.2) +
  guides(fill = FALSE) +
  coord_flip() +
  theme_wsj() +
  labs(x = "Sentiment", 
       y = "Percentage of Total NRC",
       title = "NRC Emotion Sentiment Results") +
  scale_y_continuous(limits = c(0, 30))


# Part 3: afinn understand the polarity 
afinn = get_sentiments('afinn')

genshin_reviews_afinn <- genshin_reviews %>% 
  select(id,TWEET)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=TWEET)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()

#show the summarized results 
genshin_reviews_afinn %>% 
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))


# Visualization
genshin_reviews_afinn %>% 
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  labs(title = "AFINN Sentiment Results") +
  theme_wsj()


#Part 4: Jockers (consider about the context and POS) 
genshin_reviews_Jockers <- genshin_reviews%>%
  select(id,TWEET) %>% 
  group_by(id)%>%
  unnest_tokens(output=word,input=TWEET)%>%
  inner_join(key_sentiment_jockers)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()

#show the summarized results 
genshin_reviews_Jockers %>% 
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))

#Visualization 
genshin_reviews_Jockers %>% 
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.02)+
  scale_x_continuous(breaks=seq(-1,1,0.2))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  labs(title = "Jockers Sentiment Results") +
  theme_wsj()
  











