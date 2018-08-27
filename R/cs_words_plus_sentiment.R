library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)

cs_data <- read_csv("../data/cs_ticket_2018_text.csv")
head(cs_data)
tail(cs_data)

cs_data_filtered <- cs_data %>%
  filter(`Resolution time` > 1, 
         !is.na(`Resolution time`)) %>%
  filter(Via == "Chat") %>%
  mutate(resolutiontime=`Resolution time`) 

cs_data_filtered %>% unnest_tokens(word, Subject)
cs_data_filtered

get_sentiments("bing")


count_sentiment_df <- cs_data_filtered %>% 
  mutate(subject_words=str_count(Subject, "\\S+")) %>%
  unnest_tokens(word, Subject) %>% 
  left_join(get_sentiments("bing")) %>% 
  mutate(int_sentiment = case_when(sentiment == "negative" ~ -1, sentiment == "positive" ~ 1)) %>%
  group_by(Id) %>%
  summarise(subject_words = first(subject_words),
            total_score = sum(abs(int_sentiment), na.rm = TRUE),
            signed_score = sum(int_sentiment, na.rm = TRUE),
            has_negative_sentiment = case_when(sum(int_sentiment, na.rm = TRUE) < 0 ~ 1, TRUE ~ 0),
            signed_sentiment = case_when(sum(int_sentiment, na.rm = TRUE) > 0 ~ 1, 
                                         sum(int_sentiment, na.rm = TRUE) < 0 ~ -1,
                                         TRUE ~ 0),
            resolutiontime = first(resolutiontime))




reg <- lm(resolutiontime ~ has_negative_sentiment, data=count_sentiment_df)
plot(count_sentiment_df$has_negative_sentiment, count_sentiment_df$resolutiontime)
abline(reg)
summary(reg)

reg <- lm(resolutiontime ~ subject_words, data=count_sentiment_df)
plot(count_sentiment_df$subject_words, count_sentiment_df$resolutiontime)
abline(reg)
summary(reg)


#but maybe the tickets with less words have more negative sentiment
reg <- lm(resolutiontime ~ subject_words + has_negative_sentiment, data=count_sentiment_df)
plot(count_sentiment_df$subject_words, count_sentiment_df$resolutiontime)
abline(reg)
summary(reg)
