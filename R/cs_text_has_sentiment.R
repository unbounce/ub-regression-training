library(tidyverse)
library(tidytext)
library(ggplot2)

cs_data <- read_csv("../data/cs_ticket_2018_text.csv")
head(cs_data)


cs_data_filtered <- cs_data %>%
  filter(`Resolution time` != 0, 
    !is.na(`Resolution time`)) %>%
  mutate(resolutiontime=`Resolution time`)



get_sentiments("bing")


has_sent_df <- cs_data_filtered %>% 
  unnest_tokens(word, Subject) %>% 
  left_join(get_sentiments("bing")) %>% 
  mutate(int_sentiment = case_when(sentiment == "negative" ~ -1, sentiment == "positive" ~ 1)) %>%
  group_by(Id) %>%
  summarise(total_score = sum(abs(int_sentiment), na.rm = TRUE),
            signed_score = sum(int_sentiment, na.rm = TRUE),
            has_negative_sentiment = case_when(sum(int_sentiment, na.rm = TRUE) < 0 ~ 1, TRUE ~ 0),
            signed_sentiment = case_when(sum(int_sentiment, na.rm = TRUE) > 0 ~ 1, 
                                         sum(int_sentiment, na.rm = TRUE) < 0 ~ -1,
                                         TRUE ~ 0),
            resolutiontime = first(resolutiontime))
head(has_sent_df)


# reg <- lm(resolutiontime ~ total_score, data=has_sent_df)
# plot(has_sent_df$total_score, has_sent_df$resolutiontime)
# abline(reg)
# summary(reg)

reg <- lm(resolutiontime ~ has_negative_sentiment, data=has_sent_df)
plot(has_sent_df$has_negative_sentiment, has_sent_df$resolutiontime)
abline(reg)
summary(reg)
plot(reg)
hist(residuals(reg),breaks=100,col="light grey",xlim = c(-200,400))

cor.test(has_sent_df$has_negative_sentiment, has_sent_df$resolutiontime)

reg <- lm(resolutiontime ~ signed_sentiment, data=has_sent_df)
plot(has_sent_df$signed_sentiment, has_sent_df$resolutiontime)
abline(reg)
summary(reg)




#for the curiosity sake
sum_df <- cs_data_filtered %>% 
  unnest_tokens(word, Subject) %>% 
  left_join(get_sentiments("bing")) %>% 
  mutate(int_sentiment = case_when(sentiment == "negative" ~ -1, sentiment == "positive" ~ 1)) %>%
  group_by(Id, Via, `Ticket type`) %>%
  summarise(total_score = sum(abs(int_sentiment), na.rm = TRUE),
            signed_score = sum(int_sentiment, na.rm = TRUE),
            has_negative_sentiment = case_when(sum(int_sentiment, na.rm = TRUE) < 0 ~ 1, TRUE ~ 0),
            signed_sentiment = case_when(sum(int_sentiment, na.rm = TRUE) > 0 ~ 1, 
                                         sum(int_sentiment, na.rm = TRUE) < 0 ~ -1,
                                         TRUE ~ 0),
            resolutiontime = first(resolutiontime)) %>%
  group_by(Via, 
           #`Ticket type`, 
           signed_sentiment) %>%
  summarise(meanres = mean(resolutiontime),
            medres = median(resolutiontime),
            count = n())

#how many in each category
top4_cs_df <- sum_df %>% 
  group_by(Via) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count)) %>%
  top_n(4) %>%
  select(Via)

sum_df_filter <- sum_df %>% filter(Via %in% unlist(top4_cs_df))

plot_df <- sum_df_filter %>% 
  select(Via, medres, signed_sentiment)
#%>% 
  #spread(key = signed_sentiment,value=medres)


ggplot(plot_df, aes(x=signed_sentiment, y=medres, fill=Via)) +
  geom_bar( position="dodge", stat="identity") +
  facet_wrap(~Via)





