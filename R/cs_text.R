library(tidyverse)
library(tidytext)

cs_data <- read_csv("../data/cs_ticket_2018_text.csv")
head(cs_data)
tail(cs_data)

cs_data_filtered <- cs_data %>%
  filter(#`Resolution time` != 0, 
         !is.na(`Resolution time`),
         Via != 'Phone call outbound',
         Via != 'Closed Ticket')

cs_data_filtered %>% unnest_tokens(word, Subject)
cs_data_filtered

get_sentiments("bing")


cs_data_filtered %>% 
  unnest_tokens(word, Subject) %>% 
  left_join(get_sentiments("bing")) %>% 
  mutate(int_sentiment = case_when(sentiment == "negative" ~ -1, sentiment == "positive" ~ 1)) %>%
  group_by(Id) %>%
  summarise(total_score = sum(int_sentiment, na.rm = TRUE)) %>%
  filter(total_score != 0) 


sentiment_afinn <- cs_data_filtered %>% 
  unnest_tokens(word, Subject) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Id) %>%
  summarise(total_score = sum(score, na.rm = TRUE), word_count = n(), resolutiontime = first(`Resolution time`)) %>%
  mutate(sentiment_per_word = total_score / word_count) 
#%>% filter(total_score != 0)

regression <- lm(resolutiontime ~ total_score, data=sentiment_afinn)
plot(sentiment_afinn$total_score, sentiment_afinn$resolutiontime)
abline(regression)
summary(regression)


regression <- lm(resolutiontime ~ sentiment_per_word, data=sentiment_afinn)
plot(sentiment_afinn$sentiment_per_word, sentiment_afinn$resolutiontime)
abline(regression)
summary(regression)

#bucket and average
sentiment_bucket <- sentiment_afinn %>%
  mutate(spw_bin = ntile(sentiment_per_word,200)) %>%
  group_by(spw_bin) %>%
  summarise(avg_resolutiontime = median(resolutiontime), avg_spw = mean(sentiment_per_word))

regression <- lm(avg_resolutiontime ~ avg_spw, data=sentiment_bucket)
plot(sentiment_bucket$avg_spw, sentiment_bucket$avg_resolutiontime)
abline(regression)
summary(regression)

regression <- lm(avg_resolutiontime ~ spw_bin, data=sentiment_bucket)
plot(sentiment_bucket$spw_bin, sentiment_bucket$avg_resolutiontime)
abline(regression)
summary(regression)


##Maybe it's the header that's important, let's look at sentiment in first few words
sentiment_afinn_header <- cs_data_filtered %>% 
  unnest_tokens(word, Subject) %>% 
  group_by(Id) %>%
  do(head(.,1)) %>%
  ungroup() %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Id) %>%
  summarise(total_score = sum(score, na.rm = TRUE), word_count = n(), resolutiontime = first(`Resolution time`)) %>%
  mutate(sentiment_per_word = total_score / word_count)

regression <- lm(resolutiontime ~ total_score, data=sentiment_afinn_header)
plot(sentiment_afinn_header$total_score, sentiment_afinn_header$resolutiontime)
abline(regression)
summary(regression)

cs_data_filtered %>% 
  unnest_tokens(word, Subject) %>%
  group_by(Id) %>%
  mutate(count = n())

make_regression <- function(sentiment_df, groupedfield, headersize=1000) {
  print(unique(sentiment_df[,groupedfield]))
  temp_df <- sentiment_df %>% 
    unnest_tokens(word, Subject) %>% 
    group_by(Id) %>%
    mutate(full_word_count=n()) %>%
    do(head(.,headersize)) %>%
    ungroup() %>%
    inner_join(get_sentiments("afinn")) %>% 
    group_by(Id) %>%
    summarise(total_score = sum(score, na.rm = TRUE), 
              word_count = n(), 
              full_word_count = first(full_word_count),
              resolutiontime = first(`Resolution time`)) %>%
    mutate(sentiment_per_word = total_score / full_word_count)
  
  regression <- lm(resolutiontime ~ sentiment_per_word, data=temp_df)
  plot(temp_df$sentiment_per_word, temp_df$resolutiontime)
  title(main=unique(sentiment_df[,groupedfield]))
  abline(regression)
  return(regression)
}

cs_data_via <- cs_data_filtered %>% 
  filter(`Resolution time` < 10) %>%
  group_by(`Via`) %>%
  do(model=make_regression(., groupedfield = 'Via', headersize = 3))

cs_data_type <- cs_data_filtered %>% 
  group_by(`Ticket type`) %>%
  do(model=make_regression(., groupedfield = 'Ticket type'))

lapply(cs_data_via$model, FUN=summary)

cs_data_filtered %>% group_by(Via) %>% count() %>% arrange(desc(n))


make_grouped_regression <- function(sentiment_df, groupedfield, headersize=1000) {
  print(unique(sentiment_df[,groupedfield]))
  temp_df <- sentiment_df %>% 
    unnest_tokens(word, Subject) %>% 
    group_by(Id) %>%
    mutate(full_word_count=n()) %>%
    do(head(.,headersize)) %>%
    ungroup() %>%
    inner_join(get_sentiments("afinn")) %>% 
    group_by(Id) %>%
    summarise(total_score = sum(score, na.rm = TRUE), 
              word_count = n(), 
              full_word_count = first(full_word_count),
              resolutiontime = first(`Resolution time`)) %>%
    mutate(sentiment_per_word = total_score / full_word_count) %>%
    mutate(bin = ntile(sentiment_per_word,n=100)) %>%
    group_by(bin) %>%
    summarise(avg_resolutiontime = mean(resolutiontime),
              median_resolutiontime = median(resolutiontime),
              avg_sentiment_per_word = mean(sentiment_per_word),
              median_sentiment_per_word = median(sentiment_per_word))
  
  regression <- lm(median_resolutiontime ~ median_sentiment_per_word, data=temp_df)
  plot(temp_df$median_sentiment_per_word, temp_df$median_resolutiontime)
  title(main=unique(sentiment_df[,groupedfield]))
  abline(regression)
  return(regression)
}


cs_data_via <- cs_data_filtered %>% 
  #filter(`Resolution time` < 10) %>%
  group_by(`Via`) %>%
  do(model=make_grouped_regression(., groupedfield = 'Via', headersize = 1000))

cs_data_type <- cs_data_filtered %>% 
  group_by(`Ticket type`) %>%
  do(model=make_grouped_regression(., groupedfield = 'Ticket type'))

lapply(cs_data_via$model, FUN=summary)
# average values
# sentiment_afinn_header <- cs_data_filtered %>% 
#   unnest_tokens(word, Subject) %>% 
#   group_by(Id) %>%
#   do(head(.,3)) %>%
#   ungroup() %>%
#   inner_join(get_sentiments("afinn")) %>% 
#   group_by(Id) %>%
#   summarise(total_score = sum(score, na.rm = TRUE), word_count = n(), 
#             resolutiontime = first(`Resolution time`)) %>%
#   mutate(sentiment_per_word = total_score / word_count) %>%
#   group_by(total_score) %>%
#   summarise(avg_resolutiontime = mean(resolutiontime), 
#             median_resolutiontime = median(resolutiontime))
# 
# 
# regression <- lm(median_resolutiontime ~ total_score, data=sentiment_afinn_header)
# plot(sentiment_afinn_header$total_score, sentiment_afinn_header$median_resolutiontime)
# abline(regression)
# summary(regression)