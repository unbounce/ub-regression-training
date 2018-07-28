library(dplyr)
library(stringr)

cs <- read.csv(file='../data/account_cs_tickets.csv', stringsAsFactors = F)

names(cs)

cs_df <- cs %>% 
  select(Subject, Resolution.time) %>%
  mutate(Restime = as.numeric(Resolution.time)) %>%
  filter(Restime > 1) %>%
  mutate(subject_words=str_count(Subject, "\\S+"), 
         subject_length=str_length(Subject), 
         subject_digits=str_count(Subject, "[0-9]"),
         subject_char=str_count(Subject, "[a-zA-Z]")) %>%
  mutate(subject_special=subject_length-(subject_words+subject_char) +1) 


plot(cs_df[,"subject_words"],cs_df[,"Restime"])

reg <- lm(Restime ~ subject_words, data=cs_df)
abline(reg)
summary(reg)
plot(reg)
hist(residuals(reg),breaks=100,col="light grey")


plot(cs_df[,"subject_length"],cs_df[,"Restime"])
reg <- lm(Restime ~ subject_length, data=cs_df)
abline(reg)
summary(reg)
plot(reg)
hist(residuals(reg),breaks=100,col="light grey")


# cs_per_word <- cs_df %>%
#   group_by(subject_words) %>%
#   summarize(avg_restime = mean(Restime, na.rm = TRUE)) %>% filter(!is.na(avg_restime))
# 
# cs_per_word
# 
# plot(cs_per_word)
# 
# reg <- lm(avg_restime ~ subject_words, data=cs_per_word)
# abline(reg)
# summary(reg)
# plot(reg)


cs_per_word_floor <- cs_df %>%
  mutate(floor_subject_words = floor(subject_words/5)) %>%
  group_by(floor_subject_words) %>%
  summarize(avg_restime = mean(Restime, na.rm = TRUE)) 

plot(cs_per_word_floor)

reg <- lm(avg_restime ~ floor_subject_words, data=cs_per_word_floor)
abline(reg)
summary(reg)
plot(reg)


remove_first <- cs_per_word_floor[-1,]

plot(remove_first)
reg <- lm(avg_restime ~ floor_subject_words, data=remove_first)
abline(reg)
summary(reg)
plot(reg)

