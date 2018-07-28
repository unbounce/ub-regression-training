##This file regresses on NTS

library(dplyr)

NTS <- read.csv(file='../data/NTS_weekly.csv')

NTS_1 <- NTS %>% mutate(NTS_N = lead(NTS))
regression_1 <- lm(NTS_N ~ NTS, data=NTS_1)
plot(NTS_1[,'NTS'],NTS_1[,'NTS_N'])
abline(regression_1)
summary(regression_1)
plot(regression_1)

NTS_7 <- NTS %>% mutate(NTS_N = lead(NTS,7))
regression_7 <- lm(NTS_N ~ NTS, data=NTS_7)
plot(NTS_1[,'NTS'],NTS_1[,'NTS_N'])
abline(regression_7)
summary(regression_7)



get_slope <- function(leadn){
  NTS_c <- NTS %>% mutate(NTS_N = lead(NTS,leadn))
  NTS_c[is.na(NTS_c)]
  regression <- lm(NTS_N ~ NTS, data=NTS_c)
  return(regression$coefficients[2])
}



slope_vs_lag <- do.call("rbind", lapply(1:11, FUN=get_slope))
plot(slope_vs_lag, main = "Slope for predicting NTS at given time lags", 
      xlab = "Time lag (in Weeks)", 
      ylab = "Slope of best fit line")




#can check to see that correlation gives similar results
get_corr <- function(leadn){
  NTS_c <- NTS %>% mutate(NTS_N = lead(NTS,leadn))
  correl <- cor(NTS_c$NTS_N, NTS_c$NTS, use="complete.obs")
  return(correl)
}
cor_vs_lag <- do.call("rbind", lapply(1:11, FUN=get_corr))
plot(cor_vs_lag, main = "Correlation for NTS at given time lags", 
      xlab = "Time lag (in Weeks)", 
      ylab = "Slope of best fit line")
