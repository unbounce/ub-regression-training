##This file regresses on NTS

library(dplyr)

NTS <- read.csv(file='../data/NTS.csv')

NTS_1 <- NTS %>% mutate(NTS_N = lead(NTS))
regression_1 <- lm(NTS_N ~ NTS, data=NTS_1)
plot(NTS_1[,'NTS'],NTS_1[,'NTS_N'])
abline(regression_1)
summary(regression_1)

NTS_3 <- NTS %>% mutate(NTS_N = lead(NTS,3))
regression_3 <- lm(NTS_N ~ NTS, data=NTS_3)
plot(NTS_3[,'NTS'],NTS_3[,'NTS_N'])
abline(regression_3)
summary(regression_3)
plot(regression_3)

NTS_4 <- NTS %>% mutate(NTS_N = lead(NTS,4))
regression_4 <- lm(NTS_N ~ NTS, data=NTS_4)
plot(NTS_4[,'NTS'],NTS_4[,'NTS_N'])
abline(regression_4)
summary(regression_4)
plot(regression_4)



get_slope <- function(leadn){
  NTS_c <- NTS %>% mutate(NTS_N = lead(NTS,leadn))
  regression <- lm(NTS_N ~ NTS, data=NTS_c)
  return(regression$coefficients[2])
}


slope_vs_lag <- do.call("rbind", lapply(1:8, FUN=get_slope))
plot(slope_vs_lag, main = "Slope for predicting NTS at given time lags", 
      xlab = "Time lag (in Days)", 
      ylab = "Slope of best fit line")




x = runif(50,0,50)
y = x + rnorm(50,0,5)
plot(y~x)
cor(x,y)