#Group members: Adeel Arshid,Suguru Iwashiro, Amira Elmakawy


load("~/Desktop/acs2017_ny/acs2017_ny_data.RData")

# income wage of college graduates based on race.

attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
attach(dat_use)

require(stargazer)
require(AER)
summary(dat_use)

                                                                                                                              
#INCOME WAGE OF Asian College Graduates Between The Ages Of 25-55

age_wage <- lm(INCWAGE ~ AGE + educ_college + Asian)
summary(age_wage)
plot(age_wage)
require(stargazer)

        
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, educ_college = 1, Asian = 0)
to_be_predicted2$yhat <- predict(age_wage, newdata = to_be_predicted2)

detach()



