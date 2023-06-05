install.packages("readxl")
library(readxl)
df1 <- read_xlsx("Heartbeat.xlsx") 
str(df1)

#a
library(tidyverse)
ggplot(df1, aes(Age, Heartrate)) +
     geom_point(color = "blue") +
     geom_abline(intercept = 220, slope = -1, color = "red", size = 1) +
     stat_smooth(method = lm, se = T)

#b
m1 <- lm(Heartrate ~ Age, data = df1)
summary(m1)
## H0: B0=220; B1=-1
confint(m1, level = 0.95) #--> intercept and slope does not contain null values --> Reject H0
##check if intercept = 220
2*pt(abs(210.29775-220)/2.21172,df=52,lower.tail=F)
##check if slope = -1
2*pt(abs(1-0.75621)/0.04726,df=52,lower.tail=F)
#--> p-value of both cases <0.05 --> Reject H0

#c
plot(m1, 1) #--> equal variance assumption is met
plot(m1, 2) #--> residual is normally distributed assumption is met
ggplot(df1, aes(Age, Heartrate)) +
     geom_point(color = "blue") #-->y and x has linear relationship assumption is met

#d
m2 <- lm(Heartrate ~ Age + I(Age^2), data = df1)
summary(m2)
##--> p-value of age square > 0.05 --> Accept H0 --> No quadratic effect

#e
##The rule of thumb is not correct




