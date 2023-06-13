library(readxl)
library(tidyr)
library(dplyr)
install.packages("survival")
library(survival)
library(ggplot2)
install.packages("survminer")
library(survminer)

df <- read_xlsx("Halibut.xlsx")
df <- df[,-1]
str(df)
df$Towd <- as.factor(df$Towd)


## Perform survival analysis for all data
SO1 <- with(df, Surv(Time, Death))
m1 <- survfit(SO1 ~1)
summary(m1)
## Plotting KM for all data
plot(m1)

## Perform survival analysis by Towd groups
m2 <- survfit(SO1 ~ Towd, data=df)
p <- ggsurvplot(m2,
                risk.table = T, 
                conf.int = T, 
                data = df)
p

## Compare the difference in H(t) between 2 Tow types
survdiff(SO1 ~ Towd, data = df) #log-rank test
## 2 curves significantly different

## Try using Cox hazard proportional model
m3 <- coxph(Surv(Time, Death) ~ Towd+Deldepth+Length+Handtime+Logcat,
            data = df)
summary(m3)

## Simplifying the model
m0.1 <- coxph(Surv(Time, Death)~1,data=df)
m0.2 <- coxph(Surv(Time, Death)~Towd,data=df)
m0.3 <- coxph(Surv(Time, Death)~Towd+Deldepth,data=df)
m0.4 <- coxph(Surv(Time, Death)~Towd+Deldepth+Length,data=df)
m0.5 <- coxph(Surv(Time, Death)~Towd+Deldepth+Length+Handtime,data=df)
m0.6 <- coxph(Surv(Time, Death)~Towd+Deldepth+Length+Handtime+Logcat,data=df)
##1. Step by step with anova
anova(m0.1,m0.2)$`P`[2]
anova(m0.2,m0.3)$`P`[2]
anova(m0.3,m0.4)$`P`[2]
anova(m0.4,m0.5)$`P`[2]
anova(m0.5,m0.6)$`P`[2]
##2. With Step() AIC
m4 <- step(m3)
summary(m4)

## Both approaches confirmed that the addition of Deldepth variable has no effect
## --> The best model should only include Towd, Length, Handtime, Logcat




