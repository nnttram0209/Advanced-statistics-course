install.packages("rms")
install.packages("crosstable")
install.packages("PMCMRplus")
install.packages("descr")
install.packages("survminer")
library(survminer)
library(survival)
library(ggplot2)
library(cowplot)
library(MASS)
library(descr)
library(dplyr)
library(rms)
library(crosstable)
library(stats)
library(PMCMRplus)

#--------------
# Load the data
dt <- read.csv2("lipid-lowering drug.csv")
str(dt)
dt <- dt[dt$aliptype != "Other",]
dt <- dt %>% mutate(mv = factor(mv),
                  cve = factor(cve),
                  diabetes = factor(diabetes),
                  RA = factor(RA),
                  Asthma_COPD = factor(Asthma_COPD),
                  aliptype = factor(aliptype))

#--------------
# Get the patients' characteristics at baseline (i.e., at index date)
# and perform necessary statistical hypothesis testing to compare the patients' 
# characteristics in different groups of preventive medications
crosstable(dt, 
           c(age, ftime, mv, diabetes, RA, Asthma_COPD), 
           by=aliptype,
           margin = "column",
           percent_digits = 1,
           total="both") %>%
     as_flextable(keep_id=FALSE)

## Check if gender and comorbidities (diabetes, asthma/COPD, RA) was statistically 
## different between 3 groups of preventive medications
## 1. gender
chisq.test(dt$mv, dt$aliptype)

## 2. diabetes
chisq.test(dt$diabetes, dt$aliptype) # With Chi-square test

## 3. Asthma_COPD
chisq.test(dt$Asthma_COPD, dt$aliptype) # With Chi-square test

## 4. RA
chisq.test(dt$RA, dt$aliptype) # With Chi-square test
## A warning appears: Chi-squared approximation may be incorrect
chisq.test(dt$RA, dt$aliptype)$expected
## It is because 2/4 cells have expected counts <5, which is the rule of thumb
## So, Fisher's exact test will be used instead
fisher.test(dt$RA, dt$aliptype)


## Check if age and ftime was statistically different between 3 groups of 
## preventive medications
## 5. ftime
m0.ftime <- lm(ftime~aliptype,data=dt) #With ANOVA
anova(m0.ftime)
par(mfrow = c(1,2)) 
plot(m0.ftime, 1:2)  #Check ANOVA assumptions
## This shows that homogeneity assumption is satisfied; normality assumption is violated.
## Hence, Kruskal-Wallis rank sum test is used instead
kruskalTest(ftime~aliptype,data=dt)

## 6. age
m0.age <- lm(age~aliptype,data=dt) #With ANOVA
anova(m0.age)
par(mfrow = c(1,2)) 
plot(m0.age, 1:2)   #Check ANOVA assumptions  
## This shows that homogeneity assumption is violated; normality assumption is satisfied.
## Hence, Welch’s correction is applied next
oneway.test(age~aliptype,
            data=dt,
            var.equal = F)

#--------------
# Display CVE-free rates between 3 groups of preventive medications
SO <- with(dt, Surv(ftime, as.numeric(cve)))
surv0 <- survfit(SO ~ aliptype, data =dt)
p_surv0 <- ggsurvplot(surv0, 
                      risk.table = T, 
                      conf.int = F,
                      xlab="Follow-up time (in days)",
                      ylab="Cumulative CVE-free rate",
                      data = dt)
p_surv0$plot <- p_surv0$plot + background_grid(major = "xy", minor = "xy")

#--------------
# Regression with Cox proportional hazard (PH) model
## 1. To calculate unadjusted HR
coxph0 <- coxph(Surv(ftime, as.numeric(cve)) ~ aliptype,
                data = dt)
summary(coxph0)
## 2. To calculate adjusted HR
## a. To get the right adjusted HR, model selection must be done to choose the best model
## from which the adjusted HR will be estimated.
## For model selection, I will start with the full model, then try both 
## backward selection with stepAIC() from MASS package and fastbw() function
## from rms package. In both ways, aliptype will be forced to be included in all models

## With stepAIC() 
coxph_full_step <- coxph(Surv(ftime, as.numeric(cve)) 
                         ~ aliptype + age + mv + diabetes + Asthma_COPD  + RA,
                         data = dt)         #This is the full model
coxph_step <- stepAIC(coxph_full_step,
                      scope = list(upper = ~ aliptype + age + mv + diabetes + Asthma_COPD  + RA,
                                   lower = ~ aliptype),
                      direction = "backward",
                      trace = FALSE)
summary(coxph_step)

## With fastbw()
units(dt$ftime) <- "days"
dd <- datadist(dt)
options(datadist = "dd")      
# (These steps are to calculate necessary parameters for the next step)
coxph_full_fbw <- cph(Surv(ftime, as.numeric(cve)) 
                      ~ aliptype + age + mv + diabetes + Asthma_COPD  + RA,
                      x = T,
                      y = T,
                      data = dt,
                      surv = T,
                      time.inc = 1)         #This is the same as coxph_full_step

coxph_fbw <- fastbw(coxph_full_fbw, rule="aic", type="individual", force = 1:1)
print(coxph_fbw)
## Next is bootstrapping validation of backward model selection with fastbw()
## The following step is to find the percentage of times that each variable is kept 
## in the model after 100 times of bootstrapping 
set.seed(0209)
z <- validate(coxph_full_fbw, B=100, bw=T, rule="aic", type="individual", force = 1:1)
-sort(-apply(attr(z,"kept"),2,sum)/100)
## Bootstrapping result presents that RA has a incredibly low percentage of being 
## retained in the mode, as compared to the 5 remaining covariates

## Both stepAIC() and fastbw() shows that RA was the covariate deleted from the 
## full model

## b. The next step is to check PH assumptions for the model which RA
## is not included, based on Schoenfeld residuals
 
coxph1_step_test <- cox.zph(coxph1_step)
coxph1_step_test
par(mfrow = c(2,2))
plot(coxph1_step_test)
ggcoxzph(coxph1_step_test)

coxph2 <- coxph(Surv(ftime, as.numeric(cve)) ~ age + age:ftime + mv + diabetes,
                data = dt)
cox.zph(coxph2)