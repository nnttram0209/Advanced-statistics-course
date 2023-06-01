library(readxl)
library(tidyr)

#a
d <- read_xlsx("D:/MASTER/Advanced statistics/Advanced-statistics-course/Sugars.xlsx")
d <- d %>% pivot_longer(cols=1:4,  names_to = "sugar.type",
                        values_to = "pea.len")

#b
library(tidyverse)
ggplot(d, aes(x = sugar.type, y = pea.len)) +
     geom_boxplot()
## Conclusion: The pea length might 
## likely to differ among the sugar type subgroups

#c
anova.m <- lm(pea.len ~ sugar.type, data = d)
summary(anova.m)
## p-value is 0.06803 > 0.05 --> Fail to reject H0
## Difference in peas length among 4 sugar types are not statistically significant

## Check if residuals have equal variance in all groups
bartlett.test(pea.len ~ sugar.type, data = d)
## p-value is 0.4786 > 0.05 --> Fail to reject H0 
## --> Conclusion: residuals have equal variance

## Check if residuals are normally distributed within each group
plot(anova.m, 1)
## The plot shows that there is "wedge"
## --> Conclusion: The residuals are not normally distributed

#d
install.packages("emmeans")
library(emmeans)
emm <- emmeans(anova.m, ~ sugar.type)
CI89 <- confint(emm, level = 0.89)
CI89
ggplot(CI89, aes(sugar.type, emmean)) +
     geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL))

#e
K <- contrast(emm, adjust = "sidak",
              list(c1 = c(1,-1, 0, 0),
                   c2 = c(0,-1, 1, 0),
                   c3 = c(0,-1, 0, 1)))
K

#f
set.seed(1)
x <- rbinom(n = 40, size = 1, p = 0.5)
d$sugar.level <- x

anova.m1 <- lm(pea.len ~ sugar.level, data = d)
summary(anova.m1)
## p-value is 0.371 > 0.05 --> Fail to reject H0
## Difference in peas length among 4 sugar types are not statistically significant

## Check if residuals have equal variance in all groups
bartlett.test(pea.len ~ sugar.level, data = d)
## p-value is 0.3061 > 0.05 --> Fail to reject H0 
## --> Conclusion: residuals have equal variance

## Check if residuals are normally distributed within each group
plot(anova.m1, 1)
## The plot shows that there is no suspicious pattern
## --> Conclusion: The residuals are normally distributed
