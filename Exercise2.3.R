#a
d2 <- read_xlsx("D:/MASTER/Advanced statistics/Advanced-statistics-course/Lungworms.xlsx")
str(d2)
d2$treatment <- as.factor(d2$treatment)

#b
ggplot(d2, aes(x = treatment, y = parasites)) +
     geom_boxplot()
## --> Conclusion: the number of parameters might differ among the treatment type

#c
anova.m2 <- lm(parasites ~ treatment, data = d2)
summary(anova.m2)
## p-value is < 0.05 --> Reject H0
## Difference in parasites counts among 4 subgroups are not statistically significant

## Check if residuals have equal variance in all groups
bartlett.test(parasites ~ treatment, data = d2)
## p-value is 0.02312 < 0.05 --> Reject H0 
## --> Conclusion: residuals do not have equal variance


## Check if residuals are normally distributed within each group
plot(anova.m2, 1)
## The plot shows that there is "wedge"
## --> Conclusion: The residuals are not normally distributed

#d
d2$parasites.new <- sqrt(d2$parasites + 0.5)
anova.m2 <- lm(parasites.new ~ treatment, data = d2)
summary(anova.m2)
## p-value is < 0.05 --> Reject H0
## Difference in parasites counts among 4 subgroups are not statistically significant

## Check if residuals have equal variance in all groups
bartlett.test(parasites.new ~ treatment, data = d2)
## p-value is 0.5087 > 0.05 --> Fail to reject H0 
## --> Conclusion: residuals do not have equal variance


## Check if residuals are normally distributed within each group
plot(anova.m2, 1)
## The plot shows that there is "wedge"
## --> Conclusion: The residuals are not normally distributed

emm2 <- emmeans(anova.m2, ~treatment)
CI95 <- confint(emm2)
pairs(emm2)

ggplot(CI95, aes(treatment, emmean)) +
     geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL)) +
     annotate("text", x=1,y=2.7,label="A", size=5, fontface="bold")
