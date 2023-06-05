df <- read_xlsx("Gause.xlsx")
str(df)
df

#a
g <- ggplot(df, aes(day, popsize)) + geom_point()
##K is estimated to be 600
K1 <- 600

#b
m1 <- lm(log(popsize[1:6]) ~ day[1:6], data = df)
summary(m1)
r1 <- coef(m1)[2]
## r is 0.66

#c
N0 <- df$popsize[1]
m2 <- nls(popsize~K/(1+(K/N0-1)*exp(-r*day)),
          start = list(r=r1,K=K1),
          data = df)
coef(m2)
r2 <- coef(m2)[2]
K2 <- coef(m2)[1]
## r is 0.709 and K is 590.447

#d
df.pred1 <- data.frame(day=seq(0, 16, length=100))
df.pred1$popsize <- K1/(1+(K1/N0-1)*exp(-r1*df.pred1$day))

df.pred2 <- data.frame(day=seq(0,16,length=100))
df.pred2$popsize <- predict(m2,newdata=df.pred2)

g2 <- g +
     geom_line(data=df.pred1, aes(color = "m1")) +
     geom_line(data=df.pred2, aes(color = "m2")) +
     scale_color_manual(name="model type", values=c("m1"="green","m2"="blue"))
g2

##m2 fits the best

#e
library(mgcv)
m3 <- gam(popsize ~ s(day), data=df)
summary(m3)
g3 <- g +
     geom_line(data=df.pred1, aes(color = "m1")) +
     geom_line(data=df.pred2, aes(color = "m2")) +
     geom_line(aes(y=fitted(m3), color = "m3")) +
     scale_color_manual(name="model type", values=c("m1"="green","m2"="blue", "m3"="red"))

g3

                         