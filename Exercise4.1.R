library(readxl)
install.packages("GGally")
library(GGally)
df <- read_xlsx("Urban_foxes.xlsx")
str(df)

#a
ggpairs(df, columns = c(3,1,2,4))
##--> Food and Area seem to be positive predictors of weight 
## while it is not the case for GroupSize

#b
## Food as predictor
m1 <- lm(Weight ~ Food, data = df)
summary(m1)
## GroupSize as predictor
m2 <- lm(Weight ~ GroupSize, data = df)
summary(m2)
## Food and GroupSize as predictors together
m3 <- lm(Weight ~ Food + GroupSize, data = df)
summary(m3)
## Coef of Food and GroupSize are not significant when consider separately
## but both are significant when considered altogether
## Explaination: GroupSize might depend on Food (i.e. Food increases will enlarge GroupSize)
layout(rbind(1:2), cbind(3,4))
plot(m3)

#c
install.packages("rms")
library(rms)
dd <- datadist(df)
options(datatist = "dd")
m3.full <- ols(Weight ~ ., data = df, x = T, y = T )
##c1.backward elimination
m3.bw <- fastbw(m3.full, rule="aic")
m3.bw
##c1. validate with bootstrap
## BW said no predictors are eliminated, so the just validate on the full model
m3.bw.val <- validate(m3.full, B=500, bw=T)
print(m3.bw.val, B=10)

##c2. validate with CV
install.packages("caret")
install.packages("vctrs")
library(ggplot2)
library(lattice)
library(caret)
ctrl <- trainControl(method = "LOOCV")
m3.full.t <- train(form = formula(m3.full), 
                   data = df,
                   method = "lm",
                   trControl = ctrl)
m3.full.t



