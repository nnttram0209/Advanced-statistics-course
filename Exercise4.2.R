airquality
head(airquality)

df <- airquality[,-c(5,6)]
df <- df[complete.cases(df), ] 
df$logOzone <- log(df$Ozone) 
df <- df[,-1]
df

#standardize all predictors
library(dplyr)
df.z <- df
df.z <- mutate(df.z, Solar.R.z = (Solar.R-mean(Solar.R))/sd(Solar.R))
df.z <- mutate(df.z, Wind.z = (Wind-mean(Wind))/sd(Wind))
df.z <- mutate(df.z, Temp.z = (Temp-mean(Temp))/sd(Temp))
df.z <- df.z[,-c(1,2,3)]
df.z

m.z <- lm(logOzone ~ Solar.R.z + Wind.z + Temp.z + Solar.R.z*Wind.z + Solar.R.z*Temp.z + Wind.z*Temp.z,
        data = df.z)
# equivalent to
## m.z <- lm(logOzone ~ (Solar.R.z + Wind.z + Temp.z)^2, data = df.z) 
summary(m.z)
#all predictors are significant, and no interactions found
#just perform model selectionm for the original 3 predictors


#backward elimination and bootstrapping
library(rms)
dd <- datadist(df)
options(datadist="dd")
m2 <- ols(logOzone ~.,data=df, x=T,y=T)
m2
m2bw <- fastbw(m2, rule = "aic")
m2bw
m2bwval <- validate(m2, B=1000, bw=T)
print(m2bwval, B=10)

#bestsubsets
install.packages("MuMIn")
library(MuMIn)
m3 <- lm(logOzone ~., data = df)
options(na.action = "na.fail")
dredge(m3)

#lasso
install.packages("glmnet")
library(dplyr)
library(glmnet)
y <- df %>% select(logOzone) %>% scale() %>% as.matrix()
X <- df %>% select(-logOzone) %>% as.matrix()
m_lasso_cv <- cv.glmnet(X,y)
lambda_1se <- m_lasso_cv$lambda.1se
m4_lasso <- glmnet(X, y, lambda = lambda_1se)
coef(m4_lasso)


# --> All three methods show that the best model is the model with 3 original predictors, with no interactions