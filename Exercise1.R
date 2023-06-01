#############################################################
#
# Advanced Statistics 2023, day 2
#
# Exercise 1: basic probability theory
#
#############################################################

## Load some useful packages and set ggplot2 theme
## If necessary install first with install.packages()
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

## We are going to calculate some basic descriptive
## statistics such as mean and variance, along the way
## meeting several probability distributions or densities
## that are commonly used in statistical modelling.


## Let's first generate some numbers to play with.
## For example, let's generate the integers from 0 to 10
## and store them in a vector called x:

x <- seq(from = 0, to = 10, by = 1)

## Print the contents of x: type x and press ctrl-enter 
## or cmd-enter on Macs, or click the "run" button at the top
x

## Check out the help for the seq function:
?seq

## This would have worked as well:
x <- seq(0,10,1)
x

## Now calculate the mean of x:
mean(x)

## Should be the same as:
sum(x)/length(x)

## and the variance:
var(x)

## Should be the same as
sum((x-mean(x))^2)/(length(x)-1)

## and the standard deviation:
sd(x)


## Sometimes it makes more sense to use the median
## rather than the mean as a measure of central location:
median(x)

## In this case they are the same! But now replace
## the number 10 by the number 100 (an "outlier"):

x[11] <- 100
x

mean(x)
median(x)

## The median is less sensitive to outliers!


## Another measure of variability is the so-called
## coefficient of variation, often abbreviated as cv.
## It's defined as the standard deviation divided by the mean 
## cv = sd/mean
##
## What do you think might be an advantage of using
## the cv rather than the sd? (Hint: think about units)
## 
## Unfortunately, there is no built-in cv function in R:

x <- seq(0,10,1)
cv(x)

## Instead we can do

sd(x)/mean(x)

## Or we can define our own cv function:

cv <- function(z) sd(z)/mean(z)

## Note how the function appeared in the top right window

## Apply the function to our x:

cv(x)

## Or some other dataset:

y <- seq(0,10,l = 100)
y

cv(y)


###################################################################
####### Generating pseudo-random numbers with R ###################
###################################################################

## R can generate pseudo-random numbers from various 
## probability distributions/densities, which is very
## useful for simulating data and trying out statistical 
## methods!

## Generate 100 draws from Bernoulli distribution
## with parameter p=0.5:

rbernoulli(n = 100, p = 0.5)

## Note that the output is logical: TRUE or FALSE
## instead of 0 or 1. To get numbers instead:

as.numeric(rbernoulli(n = 100, p = 0.5))

## Also note that rbernoulli (from the purrr package) has been "deprecated"!
## That means it won't be available in future versions of purrr

## Instead you can use the binomial distribution, of which
## the Bernoulli distribution is a special case (size = 1).
## In fact, the binomial distribution with parameters 
## size and p is just the distribution of the sum of 
## size Bernoulli distributions with parameter p:

rbinom(n = 100, size = 1, p = 0.5)
as.logical(rbinom(n = 100, size = 1, p = 0.5))

## Much more about the binomial distribution 
## later in the course.

## If you run the rng (random number generator) again,
## you (almost certainly) get a different sequence:

rbernoulli(n = 100, p = 0.5)

## If you want to get the same sequence, use the set.seed
## function, which initializes the rng:

set.seed(1)
rbernoulli(n = 6, p = 0.5)

## and again:

set.seed(666)
rbernoulli(n = 6, p = 0.5)

## again

set.seed(1)
rbernoulli(n = 6, p = 0.5)


## Another well-known discrete distribution is the 
## Poisson distribution. Poisson (random) variables
## can take all non-negative integers (0,1,2,3,...)
## as values. The Poisson distribution is therefore
## often used to model count data, since counts are
## also non-negative integers.
## The Poisson distribution has a single parameter,
## usually denoted by lambda, which is both the mean
## *and* the variance of the distribution. Yes, really.

## Given lambda=1, what is the probability to get a zero?
## In other words, if X ~ Poisson(1), what is Pr(X=0)?

dpois(x = 0, lambda = 1)

## So 36.8% What about Pr(X=1)?

dpois(x = 1, lambda = 1)

## Ha, it's the same!

## We can plot the entire distribution:

## Range of x-values:
x <- 0:8
x <- seq(0,8)

## Generate the corresponding probabilities
## with the dpois function. Note that this function
## takes a vector (x) as input and generates a vector
## of equal length (y) as output. It is typical for R
## that functions are vectorized in this way.

y <- dpois(x,lambda=1)

## Store x and y in a dataframe d, for use with
## ggplot2
d <- data.frame(x,y)

## Create bar plot
ggplot(data=d,aes(x=x,y=y)) +
  geom_col(fill = "orange") +
  labs(x = "x-value", y = "Probability") +
  scale_x_continuous(expand=c(0,0),limits=c(-0.5,8.5),breaks=seq(0,8,1))

## Learn more about ggplot2 here: (ctrl-click)
## https://r4ds.had.co.nz/data-visualisation.html


## Bigger lambda:
x <- 0:10
y <- dpois(x,3.5)

ggplot(data.frame(x,y),aes(x,y)) +
  geom_col(fill = "orange") +
  labs(x = "x-value", y = "Probability") +
  scale_x_continuous(expand=c(0,0),limits=c(-0.5,10.5),breaks=seq(0,10,2))

## That's starting to look a little like a normal distribution!
## Indeed, for larger and larger lambda, the approximation
## becomes closer and closer. Try it out for yourself if you like.

## One thousand random Poisson numbers:

x <- rpois(n = 1000, lambda = 2.5)

## Plot in a histogram

ggplot(data.frame(x),aes(x))+
  geom_histogram(binwidth = 1,fill="green",color="black")+
  theme_bw() +
  scale_x_continuous(expand=c(0,0),limits=c(-0.5,10.5),breaks=seq(0,10,1))

## Instead of counts, proportions (density) on y-axis:

ggplot(data.frame(x),aes(x, y = ..density..))+
  geom_histogram(binwidth = 1,fill="green",color="black")+
  theme_bw() +
  scale_x_continuous(expand=c(0,0),limits=c(-0.5,10.5),breaks=seq(0,10,1))



## We can check "empirically", with simulations,
## that mean = variance = lambda for Poisson distributions

## A bunch of lambda-values:
lambdas <- seq(0.5,5.5,length=10)

## Reserve space for a 1000 by 10 matrix (a 2D array)
random_array <- array(dim=c(1000,10))
  
## A simple for-loop to fill up the array with
## 10 columns of random poisson numbers,
## with eaxh column corresponding to a different lambda:

for (i in 1:10){
  random_array[,i] <- rpois(n = 1000, lambda = lambdas[i])
}

## Calculate the mean for each column, using apply function:

means <- apply(random_array,2,mean)
means

## and the variance

variances <- apply(random_array,2,var)
variances

## Plug into dataframe and plot:

d <- data.frame(means,variances)

ggplot(d,aes(means,variances)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(expand=c(0,0),limits=c(0,6),breaks=seq(0,6,1)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,6),breaks=seq(0,6,1))

## Indeed!
## Now try it with n=10000


############# The normal distribution

## Plot a normal distribution:

ggplot(NULL, aes(c(-3,3))) + 
  geom_area(stat = "function", fun = dnorm, fill = "skyblue", xlim = c(-3, 3), alpha = 0.3)
  
## Generate 50 random standard normal numbers (mean 0, sd 1):

x <- rnorm(n = 50, mean = 0, sd = 1)

## Plot in a histogram

ggplot(data.frame(x),aes(x))+
  geom_histogram(binwidth = 0.5,fill="green",color="black")

## Does that look normal? Repeat it a few times.

## Use QQ-plot to check normality:

qqnorm(x)
qqline(x)


## For a very clear explanation of QQ-plots, check out this video:
## (shift-click on link to open in your browser)
## https://www.youtube.com/watch?v=X9_ISJ0YpGw&app=desktop

