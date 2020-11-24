# Note: lagged malaria best predictor, decreases as lag increases
# Month matters also, and interaction between month and lagged malaria matters
# Precipitation and temperature significant predictor (1 month lag, exploring further lags)
# Nets during high season significant but not during low season
# Land use important with rice fields and residential areas significant predictors
# Degraded forest important, loss of that year important but not average loss over the last three months
# Once weather is controlled for, altitude not significant

# Data is over-dispersed, zero-inflated

library('lme4')
library('plotly')
library(MASS)
library(pscl)
library('lme4')
library('plotly')
library('gganimate')
library('ggplot2')
library('sf')
library('tmap')
library('tmaptools')
library('leaflet')
library('dplyr')
library('transformr')
library('png')
library('gifski')
library('ggsn')
library('ggpubr')
library(ape)

setwd("~/Documents/Stanford/Research /Malaria Project/Data/csv_datasets")
predictors <- read.csv('malaria_updated_nov10.csv')

# Set month as a factor
predictors$month <- as.factor(predictors$month)

# Round to 3rd decimal place
predictors$palu.pc.2nn_0.08 <- round(predictors$palu.pc.2nn_0.08, 3)

# Multiply by 1000
predictors$palu.pc.2nn_0.08 <- predictors$palu.pc.2nn_0.08*1000

# Remove NA rows
predictors <- predictors[!is.na(predictors$palu.pc.2nn_0.08),]

# Create training and test data
training_data = predictors[(predictors$year == 2014) | (predictors$year == 2015) | (predictors$year == 2016),]
test_data = predictors[predictors$year == 2017,]

# Visualization
par(mfrow=c(2,4))

p1 <- plot(predictors$palu.pc.2nn_0.08,predictors$malaria.all.lag)
p2 <- plot(palu.pc.2nn_0.08 ~ forest.cover, predictors)
p3 <- plot(palu.pc.2nn_0.08 ~ monthly.max.t.lag, predictors)
p4 <- plot(palu.pc.2nn_0.08 ~ monthly.min.t.lag, predictors)
p5 <- plot(palu.pc.2nn_0.08 ~ monthly.p.lag, predictors)
p6 <- plot(palu.pc.2nn_0.08 ~ altitude, predictors)
p7 <- plot(palu.pc.2nn_0.08 ~ loss.year.of, predictors)
p8 <- boxplot(predictors$palu.pc.2nn_0.08~predictors$med.veg)

par(mfrow=c(2,4))

p1 <- plot(predictors$palu.pc.2nn_0.08,predictors$malaria.all.lag)
p2 <- plot(palu.pc.2nn_0.08 ~ forest.cover, predictors)
p3 <- plot(palu.pc.2nn_0.08 ~ max.t.lag.2, predictors)
p4 <- plot(palu.pc.2nn_0.08 ~ min.t.lag.2, predictors)
p5 <- plot(palu.pc.2nn_0.08 ~ p.lag.2, predictors)
p6 <- plot(palu.pc.2nn_0.08 ~ altitude, predictors)
p7 <- plot(palu.pc.2nn_0.08 ~ loss.3.year.mean, predictors)
p8 <- boxplot(predictors$palu.pc.2nn_0.08~predictors$med.veg)

par(mfrow=c(1,1))

plot(palu.pc.2nn_0.08 ~ log(lu_rice), predictors)
plot(palu.pc.2nn_0.08 ~ log(lu_forest), predictors)
plot(palu.pc.2nn_0.08 ~ log(lu_water), predictors)
plot(palu.pc.2nn_0.08 ~ log(lu_res), predictors)
plot(palu.pc.2nn_0.08 ~ lu_savannah, predictors)
plot(palu.pc.2nn_0.08 ~ nets.low, predictors)

# Adult malaria

# First, let's assume we do not have access to previous malaria incidence.

# Start with linear regression

linear.corr <- lm(palu.pc.2nn_0.08 ~ Population + month + nets.high + nets.low + forest.gain +  
                 monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + 
                 forest.cover + med.veg + I(log(lu_rice + 10^(-6)))+ 
                 I(log(lu_savannah + 10^(-6)))+ I(log(lu_res+ 10^(-6)))+ 
                 I(log(lu_water+ 10^(-6))) + loss.3.year.mean + loss.year.of + 
                 loss.tot.sum, predictors)

summary(linear.corr)
linear.predictions <- predict(linear.corr)
plot(predictors$palu.pc.2nn_0.08, linear.predictions)
plot(1:length(linear.corr$residuals), linear.corr$residuals)

# Let's try Poisson distribution for count data

poisson.corr <- glm(palu.pc.2nn_0.08 ~ Population + month + nets.high + nets.low + forest.gain +  
                  monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + 
                  forest.cover + med.veg + I(log(lu_rice + 10^(-6)))+ 
                  I(log(lu_savannah + 10^(-6)))+ I(log(lu_res+ 10^(-6)))+ 
                  I(log(lu_water+ 10^(-6))) + loss.3.year.mean + loss.year.of + 
                  loss.tot.sum, family = 'poisson', predictors)

summary(poisson.corr) # Note overdispersion
poisson.predictions <- predict(poisson.corr)
plot(predictors$palu.pc.2nn_0.08, poisson.predictions)

# Let's try negative binomial

nb.corr <- glm.nb(palu.pc.2nn_0.08 ~ Population + month + nets.high + nets.low + forest.gain +  
                  monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + 
                  forest.cover + med.veg + I(log(lu_rice + 10^(-6)))+ 
                  I(log(lu_savannah + 10^(-6)))+ I(log(lu_res+ 10^(-6)))+ 
                  I(log(lu_water+ 10^(-6))) + loss.3.year.mean + loss.year.of + 
                  loss.tot.sum,predictors)

summary(nb.corr)
nb.predictions <- predict(nb.corr)
plot(predictors$palu.pc.2nn_0.08, nb.predictions)

# Zero-inflated negative binomial

zinb.corr <- zeroinfl(palu.pc.2nn_0.08 ~ Population + month + nets.high + nets.low + forest.gain +  
                       p.lag.2 + max.t.lag.2 + min.t.lag.2 + altitude + 
                       forest.cover + med.veg + I(log(lu_rice + 10^(-6)))+ 
                       I(log(lu_savannah + 10^(-6)))+ I(log(lu_res+ 10^(-6)))+ 
                       I(log(lu_water+ 10^(-6))) + loss.3.year.mean + loss.year.of + 
                       loss.tot.sum, data = predictors, dist = "negbin")

summary(zinb.corr)
zinb.predictions <- predict(zinb.corr)
plot(predictors$palu.pc.2nn_0.08, zinb.predictions)
hist(zinb.predictions)


# Ok, now with training and test data

tot.corr <- lm(palu.pc.2nn_0.08 ~ month + nets.high + nets.low + forest.gain +  
                 monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + 
                 forest.cover + med.veg + I(log(lu_rice + 10^(-6)))+ 
                 I(log(lu_savannah + 10^(-6)))+ I(log(lu_res+ 10^(-6)))+ 
                 I(log(lu_water+ 10^(-6))) + loss.3.year.mean + loss.year.of + 
                 loss.tot.sum, training_data)

plot(1:length(tot.corr$residuals),tot.corr$residuals)
plot(1:length(predictors$palu.pc.2nn_0.08),predictors$palu.pc.2nn_0.08)
plot(1:(length(predictors$palu.pc.2nn_0.08)-1),diff(predictors$palu.pc.2nn_0.08))
hist(predictors$palu.pc.2nn_0.08)
hist(diff(predictors$palu.pc.2nn_0.08))
predictions <- predict(tot.corr, test_data)

plot(test_data$palu.pc.2nn_0.08, predictions)


# What if we have access to previous months?


linear.corr <- lm(log.malaria ~ malaria.all.lag*month + Population + month + nets.high + nets.low + forest.gain +  monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + forest.cover + med.veg + I(log(lu_rice + 10^(-6)))+ I(log(lu_savannah + 10^(-6)))+ I(log(lu_res+ 10^(-6)))+ I(log(lu_water+ 10^(-6))) + loss.3.year.mean + loss.year.of + loss.tot.sum, predictors)
summary(linear.corr)

linear.pred <- predict(linear.corr, na.action =NULL)
plot(predictors$palu.pc.2nn_0.08, linear.pred)



# Use different functions/lag



