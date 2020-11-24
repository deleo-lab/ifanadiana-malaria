library('lme4')
library('plotly')

predictors <- read.csv('/Users/Krti/Desktop/malaria-ifanadiana/malaria_predictors_updated.csv')

# Visualization
par(mfrow=c(2,4))
p1 <- plot(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag), col = "red")

p2 <- plot(predictors$palu.pc.2nn_0.08 ~ predictors$forest.cover, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$forest.cover), col = "red")

p3 <- plot(palu.pc.2nn_0.08 ~ predictors$monthly.max.t.lag, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$monthly.max.t.lag), col = "red")

p4 <- plot(palu.pc.2nn_0.08 ~ predictors$monthly.min.t.lag, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$monthly.min.t.lag), col = "red")

p5 <- plot(palu.pc.2nn_0.08 ~ predictors$monthly.p.lag, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$monthly.p.lag), col = "red")

p6 <- plot(palu.pc.2nn_0.08 ~ predictors$altitude, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$altitude), col = "red")

p7 <- plot(palu.pc.2nn_0.08 ~ predictors$loss.14, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$loss.14), col = "red")

p8 <- boxplot(predictors$palu.pc.2nn_0.08 ~ predictors$med.veg, predictors)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag), col = "red")


par(mfrow=c(1,1))



# Adult malaria
time.corr <- lm(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag)
summary(time.corr)
plot(predictors$palu.pc.2nn_0.08,predictors$malaria.all.lag)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag), col = "red")

pop.corr <- lm(palu.pc.2nn_0.08 ~ predictors$malaria.all.lag + Population, predictors)
summary(pop.corr)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag), col = "red")

weather.corr <- lm(predictors$palu.pc.2nn_0.08 ~ predictors$monthly.p.lag + predictors$monthly.max.t.lag + predictors$monthly.min.t.lag + predictors$altitude, predictors)
summary(weather.corr)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$monthly.p.lag + predictors$monthly.max.t.lag + predictors$monthly.min.t.lag + predictors$altitude), col = "red")

land.corr <-  lm(palu.pc.2nn_0.08 ~ month + monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + forest.cover + loss.14 + loss.10 + med.veg, predictors)
summary(land.corr)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$monthly.p.lag + predictors$monthly.max.t.lag + predictors$monthly.min.t.lag + predictors$altitude + predictors$forest.cover + predictors$loss.14 + predictors$loss.10 + predictors$med.veg, predictors), col = "red")

tot.corr <- lm(palu.pc.2nn_0.08 ~ malaria.all.lag + monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude + forest.cover + loss.14 + loss.10 + med.veg, predictors)
summary(tot.corr)
abline(lm(predictors$palu.pc.2nn_0.08 ~ predictors$malaria.all.lag), col = "red")

# Children malaria

weather.corr <- lm(palu.und5.pc.3nn_0.19 ~ monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude, predictors)
summary(weather.corr)
plot(predictors$palu.und5.pc.3nn_0.19, monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude, predictors)
abline(lm(predictors$palu.und5.pc.3nn_0.19 ~ monthly.p.lag + monthly.max.t.lag + monthly.min.t.lag + altitude, predictors), col = "red")


# Building our datasets 

malaria <- read.csv('dat_izzy_May15.csv')

alt <- read.csv('altitude.bf.csv') # Read altitude
bednets <- read.csv('bednets.bs.bf.csv') # Read bed net use

malaria$altitude <- 0
malaria$nets.high <- 0
malaria$nets.low <- 0

for (i in 0:194){
  
  malaria$altitude[malaria$ID == i] <- alt$alt_bf[i+1]
  malaria$nets.high[malaria$ID == i] <- bednets$highseason[i+1]
  malaria$nets.low[malaria$ID == i] <- bednets$lowseason[i+1]
  
}



# Statistical Testing

print(chisq.test(predictors))

Xsq <- chisq.test(predictors)
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals







