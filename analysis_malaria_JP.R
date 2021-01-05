# Main analysis script
# jp22
# Last modified: January 2nd 2020

# TO DO: 
# Explanatory model
# Lasso regression

# Predictive model
# Include interaction term, no need for mixed effect model
# Train on three years, test on one year

#### PACKAGES ####

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
library(lmerTest)
library('MuMIn')
library('glmmADMB')
library(corrr)
library('car')

#### FUNCTIONS ####

# Output formula based on logical vector
make.formula <- function(formula.reg, adult = TRUE, lag.malaria = 0, quadratic = FALSE, random.id = FALSE, time.interaction = FALSE) {
  
  # Start the string with adult or children malaria prevalence, and add lagged malaria if needed
  if (adult == TRUE) {
    
    formula.string <- 'palu.pc.2nn_0.08 ~'
    
    if (lag.malaria == 1) {
      formula.string <- paste(formula.string, 'malaria.all.lag + ')
    } else if (lag.malaria == 2) {
      formula.string <- paste(formula.string, 'malaria.all.lag2 + ')
    } else if (lag.malaria == 3) {
      formula.string <- paste(formula.string, 'malaria.all.lag3 + ')
    }
    
    if (time.interaction) {
      
      if (lag.malaria == 1) {
        formula.string <- paste(formula.string, 'malaria.all.lag*month + ')
      } else if (lag.malaria == 2) {
        formula.string <- paste(formula.string, 'malaria.all.lag2*month + ')
      } else if (lag.malaria == 3) {
        formula.string <- paste(formula.string, 'malaria.all.lag3*month + ')
      }
      
    }
    
  }
  else {
    
    formula.string <- 'palu.und5.pc.3nn_0.19 ~'
    
    if (lag.malaria == 1) {
      formula.string <- paste(formula.string, 'malaria.under5.lag + ')
    } else if (lag.malaria == 2) {
      formula.string <- paste(formula.string, 'malaria.under5.lag2 + ')
    } else if (lag.malaria == 3) {
      formula.string <- paste(formula.string, 'malaria.under5.lag3 + ')
    }
    
    if (time.interaction) {
      
      if (lag.malaria == 1) {
        formula.string <- paste(formula.string, 'malaria.under5.lag*month + ')
      } else if (lag.malaria == 2) {
        formula.string <- paste(formula.string, 'malaria.under5.lag2*month + ')
      } else if (lag.malaria == 3) {
        formula.string <- paste(formula.string, 'malaria.under5.lag3*month + ')
      }
      
    }
    
  }
  
  # Select variables according to vector
  var.selected <- var.names[formula.reg]
  
  # Join and separate with + 
  var.string <- paste(var.selected, collapse = ' + ')
  
  formula.string <- paste(formula.string,var.string)
  
  # Add quadratic term if needed
  
  if (quadratic) {
    quad.names <- var.names[c(7,8,9,10,11,12,13,14)]
    quad.terms <- formula.reg[c(7,8,9,10,11,12,13,14)]
    
    quad.selected <- quad.names[quad.terms]
    
    if (length(quad.selected) != 0){
      quad.string <- paste(quad.selected, collapse = '^2) + I(')
      
      quad.string <- paste('+ I(', quad.string, '^2)', sep = '')
      
      formula.string <- paste(formula.string, quad.string)
    }
  }
  
  # Add ID as random effects if needed
  
  if (random.id) {
    
    formula.string <- paste(formula.string, '+ (1|ID)')
    
  }
  
  formula.reg <- as.formula(formula.string)
  
  return(formula.reg)
}

# Perform final processing steps
final.processing <- function(predictors){
  # Set month as a factor
  predictors$month <- as.factor(predictors$month)
  predictors$min.veg <- as.factor(predictors$min.veg)
  predictors$maj.veg <- as.factor(predictors$maj.veg)
  
  # Round to 3rd decimal place
  predictors$palu.pc.2nn_0.08 <- round(predictors$palu.pc.2nn_0.08, 3)
  predictors$palu.und5.pc.3nn_0.19 <- round(predictors$palu.und5.pc.3nn_0.19,3)
  
  # Multiply by 1000
  predictors$palu.pc.2nn_0.08 <- predictors$palu.pc.2nn_0.08*1000
  predictors$palu.und5.pc.3nn_0.19 <- predictors$palu.und5.pc.3nn_0.19*1000
  
  # Remove NA rows
  predictors <- predictors[!is.na(predictors$palu.pc.2nn_0.08),]
  
  predictors$min.t.away.lag <- abs(predictors$monthly.min.t.lag - 25.5)
  predictors$max.t.away.lag <- abs(predictors$monthly.max.t.lag - 25.5)
  
  months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  
  
  for (i in 1:length(predictors$ID)) {
    
    m <- months[predictors$month[i]]
    #print(m)
    year <- toString(predictors$year[i])
    #print(year)
    predictors$date[i] <- paste(m,year)
    
  }
  
  predictors$date <- factor(predictors$date, levels = unique(predictors$date))
  
  
  return(predictors)
  
}

# Standardize continuous variables
standardize <- function(predictors){
  
  
  var.to.norm <- predictors[,c('Population','nets.high','nets.low','altitude', 
                            'monthly.p', 'monthly.max.t', 'monthly.min.t',
                            'mean.ann.temp','min.ann.temp','max.ann.temp',
                            'mean.ann.p','wet.p','dry.p',
                            'monthly.p.lag', 'monthly.max.t.lag', 'monthly.min.t.lag',
                            'max.t.lag.2','min.t.lag.2', 'p.lag.2',
                            'max.t.lag.3','min.t.lag.3','p.lag.3',
                            'lu_savannah','lu_res','lu_water','lu_rice',
                            'cover.year.of','cover.year.previous','loss.year.previous',
                            'loss.prop.year.of','loss.prop.year.previous','forest.gain',
                            'real.dist.csb')]
  
  scaled.predictors <- scale(var.to.norm)
  
  
  predictors[,c('Population','nets.high','nets.low','altitude', 
                'monthly.p', 'monthly.max.t', 'monthly.min.t',
                'mean.ann.temp','min.ann.temp','max.ann.temp',
                'mean.ann.p','wet.p','dry.p',
                'monthly.p.lag', 'monthly.max.t.lag', 'monthly.min.t.lag',
                'max.t.lag.2','min.t.lag.2', 'p.lag.2',
                'max.t.lag.3','min.t.lag.3','p.lag.3',
                'lu_savannah','lu_res','lu_water','lu_rice',
                'cover.year.of','cover.year.previous','loss.year.previous',
                'loss.prop.year.of','loss.prop.year.previous','forest.gain',
                'real.dist.csb')] <- scaled.predictors
  
  return(predictors)
  
}

# Make different combinations of variables based on groups
make.param.matrix <- function(var.names){
  
  param.group <- matrix(FALSE, nrow=length(var.names), ncol=10)
  param.group[c(1,2,3,4),1] <- TRUE
  param.group[c(5,6),2] <- TRUE
  param.group[c(7,8),3] <- TRUE
  param.group[c(9,10),4] <- TRUE
  param.group[c(11,12),5] <- TRUE
  param.group[c(13,14),6] <- TRUE
  param.group[c(15,16,17,18),7] <- TRUE
  param.group[c(19,20),8] <- TRUE
  param.group[c(21,22),9] <- TRUE
  param.group[23,10] <- TRUE
  
  # All possible combination of groups
  n <- 10
  l <- rep(list(0:1), n)
  combi.group <- expand.grid(l)
  combi.group <- combi.group[-1,]
  
  combi.param <- matrix(0,nrow = nrow(combi.group),ncol = nrow(param.group))
  
  for (row in 1:nrow(combi.group)) {
    
    select.group <- combi.group[row,]
    select.columns <- param.group[,as.logical(unlist(select.group))]
    
    if (is.null(nrow(select.columns))){
      combi.param[row,] <- as.logical(select.columns)
    } else {
      combi.param[row,] <- as.logical(rowSums(select.columns))
    }
    
  }
  
  return(combi.param)
  
}

# Select combination of variables based on AIC and RMSE, LINEAR MODEL
best.linear <- function(var.matrix, predictors){
  
  best.AIC <- Inf
  best.BIC <- Inf
  best.RMSE <- Inf
  
  for (row in 1:nrow(var.matrix)){
    
    var.vector <- as.logical(var.matrix[row,])
    
    # Only linear terms
    
    linear.formula <- make.formula(var.vector)
    linear.temp <- lm(linear.formula, predictors)
    aic <- AIC(linear.temp)
    bic <- BIC(linear.temp)
    
    if (aic < best.AIC){
      best.AIC <- aic
      formula.best.AIC <- linear.formula
    }
    
    if (bic < best.BIC){
      best.BIC <- bic
      formula.best.BIC <- linear.formula
    }
    
    pred.linear.temp <- predict(linear.temp)
    rmse <- (sum((predictors$palu.pc.2nn_0.08 - pred.linear.temp)^2, na.rm = TRUE)/length(pred.linear.temp[!is.na(pred.linear.temp)]))^(0.5)
    
    if (rmse < best.RMSE){
      best.RMSE <- rmse
      formula.best.RMSE <- linear.formula
    }
    
    # Quadratic terms
    
    quad.formula <- make.formula(var.vector, quadratic = TRUE)
    quad.temp <- lm(quad.formula, predictors)
    aic <- AIC(quad.temp)
    bic <- BIC(quad.temp)
    
    if (aic < best.AIC){
      best.AIC <- aic
      formula.best.AIC <- quad.formula
    }
    
    if (bic < best.BIC){
      best.BIC <- bic
      formula.best.BIC <- quad.formula
    }
    
    pred.quad.temp <- predict(quad.temp)
    rmse <- (sum((predictors$palu.pc.2nn_0.08 - pred.quad.temp)^2, na.rm = TRUE)/length(pred.quad.temp[!is.na(pred.quad.temp)]))^(0.5)
    
    if (rmse < best.RMSE){
      best.RMSE <- rmse
      formula.best.RMSE <- quad.formula
    }
    
  }
  
  return(list(best.AIC, formula.best.AIC, best.BIC, formula.best.BIC, best.RMSE, formula.best.RMSE))
  
}

# Select combination of variables based on AIC and RMSE, HURDLE MODEL
best.hurdle <- function(var.matrix, predictors){
  
  best.AIC <- Inf
  best.BIC <- Inf
  best.RMSE <- Inf
  
  for (row in 1:nrow(var.matrix)){
    
    var.vector <- as.logical(var.matrix[row,])
    
    # Only linear terms
    
    linear.formula <- make.formula(var.vector)
    linear.temp <- hurdle(linear.formula, predictors, dist = "negbin", zero.dist = "binomial")
    aic <- AIC(linear.temp)
    bic <- BIC(linear.temp)
    
    if (aic < best.AIC){
      best.AIC <- aic
      formula.best.AIC <- linear.formula
    }
    
    if (bic < best.BIC){
      best.BIC <- bic
      formula.best.BIC <- linear.formula
    }
    
    pred.linear.temp <- predict(linear.temp)
    rmse <- (sum((predictors$palu.pc.2nn_0.08 - pred.linear.temp)^2, na.rm = TRUE)/length(pred.linear.temp[!is.na(pred.linear.temp)]))^(0.5)
    
    if (rmse < best.RMSE){
      best.RMSE <- rmse
      formula.best.RMSE <- linear.formula
    }
    
    # Quadratic terms
    
    quad.formula <- make.formula(var.vector, quadratic = TRUE)
    quad.temp <- hurdle(quad.formula, predictors, dist = "negbin", zero.dist = "binomial")
    aic <- AIC(quad.temp)
    bic <- BIC(quad.temp)
    
    if (aic < best.AIC){
      best.AIC <- aic
      formula.best.AIC <- quad.formula
    }
    
    if (bic < best.BIC){
      best.BIC <- bic
      formula.best.BIC <- quad.formula
    }
    
    pred.quad.temp <- predict(quad.temp)
    rmse <- (sum((predictors$palu.pc.2nn_0.08 - pred.quad.temp)^2, na.rm = TRUE)/length(pred.quad.temp[!is.na(pred.quad.temp)]))^(0.5)
    
    if (rmse < best.RMSE){
      best.RMSE <- rmse
      formula.best.RMSE <- quad.formula
    }
    
  }
  
  return(list(best.AIC, formula.best.AIC, best.BIC, formula.best.BIC, best.RMSE, formula.best.RMSE))
  
  
}

# Remove one variable at a time, and evaluate AIC and RMSE
remove.one.hurdle <- function(ignore = NULL, dataset){
  
  size <- length(var.names)
  all.in <- rep(1,size)
  all.in[ignore] <- 0
  
  all.in.formula <- make.formula(as.logical(all.in), quadratic = TRUE)
  print(all.in.formula)
  hurdle.model <- hurdle(all.in.formula,dataset,dist = "negbin", zero.dist = "binomial")
  
  aic.best <- AIC(hurdle.model)
  bic.best <- BIC(hurdle.model)
  pred.best <- predict(hurdle.model)
  rmse.best <- (sum((dataset$palu.pc.2nn_0.08 - pred.best)^2, na.rm = TRUE)/length(pred.best[!is.na(pred.best)]))^(0.5)
  
  aic.vector <- rep(0, size)
  bic.vector <- rep(0, size)
  rmse.vector <- rep(0, size)
  
  for (i in 1:size) {
    
    one.out <- all.in
    one.out[i] <- 0
    
    var.vector <- as.logical(one.out)
    
    one.out.formula <- make.formula(var.vector, quadratic = TRUE)
    print(one.out.formula)
    one.out.model <- hurdle(one.out.formula,dataset,dist = "negbin", zero.dist = "binomial")
    
    aic.vector[i] <- AIC(one.out.model)
    bic.vector[i] <- BIC(one.out.model)
    
    
    pred <- predict(one.out.model)
    rmse <- (sum((dataset$palu.pc.2nn_0.08 - pred)^2, na.rm = TRUE)/length(pred[!is.na(pred)]))^(0.5)
    
    rmse.vector[i] <- rmse
      
      
  }
  
  aic.vector <- aic.vector - aic.best
  bic.vector <- bic.vector - bic.best
  rmse.vector <- rmse.vector - rmse.best
  
  names(aic.vector) <- var.names
  names(bic.vector) <- var.names
  names(rmse.vector) <- var.names
  
  return(list(aic.vector,bic.vector,rmse.vector))

}

# Factor analysis for forest loss / land use
factor.analysis.land <- function(predictors){
  
  df.fa <- predictors[,c('lu_savannah','cover.year.of','cover.year.previous',
                      'loss.prop.year.previous','loss.prop.year.of')]

  forest.fa <- factanal(df.fa,2)
  forest.fa
  
  par(mfrow = c(1,1))
  plot(forest.fa$loadings[,1], 
       forest.fa$loadings[,2],
       xlab = "Factor 1", 
       ylab = "Factor 2", 
       ylim = c(-1,1),
       xlim = c(-1,1),
       main = "No rotation")
  abline(h = 0, v = 0)
  text(forest.fa$loadings[,1]-0.08, 
       forest.fa$loadings[,2]+0.08,
       colnames(df.fa),
       col="blue")
  
  forest.fa$factors
  forest.fa$loadings
  
  predictors$factor1.cover <- apply(as.matrix(df.fa) %*% diag(forest.fa$loadings[,1]), 1, sum)
  predictors$factor2.loss <- apply(as.matrix(df.fa) %*% diag(forest.fa$loadings[,2]), 1, sum)
  
  
}

# Factor analysis for climatic variable
factor.analysis.clim <- function(predictors){
  
  df.fa <- predictors[,c('mean.ann.temp','min.ann.temp','max.ann.temp',
                         'mean.ann.p','wet.p','dry.p')]
  
  clim.fa <- factanal(df.fa,2)
  clim.fa
  
  par(mfrow = c(1,1))
  
  plot(clim.fa$loadings[,1], 
       clim.fa$loadings[,2],
       xlab = "Factor 1", 
       ylab = "Factor 2", 
       ylim = c(-1,1),
       xlim = c(-1,1),
       main = "No rotation")
  abline(h = 0, v = 0)
  text(clim.fa$loadings[,1]-0.08, 
       clim.fa$loadings[,2]-0.08,
       colnames(df.fa),
       col="blue")
  
  predictors$factor.ann.p <- apply(as.matrix(df.fa) %*% diag(clim.fa$loadings[,1]), 1, sum)
  predictors$factor.ann.t <- apply(as.matrix(df.fa) %*% diag(clim.fa$loadings[,2]), 1, sum)
  
  return(predictors)

}

# Assess robustness by removing one variable at a time and taking mean of estimates 
robustness <- function(ignore = NULL, dataset){
  
  size <- length(var.names)
  all.in <- rep(1,size)
  all.in[ignore] <- 0
  
  var.vector <- as.logical(all.in)
  all.in.formula <- make.formula(var.vector, quadratic = TRUE)
  
  all.in.model <- hurdle(all.in.formula,dataset,dist = "negbin", zero.dist = "binomial")
  
  all.in.summary <- summary(all.in.model)
  
  all.in.summary$coefficients
  
  count.total.rows <- dim(all.in.summary$coefficients$count)[1]
  
  count.matrix.estimate <- matrix(NA,nrow = count.total.rows, ncol = size)
  count.matrix.p <- matrix(NA,nrow = count.total.rows, ncol = size)
  
  row.names(count.matrix.estimate) <- row.names(all.in.summary$coefficients$count)
  row.names(count.matrix.p) <- row.names(all.in.summary$coefficients$count)
  
  bi.total.rows <- dim(all.in.summary$coefficients$zero)[1]
  
  bi.matrix.estimate <- matrix(NA,nrow = bi.total.rows, ncol = size)
  bi.matrix.p <- matrix(NA,nrow = bi.total.rows, ncol = size)
  
  row.names(bi.matrix.estimate) <- row.names(all.in.summary$coefficients$zero)
  row.names(bi.matrix.p) <- row.names(all.in.summary$coefficients$zero)
  
  
  for (i in 1:size) {
    
    if (all.in[i] == 0){
      next
    }
    
    one.out <- all.in
    one.out[i] <- 0
    
    var.vector <- as.logical(one.out)
    
    one.out.formula <- make.formula(var.vector, quadratic = TRUE)
    
    print(one.out.formula)
    
    one.out.model <- hurdle(one.out.formula,dataset,dist = "negbin", zero.dist = "binomial")
    
    one.out.summary <- summary(one.out.model)
    
    one.out.bi.names <- row.names(one.out.summary$coefficients$zero)
    one.out.count.names <- row.names(one.out.summary$coefficients$count)
    
    count.matrix.estimate[one.out.count.names,i] <- one.out.summary$coefficients$count[,1]
    count.matrix.p[one.out.count.names,i] <- one.out.summary$coefficients$count[,4]
    
    bi.matrix.estimate[one.out.bi.names,i] <- one.out.summary$coefficients$zero[,1]
    bi.matrix.p[one.out.bi.names,i] <- one.out.summary$coefficients$zero[,4]
    
  }
  
  return(list(count.matrix.estimate, count.matrix.p, bi.matrix.estimate, bi.matrix.p))
  
}

### MAIN CODE ####

setwd("~/Documents/Stanford/Research /Malaria Project/Data/csv_datasets")
predictors <- read.csv('malaria_updated_dec15.csv')

predictors <- final.processing(predictors)
predictors <- standardize(predictors)

## Inspect correlation matrices to find colinearities

corr.others <- correlate(predictors[,c('Population','nets.high','nets.low','real.dist.csb')]) # All good

corr.land <- correlate(predictors[,c('lu_savannah','lu_res','lu_water','lu_rice',
                                     'cover.year.of','loss.prop.year.previous','loss.year.previous')])
# --> Remove loss.year.previous

corr.weather <- correlate(predictors[,c('monthly.p', 'monthly.max.t', 'monthly.min.t',
                                        'mean.ann.temp','min.ann.temp','max.ann.temp',
                                        'mean.ann.p','wet.p','dry.p',
                                        'monthly.p.lag', 'monthly.max.t.lag', 'monthly.min.t.lag',
                                        'max.t.lag.2','min.t.lag.2', 'p.lag.2',
                                        'max.t.lag.3','min.t.lag.3','p.lag.3')])

# --> Reduce ann.temp and ann.p, only keep min.t

## Do factor analysis on these

predictors <- factor.analysis.clim(predictors)

## These are the remaining variables we want to use

var.names <- c('Population','nets.high','nets.low','real.dist.csb',
               'factor.ann.p','factor.ann.t',
               'monthly.p', 'monthly.min.t',
               'monthly.p.lag', 'monthly.min.t.lag',
               'min.t.lag.2', 'p.lag.2',
               'min.t.lag.3','p.lag.3',
               'lu_savannah','lu_res','lu_water','lu_rice',
               'med.veg','min.veg',
               'cover.year.of', 'loss.prop.year.previous',
               'month')

# Look at model summary when they are all included

all <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb +
                           factor.ann.p + factor.ann.t +
                           monthly.p +  monthly.min.t + 
                           monthly.p.lag +  monthly.min.t.lag +
                           min.t.lag.2 + p.lag.2 + 
                           min.t.lag.3 + p.lag.3 + 
                           lu_savannah + lu_res + lu_water + lu_rice +
                           med.veg + min.veg + 
                           cover.year.of + loss.prop.year.previous + 
                           month +
                           I(monthly.p^2) + I(monthly.min.t^2) + 
                           I(monthly.p.lag^2) + I(monthly.min.t.lag^2) + 
                           I(min.t.lag.2^2) + I(p.lag.2^2) + 
                           I(min.t.lag.3^2) + I(p.lag.3^2),
                         predictors,dist = "negbin", zero.dist = "binomial")

summary(all)

pred <- predict(all)
rmse <- (sum((predictors$palu.pc.2nn_0.08 - pred)^2, na.rm = TRUE)/length(pred[!is.na(pred)]))^(0.5)

AIC(all)
BIC(all)

## Create all possible combinations using groups of variables

variable.matrix <- make.param.matrix(var.names)

##  Best models for linear regression
best.linear.models <- best.linear(variable.matrix,predictors)
best.linear.models[2]
best.lm <- lm(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                factor.ann.p + factor.ann.t + monthly.p + monthly.min.t + 
                min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + lu_res + lu_water + 
                lu_rice + med.veg + min.veg + cover.year.of + loss.prop.year.previous + 
                month + I(monthly.p^2) + I(monthly.min.t^2) + I(min.t.lag.2^2) + I(p.lag.2^2) + 
                I(min.t.lag.3^2) + I(p.lag.3^2),predictors)

best.lm <- lm(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                factor.ann.p + factor.ann.t + monthly.p + monthly.min.t + 
                monthly.p.lag + monthly.min.t.lag + lu_savannah + lu_res + lu_water + 
                lu_rice + med.veg + min.veg + cover.year.of + loss.prop.year.previous ,predictors)


summary(best.lm)

## Best models for hurdle model
best.hurdle.models <- best.hurdle(variable.matrix,predictors)

best.hurdle.models[4]

# This is the best model (lowest RMSE):

best.model.all <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                           factor.ann.p + factor.ann.t + monthly.p + monthly.min.t + 
                           min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                           lu_res + lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                           loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                           I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                         dist = "negbin", zero.dist = "binomial")

summary(best.model.all)

AIC(best.model.all)
BIC(best.model.all)
pred.best <- predict(best.model.all)
(sum((predictors$palu.pc.2nn_0.08 - pred.best)^2, na.rm = TRUE)/length(pred.best[!is.na(pred.best)]))^(0.5)

vif(best.model.all)

## Try the same model with a simple negative binomial

best.model.nb <- glm.nb(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                          factor.ann.p + factor.ann.t + monthly.p + monthly.min.t + 
                          min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                          lu_res + lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                          loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                          I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors)

AIC(best.model.nb)
pred.nb <- predict.glm(best.model.nb)
rmse.nb <- (sum((predictors$palu.pc.2nn_0.08 - pred.nb)^2, na.rm = TRUE)/length(pred.nb[!is.na(pred.nb)]))^(0.5)

summary(best.model.nb)

## 'One-out' analysis: Try removing one element at a time

bic_rmse <- remove.one.hurdle(ignore = c(9,10), predictors)
bic_rmse[1]
bic_rmse[2]
bic_rmse[3]

# AIC: factor.ann.t, lu_res
# BIC: factor.ann.t, lu_res, lu_water
# RMSE: none

# --> Conclusion, remove factor.ann.t, lu_res

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                         dist = "negbin", zero.dist = "binomial")


summary(best.model.simp)

AIC(best.model.simp) # 67036
BIC(best.model.simp) # 67233 -> 67218 -> 67041 (with interaction)
pred.best <- predict(best.model.simp)
(sum((predictors$palu.pc.2nn_0.08 - pred.best)^2, na.rm = TRUE)/length(pred.best[!is.na(pred.best)]))^(0.5) # 51.45 -> 51.17
#(sum((predictors$palu.und5.pc.3nn_0.19 - pred.best)^2, na.rm = TRUE)/length(pred.best[!is.na(pred.best)]))^(0.5) # 51.45 -> 51.17


## Futher tuning: Add interaction 

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + month*monthly.p + month*monthly.min.t + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

summary(best.model.simp)

AIC(best.model.simp) # 67036
BIC(best.model.simp) # 67233 -> 67218 -> 67041 (with interaction)
pred.best <- predict(best.model.simp)
(sum((predictors$palu.pc.2nn_0.08 - pred.best)^2, na.rm = TRUE)/length(pred.best[!is.na(pred.best)]))^(0.5) # 51.45 -> 51.17


# Interaction instead of lag

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            lu_savannah + lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + month*monthly.p + month*monthly.min.t + I(monthly.p^2) + I(monthly.min.t^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

summary(best.model.simp)

AIC(best.model.simp) # 67036
BIC(best.model.simp) # 67233 -> 67218 -> 67041 (with interaction)
pred.best <- predict(best.model.simp)

## Robustness analysis ##

estimate_p <- robustness(ignore = c(6,9,10,16), predictors)

# Binomial part

bi.matrix.estimate <- estimate_p[[3]]
bi.matrix.p <- estimate_p[[4]]

bi.matrix.sig <- (bi.matrix.p < 0.05)
sig.prop.bi <- round(rowSums(bi.matrix.sig, na.rm = TRUE)/(ncol(bi.matrix.p)-5),3)

bi.matrix.estimate[bi.matrix.sig == 0] <- NA
est.bi <- rowMeans(bi.matrix.estimate, na.rm = TRUE)

# NB count part

count.matrix.estimate <- estimate_p[[1]]
count.matrix.p <- estimate_p[[2]]

count.matrix.sig <- (count.matrix.p < 0.05)
sig.prop.count <- round(rowSums(count.matrix.sig, na.rm = TRUE)/(ncol(count.matrix.p)-5),3)

count.matrix.estimate[count.matrix.sig == 0] <- NA
est.count <- rowMeans(count.matrix.estimate, na.rm = TRUE)

robust.check <- data.frame('est.bi' = est.bi, 'sig.bi' = sig.prop.bi, 'est.count' = est.count[1:40], 'sig.count' = sig.prop.count[1:40])


## Remove quadratic one at a time

# All
best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67036



best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67034

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2)  + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67091

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2)  + I(monthly.min.t^2) + 
                            I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67106


best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67041

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(p.lag.2^2) + I(min.t.lag.3^2) + I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67106

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) +  I(p.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67104

best.model.simp <- hurdle(palu.pc.2nn_0.08 ~ Population + nets.high + nets.low + real.dist.csb + 
                            factor.ann.p + monthly.p + monthly.min.t + 
                            min.t.lag.2 + p.lag.2 + min.t.lag.3 + p.lag.3 + lu_savannah + 
                            lu_water + lu_rice + med.veg + min.veg + cover.year.of + 
                            loss.prop.year.previous + month + I(monthly.p^2) + I(monthly.min.t^2) + 
                            I(min.t.lag.2^2) + I(p.lag.2^2) + I(min.t.lag.3^2), predictors,
                          dist = "negbin", zero.dist = "binomial")

AIC(best.model.simp) # 67055



## Prediction stuff

sum((predictors$palu.pc.2nn_0.08 > 50) & (pred.best > 50))/sum(predictors$palu.pc.2nn_0.08 > 50) 
sum((predictors$palu.pc.2nn_0.08 < 50) & (pred.best > 50))/sum(predictors$palu.pc.2nn_0.08 < 50) 

sum((predictors$palu.pc.2nn_0.08 < 5) & (pred.best < 5))/sum(predictors$palu.pc.2nn_0.08 < 5) 

plot(predictors$palu.pc.2nn_0.08,pred.best)
plot(predictors$palu.und5.pc.3nn_0.19,pred.best)

# Remove variables one at a time to evaluate robustness
remove.one.hurdle(ignore = c(9,10), predictors)

## Extra stuff

linear.mixed <- lmer(test, predictors)
summary(lmer(test, predictors))
r.squaredGLMM(linear.mixed)

test <- summary(best.model.all)
rowNames(test$coefficients$count)
test2<- test$coefficients$count
row.names(test2)
