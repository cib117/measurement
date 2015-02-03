rm(list=ls())
set.seed(1989)
## Set working directory
setwd('/Users/christopher/Dropbox/measurement/Homework 1')
## Load packages
library(ggplot2); library(reshape); library(gam)
## Load functions for ols, rf, and gam cross-validation
rf.custom <- function(data.train,data.test){
  library(randomForest)
  data.train$fold <- NULL
  data.test$fold <- NULL
  ivar<- names(data.train)[!is.element(names(data.train),c("y"))] ## extract predictors
  formula <- as.formula(paste0("y", "~", paste0(ivar, collapse = "+"))) ## formula
  y.test <- data.test$y ## out of sample outcome
  X.test <-  data.test[,ivar] ## out of sample predictors
  rf <-randomForest(formula, data = data.train) ## Run model
  predicted <- predict(rf, X.test, type="response") ## Predicted y-hats
  test.error <- sqrt(mean((y.test - predicted)^2)) ## RMSE
  performance <- list(test.error=test.error) ## Return as a list
}
ols.custom <- function(data.train,data.test){
  data.train$fold <- NULL
  data.test$fold <- NULL
  ivar<- names(data.train)[!is.element(names(data.train),c("y"))] ## extract predictors
  formula <- as.formula(paste0("y", "~", paste0(ivar, collapse = "+"))) ## formula
  y.test <- data.test$y ## out of sample outcome
  X.test <-  data.test[,ivar] ## out of sample predictors
  ols <-lm(formula, data = data.train) ## Run model
  predicted <- predict(ols, X.test, type="response") ## Predicted y-hats
  test.error <- sqrt(mean((y.test - predicted)^2)) ## RMSE
  performance <- list(test.error=test.error) ## Return as a list
}
gamspline.custom <- function(data.train,data.test){
  library(gam)
  data.train$fold <- NULL
  data.test$fold <- NULL
  ivar<- names(data.train)[!is.element(names(data.train),c("y"))] ## extract predictors
  ivar.spline <- paste(paste("s(",ivar, sep=''),")",sep='')
  formula <- as.formula(paste0("y", "~", paste0(ivar.spline, collapse = "+"))) ## formula
  y.test <- data.test$y ## out of sample outcome
  X.test <-  data.test[,ivar] ## out of sample predictors
  rf <-gam(formula, data = data.train) ## Run model
  predicted <- predict(rf, X.test, type="response") ## Predicted y-hats
  test.error <- sqrt(mean((y.test - predicted)^2)) ## RMSE
  performance <- list(test.error=test.error) ## Return as a list
}
## Load data
data <- read.csv('data.csv')

## Summary of data
summary(data[,c(2:5)])
## Plot data
## Histograms
d <- melt(data[,c(2:5)])
ggplot(d,aes(x = value)) +  facet_wrap(~variable,scales = "free_x") +  geom_histogram() + theme_bw()

## Correlation plot
corrplot<-function(X){
  # Returns a correlation plot of Xs
  # X: a matrix or dataframe of the independent variables
  # Look at strength of pairwise correlations
  qplot(x=X1, y=X2, data=melt(cor(X, use="p")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1)) + xlab("Variable1") + ylab("Variable 2")
}
corrplot(data[,2:5])
## Strongest relationship is between y and x2 (negative correlation)

## Explore relationships using GAM with splines
mod1 <- (gam(y ~ s(x1) + s(x2) + s(x3), data=data))
par(mfrow=c(2,2))
plot(mod1, se=T, residual=T)
## We can see nonlinearities in the relationships
## The is particularly clear for x2

## Assess model fit using 5-fold cv
## Should always assess the predictive validity of a model
data$unit <- NULL ## drop unit variable
## Shuffle and split data for cross validation
folds <- 5
data<-data[sample(nrow(data)),]
data$fold = cut(1:nrow(data), breaks=folds, labels=F)
## Empty vectors for plotting purposes
min.te.error <- c()
max.te.error <- c()
mean.te.error <- c()
method <- c("OLS", "GAM with splines", "Random Forest")
## OlS
ols.te.error <- c()
for (j in 1:folds){
  ols.perf <-ols.custom(data[data$fold != j,], data[data$fold == j,])
  ols.te.error <- c(ols.te.error, ols.perf$test.error)
}
mean.te.error <- c(mean.te.error,mean(ols.te.error))
min.te.error <- c(min.te.error, min(ols.te.error))
max.te.error <- c(max.te.error, max(ols.te.error))

## GAM with Splines
gams.te.error <- c()
for (j in 1:folds){
  gams.perf <-gamspline.custom(data[data$fold != j,], data[data$fold == j,])
  gams.te.error <- c(gams.te.error, gams.perf$test.error)
}
mean.te.error <- c(mean.te.error,mean(gams.te.error))
min.te.error <- c(min.te.error, min(gams.te.error))
max.te.error <- c(max.te.error, max(gams.te.error))

## RF
rf.te.error <- c()
for (j in 1:folds){
  rf.perf <-rf.custom(data[data$fold != j,], data[data$fold == j,])
  rf.te.error <- c(rf.te.error, rf.perf$test.error)
}
mean.te.error <- c(mean.te.error,mean(rf.te.error))
min.te.error <- c(min.te.error, min(rf.te.error))
max.te.error <- c(max.te.error, max(rf.te.error))

## Comparison of models
plot.df <- data.frame(mean.te.error,min.te.error,max.te.error,method)
plot.df<- transform(plot.df, method = reorder(method, mean.te.error)) 
limits <- aes(ymax=max.te.error, ymin=min.te.error)
p <- ggplot(plot.df, aes(y=mean.te.error, x=method))
p + geom_point() + geom_errorbar(limits, width=0.05)+
  theme_bw() + ggtitle('Summary of model performance') +
  ylab("RMSE") + xlab('Model') + ylim(0,10000)+ coord_flip()
## For out of sample performance: RF > GAM > OLS