library(mlr)
library(glmnet)
library(tidyverse)
library(magrittr)
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing$lmedv <- log(housing$medv)
housing$medv <- NULL
formula <- as.formula(lmedv ~ .^3 +
                          poly(crim , 6) +
                          poly(zn , 6) +
                          poly(indus , 6) +
                          poly(nox , 6) +
                          poly(rm , 6) +
                          poly(age , 6) +
                          poly(dis , 6) +
                          poly(rad , 6) +
                          poly(tax , 6) +
                          poly(ptratio , 6) +
                          poly(b, 6) +
                          poly(lstat , 6))
mod_matrix <- data.frame(model.matrix(formula,housing))

mod_matrix[, 1] = housing$lmedv
colnames(mod_matrix )[1] = "lmedv" 
head(mod_matrix) 
n <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
test <- setdiff(1:n, train)
housing.train <- mod_matrix[train ,]
housing.test <- mod_matrix[test , ]

predAlg <- makeLearner("regr.glmnet")

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

tuneMethod <- makeTuneControlRandom(maxit = 50L)

tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

resample(predAlg,theTask,resampleStrat,measures=list(rmse))

finalModel <- train(learner = predAlg, task = theTask)

prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))

print(get.rmse(prediction$data$truth,prediction$data$response))

getLearnerModel(finalModel)$beta

#!!! Ridge Regression!!!

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

resample(predAlg,theTask,resampleStrat,measures=list(rmse))

finalModel <- train(learner = predAlg, task = theTask)

prediction <- predict(finalModel, newdata = housing.test)

print(get.rmse(prediction$data$truth,prediction$data$response))

getLearnerModel(finalModel)$beta

#!!! elastic net model !!!

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

resample(predAlg,theTask,resampleStrat,measures=list(rmse))

finalModel <- train(learner = predAlg, task = theTask)

prediction <- predict(finalModel, newdata = housing.test)

print(get.rmse(prediction$data$truth,prediction$data$response))

getLearnerModel(finalModel)$beta

