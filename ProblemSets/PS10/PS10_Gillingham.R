library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

income$native.country <- NULL
income$fnlwgt         <- NULL

income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)

levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]
income$high.earner <- as.character(income$high.earner)

task <- makeRegrTask(id = "task", data = income.train, target = "high.earner")
print(task)

predAlg <- makeLearner("regr.lm")
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)
sampleResults <- resample(learner = predAlg, task = task, resampling = resampleStrat, measures = list(rmse))
print(sampleResults$aggr)
finalModel <- train(learner = predAlg, task = task)
prediction <- predict(finalModel, newdata = income.test)
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)
resample(predAlg,theTask,resampleStrat,measures=list(rmse))
finalModel <- train(learner = predAlg, task = task)
prediction <- predict(finalModel, newdata = income.test)
print(head(prediction$data))

predAlg <- makeLearner("regr.glmnet")
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

tuneMethod <- makeTuneControlRandom(maxit = 10L)

Tree <- makeLearner(cl= "classif.rpart", predict.type = "response")
Logg <- makeLearner(cl= "classif.glmnet", predict.type = "response")
NN <- makeLearner(cl= "classif.nnet", predict.type = "response")
Bay <- makeLearner(cl= "classif.naiveBayes", predict.type = "response")
kNN <- makeLearner(cl= "classif.kknn", predict.type = "response")
Svmm <- makeLearner(cl= "classif.svm", predict.type = "response")

tunedModel <- tuneParams(learner = predAlg,
                         task = task,
                         resampling = resampleStrat,
                         measures = rmse,       
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

