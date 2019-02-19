##################### RUN THIS FIRST
# START
library(readr)
library(e1071)
library(ggplot2)
library(jpeg)
library(caret)

source("./funcs.R")

# END


######################################
# Exercise 1
# a) Load the digits.rds file
# b) Plot some of the digits using the functions I provided
#    (digits.plot() and digits.plot.multiple()).
# c) Fit an SVM model on the training data
# d) Use your model to make predictions for the test set
# e) Generate a confusion matrix for your model
# f) Use the caret::multiClassSummary() function to evaluate your model
digits <- readRDS("~/Seminar-20190219/digits.rds")
svmfitter =  svm(class~., data = digits$train, kernel="poly",scale=FALSE)

digits.plot.multiple(digits$train)
pre = predict(svmfitter, newdata = digits$test)
table(true = digits$test$class, pred=predict(svmfitter,digits$test))


df = data.frame(obs = digits$test$class, pred=predict(svmfitter,digits$test))
multiClassSummary(data = df, lev = levels(digits$test$class))

######################################
# Exercise 2
# a) Load the sat.rds file
# b) Plot some of the satellite images using the functions I provided
#    (sat.plot() and sat.plot.multiple()).
# c) Fit an SVM model on the training data
# d) Use your model to make predictions for the test set
# e) Generate a confusion matrix for your model
# f) Use the caret::multiClassSummary() function to evaluate your model
sat = readRDS("~/Seminar-20190219/sat.rds")

svmfitter2 =  svm(class~., data = sat$train, kernel="radial",scale=FALSE, gamma = 0.15)

arguments_to_try = list("cost" = c( 0.01, 0.1, 0.2, 1, 10, 100))
tuned_models = tune(svm,class ~ ., data=sat$train, kernel = "radial",
                    ranges = arguments_to_try)
bestmod = tuned_models$best.model

pre2 = predict(svmfitter2,newdata = sat$test)

sat.plot.multiple(sat$train,2)

table(true = sat$test$class, pred=predict(svmfitter2,sat$test))

df2 = data.frame(obs = sat$test$class, pred=predict(svmfitter2,sat$test))
multiClassSummary(data = df2, lev = levels(sat$test$class))

