set.seed(27)
library(ISLR)
library(tree)
library(randomForest)

data = ISLR::Carseats
dim(data)
str(data)
summary(data)


hist(data$Sales)
median(data$Sales)
high = as.factor(data$Sales > 8)
data$Sales = high

test_indices = sample(nrow(data),0.3*nrow(data),replace=FALSE)
training_set = data[-test_indices,]
test_set = data[test_indices,]
nrow(training_set) + nrow(test_set) == nrow(data)

tree = tree(Sales ~ .,training_set)
summary(tree)

plot(tree)


text(tree, pretty=0)

test_predictions = predict(tree,test_set,type="class")
confusion_matrix = table(test_set$Sales,test_predictions)

(confusion_matrix[1,1] + confusion_matrix[2,2])/sum(confusion_matrix)

#pruning the tree

# Pruned to 3 leaves
ptree = prune.tree(tree,best= 3)
plot(ptree)
text(ptree, pretty=0)

# Evaluate
test_predictions2 = predict(ptree, test_set)
rmse2 = mean((test_set$Sales - test_predictions2)^2)^0.5

# Best based on cv
cv_results = cv.tree(tree, FUN = prune.tree)

bestsize = cv_results$size[order(cv_results$dev,decreasing = FALSE)][1]
besttree = prune.tree(tree, best = bestsize)


plot(cv_results$size, cv_results$dev, type = "b")

plot(cv_results$k, cv_results$dev, type = "b")

# Evaluate
test_predictions3 = predict(besttree, test_set)
rmse3 =  mean((test_set$Sales - test_predictions3)^2)^0.5

