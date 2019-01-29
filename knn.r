set.seed(27)
library(ISLR)
library(class)

data = ISLR::Caravan
dim(data)
str(data)
summary(data)
?Caravan
table(data$Purchase)
prop.table(table(data$Purchase))


output_index = which(colnames(data) == "Purchase")


x = data[,-output_index]
y = data[,output_index]
test_indices = sample(1:nrow(data),
                      size = round(0.2*nrow(data)),
                      replace = FALSE)

train_x = x[-test_indices,]
test_x = x[test_indices,]
train_y = y[-test_indices]
test_y = y[test_indices]

nrow(train_x) + nrow(test_x) == nrow(data)
length(train_y) + length(test_y) == nrow(data)


std_x = scale(x)
# Thought: Technically, scaling both the training data
# and the test data together is not the right thing
# to do. For now it's not a huge deal, but we'll
# consider this later on. What might be the problem?
# we need to use the training data's std and mean to standarise the testing data
std_train_x = std_x[-test_indices,]
std_test_x = std_x[test_indices,]


predicted_knn1 = knn( std_train_x , std_test_x , train_y , k =1)
confusion_matrix = table(predicted_knn1, test_y)

confusion_matrix = table(predicted_knn1, test_y)
true_neg = confusion_matrix["No","No"]
true_pos = confusion_matrix["Yes","Yes"]
false_pos = confusion_matrix["Yes","No"]
false_neg = confusion_matrix["No","Yes"]
misclassification_rate = mean(predicted_knn1 != test_y)
misclassification_rate == (false_pos + false_neg) / sum(confusion_matrix)
precision = true_pos / (true_pos + false_pos)
recall = true_pos / (true_pos + false_neg)

#train vs train don't do this in practice!! duplicate roles makes our classifier confused
set.seed(7)
testindx = sample(1:nrow(std_train_x),
                      size = 100,
                      replace = FALSE);

selfTestX = std_train_x[testindx,]
selfTestY= train_y[testindx]
nrow(selfTestX) == 100
knntrain = knn(std_train_x,std_train_x, train_y, k = 1)

confusion_matrixtt = table(knntrain, train_y)

all_no = rep("No",length(test_y))
no_confusion_matrix = table(all_no, test_y,dnn = c("No","Yes"))
no_confusion_matrix = rbind(no_confusion_matrix,"Yes"=c(0,0))
# (Had to add empty row because no predicted yes's)
no_true_neg = no_confusion_matrix["No","No"]
no_true_pos = no_confusion_matrix["Yes","Yes"]
no_false_pos = no_confusion_matrix["Yes","No"]
no_false_neg = no_confusion_matrix["No","Yes"]
no_misclassification_rate = mean(all_no != test_y)
no_misclassification_rate == (no_false_pos + no_false_neg) / sum(no_confusion_matrix)
no_precision = no_true_pos / (no_true_pos + no_false_pos)
no_recall = no_true_pos / (no_true_pos + no_false_neg)



comparison = data.frame("misclassification" = c(misclassification_rate,no_misclassification_rate),
                        "precision" = c(precision,no_precision),
                        "recall" = c(recall,no_recall))
rownames(comparison) = c("knn1","always_no")
comparison


getMetrics = function(predicted_classes,true_classes) {
  confusion_matrix = table(predicted_classes,true_classes)
  true_neg = confusion_matrix["No","No"]
  true_pos = confusion_matrix["Yes","Yes"]
  false_pos = confusion_matrix["Yes","No"]
  false_neg = confusion_matrix["No","Yes"]
  misclassification_rate = mean(predicted_classes != true_classes)
  precision = true_pos / (true_pos + false_pos)
  recall = true_pos / (true_pos + false_neg)
  return(c("misclassification" = misclassification_rate,
           "precision" = precision,
           "recall" = recall))
}


#changing k 

predicted = knn( std_train_x , std_test_x , train_y , k =1)
getMetrics(predicted,test_y)
predicted = knn( std_train_x , std_test_x , train_y , k =2)
getMetrics(predicted,test_y)
predicted = knn( std_train_x , std_test_x , train_y , k =3)
getMetrics(predicted,test_y)
predicted = knn( std_train_x , std_test_x , train_y , k =4)
getMetrics(predicted,test_y)
predicted = knn( std_train_x , std_test_x , train_y , k =5)
getMetrics(predicted,test_y)
