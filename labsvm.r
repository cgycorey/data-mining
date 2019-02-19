library(e1071)
set.seed(47)
group1 = matrix(rnorm(40,mean=0),ncol=2)
group2 = matrix(rnorm(40,mean=1.5),ncol=2)
x = rbind(group1,group2)
y = c(rep(-1,20), rep(1,20))
plot(x, col=y+3,asp=1)
data = data.frame(x=x, y=as.factor(y))

svmfit=svm(y~., data=data, kernel="linear",scale=FALSE)
plot(svmfit, data)
table(true = data$y, pred=predict(svmfit,data[,1:2]))

svmfit=svm(y~., data=data, kernel="linear",cost=10,scale=FALSE)
plot(svmfit, data)
table(true = data$y, pred=predict(svmfit,data[,1:2]))

arguments_to_try = list("cost" = c(0.001, 0.01, 0.1, 1, 10, 100))
tuned_models = tune(svm,y ~ ., data=data, kernel = "linear",
                    ranges = arguments_to_try)
summary(tuned_models)

best_model = tuned_models$best.model
plot(best_model, data)
table(true = data$y, pred=predict(best_model,data[,1:2]))

set.seed(7)
# I'm going to create a ring of radius r
r = 2
x1 = r * cos(seq(0,2*pi,length.out = 100))
x2 = r * sin(seq(0,2*pi,length.out = 100))
circle = data.frame(x1=x1,x2=x2)
# Add some noise:
circle = circle + rnorm(200,0,0.3)
# Create a central cluster:
x1 = rnorm(50,0,0.5)
x2 = rnorm(50,0,0.5)
cluster = data.frame(x1=x1,x2=x2)
# combine into a single dataset,
# make circle class 1, and cluster
# class -1:
circle_data = rbind(circle,cluster)
y = c(rep(1,nrow(circle)),rep(-1,nrow(cluster)))
circle_data = cbind(circle_data,y=as.factor(y))
plot(circle_data[,1:2],col=y+3,asp=1)

svmfit = svm(y~. ,data=circle_data, kernel="radial",scale=FALSE)
plot(svmfit, circle_data)
table(true = circle_data$y, pred=predict(svmfit,circle_data[,1:2]))
