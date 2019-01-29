# install.packages("foreign")
library(foreign)
weather = read.arff("~/datamining cw/weather.numeric.arff")

colnames(weather)
rownames(weather)
names(weather)

# sizes and subsetting
dim(weather)
nrow(weather)
ncol(weather)
head(weather)
tail(weather)
weather[1,]
weather[,1]
weather[,c(1,3)]
weather[-c(2,3),]
weather$temperature
weather[,'temperature']
weather[,"play"]
# weather[,-"play"] # This is an error. Understand why
weather[,c("outlook", "play")]
weather[,1:3]
# weather[,1 3] # This is an error, understand why
weather[,1,drop=FALSE]
summary(weather)
str(weather)

# attaching
# temperature # will error
attach(weather)
temperature

plot(weather$temperature,weather$humidity)
plot(temperature,humidity)
pairs(weather)
plot(temperature,humidity,col=play)



preds=weather[,-5]
play0=weather[,"play"]
play0

# explore data types: factors, logical, integer
playl=(play0=="yes")
play0=playl

# install and run Naïve Bayes
# install.packages("naivebayes")
library(naivebayes)
nb=naive_bayes(x=preds,y=play0)
predict(nb,newdata=preds)
predict(nb,newdata=preds,type="prob")
play1=(predict(nb,newdata=preds,type="prob"))[,2]
play0
play1
as.integer(play1) # <- Note that this is NOT what we would want!
play1>0.5
plot(play0,play1,xlab="actual",ylab="predicted")
plot(x=jitter(as.integer(play0)),y=jitter(play1,amount=0.02),xlab="actual",
     ylab="predicted")
# jittering the plot sometimes helps for visualisation purposes

# Now let's also look at RMS error and a confusion matrix.
RMS <- function(num) sqrt(mean(num^2));
RMS(play0-play1)
RMS(play0-(play1>0.5))
library(ModelMetrics)
confusionMatrix(actual=play0,predicted=(play1>0.5))
confusionMatrix(actual=play0,predicted=play1,cutoff=0.5)
confusionMatrix(actual=play0,predicted=play1)


preds2=preds
preds2$rnd1=runif(14,0,1)
head(preds2)
nb2=naive_bayes(x=preds2,y=play0,laplace = 1)
play2=(predict(nb2,newdata=preds2,type="prob"))[,2]
matplot(x=jitter(as.integer(play0),factor=0.2),
        cbind(jitter(play1,amount=0.0001),jitter(play2,amount=0.0001)),
        xlab="actual",ylab="predicted",pch=19)
confusionMatrix(actual=play0,predicted=play1)
confusionMatrix(actual=play0,predicted=play2)
RMS(play0-play1)
RMS(play0-play2)
plot(play2~play1)
# Add another noise predictor.
preds2$rnd2=runif(14,0,1)
nb2=naive_bayes(x=preds2,y=play0)
play2=(predict(nb2,newdata=preds2,type="prob"))[,2]
confusionMatrix(actual=play0,predicted=play2)
RMS(play0-play2)
plot(play2~play1)
