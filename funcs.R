library(readr)
library(e1071)
library(ggplot2)
library(jpeg)
library(caret)

digits.plot = function(data,model=NULL) {
  imageSize = 28
  length = imageSize*imageSize
  row = ceiling(1:length / imageSize)
  col = (((1:length) - 1) %% imageSize) + 1
  long = data.frame(y=row,x=col,val=as.numeric(data[,-1]))
  long$true = paste0("TRUE: ",data$class)
  long$predicted = "PRED: -"
  if (!is.null(model)) {
    predicted = predict(model,data)
    long$predicted = paste0("PRED: ",predicted[1])
  }
  ggplot(long,aes(x,-y,fill=val)) + geom_tile() + facet_wrap(~true + predicted) +
    theme(strip.text.x = element_text(size = 20))
}

digits.plot.multiple = function(data,numberOfImages=4,model=NULL) {
  imageSize = 28
  df = NULL  
  for (i in 1:numberOfImages) {
    randomIndex = sample(nrow(data),1)
    length = imageSize*imageSize
    row = ceiling(1:length / imageSize)
    col = (((1:length) - 1) %% imageSize) + 1
    long = data.frame(instance = i, y=row,x=col,val=as.numeric(data[randomIndex,-1]))
    long$true = paste0("TRUE: ",data[randomIndex,"class"])
    long$predicted = "PRED: -"
    if (!is.null(model)) {
      predicted = predict(model,data[randomIndex,])
      long$predicted = paste0("PRED: ",predicted[1])
    }
    df = rbind(df,long)
  }
  ggplot(df,aes(x,-y,fill=val)) + geom_tile() + facet_wrap(~instance + true + predicted) +
    theme(strip.text.x = element_text(size = 12))
}


sat.plot = function(data,model=NULL) {
  length = 32*32
  row = ceiling(1:length / 32)
  col = (((1:length) - 1) %% 32) + 1
  long = data.frame(y=row,x=col,val=as.numeric(data[,-1]))
  long$true = paste0("TRUE: ",data$class)
  long$predicted = "PRED: -"
  if (!is.null(model)) {
    predicted = predict(model,data)
    long$predicted = paste0("PRED: ",predicted[1])
  }
  ggplot(long,aes(x,-y,fill=val)) + geom_tile() + scale_fill_gradient(low = "#333333", high = "#dddddd") + facet_wrap(~true + predicted) +
    theme(strip.text.x = element_text(size = 20))
}

sat.plot.multiple = function(data,numberOfImages=4,model=NULL) {
  imageSize = 32
  df = NULL  
  for (i in 1:numberOfImages) {
    randomIndex = sample(nrow(data),1)
    length = imageSize*imageSize
    row = ceiling(1:length / imageSize)
    col = (((1:length) - 1) %% imageSize) + 1
    long = data.frame(instance = i, y=row,x=col,val=as.numeric(data[randomIndex,-1]))
    long$true = paste0("TRUE: ",data[randomIndex,"class"])
    long$predicted = "PRED: -"
    if (!is.null(model)) {
      predicted = predict(model,data[randomIndex,])
      long$predicted = paste0("PRED: ",predicted[1])
    }
    df = rbind(df,long)
  }
  ggplot(df,aes(x,-y,fill=val)) + geom_tile() + scale_fill_gradient(low = "#333333", high = "#dddddd") + facet_wrap(~instance + true + predicted) +
    theme(strip.text.x = element_text(size = 12))
}
