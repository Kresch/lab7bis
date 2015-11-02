#which combination of parties(voting) explains the size of a swedish city?

#From the dataset of votes we have a column of Rostb. (Allowed to vote in each city.). Can this
#size be explained by fitting an LM with the partysizes(in percentage) as covariates?

#extract the data we need:
library(lattice)
library(caret)
divide_data<-function(theData){
  data <- data.frame(theData[,unlist(lapply(colnames(theData),function(y) substr(y,start=nchar(y)-2,stop=nchar(y))))=="tal"])
  data<-data.frame(data,theData$Rostb)
  colnames(data)<-c(colnames(theData)[unlist(lapply(colnames(theData),function(y) substr(y,start=nchar(y)-2,stop=nchar(y))))=="tal"],"Rostb")
  set.seed(12345)
  in_train <- createDataPartition(y = data$Rostb,p=0.75,list = FALSE)
  train <- theData[in_train,]
  test<-theData[-in_train,]
  out<-list(train=train,test=test)
  #how to actually get the coefficients???
  return(out)
}
data<-divide_data(theData)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(12345)
model1<-train(data$train$Rostb~.,data=data$train,method="lm",trControl=fitControl)
set.seed(12345)
model2<-train(data$train$Rostb~.,data=data$train,method="leapForward",trControl=fitControl)
