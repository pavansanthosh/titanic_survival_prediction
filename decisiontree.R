#jai sri ram
dataset=read.csv('train.csv')
head(dataset)
dataset=dataset[1:6]
head(dataset)

#encoding the categorical
dataset$Survived=factor(dataset$Survived,levels=c(0,1))
head(dataset$Survived)
#encoding sex
dataset$Sex=factor(dataset$Sex,levels=c("male","female"),labels=c(1,2))
head(dataset$Sex)

#deleting the UNREQUIRED ATTRIBUTE
dataset=dataset[-4]
head(dataset)

#basic cal on dataset
length(dataset)
nrow(dataset)

#taking care of missing values
dataset$Age=ifelse(is.na(dataset$Age),
                   ave(dataset$Age,FUN = function(x) mean(x,na.rm=T)),
                   dataset$Age)
dataset$Pclass=ifelse(is.na(dataset$Pclass),
                      ave(dataset$Pclass,FUN= function(x) mean(x,na.rm=T)),
                      dataset$Pclass)

head(dataset)

# #splitting the dataset into training and test set
# library(caTools)
# set.seed(123)
# split=sample.split(dataset$Survived,SplitRatio = 0.2)
# training_set=subset(dataset,split==FALSE)
# test_set=subset(dataset,split==TRUE)

# head(training_set)

#fitting classifier
library(rpart)
classifier=rpart(formula=Survived~.,
                 data=dataset)

test_set=read.csv("test.csv")
head(test_set)
test_set=test_set[1:5]
head(test_set)
test_set=test_set[-3]
head(test_set)



#predicting the test set results
y_pred=predict(classifier,data=test_set,type = 'class')
head(y_pred)

#importing result dataset
result=read.csv("gender_submission.csv")
result=result[-1]
head(result)
# y_pred$PID=test_set$PassengerId
# head(y_pred)

#buidling confusion matrix
cm=table(result,y_pred)
cm