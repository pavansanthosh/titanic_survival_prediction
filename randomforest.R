#jai sri ram
#importing dataset
dataset=read.csv('train.csv')
head(dataset)
dataset=dataset[2:6]
head(dataset)

#deleting the UNREQUIRED ATTRIBUTE
dataset=dataset[-3]
head(dataset)

#encoding the categorical of dataset
dataset$Survived=factor(dataset$Survived,levels=c(0,1))
head(dataset$Survived)

dataset$Sex=factor(dataset$Sex,levels=c("male","female"),labels=c(1,2))
head(dataset$Sex)

#taking care of missing values of dataset
dataset$Age=ifelse(is.na(dataset$Age),
                   ave(dataset$Age,FUN = function(x) mean(x,na.rm=T)),
                   dataset$Age)
dataset$Pclass=ifelse(is.na(dataset$Pclass),
                      ave(dataset$Pclass,FUN= function(x) mean(x,na.rm=T)),
                      dataset$Pclass)

head(dataset)


################################################################################################
#importing the test set
test_set=read.csv("test.csv")
head(test_set)
test_set=test_set[2:5]
head(test_set)
test_set=test_set[-2]
head(test_set)

#encoding the categorical of test_Set

test_set$Sex=factor(test_set$Sex,levels=c("male","female"),labels=c(1,2))
head(test_set$Sex)

#taking care of missing values
test_set$Age=ifelse(is.na(test_set$Age),
                    ave(test_set$Age,FUN = function(x) mean(x,na.rm=T)),
                    test_set$Age)
test_set$Pclass=ifelse(is.na(test_set$Pclass),
                       ave(test_set$Pclass,FUN= function(x) mean(x,na.rm=T)),
                       test_set$Pclass)

head(test_set)


##############################################################################################
#importing result dataset
result=read.csv("gender_submission.csv")
result=result[-1]
head(result)

#encoding the categorical
result$Survived=factor(result$Survived,levels=c(0,1))
head(result$Survived)



#fitting RANDOM FOREST classifier
library(randomForest)
classifier=randomForest(x=dataset[-1],y=dataset$Survived,ntree=40)


#predicting the test set results
y_pred=predict(classifier,newdata=test_set)
y_pred


#buidling confusion matrix
cm=table(result$Survived,y_pred)
cm