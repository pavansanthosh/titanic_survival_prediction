#JAI SRI RAM
#importing the dataset
training_set=read.csv('train.csv')
head(training_set)


#taking care of missing values of dataset
training_set$Age=ifelse(is.na(training_set$Age),
                   ave(training_set$Age,FUN = function(x) mean(x,na.rm=T)),
                   training_set$Age)
training_set$Pclass=ifelse(is.na(training_set$Pclass),
                      ave(dataset$Pclass,FUN= function(x) mean(x,na.rm=T)),
                      training_set$Pclass)

head(training_set)

#importing the test set
test_set=read.csv("test.csv")
head(test_set)


#taking care of missing values
test_set$Age=ifelse(is.na(test_set$Age),
                    ave(test_set$Age,FUN = function(x) mean(x,na.rm=T)),
                    test_set$Age)
test_set$Pclass=ifelse(is.na(test_set$Pclass),
                       ave(test_set$Pclass,FUN= function(x) mean(x,na.rm=T)),
                       test_set$Pclass)

head(test_set)




######applying PCA

library(caret)
#install.packages('caret')
options(repos='http://cran.rstudio.com/')
pca= preProcess(training_set[-2],method='pca',pcaComp = 2)
training_set=predict(pca,training_set)
training_set=training_set[c(7,8,1)]
test_set=predict(pca,test_set)
test_set=test_set[c(6,7)]


# Fitting classifier to the Training set
library(e1071)
classifier=svm(formula=Survived~.,
               data = training_set,
               type='C-classification',
               kernel='sigmoid')


#predicting the test set results
y_pred=predict(classifier,newdata=test_set)
y_pred

result=as.data.frame(y_pred,row.names=NULL)
head(result)


summary(classifier)

###############################################




# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

