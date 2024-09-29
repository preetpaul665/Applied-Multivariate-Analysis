# Practical Problem Set IV

# Problem 1

## Load the library "rpart" for tree classification problem

library(rpart)
help(rpart)

# Loading the built-in dataset "kyphosis" from "rpart" library

data = kyphosis
View(data)

# Loading the packages "caret" & "e1071"

library(caret)
library(e1071)

# Train - test split (67 - 33 ratio)

set.seed(100)

?createDataPartition()
index <- createDataPartition(data$Age, p = 0.67, list=F)
trainset <- data[index, ]
testset <- data[-index, ]

dim(trainset)
dim(testset)

# Modelling the classification tree 

model <- rpart(Kyphosis~. , data = trainset, method = "class")
model
plot(model)
text(model, use.n = TRUE)

# Prediction on the training set

prediction1 <- predict(model, trainset)


# Prediction on the testing set

prediction2 <- predict(model,testset)


# Creating a confusion matrix for train set

absent1 <- prediction1[,1] > prediction1[,2]  

##Taking only those values of absent which has more probability than present

pref1 = ifelse(absent1 == TRUE, "absent","present")
tab1 <- table(pref1,trainset[,1])
tab1
confusionMatrix(tab1)

### Obtaining Accuracy as a Percentage

cat("Accuracy of the Decision Tree model for train set is",confusionMatrix(tab1)$overall['Accuracy']*100) 


# Creating a confusion matrix for test set

absent2 <- prediction2[,1] > prediction2[,2]  

##Taking only those values of absent which has more probability than present

pref2 = ifelse(absent2 == TRUE, "absent","present")
tab2 <- table(pref2,testset[,1])
tab2
confusionMatrix(tab2)

### Obtaining Accuracy as a Percentage

cat("Accuracy of the Decision Tree model for test set is",confusionMatrix(tab2)$overall['Accuracy']*100) 


# Loading the package "nnet" & "neuralnet" for MLP

library(nnet)
library(neuralnet)
?nnet()
?neuralnet()

# Fitting the MLP Model
MLP <- nnet(Kyphosis~., data = trainset, size = 2)
MLP
summary(MLP)

# Plotting the MLP Model
MLP1 <- neuralnet(Kyphosis~., data = trainset, hidden = 2)
plot(MLP1)

# Predicting the MLP model for train set

MLP_train <- predict(MLP, trainset, type = "class")

# Creating a confusion matrix for train set

conf_train <- confusionMatrix(as.factor(MLP_train), trainset$Kyphosis)
conf_train

### Obtaining Accuracy as a Percentage

cat("Accuracy of the MLP model for train set is",conf_train$overall['Accuracy']*100) 

# Predicting the MLP model for test set

MLP_test <- predict(MLP, testset, type = "class")

# Creating a confusion matrix for test set

conf_test <- confusionMatrix(as.factor(MLP_test), testset$Kyphosis)
conf_test

### Obtaining Accuracy as a Percentage

cat("Accuracy of the MLP model for train set is",conf_test$overall['Accuracy']*100) 


# Loading the package "e1071" for svm

library(e1071)
help(svm)
set.seed(100)

# Fitting Support Vector Machine Model with linear kernel

SVM1=svm(Kyphosis~., data = trainset, kernel = "linear", type = "C-classification")
SVM1
summary(SVM1)


## Plotting the SVM Model with linear kernel

plot(SVM1, trainset, formula = Age~Number)
plot(SVM1, trainset, formula = Number~Start)

# Predicting the SVM model for train set

SVM_train <- predict(SVM1, trainset)
SVM_train

# Creating a confusion matrix for train set

confusion1 <- confusionMatrix(SVM_train, trainset$Kyphosis)
confusion1

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with linear kernel for train set is",confusion1$overall['Accuracy']*100) 

# Predicting the SVM model for test set

SVM_test <- predict(SVM1, testset)
SVM_test

# Creating a confusion matrix for test set

confusion2 <- confusionMatrix(SVM_test, testset$Kyphosis)
confusion2

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with linear kernel for test set is",confusion2$overall['Accuracy']*100) 

# Fitting Support Vector Machine Model with RBF kernel

SVM2=svm(Kyphosis~., data = trainset, kernel = "radial", type = "C-classification")
SVM2
summary(SVM2)

## Plotting the SVM Model with RBF kernel

plot(SVM2, trainset, formula = Age~Number)
plot(SVM2, trainset, formula = Number~Start)

# Predicting the  model for train set

SVM_train2 <- predict(SVM2, trainset)
SVM_train2

# Creating a confusion matrix for train set

confusion3 <- confusionMatrix(SVM_train2, trainset$Kyphosis)
confusion3

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with RBF kernel for train set is",confusion3$overall['Accuracy']*100) 

# Predicting the MLP model for test set

SVM_test2 <- predict(SVM2, testset)
SVM_test2

# Creating a confusion matrix for test set

confusion4 <- confusionMatrix(SVM_test2, testset$Kyphosis)
confusion4

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with RBF kernel for test set is",confusion4$overall['Accuracy']*100) 


########################################################################################

# Problem 2

# Import the dataset

wine <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\Programming\\winequality-white.csv", sep=";")

View(wine)
dim(wine)  # dimension of the dataset

## Checking for missing values
which(is.na(wine))

##Creating 3 classes with approx. equal #obs. based on quality

wine$quality <- cut(wine$quality, breaks = c(0,3,6,9), labels = c("poor","medium","good"))


# Train - test split (67 - 33 ratio)

set.seed(100)

index1 <- sample(nrow(wine), 3265)
trainst <- wine[index1, ]
testst <- wine[-index1, ]

dim(trainst)   # Dimension of the train set
dim(testst)    # Dimension of the test set

# Modelling the classification tree without pruning

set.seed(100)
library(rpart)

model2 <- rpart(quality~., data = trainst, method = "class")
model2
plot(model2)
text(model2, use.n = TRUE)

# Displaying a table of optimal prunings based on a complexity parameter

printcp(model2)
plotcp(model2)  ## plotting cp vs. cross-validation error

## From the table we can clearly see that, cp=0.011 has the lowest error and standard deviation

# Pruning the unpruned decision tree

model_prune <- prune(model2, cp = 0.011)
plot(model_prune)
text(model_prune, use.n = TRUE)

# Plotting using "rpart.plot" library

library(rpart.plot)
prp(model_prune, extra = 1)

# Prediction on the training set

pred_train_CART<- predict(model_prune, trainst, type = "class")

# Creating a confusion matrix for train set

library(caret)

confusion_train_CART <- confusionMatrix(pred_train_CART, as.factor(trainst$quality))
confusion_train_CART

### Obtaining Accuracy as a Percentage

cat("Accuracy of the Decision Tree model for train set is",confusion_train_CART$overall['Accuracy']*100) 


# Prediction on the test set

pred_test_CART <- predict(model_prune, testst, type = "class")

# Creating a confusion matrix for test set

confusion_test_CART <- confusionMatrix(pred_test_CART, as.factor(testst$quality))
confusion_test_CART

### Obtaining Accuracy as a Percentage

cat("Accuracy of the Decision Tree model for test set is",confusion_test_CART$overall['Accuracy']*100) 

# MLP with one hidden layer containing 3 units

library(nnet)
library(neuralnet)
set.seed(100)

MLP_h3 <- nnet(quality~., data = trainst, size = 3)
MLP_h3
summary(MLP_h3)

# Plotting the MLP model

plot(neuralnet(quality~., data = trainst, hidden = 3))

# Prediction of the train set

MLP_h3_train <- as.factor(predict(MLP_h3, trainst, type = "class"))

# Creating a confusion matrix for train set

confusionMLP3 <- confusionMatrix(MLP_h3_train, trainst$quality)
confusionMLP3

### Obtaining Accuracy as a Percentage

cat("Accuracy of the MLP model containing 3 units for train set is",confusionMLP3$overall['Accuracy']*100) 

# Prediction of the test set

MLP_h3_test <- as.factor(predict(MLP_h3, testst, type = "class"))

# Creating a confusion matrix for test set

confusionMLP3_test <- confusionMatrix(MLP_h3_test, testst$quality)
confusionMLP3_test

### Obtaining Accuracy as a Percentage

cat("Accuracy of the MLP model containing 3 units for test set is",confusionMLP3_test$overall['Accuracy']*100) 

# MLP with one hidden layer containing 4 units

library(nnet)
library(neuralnet)
set.seed(100)

MLP_h4 <- nnet(quality~., data = trainst, size = 4)
MLP_h4
summary(MLP_h4)

# Prediction of the train set

MLP_h4_train <- as.factor(predict(MLP_h4, trainst, type = "class"))

# Creating a confusion matrix for train set

confusionMLP4 <- confusionMatrix(MLP_h3_train, trainst$quality)
confusionMLP4

### Obtaining Accuracy as a Percentage

cat("Accuracy of the MLP model containing 4 units for train set is",confusionMLP4$overall['Accuracy']*100) 

# Prediction of the test set

MLP_h4_test <- as.factor(predict(MLP_h4, testst, type = "class"))

# Creating a confusion matrix for test set

confusionMLP4_test <- confusionMatrix(MLP_h3_test, testst$quality)
confusionMLP4_test

### Obtaining Accuracy as a Percentage

cat("Accuracy of the MLP model containing 4 units for train set is",confusionMLP4_test$overall['Accuracy']*100) 

# Loading the package "e1071" for svm

library(e1071)
set.seed(100)

# Fitting Support Vector Machine Model with linear kernel

SVM_linear=svm(quality~., data = trainst, kernel = "linear", type = "C-classification")
SVM_linear
summary(SVM_linear)

# Predicting the SVM model for train set

predict_train_linear <- predict(SVM_linear, trainst)

# Creating a confusion matrix for train set

confusion_train_SVM <- confusionMatrix(predict_train_linear, trainst$quality)
confusion_train_SVM

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with linear kernel for train set is",confusion_train_SVM$overall['Accuracy']*100) 

# Predicting the SVM model for test set

predict_test_linear <- predict(SVM_linear, testst)

# Creating a confusion matrix for test set

confusion_test_SVM <- confusionMatrix(predict_test_linear, testst$quality)
confusion_test_SVM

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with linear kernel for test set is",confusion_test_SVM$overall['Accuracy']*100) 

# Fitting Support Vector Machine Model with RBF kernel

SVM_RBF=svm(quality~., data = trainst, kernel = "radial", type = "C-classification")
SVM_RBF
summary(SVM_RBF)

# Predicting the SVM model for train set

predict_train_RBF <- predict(SVM_RBF, trainst)

# Creating a confusion matrix for train set

confusion_train_RBF <- confusionMatrix(predict_train_RBF, trainst$quality)
confusion_train_RBF

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with RBF kernel for train set is",confusion_train_RBF$overall['Accuracy']*100) 

# Predicting the SVM model for test set

predict_test_RBF <- predict(SVM_RBF, testst)

# Creating a confusion matrix for test set

confusion_test_RBF <- confusionMatrix(predict_test_RBF, testst$quality)
confusion_test_RBF

### Obtaining Accuracy as a Percentage

cat("Accuracy of the SVM model with RBF kernel for test set is",confusion_test_RBF$overall['Accuracy']*100) 













