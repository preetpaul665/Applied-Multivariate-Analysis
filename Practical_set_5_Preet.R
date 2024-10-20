# Practical Problem Set V

# Problem 1

diabetes <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\Programming\\diabetes.csv")
View(diabetes)
dim(diabetes)  # dimension of the dataset
summary(diabetes)
# Finding the missing values

which(is.na(diabetes))

library(class)
help(knn.cv)   # knn.cv() stands for k-Nearest Neighbour Cross-Validatory Classification
help(condense)




# Create train - test split (2:1 ratio)


library(caret)

set.seed(519)
index <- createDataPartition(diabetes$Pregnancies, p = (2/3), list = FALSE)
train <- diabetes[index, ]
test <- diabetes[-index, ]
dim(train)
dim(test)

# Part (a)-(i)

# kNN classification 

## Choosing the optimal k for k-NN classification

accuracy <- NULL

for (i in 1:100)
{
  model.train = knn.cv(train, cl = train$Outcome, k = i)
  accuracy = c(accuracy, confusionMatrix(model.train, as.factor(train$Outcome))$overall['Accuracy']*100)
}

as.vector(accuracy)
max(as.vector(accuracy))
which.max(as.vector(accuracy))  # Hence, k = 14 has the maximum accuracy in case of k-NN rule

# Fitting k-NN rule using the optimal k through leave-one-out cross-validation 

model.knn = knn.cv(train, cl = train$Outcome, k = which.max(as.vector(accuracy)))

## Confusion Matrix and Accuracy

confusionMatrix(model.knn, as.factor(train$Outcome))
cat("The Accuracy of the k-NN rule using optimal k is ", confusionMatrix(model.knn, as.factor(train$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

cat("Precision of the k-NN rule is", confusionMatrix(model.knn, as.factor(train$Outcome))$byClass['Precision']*100)
cat("Recall of the k-NN rule is", confusionMatrix(model.knn, as.factor(train$Outcome))$byClass['Recall']*100)
cat("F1 - score of the k-NN rule is", confusionMatrix(model.knn, as.factor(train$Outcome))$byClass['F1']*100)


# Part (a)-(ii)
set.seed(519)
keep <- condense(as.matrix(train), cl = train$Outcome)
train2 <- as.matrix(train)
model <- knn.cv(train2[keep], cl = as.vector(train$Outcome)[keep])

## Confusion Matrix and Accuracy

confusionMatrix(model, as.factor(train$Outcome[keep]))
cat("The Accuracy of the k-NN rule using condensed training set is ", confusionMatrix(model, as.factor(train$Outcome[keep]))$overall['Accuracy']*100)

## Precision, Recall and F1-score

cat("Precision of the k-NN rule using condensed training set is ", confusionMatrix(model, as.factor(train$Outcome[keep]))$byClass['Precision']*100)
cat("Recall of the k-NN rule using condensed training set is ", confusionMatrix(model, as.factor(train$Outcome[keep]))$byClass['Recall']*100)
cat("F1-score of the k-NN rule using condensed training set is ", confusionMatrix(model, as.factor(train$Outcome[keep]))$byClass['F1']*100)

# Part (a)-(iii)

# Getting all the functions in "class" package
ls("package:class")

help(multiedit)

fit <- multiedit(train, cl = train$Outcome, k = 10)
train_ME <- train[fit, , drop = FALSE]
dim(train_ME)
knn_ME <- knn(train_ME[,-9], train[,-9], cl = train_ME$Outcome, k = 10)
knn_ME

# Part (b)

library(MASS)
help(lda)

model.lda <- lda(Outcome~., data = train)

## For training set

predict.lda.train <- predict(model.lda, data = train)

## Confusion Matrix and Accuracy

confusionMatrix(predict.lda.train$class, as.factor(train$Outcome))
cat("Accuracy of the LDA model for train set is ", confusionMatrix(predict.lda.train$class, as.factor(train$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

cat("Precision of the LDA model for train set is ", confusionMatrix(predict.lda.train$class, as.factor(train$Outcome))$byClass['Precision']*100)
cat("Recall of the LDA model for train set is ", confusionMatrix(predict.lda.train$class, as.factor(train$Outcome))$byClass['Recall']*100)
cat("F1-score of the LDA model for train set is ", confusionMatrix(predict.lda.train$class, as.factor(train$Outcome))$byClass['F1']*100)

## For testing set

model.test <- lda(Outcome~., data = test)
predict.lda.test <- predict(model.test, data = test)

## Confusion Matrix and Accuracy

confusionMatrix(predict.lda.test$class, as.factor(test$Outcome))
cat("Accuracy of the LDA model for test set is", confusionMatrix(predict.lda.test$class, as.factor(test$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.lda.test$class, as.factor(test$Outcome))$byClass
cat("Precision of the LDA model for test set is ",confusionMatrix(predict.lda.test$class, as.factor(test$Outcome))$byClass['Precision']*100)
cat("Recall of the LDA model for test set is ",confusionMatrix(predict.lda.test$class, as.factor(test$Outcome))$byClass['Recall']*100)
cat("F1-score of the LDA model for test set is ",confusionMatrix(predict.lda.test$class, as.factor(test$Outcome))$byClass['F1']*100)

# Part (c)

library(MASS)
help(qda)

## For training set

model.qda <- qda(Outcome~., data = train)

## Prediction on the training set

predict.qda.train <- predict(model.qda, data = train)

## Confusion Matrix and Accuracy

confusionMatrix(predict.qda.train$class, as.factor(train$Outcome))
cat("Accuracy of the LDA model for train set is ", confusionMatrix(predict.qda.train$class, as.factor(train$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

cat("Precision of the QDA model for train set is ", confusionMatrix(predict.qda.train$class, as.factor(train$Outcome))$byClass['Precision']*100)
cat("Recall of the QDA model for train set is ", confusionMatrix(predict.qda.train$class, as.factor(train$Outcome))$byClass['Recall']*100)
cat("F1-score of the QDA model for train set is ", confusionMatrix(predict.qda.train$class, as.factor(train$Outcome))$byClass['F1']*100)

## For testing set

model.qda.test <- qda(Outcome~., data = test)
predict.qda.test <- predict(model.qda.test, data = test)

## Confusion Matrix and Accuracy

confusionMatrix(predict.qda.test$class, as.factor(test$Outcome))
cat("Accuracy of the QDA model for test set is", confusionMatrix(predict.qda.test$class, as.factor(test$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.qda.test$class, as.factor(test$Outcome))$byClass
cat("Precision of the QDA model for test set is ",confusionMatrix(predict.qda.test$class, as.factor(test$Outcome))$byClass['Precision']*100)
cat("Recall of the QDA model for test set is ",confusionMatrix(predict.qda.test$class, as.factor(test$Outcome))$byClass['Recall']*100)
cat("F1-score of the QDA model for test set is ",confusionMatrix(predict.qda.test$class, as.factor(test$Outcome))$byClass['F1']*100)

# Part (d)



# Getting all the functions in "mda" package
ls("package:mda")

help(mda)

## For training set

library(mda)
model.mars <- mda(Outcome~., data = train, method = mars)
model.mars
predict.mars <- predict(model.mars, data = train)
predict.mars

## Confusion Matrix and Accuracy

confusionMatrix(predict.mars, as.factor(train$Outcome))
cat("Accuracy of the FDA using MARS is ", confusionMatrix(predict.mars, as.factor(train$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.mars, as.factor(train$Outcome))$byClass
cat("Precision of the FDA using MARS is ", confusionMatrix(predict.mars, as.factor(train$Outcome))$byClass['Precision']*100)
cat("Recall of the FDA using MARS is ", confusionMatrix(predict.mars, as.factor(train$Outcome))$byClass['Recall']*100)
cat("F1-score of the FDA using MARS is ", confusionMatrix(predict.mars, as.factor(train$Outcome))$byClass['F1']*100)

## For testing set

model.test.mars <- mda(Outcome~., data = test, method = mars)
model.test.mars
predict.test.mars <- predict(model.mars, newdata = test)
predict.test.mars

## Confusion Matrix and Accuracy

confusionMatrix(predict.test.mars, as.factor(test$Outcome))
cat("Accuracy of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars, as.factor(test$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.test.mars, as.factor(test$Outcome))$byClass
cat("Precision of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars, as.factor(test$Outcome))$byClass['Precision']*100)
cat("Recall of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars, as.factor(test$Outcome))$byClass['Recall']*100)
cat("F1-score of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars, as.factor(test$Outcome))$byClass['F1']*100)

# Part (e)

## For training set

model.pda <- mda(Outcome~., data = train, method = gen.ridge)
model.pda
predict.pda <- predict(model.pda, data = train)
predict.pda

## Confusion Matrix and Accuracy

confusionMatrix(predict.pda, as.factor(train$Outcome))
cat("Accuracy of the PDA is ", confusionMatrix(predict.pda, as.factor(train$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.pda, as.factor(train$Outcome))$byClass
cat("Precision of the PDA is ", confusionMatrix(predict.pda, as.factor(train$Outcome))$byClass['Precision']*100)
cat("Recall of the PDA is ", confusionMatrix(predict.pda, as.factor(train$Outcome))$byClass['Recall']*100)
cat("F1-score of the PDA is ", confusionMatrix(predict.pda, as.factor(train$Outcome))$byClass['F1']*100)

## For testing set

model.test.pda <- mda(Outcome~., data = test, method = gen.ridge)
model.test.pda
predict.test.pda <- predict(model.pda, newdata = test)
predict.test.pda

## Confusion Matrix and Accuracy

confusionMatrix(predict.test.pda, as.factor(test$Outcome))
cat("Accuracy of the PDA is ", confusionMatrix(predict.test.pda, as.factor(test$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.test.pda, as.factor(test$Outcome))$byClass
cat("Precision of the PDA in test set is ", confusionMatrix(predict.test.pda, as.factor(test$Outcome))$byClass['Precision']*100)
cat("Recall of the PDA in test set is ", confusionMatrix(predict.test.pda, as.factor(test$Outcome))$byClass['Recall']*100)
cat("F1-score of the PDA in test set is ", confusionMatrix(predict.test.pda, as.factor(test$Outcome))$byClass['F1']*100)

# Part (f)

# For training set

mda.2s <- mda(Outcome~., data = train, subclasses = 2)
mda.2s
predict.mda.2s <- predict(mda.2s, data = train)
predict.mda.2s

## Confusion Matrix and Accuracy

confusionMatrix(predict.mda.2s, as.factor(train$Outcome))
cat("Accuracy of the MDA is ", confusionMatrix(predict.mda.2s, as.factor(train$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.mda.2s, as.factor(train$Outcome))$byClass
cat("Precision of the MDA is ", confusionMatrix(predict.mda.2s, as.factor(train$Outcome))$byClass['Precision']*100)
cat("Recall of the MDA is ", confusionMatrix(predict.mda.2s, as.factor(train$Outcome))$byClass['Recall']*100)
cat("F1-score of the MDA is ", confusionMatrix(predict.mda.2s, as.factor(train$Outcome))$byClass['F1']*100)

# For testing set

mda.test.2s <- mda(Outcome~., data = test, subclasses = 2)
mda.test.2s
predict.mda.test.2s <- predict(mda.2s, newdata = test)
predict.mda.test.2s

## Confusion Matrix and Accuracy

confusionMatrix(predict.mda.test.2s, as.factor(test$Outcome))
cat("Accuracy of the MDA in test set is ", confusionMatrix(predict.mda.test.2s, as.factor(test$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.mda.test.2s, as.factor(test$Outcome))$byClass
cat("Precision of the MDA in test set is ", confusionMatrix(predict.mda.test.2s, as.factor(test$Outcome))$byClass['Precision']*100)
cat("Recall of the MDA in test set is ", confusionMatrix(predict.mda.test.2s, as.factor(test$Outcome))$byClass['Recall']*100)
cat("F1-score of the MDA in test set is ", confusionMatrix(predict.mda.test.2s, as.factor(test$Outcome))$byClass['F1']*100)

# Part (g)

help(glm)

# For training set

model.lr <- glm(Outcome~., family = binomial, data = train)
model.lr
summary(model.lr)
predict.lr <- predict(model.lr, data = train)
predict.lr

## Confusion Matrix and Accuracy

pre1 <- ifelse(predict.lr>0, 1, 0)
tab1 <- table(pre1, train$Outcome)
tab1
confusionMatrix(tab1)
cat("Accuracy of the Logistic Regression is ", confusionMatrix(tab1)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab1)$byClass
cat("Precision of the Logistic Regression is ", confusionMatrix(tab1)$byClass['Precision']*100)
cat("Recall of the Logistic Regression is ", confusionMatrix(tab1)$byClass['Recall']*100)
cat("F1-score of the Logistic Regression is ", confusionMatrix(tab1)$byClass['F1']*100)

# For testing set

predict.test.lr <- predict(model.lr, newdata = test)
predict.test.lr

## Confusion Matrix and Accuracy

pre2 <- ifelse(predict.test.lr>0, 1, 0)
tab2 <- table(pre2, test$Outcome)
tab2
confusionMatrix(tab2)
cat("Accuracy of the Logistic Regression in test set is ", confusionMatrix(tab2)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab2)$byClass
cat("Precision of the Logistic Regression in test set is ", confusionMatrix(tab2)$byClass['Precision']*100)
cat("Recall of the Logistic Regression in test set is ", confusionMatrix(tab2)$byClass['Recall']*100)
cat("F1-score of the Logistic Regression in test set is ", confusionMatrix(tab2)$byClass['F1']*100)

# Part (h)-(i)

library(adabag)

# Getting all the functions in "adabag" package
ls("package:adabag")

help(bagging)

# For training set

train$Outcome <- as.factor(train$Outcome)
model.bag <- bagging(Outcome~., data = train)
model.bag
predict.train.bag <- predict(model.bag, newdata = train)
predict.train.bag

## Confusion Matrix and Accuracy

tab3 <- predict.train.bag$confusion
tab3
confusionMatrix(tab3)
cat("Accuracy of the bagging is ", confusionMatrix(tab3)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab3)$byClass
cat("Precision of the bagging is ", confusionMatrix(tab3)$byClass['Precision']*100)
cat("Recall of the bagging is ", confusionMatrix(tab3)$byClass['Recall']*100)
cat("F1-score of the bagging is ", confusionMatrix(tab3)$byClass['F1']*100)

# For testing set

predict.test.bag <- predict(model.bag, newdata = test)
predict.test.bag

## Confusion Matrix and Accuracy

tab4 <- predict.test.bag$confusion
tab4
confusionMatrix(tab4)
cat("Accuracy of the bagging is ", confusionMatrix(tab4)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab4)$byClass
cat("Precision of the bagging in test set is ", confusionMatrix(tab4)$byClass['Precision']*100)
cat("Recall of the bagging in test set is ", confusionMatrix(tab4)$byClass['Recall']*100)
cat("F1-score of the bagging in test set is ", confusionMatrix(tab4)$byClass['F1']*100)

# Part (h)-(ii)

help(boosting)

# For training set

model.boost <- boosting(Outcome~., data = train, coeflearn = 'Freund')
model.boost
predict.train.boost <- predict(model.boost, newdata = train)
predict.train.boost

## Confusion Matrix and Accuracy

tab5 <- predict.train.boost$confusion
tab5
confusionMatrix(tab5)
cat("Accuracy of the boosting is ", confusionMatrix(tab5)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab5)$byClass
cat("Precision of the bagging is ", confusionMatrix(tab5)$byClass['Precision']*100)
cat("Recall of the bagging is ", confusionMatrix(tab5)$byClass['Recall']*100)
cat("F1-score of the bagging is ", confusionMatrix(tab5)$byClass['F1']*100)

# For testing set

predict.test.boost <- predict(model.boost, newdata = test)
predict.test.boost

## Confusion Matrix and Accuracy

tab6 <- predict.test.boost$confusion
tab6
confusionMatrix(tab6)
cat("Accuracy of the boosting in test set is ", confusionMatrix(tab6)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab6)$byClass
cat("Precision of the bagging in test set is ", confusionMatrix(tab6)$byClass['Precision']*100)
cat("Recall of the bagging in test set is ", confusionMatrix(tab6)$byClass['Recall']*100)
cat("F1-score of the bagging in test set is ", confusionMatrix(tab6)$byClass['F1']*100)

# Part (h)-(iii)

library(randomForest)

# Getting all the functions in "randomForest" package
ls("package:randomForest")

help(randomForest)

# For training set

model.rf <- randomForest(Outcome~., data = train, importance = TRUE)
model.rf
predict.train.rf <- predict(model.rf, newdata = train)
predict.train.rf

## Confusion Matrix and Accuracy

confusionMatrix(predict.train.rf, train$Outcome)
cat("Accuracy of the random forest is ", confusionMatrix(predict.train.rf, train$Outcome)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.train.rf, train$Outcome)$byClass
cat("Precision of the random forest is ", confusionMatrix(predict.train.rf, train$Outcome)$byClass['Precision']*100)
cat("Recall of the random forest is ", confusionMatrix(predict.train.rf, train$Outcome)$byClass['Recall']*100)
cat("F1-score of the random forest is ", confusionMatrix(predict.train.rf, train$Outcome)$byClass['F1']*100)

# For testing set

predict.test.rf <- predict(model.rf, newdata = test)
predict.test.rf

## Confusion Matrix and Accuracy

confusionMatrix(predict.test.rf, as.factor(test$Outcome))
cat("Accuracy of the random forest in test set is ", confusionMatrix(predict.test.rf, as.factor(test$Outcome))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.test.rf, as.factor(test$Outcome))$byClass
cat("Precision of the random forest in test set is ", confusionMatrix(predict.test.rf, as.factor(test$Outcome))$byClass['Precision']*100)
cat("Recall of the random forest in test set is ", confusionMatrix(predict.test.rf, as.factor(test$Outcome))$byClass['Recall']*100)
cat("F1-score of the random forest in test set is ", confusionMatrix(predict.test.rf, as.factor(test$Outcome))$byClass['F1']*100)

#################################################################################################

# Problem 2

library(ROCR)

# Getting all the functions in "ROCR" package
ls("package:ROCR")

help(prediction)

pred1 <- prediction(predict.lr, train$Outcome)
perf1 <- performance(pred1, "tpr", "fpr") # "tpr" stands for true positive rate and "fpr" stands for false positive rate
plot(perf1, col = "green", lwd=2, main = "ROC curve of Logistic Regression")
abline(a=0,b=1)

##################################################################################################

# Problem 3

## Plotting the variable importance measures in random forest model

randomForest::varImpPlot(model.rf, sort = FALSE)
bp <- barplot(as.matrix(varImp(model.rf)),col = c(1:8),beside = T, legend.text = T)

#################################################################################################

# Problem 4

glass <- read.delim(file.choose(), sep = ",")
View(glass)
dim(glass)

## Rearrangement of the dataframe 

names(glass)
glass[nrow(glass)+1, ] = c(1,1.52101,13.64,4.49,1.10,71.78,0.06,8.75,0.00,0.00,1)

# Function of moving the any row to top

move_to_top <- function(df,n)
{
  df[c(n, setdiff(1:nrow(df),n)), ]
}

head(move_to_top(glass, 214))
glass <- move_to_top(glass, 214)

rownames(glass) <- glass[,1]
glass <- glass[,-1]
colnames(glass) <- c(1:9,"glass type")
View(glass)

# Checking for missing values

which(is.na(glass))

# Create train - test split (2:1 ratio)

library(caret)

set.seed(19)
index1 <- createDataPartition(glass$`glass type`, p = (2/3), list = FALSE)
training <- glass[index1, ]
testing <- glass[-index1, ]
dim(training)
dim(testing)

# Part (a)-(i)

# kNN classification 

## Choosing the optimal k for k-NN classification

library(class)

accuracy1 <- NULL

for (i in 1:100)
{
  model.training = knn.cv(training, cl = training$`glass type`, k = i)
  accuracy1 = c(accuracy1, confusionMatrix(model.training, as.factor(training$`glass type`))$overall['Accuracy']*100)
}

as.vector(accuracy1)
max(as.vector(accuracy1))
which.max(as.vector(accuracy1))  # Hence, k = 14 has the maximum accuracy in case of k-NN rule

# Fitting k-NN rule using the optimal k through leave-one-out cross-validation 

model.knn1 = knn.cv(training, cl = training$`glass type`, k = which.max(as.vector(accuracy1)))

## Confusion Matrix and Accuracy

confusionMatrix(model.knn1, as.factor(training$`glass type`))
cat("The Accuracy of the k-NN rule using optimal k is ", confusionMatrix(model.knn1, as.factor(training$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score
confusionMatrix(model.knn1, as.factor(training$`glass type`))$byClass[,'Precision']
list("Precisions of the k-NN rule is", confusionMatrix(model.knn1, as.factor(training$`glass type`))$byClass[,'Precision']*100)
list("Recalls of the k-NN rule is", confusionMatrix(model.knn1, as.factor(training$`glass type`))$byClass[,'Recall']*100)
list("F1 - scores of the k-NN rule is", confusionMatrix(model.knn1, as.factor(training$`glass type`))$byClass[,'F1']*100)

# Part (a)-(ii)

keep1 <- condense(as.matrix(training), cl = training$`glass type`)
keep1
training2 <- as.matrix(training)
model.condense <- knn.cv(training2[keep1], cl = as.vector(training$`glass type`)[keep1])
model.condense

## Confusion Matrix and Accuracy

confusionMatrix(model.condense, as.factor(training$`glass type`[keep1]))
cat("The Accuracy of the k-NN rule using condensed training set is ", confusionMatrix(model.condense, as.factor(training$`glass type`[keep1]))$overall['Accuracy']*100)

## Precision, Recall and F1-score

list("Precision of the k-NN rule using condensed training set is ", confusionMatrix(model.condense, as.factor(training$`glass type`[keep1]))$byClass[,'Precision']*100)
list("Recall of the k-NN rule using condensed training set is ", confusionMatrix(model.condense, as.factor(training$`glass type`[keep1]))$byClass[,'Recall']*100)
list("F1-score of the k-NN rule using condensed training set is ", confusionMatrix(model.condense, as.factor(training$`glass type`[keep1]))$byClass[,'F1']*100)

# Getting all the functions in "class" package
ls("package:class")

fit2 <- multiedit(training, cl = training$`glass type`, k = 1)
train_ME2 <- training[fit2, , drop = FALSE]
dim(train_ME2)
knn_ME2 <- knn(train_ME2[,-10], training[,-10], cl = train_ME2$`glass type`, k = 10)
knn_ME2
levels(knn_ME2) <- c('1','2','3','5','6','7')

## Confusion Matrix and Accuracy

confusionMatrix(knn_ME2, as.factor(training$`glass type`))
cat("The Accuracy of the MULTIEDIT knn-rule is ", confusionMatrix(knn_ME2, as.factor(training$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

list("Precision of the MULTIEDIT knn-rule is ", confusionMatrix(knn_ME2, as.factor(training$`glass type`))$byClass[,'Precision']*100)
list("Recall of the MULTIEDIT knn-rule is ", confusionMatrix(knn_ME2, as.factor(training$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the MULTIEDIT knn-rule is ", confusionMatrix(knn_ME2, as.factor(training$`glass type`))$byClass[,'F1']*100)

# Part (b)

library(MASS)

model.lda2 <- lda(`glass type`~., data = training)

## For training set

predict.lda.train2 <- predict(model.lda2, data = training)

## Confusion Matrix and Accuracy

confusionMatrix(predict.lda.train2$class, as.factor(training$`glass type`))
cat("Accuracy of the LDA model for train set is ", confusionMatrix(predict.lda.train2$class, as.factor(training$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

list("Precision of the LDA model for train set is ", confusionMatrix(predict.lda.train2$class, as.factor(training$`glass type`))$byClass[,'Precision']*100)
list("Recall of the LDA model for train set is ", confusionMatrix(predict.lda.train2$class, as.factor(training$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the LDA model for train set is ", confusionMatrix(predict.lda.train2$class, as.factor(training$`glass type`))$byClass[,'F1']*100)

## For testing set

predict.lda.test2 <- predict(model.lda2, newdata = testing)

## Confusion Matrix and Accuracy

confusionMatrix(predict.lda.test2$class, as.factor(testing$`glass type`))
cat("Accuracy of the LDA model for train set is ", confusionMatrix(predict.lda.test2$class, as.factor(testing$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

list("Precision of the LDA model for test set is ", confusionMatrix(predict.lda.test2$class, as.factor(testing$`glass type`))$byClass[,'Precision']*100)
list("Recall of the LDA model for test set is ", confusionMatrix(predict.lda.test2$class, as.factor(testing$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the LDA model for test set is ", confusionMatrix(predict.lda.test2$class, as.factor(testing$`glass type`))$byClass[,'F1']*100)

# Part (c)

library(MASS)

## For training set

model.qda2 <- qda(`glass type`~., data = training)

## Since, some groups are too small, QDA analysis cannot be possible for this "glass" dataset.

# Part (d)

library(mda)

# Getting all the functions in "mda" package
ls("package:mda")

help(mda)

## For training set

model.mars2 <- mda(`glass type`~., data = training, method = mars)
model.mars2
predict.mars2 <- predict(model.mars2, data = training)
predict.mars2

## Confusion Matrix and Accuracy

confusionMatrix(predict.mars2, as.factor(training$`glass type`))
cat("Accuracy of the FDA using MARS is ", confusionMatrix(predict.mars2, as.factor(training$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.mars2, as.factor(training$`glass type`))$byClass
list("Precision of the FDA using MARS is ", confusionMatrix(predict.mars2, as.factor(training$`glass type`))$byClass[,'Precision']*100)
list("Recall of the FDA using MARS is ", confusionMatrix(predict.mars2, as.factor(training$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the FDA using MARS is ", confusionMatrix(predict.mars2, as.factor(training$`glass type`))$byClass[,'F1']*100)

## For testing set

predict.test.mars2 <- predict(model.mars2, newdata = testing)
predict.test.mars2

## Confusion Matrix and Accuracy

confusionMatrix(predict.test.mars2, as.factor(testing$`glass type`))
cat("Accuracy of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars2, as.factor(testing$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.test.mars2, as.factor(testing$`glass type`))$byClass
list("Precision of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars2, as.factor(testing$`glass type`))$byClass[,'Precision']*100)
list("Recall of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars2, as.factor(testing$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the FDA using MARS in test set is ", confusionMatrix(predict.test.mars2, as.factor(testing$`glass type`))$byClass[,'F1']*100)

# Part (e)

## For training set

model.pda2 <- mda(`glass type`~., data = training, method = gen.ridge)
model.pda2
predict.pda2 <- predict(model.pda2, data = training)
predict.pda2

## Confusion Matrix and Accuracy

confusionMatrix(predict.pda2, as.factor(training$`glass type`))
cat("Accuracy of the PDA is ", confusionMatrix(predict.pda2, as.factor(training$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.pda2, as.factor(training$`glass type`))$byClass
list("Precision of the PDA is ", confusionMatrix(predict.pda2, as.factor(training$`glass type`))$byClass[,'Precision']*100)
list("Recall of the PDA is ", confusionMatrix(predict.pda2, as.factor(training$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the PDA is ", confusionMatrix(predict.pda2, as.factor(training$`glass type`))$byClass[,'F1']*100)

## For testing set


predict.test.pda2 <- predict(model.pda2, newdata = testing)
predict.test.pda2

## Confusion Matrix and Accuracy

confusionMatrix(predict.test.pda2, as.factor(testing$`glass type`))
cat("Accuracy of the PDA is ", confusionMatrix(predict.test.pda2, as.factor(testing$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.test.pda2, as.factor(testing$`glass type`))$byClass
list("Precision of the PDA in test set is ", confusionMatrix(predict.test.pda2, as.factor(testing$`glass type`))$byClass[,'Precision']*100)
list("Recall of the PDA in test set is ", confusionMatrix(predict.test.pda2, as.factor(testing$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the PDA in test set is ", confusionMatrix(predict.test.pda2, as.factor(testing$`glass type`))$byClass[,'F1']*100)

# Part (f)

# For training set

mda.2 <- mda(`glass type`~., data = training)
mda.2
predict.mda.2 <- predict(mda.2, data = training)
predict.mda.2

## Confusion Matrix and Accuracy

confusionMatrix(predict.mda.2, as.factor(training$`glass type`))
cat("Accuracy of the MDA is ", confusionMatrix(predict.mda.2, as.factor(training$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.mda.2, as.factor(training$`glass type`))$byClass
list("Precision of the MDA is ", confusionMatrix(predict.mda.2, as.factor(training$`glass type`))$byClass[,'Precision']*100)
list("Recall of the MDA is ", confusionMatrix(predict.mda.2, as.factor(training$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the MDA is ", confusionMatrix(predict.mda.2, as.factor(training$`glass type`))$byClass[,'F1']*100)

# For testing set

predict.mda.test.2 <- predict(mda.2, newdata = testing)
predict.mda.test.2

## Confusion Matrix and Accuracy

confusionMatrix(predict.mda.test.2, as.factor(testing$`glass type`))
cat("Accuracy of the MDA in test set is ", confusionMatrix(predict.mda.test.2, as.factor(testing$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.mda.test.2, as.factor(testing$`glass type`))$byClass
list("Precision of the MDA in test set is ", confusionMatrix(predict.mda.test.2, as.factor(testing$`glass type`))$byClass[,'Precision']*100)
list("Recall of the MDA in test set is ", confusionMatrix(predict.mda.test.2, as.factor(testing$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the MDA in test set is ", confusionMatrix(predict.mda.test.2, as.factor(testing$`glass type`))$byClass[,'F1']*100)

# Part (g)

help(glm)

## Changing all 6 levels to 2 levels as "0" and "1" to perform logistic regression

glass[, ncol(glass)+1] <- glass$`glass type`
colnames(glass) <- c(1:9,"glass type","Output")
glass$Output <- ifelse(glass$Output>4, 1, 0)
View(glass)

## Making new train-test split

set.seed(19)
newindex <- sample(length(glass$`glass type`), size = 144, replace = F)
newtrain <- glass[newindex,]
newtest <- glass[-newindex,]
dim(newtrain)
dim(newtest)

# For training set

model.lr2 <- glm(Output~`1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`, family = binomial, data = newtrain)
model.lr2
summary(model.lr2)
predict.lr2 <- predict(model.lr2, data = newtrain)
predict.lr2

## Confusion Matrix and Accuracy

pre3 <- ifelse(predict.lr2>0, 1, 0)
tab7 <- table(pre3, newtrain$Output)
tab7
confusionMatrix(tab7)
cat("Accuracy of the Logistic Regression is ", confusionMatrix(tab7)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab7)$byClass
cat("Precision of the Logistic Regression is ", confusionMatrix(tab7)$byClass['Precision']*100)
cat("Recall of the Logistic Regression is ", confusionMatrix(tab7)$byClass['Recall']*100)
cat("F1-score of the Logistic Regression is ", confusionMatrix(tab7)$byClass['F1']*100)

# For testing set

predict.test.lr2 <- predict(model.lr2, newdata = newtest)
predict.test.lr2

## Confusion Matrix and Accuracy

pre3 <- ifelse(predict.test.lr2>0, 1, 0)
tab8 <- table(pre3, newtest$Output)
tab8
confusionMatrix(tab8)
cat("Accuracy of the Logistic Regression in test set is ", confusionMatrix(tab8)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab8)$byClass
cat("Precision of the Logistic Regression in test set is ", confusionMatrix(tab8)$byClass['Precision']*100)
cat("Recall of the Logistic Regression in test set is ", confusionMatrix(tab8)$byClass['Recall']*100)
cat("F1-score of the Logistic Regression in test set is ", confusionMatrix(tab8)$byClass['F1']*100)


# Part (h)-(i)

glass <- glass[,-11]
View(glass)

set.seed(519)
index4 <- createDataPartition(glass$`1`, p = (2/3), list = FALSE)
train_set <- glass[index4, ]
test_set <- glass[-index4, ]
dim(test_set)
dim(train_set)
attach(train_set)

# Getting all the functions in "adabag" package
ls("package:adabag")

help("bagging")

# For training set

train_set$`glass type` <- as.factor(train_set$`glass type`)
model.bag2 <- bagging(`glass type` ~., data = train_set)
model.bag2
predict.train.bag2 <- predict(model.bag2, newdata = train_set)
predict.train.bag2

## Confusion Matrix and Accuracy

tab9 <- predict.train.bag2$confusion
tab9
confusionMatrix(tab9)
cat("Accuracy of the bagging is ", confusionMatrix(tab9)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab9)$byClass
cat("Precision of the bagging is ", confusionMatrix(tab9)$byClass['Precision']*100)
cat("Recall of the bagging is ", confusionMatrix(tab9)$byClass['Recall']*100)
cat("F1-score of the bagging is ", confusionMatrix(tab9)$byClass['F1']*100)

# For testing set

predict.test.bag2 <- predict(model.bag2, newdata = test_set)
predict.test.bag2

## Confusion Matrix and Accuracy

tab10 <- predict.test.bag2$confusion
tab10
confusionMatrix(tab10)
cat("Accuracy of the bagging is ", confusionMatrix(tab10)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab10)$byClass
cat("Precision of the bagging in test set is ", confusionMatrix(tab10)$byClass['Precision']*100)
cat("Recall of the bagging in test set is ", confusionMatrix(tab10)$byClass['Recall']*100)
cat("F1-score of the bagging in test set is ", confusionMatrix(tab10)$byClass['F1']*100)

# Part (h)-(ii)

help(boosting)

# For training set

model.boost2 <- boosting(`glass type`~., data = train_set, coeflearn = 'Freund')
model.boost2
predict.train.boost2 <- predict(model.boost2, newdata = train_set)
predict.train.boost2

## Confusion Matrix and Accuracy

tab11 <- predict.train.boost2$confusion
tab11
confusionMatrix(tab11)
cat("Accuracy of the boosting is ", confusionMatrix(tab11)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab11)$byClass
cat("Precision of the bagging is ", confusionMatrix(tab11)$byClass['Precision']*100)
cat("Recall of the bagging is ", confusionMatrix(tab11)$byClass['Recall']*100)
cat("F1-score of the bagging is ", confusionMatrix(tab11)$byClass['F1']*100)

# For testing set

predict.test.boost2 <- predict(model.boost2, newdata = test)
predict.test.boost2

## Confusion Matrix and Accuracy

tab12 <- predict.test.boost2$confusion
tab12
confusionMatrix(tab12)
cat("Accuracy of the boosting in test set is ", confusionMatrix(tab12)$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(tab12)$byClass
cat("Precision of the bagging in test set is ", confusionMatrix(tab12)$byClass['Precision']*100)
cat("Recall of the bagging in test set is ", confusionMatrix(tab12)$byClass['Recall']*100)
cat("F1-score of the bagging in test set is ", confusionMatrix(tab12)$byClass['F1']*100)

# Part (h)-(iii)

library(randomForest)

# Getting all the functions in "randomForest" package
ls("package:randomForest")

help(randomForest)

# For training set

model.rf2 <- randomForest(`glass type`~., data = train_set, importance = TRUE)
model.rf2
predict.train.rf2 <- predict(model.rf2, newdata = train_set)
predict.train.rf2

## Confusion Matrix and Accuracy

confusionMatrix(predict.train.rf2, as.factor(train_set$`glass type`))
cat("Accuracy of the random forest is ", confusionMatrix(predict.train.rf2, as.factor(train_set$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.train.rf2, as.factor(train_set$`glass type`))$byClass
list("Precision of the random forest is ", confusionMatrix(predict.train.rf2, as.factor(train_set$`glass type`))$byClass[,'Precision']*100)
list("Recall of the random forest is ", confusionMatrix(predict.train.rf2, as.factor(train_set$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the random forest is ", confusionMatrix(predict.train.rf2, as.factor(train_set$`glass type`))$byClass[,'F1']*100)

# For testing set

predict.test.rf2 <- predict(model.rf2, newdata = test_set)
predict.test.rf2

## Confusion Matrix and Accuracy

confusionMatrix(predict.test.rf2, as.factor(test_set$`glass type`))
cat("Accuracy of the random forest in test set is ", confusionMatrix(predict.test.rf2, as.factor(test_set$`glass type`))$overall['Accuracy']*100)

## Precision, Recall and F1-score

confusionMatrix(predict.test.rf2, as.factor(test_set$`glass type`))$byClass
list("Precision of the random forest in test set is ", confusionMatrix(predict.test.rf2, as.factor(test_set$`glass type`))$byClass[,'Precision']*100)
list("Recall of the random forest in test set is ", confusionMatrix(predict.test.rf2, as.factor(test_set$`glass type`))$byClass[,'Recall']*100)
list("F1-score of the random forest in test set is ", confusionMatrix(predict.test.rf2, as.factor(test_set$`glass type`))$byClass[,'F1']*100)

##############################################################################################################

# Problem 6

## Plotting the variable importance measures in random forest model

randomForest::varImpPlot(model.rf2, sort = FALSE)
bp <- barplot(as.matrix(varImp(model.rf2)),col = c(10:19),beside = T, legend.text = T)



