library(rpart)
library(rpart.plot)
library(tree)
library(caTools)
library(randomForest)
library(e1071)
library(tree)
library(ROCR)
Sys.setenv(LANG = "en")

cw.all <- read.csv('creditworthiness.csv')
cw.known <- subset(cw.all,credit.rating > 0)
cw.unknown <- subset(cw.all,credit.rating == 0)
cw.train <- cw.known[1:(nrow(cw.known)/2),] # first half
cw.test <- cw.known[-(1:(nrow(cw.known)/2)),] # second half
length(cw.train)
nrow(cw.train)
length(cw.test)
nrow(cw.test)
medianCust = data.frame()
newData =c(0,1,1,0,3,0,3,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
medianCust = rbind(medianCust, newData)
colnames(medianCust) = names(cw.known)[-46]




# ------------------- question 2 ---------------------------
#train.rpart = rpart(factor(credit.rating)~., data=cw.train)
#print(train.rpart)
#cust.pred = predict(train.rpart, medianCust, type="class")
#cust.pred
#cust.pred <- predict(train.rpart, newdata = cw.test, type = "class")
#cust.pred
tree.cw.train = tree(factor(credit.rating)~., data=cw.train)
tree.cw.train
cust.pred.tree = predict(tree.cw.train, medianCust, type = "class")
cust.pred.tree


tree.pred <- predict(tree.cw.train, cw.test, type = "class")
confusion = with(cw.test, table(tree.pred, credit.rating))
print(confusion)

accuracy_Test <- sum(diag(confusion))/sum(confusion)
accuracy_Test

#confusion <- table(cust.pred.tree, cw.test$credit.rating)
#print(confusion)
#sum(diag(confusion))/sum(confusion)

# get the count of all classes in credit.rating using the table() function
beforeCountFreq = table(cw.train$credit.rating)
#find the probability of each class
beforeClassProb = beforeCountFreq/sum(beforeCountFreq)
#calculate entropy (before split)
beforeEntropy = -sum(beforeClassProb * log2(beforeClassProb))

# functionary == 0
countFreq0 = table(cw.train$credit.rating[cw.train$functionary == 0])
classProb0 = countFreq0/sum(countFreq0)
(functionaryEnt0 = -sum(classProb0 * log2(classProb0)))

# functionary == 1
countFreq1 = table(cw.train$credit.rating[cw.train$functionary == 1])
classProb1 = countFreq1/sum(countFreq1)
(functionaryEnt1 = -sum(classProb1 * log2(classProb1)))

ent = (beforeEntropy - (functionaryEnt0 * sum(countFreq0) +
                          functionaryEnt1 * sum(countFreq1)) /
         sum(sum(countFreq0) + sum(countFreq1)))
ent

#random forest
cw.train$credit.rating <- factor(cw.train$credit.rating)
rf.cw.train = randomForest(credit.rating~., data = cw.train)
rf.pred = predict(rf.cw.train, cw.test[,-46])

confusionRF = with(cw.test, table(rf.pred, credit.rating))
confusionRF
sum(diag(confusionRF))/sum(confusionRF)
# Fit to a model using randomForest after the tuning
RFTuned.cw.train = randomForest(credit.rating~., data = cw.train, mtry
                                = 12, ntree=500, stepFactor=2, improve=0.2)
RFTuned.pred = predict(RFTuned.cw.train, cw.test[,-46])
# Produce confusion matrix after the tuning
confusionRFTuned = with(cw.test, table(RFTuned.pred, credit.rating))
confusionRFTuned
# Calculate the accuracy rate after the tuning
sum(diag(confusionRFTuned))/sum(confusionRFTuned)



# ------------------- question 3 ---------------------------
svmfit = svm(factor(credit.rating )~ ., data = cw.train, kernel = "radial")
print(svmfit)
predict(svmfit, medianCust, decision.values = TRUE)
# Predict the crefusion matrix for predicting the credit rating from the SVM on the test set
svm.pred = predict(svmfit, cw.test[,-46])
# Generate the confusion matrix
confusionSVM = with(cw.test, table(svm.pred, credit.rating))
# Overall accuracy rate
sum(diag(confusionSVM))/sum(confusionSVM)
summary(tune.svm(credit.rating ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(0:2), gamma = 10^c(-4:-1)))
# Fit a model using SVM
svmTuned = svm(credit.rating ~ ., data = cw.train, kernel = "radial",
               cost=100,
               gamma = 0.0001)
# Predict the values on test set
svmTuned.pred = predict(svmTuned, cw.test[,-46])# Produce confusion matrix
confusionTunedSVM = with(cw.test, table(svmTuned.pred, credit.rating))
# Overall accuracy rate
sum(diag(confusionTunedSVM))/sum(confusionTunedSVM)




# ------------------- question 4 ---------------------------
nb = naiveBayes(credit.rating~. ,data=cw.train)
predict(nb, medianCust, type='class')
predict(nb, medianCust, type='raw')

#predict the values on test set
nb.pred = predict(nb, cw.test[,-46])
#produce confusion matrix
confusionNB = with(cw.test, table(nb.pred, credit.rating))
confusionNB
#calculate the accuracy rate
sum(diag(confusionNB))/sum(confusionNB)
nb


# ------------------- question 6 ---------------------------
glm.fit <- glm((credit.rating==1)~., data = cw.train, family = binomial)

options(width = 130)
summary(glm.fit)

# Fit an svm model of your choice to the training set 
summary(tune.svm((credit.rating==1)~., data = cw.train, kernel = "radial", cost = 10^c(-2:2), gamma = 10^c(-4:1), type = "C"))

svm2 = svm((credit.rating==1)~., data = cw.train, type = "C")

#Fit a model using svm 
svm.fit = svm((credit.rating==1) ~ .,data = cw.train, type = 'C', gamma = 0.001, cost = 100, kernel = "radial") 
svm.fit

# Predict the values on test set[SVM]
svm.fit.pred = predict(svm.fit, cw.test[,-46], decision.values =TRUE)
# Predict the values on test set[GLM]
glm.fit.pred = predict(glm.fit, cw.test[,-46])

# Predict the values on test set[SVM] 
svm.fit.pred = predict(svm.fit, cw.test[,-46], decision.values = TRUE) 
# Predict the values on test set[GLM] 
glm.fit.pred = predict(glm.fit, cw.test[,-46]) 
# Make prediction using SVM 
confusionSVM = prediction(-attr(svm.fit.pred, "decision.values"), cw.test$credit.rating == 1) 
# Create rocs curve based on prediction 
rocsSVM <- performance(confusionSVM, "tpr", "fpr") 
# Make prediction using Logistic Regression 
confusionGLM = prediction(glm.fit.pred, cw.test$credit.rating == 1) 
# create rocs curve based on prediction 
rocsGLM <- performance(confusionGLM, "tpr", "fpr") 
# Plot the graph 
plot(rocsGLM, col = 1) 
plot(rocsSVM, col = 2, add = TRUE) > abline(0, 1, lty=3) 
# Add the legend to the graph 
legend(0.6, 0.6, c('glm','svm'), 1:2)