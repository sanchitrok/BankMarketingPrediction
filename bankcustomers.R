#Classificatiom
bank<-read.csv("bank.csv")
View(bank)
summary(bank)
str(bank)
library(e1071)
library (caret)
library(dplyr)
# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(bank$deposit, SplitRatio = 0.75)
training_set = subset(bank, split == TRUE)
test_set = subset(bank, split == FALSE)
any(is.na(bank))


# Making the Confusion Matrix
cm = table(test_set[, 17], y_pred)
confusionMatrix(cm)
#Naive Bayes
classifier = naiveBayes(x = training_set[-17],
                        y = training_set$deposit)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-17])

# Making the Confusion Matrix
cm = table(test_set[, 17], y_pred)
confusionMatrix(cm)
#Decision tree
library(rpart)
library(rpart.plot)
dectree<-rpart(deposit~.,data=training_set,method="class",control=rpart.control(minsplit=10),parms=list(split="gini"))
plot(dectree)
rpart.plot(dectree)
#predict
fit<-predict(dectree,newdata=test_set,type="class")
cm<-table(test_set$deposit,fit)
confusionMatrix(cm)
plotcp(dectree)
printcp(dectree)
opt<-which.min(dectree$cptable[,"xerror"])
cp<-dectree$cptable[opt,"CP"]
#prune
treeprune<-prune(dectree,cp=cp)
treeprune
rpart.plot(treeprune)
#Random Forest
library(randomForest)
classifier<-randomForest(x=training_set[-17],y=training_set$deposit,ntree=500)
y<-predict(classifier,newdata=test_set[-17])
c<-table(test_set[,17],y)
confusionMatrix(c)
plot(classifier)
importance(classifier)
varImpPlot(classifier)
