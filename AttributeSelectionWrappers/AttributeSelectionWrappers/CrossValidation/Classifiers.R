## classifiers 
library("randomForest")

classifier.naivebayes = function(train, test, testindicates, classTested = 1) {

    # simple example of a classifier
    # requires library(e1071)
    modelBayes = naiveBayes(formula = train[, classTested] ~ ., method = "class", data = train)
    testPred = predict(modelBayes, test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

classifier.decisiontree = function(train, test, testindicates, classTested = 1) {

    # decision tree classifier
    # requires library(rpart)
    modelTree = rpart(formula = train[, classTested] ~ ., method = "class", data = train)
    testPred = predict(modelTree, newData = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

classifier.randomforest = function(train, test, testindicates, classTested = 1) {

    #set.seed(1234)

    modelForest = randomForest(formula = train[, classTested] ~ ., ntree = 100, proximity = T, data = train)
    testPred = predict(modelForest, newData = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

classifier.decisiontreemicrosoft = function(train, test, testindicates, classTested = 1) {

    modelTree = rxDTree(formula = train[, classTested] ~ ., method = "class", data = train)
    testPred = rxPredict(modelTree, data = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

# classifiers end
