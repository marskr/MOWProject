library(ROCR)
library(e1071)
source("DataNormalization/DataEncryption.R")

head(encData, 5)

kfcv.sizes = function(n, k = 10) {

    # generate sample sizes for k-fold cross validation on a data set of
    # size n 
    # create sizes of provided data division into k folds
    # EXAMPLE: kfcv.sizes(n, k = ...)

    sizes = c()
    for (i in 1:k) {
        first = 1 + (((i - 1) * n) %/% k)
        last = ((i * n) %/% k)
        sizes = append(sizes, last - first + 1)
    }
    sizes
}

kfcv.testing = function(n, k = 10) {

    # generate testing sample indices for k-fold cross validation on a
    # data set of size n
    # EXAMPLE: kfcv.testing(n, k = ...)

    indices = list()
    sizes = kfcv.sizes(n, k = k)
    values = 1:n
    for (i in 1:k) {
        # take a random sample of given size
        s = sample(values, sizes[i])
        # append random sample to list of indices
        indices[[i]] = s
        # remove sample from values
        values = setdiff(values, s)
    }
    indices
}

kfcv.classifier = function(data, class, classTested = 1, classifier, k = 10) {

    # run k-fold cross validation with an arbitrary classifier
    # EXAMPLE: kfcv.classifier(data, class, classifier, k = ...)
    #
    # where data is the data frame (each column is an attribute, and
    # each row is an observation), class is the column index for the
    # attribute to be predicted, and classifier is a function which
    # takes a training set, a test set, and a class column index

    all.err = numeric(0)
    result = list()
    alltestingindices = kfcv.testing(dim(data)[1])

    for (i in 1:k) {
        testingindices = alltestingindices[[i]]
        train = data[-testingindices,] # take all rows that are not in testingindicates list
        test = data[testingindices,] # take all rows that are in testingindicates list

        #result[[i]] = classifier.naivebayes(train, test, class)
        result[[i]] = classifier.decisiontree(train, test, class, testingindices, classTested)

        #print(result[[i]])
        #print(kfcv.computeTP(result[[i]]))
        #print(kfcv.computeFP(result[[i]]))
        #print(kfcv.computeFN(result[[i]]))
        #print(kfcv.computeTN(result[[i]]))

        err = kfcv.computeACC(result[[i]])
        
        all.err = rbind(all.err, err)
    }
    err.cv = mean(all.err)
}

kfcv.increment = function(data, value) {

    eval.parent(substitute(data <- data + value))
}

classifier.naivebayes = function(train, test, class) {

    # simple example of a classifier
    # requires library(e1071)
    model = naiveBayes(train[, - class], train[, class])
    table(predict(model, test[, - class]), test[, class])
}

classifier.decisiontree = function(train, test, class, testindicates, classTested = 1) {

    # decision tree classifier
    # requires library(rpart)
    modelTree = rpart(train[, classTested] ~ train[, class], method = "class", data = train) #%% dim(test)[2]
    testPred = predict(modelTree, newData = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

# coefficent calculations https://en.wikipedia.org/wiki/Confusion_matrix

kfcv.computeTP = function(result) {

    result[1, 1]
}

kfcv.computeTN = function(result) {

    sum(result) - sum(result[1, ]) - sum(result[, 1]) + sum(result[1, 1])
}

kfcv.computeFP = function(result) {

    sum(result[1, ]) - result[1, 1]
}

kfcv.computeFN = function(result) {

    sum(result[, 1]) - result[1, 1]
}

kfcv.computeTPR = function(result) {

    kfcv.computeTP(result)/(kfcv.computeTP(result) + kfcv.computeFN(result))
}

kfcv.computeTNR = function(result) {

    kfcv.computeTN(result)/(kfcv.computeTN(result) + kfcv.computeFP(result))
}

kfcv.computePPV = function(result) {

    kfcv.computeTP(result)/(kfcv.computeTP(result) + kfcv.computeFP(result))
}

kfcv.computeNPV = function(result) {
    
    kfcv.computeTN(result)/(kfcv.computeTN(result) + kfcv.computeFN(result))
}

kfcv.computeFNR = function(result) {

    kfcv.computeFN(result)/(kfcv.computeFN(result) + kfcv.computeTP(result))
}

kfcv.computeFPR = function(result) {

    kfcv.computeFP(result)/(kfcv.computeFP(result) + kfcv.computeTN(result))
}

kfcv.computeFDR = function(result) {

    kfcv.computeFP(result)/(kfcv.computeFP(result) + kfcv.computeTP(result))
}

kfcv.computeFOR = function(result) {

    kfcv.computeFN(result)/(kfcv.computeFN(result) + kfcv.computeTN(result))
}

kfcv.computeACC = function(result) {

    (kfcv.computeTP(result) + kfcv.computeTN(result)) / sum(result)
}

# coefficient calculations end

kfcv.error = function(data, n, classTested = 1, k = 10) {

    all.err = numeric(0)
    for (i in 1:n) {
        err = kfcv.classifier(data, i, classTested, classifier.decisiontree, 5)
        cat(err, " is the error of iteration ", i, "\n")
        if (err != 0)
            all.err = rbind(all.err, err)
    }
    max(all.err)
}

#kfcv.sizes(dim(encData), 5)
#kfcv.testing(dim(encData)[1], 5)
#kfcv.classifier(encData, 1, classifier.decisiontree , 5)

print(kfcv.error(encData, 5, 1, 5))

