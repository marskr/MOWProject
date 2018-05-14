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

        err = 1.0 - kfcv.sum(result[[i]]) / sum(result[[i]])
        #print(result[[i]][1, 1] + result[[i]][2, 2]) / sum(result[[i]])

        all.err = rbind(all.err, err)
    }
    err.cv = mean(all.err)
}

kfcv.sum = function(result) {

    diagonalSum = 0
    for (i in 1:(dim(result)[1])) {
        kfcv.increment(diagonalSum, result[i, i])
    }
    diagonalSum
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
    index = ((class + 1) %% dim(train)[2]) + 1

    modelTree = rpart(train[, 1] ~ train[, class], method = "class", data = train) #%% dim(test)[2]
    testPred = predict(modelTree, newData = test, type = "class")

    table(testPred[testindicates], test[, class])
}

kfcv.error = function(data, n, k = 10) {

    all.err = numeric(0)
    for (i in 1:n) {
        err = kfcv.classifier(data, i, 2, classifier.decisiontree, 5)
        cat(err, " is the error of iteration ", i, "\n")
        if (err != 0)
            all.err = rbind(all.err, err)
        }
    min(all.err)
}

#kfcv.sizes(dim(transformedData), 5)
#kfcv.testing(dim(transformedData)[1], 5)
#kfcv.classifier(transformedData, 1, classifier.naivebayes, 5)
#kfcv.error(transformedData, 5, 5)
