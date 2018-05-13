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

kfcv.classifier = function(data, class, classifier, k = 10) {
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
        result[[i]] = classifier.decisiontree(train, test, class, testingindices)

        err = 1.0 - (result[[i]][1, 1] + result[[i]][2, 2]) / sum(result[[i]])
        all.err = rbind(all.err, err)
    }
    err.cv = mean(all.err)
    err.cv
}

classifier.naivebayes = function(train, test, class) {
    # simple example of a classifier
    # requires library(e1071)
    model = naiveBayes(train[, - class], train[, class])
    table(predict(model, test[, - class]), test[, class])
}

classifier.decisiontree = function(train, test, class, testindicates) {
    # decision tree classifier
    # requires library(rpart)
    modelTree = rpart(train[, class] ~ train[, class + 2], method = "class", data = train)
    testPred = predict(modelTree, newData = test, type = "class")

    mc = table(testPred[testindicates], test[, class])
}

kfcv.error = function(data, n, k = 10) {

    all.err = numeric(0)
    for (i in 1:n) {
        err = kfcv.classifier(data, i, classifier.naivebayes, 5)
        cat(err, " is the error of iteration ", i, "\n")
        if (err != 0)
            all.err = rbind(all.err, err)
    }
    min(all.err)
}

kfcv.sizes(dim(encData), 3)
kfcv.testing(dim(encData)[1], 3)
kfcv.classifier(encData, 1, classifier.naivebayes, 3)
kfcv.error(encData, 3, 3)

