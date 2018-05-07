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

    result = list()
    alltestingindices = kfcv.testing(dim(data)[1])
    for (i in 1:k) {
        testingindices = alltestingindices[[i]]
        train = data[-testingindices,] # take all rows that are not in testingindicates list
        test = data[testingindices,] # take all rows that are in testingindicates list
        result[[i]] = classifier(train, test, class)
    }
    result
}

classifier.naivebayes = function(train, test, class) {
    # simple example of a classifier
    # requires library(e1071)
    model = naiveBayes(train[, - class], train[, class])
    table(predict(model, test[, - class]), test[, class])
}

kfcv.sizes(dim(encData), 5)
kfcv.testing(dim(encData)[1], 5)
kfcv.classifier(encData, 3, classifier.naivebayes, 5)
