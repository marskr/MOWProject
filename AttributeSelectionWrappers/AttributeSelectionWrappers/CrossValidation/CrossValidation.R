library(ROCR)
library(e1071)
source("DataNormalization/DataEncryption.R")
source("DataNormalization/WriteToFile.R")

separator = '\t'
appending = TRUE
resFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/CrossValidation/CrossValidationResult.txt"

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
    # obtain list of lists of indexes, for cross validation of provided dataset
    alltestingindices = kfcv.testing(dim(data)[1])

    for (i in 1:k) {

        testingindices = alltestingindices[[i]]
        train = data[-testingindices,] # take all rows that are not in testingindicates list
        test = data[testingindices,] # take all rows that are in testingindicates list

        # using classifier to obtain confusion matrix as output
        result[[i]] = classifier(train, test, class, testingindices, classTested)

        # resubstitution error computation:
        err = 1 - kfcv.computeACC(result[[i]])
        
        all.err = rbind(all.err, err)
    }

    # compute mean of all resubstitute errors (k-folds cross validation)
    err.cv = mean(all.err)
}

kfcv.stats = function(data, class, classTested = 1, classifier, k = 10) {

    # creation of object that will store coefficients computed using confusion matrix
    all.TPR = numeric(0)
    all.TNR = numeric(0)
    all.PPV = numeric(0)
    all.NPV = numeric(0)
    all.FNR = numeric(0)
    all.FPR = numeric(0)
    all.FDR = numeric(0)
    all.FOR = numeric(0)
    all.ACC = numeric(0)

    result = list()
    alltestingindices = kfcv.testing(dim(data)[1])

    for (i in 1:k) {

        testingindices = alltestingindices[[i]]
        train = data[-testingindices,] # take all rows that are not in testingindicates list
        test = data[testingindices,] # take all rows that are in testingindicates list

        # computation of confusion matrix
        result[[i]] = classifier(train, test, class, testingindices, classTested)

        # compute coefficients using confusion matrix
        errTPR = kfcv.computeTPR(result[[i]])
        errTNR = kfcv.computeTNR(result[[i]])
        errPPV = kfcv.computePPV(result[[i]])
        errNPV = kfcv.computeNPV(result[[i]])
        errFNR = kfcv.computeFNR(result[[i]])
        errFPR = kfcv.computeFPR(result[[i]])
        errFDR = kfcv.computeFDR(result[[i]])
        errFOR = kfcv.computeFOR(result[[i]])
        errACC = kfcv.computeACC(result[[i]])

        # add coefficients to previously created objects
        all.TPR = rbind(all.TPR, errTPR)
        all.TNR = rbind(all.TNR, errTNR)
        all.PPV = rbind(all.PPV, errPPV)
        all.NPV = rbind(all.NPV, errNPV)
        all.FNR = rbind(all.FNR, errFNR)
        all.FPR = rbind(all.FPR, errFPR)
        all.FDR = rbind(all.FDR, errFDR)
        all.FOR = rbind(all.FOR, errFOR)
        all.ACC = rbind(all.ACC, errACC)
    }

    # calculate means from coefficient objects
    err.TPR = mean(all.TPR)
    err.TNR = mean(all.TNR)
    err.PPV = mean(all.PPV)
    err.NPV = mean(all.NPV)
    err.FNR = mean(all.FNR)
    err.FPR = mean(all.FPR)
    err.FDR = mean(all.FDR)
    err.FOR = mean(all.FOR)
    err.ACC = mean(all.ACC)

    # write all computations to file
    write("", resFile, sep = separator, append = FALSE)
    writeToFile("Recall/Sensivity(TPR): ", err.TPR, resFile)
    writeToFile("Specifity/True negative rate(TNR): ", err.TNR, resFile)
    writeToFile("Precision/Positive predictive value(PPV): ", err.TPR, resFile)
    writeToFile("Negative predictive value(NPV): ", err.TPR, resFile)
    writeToFile("Miss rate/False negative rate(FNR): ", err.FNR, resFile)
    writeToFile("Fall-out/False positive rate(FPR): ", err.FPR, resFile)
    writeToFile("False discovery rate(FDR): ", err.FDR, resFile)
    writeToFile("False omission rate(FOR): ", err.FOR, resFile)
    writeToFile("Accuracy(ACC): ", err.ACC, resFile)
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

kfcv.error = function(data, n, classifier, classTested = 1, k = 10) {

    all.err = numeric(0)
    for (i in 1:n) {
        err = kfcv.classifier(data, i, classTested, classifier, 5)
        cat(err, " is the error of iteration ", i, "\n")
        if (err != 0)
            all.err = rbind(all.err, err)
        else
            all.err = rbind(all.err, 1)
    }

    kfcv.stats(data, which.min(all.err), classTested, classifier, 5)
}

classifier.naivebayes = function(train, test, class, testindicates, classTested = 1) {

    # simple example of a classifier
    # requires library(e1071)
    model = naiveBayes(train[, - class], train[, class])

    table(predict(model, test[, - class]), test[, class])
}

classifier.decisiontree = function(train, test, class, testindicates, classTested = 1) {

    # decision tree classifier
    # requires library(rpart)
    modelTree = rpart(train[, classTested] ~ train[, class], method = "class", data = train)
    testPred = predict(modelTree, newData = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

#kfcv.sizes(dim(encData), 5)
#kfcv.testing(dim(encData)[1], 5)
#kfcv.classifier(encData, 1, classifier.decisiontree , 5)

kfcv.error(encData, 5, classifier.decisiontree, 1, 5)

