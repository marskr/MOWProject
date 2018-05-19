library(ROCR)
library(e1071)
library(Hmisc)
source("DataNormalization/DataEncryption.R")
source("DataNormalization/WriteToFile.R")

iterator = 1
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
    writeToFile("****************************************", 
                "*********** Iteration number ***********", resFile)
    writeToFile(iterator, "****************************************", resFile)
    writeToFile("Recall/Sensivity(TPR): ", err.TPR, resFile)
    writeToFile("Specifity/True negative rate(TNR): ", err.TNR, resFile)
    writeToFile("Precision/Positive predictive value(PPV): ", err.TPR, resFile)
    writeToFile("Negative predictive value(NPV): ", err.TPR, resFile)
    writeToFile("Miss rate/False negative rate(FNR): ", err.FNR, resFile)
    writeToFile("Fall-out/False positive rate(FPR): ", err.FPR, resFile)
    writeToFile("False discovery rate(FDR): ", err.FDR, resFile)
    writeToFile("False omission rate(FOR): ", err.FOR, resFile)
    writeToFile("Accuracy(ACC): ", err.ACC, resFile)
    writeToFile("----------------------------------------", 
                "----------------------------------------", resFile)

    # iterator to save into file which loop was computed
    inc(iterator) <<- 1
}

## coefficent calculations https://en.wikipedia.org/wiki/Confusion_matrix

#' compute true positive rate, as an input we take confusion matrix
kfcv.computeTP = function(result) {

    result[1, 1]
}

#' compute true negative rate, as an input we take confusion matrix
kfcv.computeTN = function(result) {

    sum(result) - sum(result[1, ]) - sum(result[, 1]) + sum(result[1, 1])
}

#' compute false positive rate, as an input we take confusion matrix
kfcv.computeFP = function(result) {

    sum(result[1, ]) - result[1, 1]
}

#' compute false negative rate, as an input we take confusion matrix
kfcv.computeFN = function(result) {

    sum(result[, 1]) - result[1, 1]
}

#' compute sensivity/recall rate, as an input we take confusion matrix
kfcv.computeTPR = function(result) {

    kfcv.computeTP(result)/(kfcv.computeTP(result) + kfcv.computeFN(result))
}

#' compute true negative rate, as an input we take confusion matrix
kfcv.computeTNR = function(result) {

    kfcv.computeTN(result)/(kfcv.computeTN(result) + kfcv.computeFP(result))
}

#' compute positive predictive value, as an input we take confusion matrix
kfcv.computePPV = function(result) {

    kfcv.computeTP(result)/(kfcv.computeTP(result) + kfcv.computeFP(result))
}

#' compute negative predictive value, as an input we take confusion matrix
kfcv.computeNPV = function(result) {
    
    kfcv.computeTN(result)/(kfcv.computeTN(result) + kfcv.computeFN(result))
}

#' compute false negative/miss rate, as an input we take confusion matrix
kfcv.computeFNR = function(result) {

    kfcv.computeFN(result)/(kfcv.computeFN(result) + kfcv.computeTP(result))
}

#' compute fall out/false positive rate, as an input we take confusion matrix
kfcv.computeFPR = function(result) {

    kfcv.computeFP(result)/(kfcv.computeFP(result) + kfcv.computeTN(result))
}

#' compute false discovery rate, as an input we take confusion matrix
kfcv.computeFDR = function(result) {

    kfcv.computeFP(result)/(kfcv.computeFP(result) + kfcv.computeTP(result))
}

#' compute false omission rate, as an input we take confusion matrix
kfcv.computeFOR = function(result) {

    kfcv.computeFN(result)/(kfcv.computeFN(result) + kfcv.computeTN(result))
}

#' compute accuracy, as an input we take confusion matrix
kfcv.computeACC = function(result) {

    (kfcv.computeTP(result) + kfcv.computeTN(result)) / sum(result)
}

# coefficient calculations end

## classifiers 

classifier.naivebayes = function(train, test, class, testindicates, classTested = 1) {

    # simple example of a classifier
    # requires library(e1071)
    model = naiveBayes(train[, - class], train[, class])

    table(predict(model, test[, - class]), test[, class])
}

classifier.decisiontree = function(train, test, class, testindicates, classTested = 1) {

    # decision tree classifier
    # requires library(rpart)
    modelTree = rpart(train[, classTested] ~., method = "class", data = train) #train[, class], method = "class", data = train)
    testPred = predict(modelTree, newData = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

classifier.decisiontreemicrosoft = function(train, test, class, testindicates, classTested = 1) {

    modelTree = rxDTree(formula = train[, classTested] ~., method = "class", data = train)
    testPred = rxPredict(modelTree, data = test, type = "class")

    table(testPred[testindicates], test[, classTested])
}

# classifiers end

kfcv.subsets = function(elementsno, level) {

    subsetslist = c()

    for (i in 1:elementsno) {

        subsetslist = append(subsetslist, i)
        #cat("iteration ", i, "\n")
    }
    #subsetslist
    combn(subsetslist, elementsno - level, simplify = FALSE)
}

kfcv.error = function(data, n, classifier, classTested = 1, k = 10) {

    all.err = numeric(0)
    for (i in 1:n) {
        err = kfcv.classifier(data, i, classTested, classifier, 5)
        #cat(err, " is the error of iteration ", i, "\n")
        if (err != 0)
            all.err = rbind(all.err, err)
    }

    kfcv.stats(data, which.min(all.err), classTested, classifier, k)

    mean(all.err)
}

kfcv.main = function(data, n, classifier, classTested = 1, k = 10, stopval = 1) {

    write("", resFile, sep = separator, append = FALSE)

    elemno = dim(data)[2]

    # fields that contain error computing (1 - accuracy)
    all.err = numeric(0)
    prev.err = double(0)
    post.err = double(0)

    # vector gathering all deltas for finding minimal one (delta) 
    all.delta = double(0)
    delta = double(0)
    prev.delta = double(0)
    post.delta = double(0)

    # setting up basic fields 
    prev.err = 0.0
    post.err = 0.0
    delta = 0.0
    all.delta = 100

    for (i in 1:elemno) {

        # that kfcv.subsets method generates subsets of provided data in every iteration 
        subsetslist = kfcv.subsets(elemno, i)
        subsetlen = length(subsetslist)
        print(subsetlen)

        # finding minimal delta before computation step
        prev.delta = min(all.delta)

        for (j in 1:subsetlen) {

            set = subsetslist[[j]]
            err = kfcv.error(data[, set], n, classifier, classTested, k)
            all.err = rbind(all.err, err)
            #cat(j, " iteration error: ", err, " \n")

            post.err = err * 100
            delta = abs(post.err - prev.err)
            prev.err = post.err
            all.delta = rbind(all.delta, delta)

            cat("The delta is ", delta, " post: ", post.err, " prev: ", prev.err, "\n")
        }

        # finding max delta after computation step
        post.delta = min(all.delta)

        # if difference between deltas is lower than stop criterium - we are breaking computations
        if (abs(post.delta - prev.delta) < stopval)
            break
    }

    # at the end we have to compute also first level of data (with not selected features)
    err = kfcv.error(data, n, classifier, classTested, k)

    # in every step we binding computed error with error list
    all.err = rbind(all.err, err)

    # at the end we are returning min error 
    cat("the minimum error was ", min(all.err), " for iteration ", which.min(all.err))
}

#kfcv.sizes(dim(encData), 5)
#kfcv.testing(dim(encData)[1], 5)
#kfcv.classifier(encData, 1, classifier.decisiontree , 5)

# number of cross validation that we will perform, to get average of them.
n = 3

# for cross validation. Number of folds, that we are providing
k = 10

# the type of classifier used (naivebayes, decisiontree)
classifier = classifier.decisiontree

# the class that we are trying get dependencies
classTested = 1

# the stop criterium (difference between errors gives us delta & then we are checking delta differences)
# if stopval is higher than delta difference - we are stopping wrapper. If you set 0 - than no stop criterium will be used. 
stopval = 0

kfcv.main(encData, n, classifier, classTested, k, stopval)

