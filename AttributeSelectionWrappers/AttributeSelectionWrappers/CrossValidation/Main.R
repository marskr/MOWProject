library(datasets)
library(ROCR)
library(e1071)
library(Hmisc)
library(randomForest)
library(MASS)

source("DataNormalization/WriteToFile.R")
source("DataNormalization/ReadCSV.R")
source("CrossValidation/Classifiers.R")
source("CrossValidation/Coefficients.R")
source("CrossValidation/CrossValidation.R")

iterator = 0

# the number, which is responsible for checking, if we want to comply to stop criterium (or not)
boundaryCrossedIterator = 0

separator = '\t'
appending = TRUE

# directory of file, that contains cross-validation result
resFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/CrossValidation/CrossValidationResult.txt"

# directory of file, that contains tested class result
rocFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/CrossValidation/ReceiverOperationStatisticResult.txt"

# directory of file, that contains command line result
cmdFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/CrossValidation/CommandLineResult.txt"


kfcv.main = function(data, classifier, ROC, classTested = 1, n = 3, k = 10, stopval = 1, count = 1) {

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

    # we have to break iterating, when just 1 attribute is left - making prediction of one attribute makes no sense
    res = elemno - 1

    for (i in 1:(elemno)) {

        # break moment
        if (i == res) break

        # that kfcv.subsets method generates subsets of provided data in every iteration 
        subsetslist = kfcv.subsets(elemno, i)
        subsetlen = length(subsetslist)
        print(subsetlen)

        # finding minimal delta before computation step
        prev.delta = min(all.delta)

        res2 = subsetlen - 1

        for (j in 1:res2) {

            set = subsetslist[[j]]
            writeToFile("Actual set is: ", set, cmdFile)
            err = kfcv.error(data[, set], classifier, ROC, classTested, n, k)
            all.err = rbind(all.err, err)
            #cat(j, " iteration error: ", err, " \n")

            post.err = err * 100
            delta = abs(post.err - prev.err)

            writeToFile("****************************************",
                "*********** Iteration number ***********", cmdFile)
            writeToFile(iterator, "****************************************", cmdFile)
            writeToFile("The delta is ", delta, cmdFile)
            writeToFile("Subset list is ", set, cmdFile)
            writeToFile("----------------------------------------",
                "----------------------------------------", cmdFile)

            cat("The delta is ", delta, " post: ", post.err, " prev: ", prev.err, "\n")

            prev.err = post.err

            all.delta = rbind(all.delta, delta)
        }

        # finding max delta after computation step
        post.delta = min(all.delta)

        if (is.nan(post.delta) || is.nan(prev.delta))
            next

        # if difference between deltas is lower than stop criterium - we are breaking computations
        if (abs(post.delta - prev.delta) < stopval)
            inc(boundaryCrossedIterator) <<- 1

        if (boundaryCrossedIterator >= count)
            break
        }

    # at the end we have to compute also first level of data (with not selected features)
    err = kfcv.error(data, classifier, ROC, classTested, n, k)

    # in every step we binding computed error with error list
    all.err = rbind(all.err, err)

    # omit not a number values
    all.err = na.omit(all.err)

    # at the end we are returning min error 
    cat("the minimum error was ", min(all.err), " for iteration ", which.min(all.err))

    writeToFile("the minimum error was ", min(all.err), cmdFile)
    writeToFile("for iteration ", which.min(all.err), cmdFile)
}

#kfcv.sizes(dim(encData), 5)
#kfcv.testing(dim(encData)[1], 5)
#kfcv.classifier(encData, 1, classifier.decisiontree , 5)

# airData, Aids2
dataset = airData[, 1:10]

# number of cross validation that we will perform, to get average of them.
n = 1

# for cross validation. Number of folds, that we are providing
k = 3

# the type of classifier used (naivebayes, decisiontree, randomforest)
classifier = classifier.randomforest

# the type of ROC analysis (and classifier that will be used for classification)
ROC = ROC.randomforest

# the class that we are trying get dependencies
classTested = 3

# the stop criterium (difference between errors gives us delta & then we are checking delta differences)
# if stopval is higher than delta difference - we are stopping wrapper. If you set 0 - than no stop criterium will be used. 
stopval = double(0)
stopval = 5

# how many times the stopval must be exceeded to stop the algorithm
count = 1

#kfcv.main(dataset, classifier, ROC, classTested, n, k, stopval, count)

size =  dim(dataset)[2]

# cleaning all previously-saved files 
write("", resFile, sep = separator, append = FALSE)
write("", rocFile, sep = separator, append = FALSE)
write("", cmdFile, sep = separator, append = FALSE)

# checking all independent values 
for (i in 1:size) {

    # the number, that increments inside write to file code part, after every saved measure - we want to know 
    # which iteration of algorithm was saved to file
    iterator = 1

    writeToFile("Class tested: ", i, resFile)
    writeToFile("Class tested: ", i, rocFile)
    writeToFile("Class tested: ", i, cmdFile)

    kfcv.main(Aids2, classifier, ROC, i, n, k, stopval, count)
}
