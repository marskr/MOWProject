library(ROCR)
library(e1071)
library(Hmisc)
source("DataNormalization/DataEncryption.R")
source("DataNormalization/WriteToFile.R")
source("CrossValidation/Classifiers.R")
source("CrossValidation/Coefficients.R")
source("CrossValidation/CrossValidation.R")

iterator = 1
separator = '\t'
appending = TRUE
resFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/CrossValidation/CrossValidationResult.txt"

#head(encData, 5)

kfcv.main = function(data, classifier, classTested = 1, n = 3, k = 10, stopval = 1) {

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

    # we have to break iterating, when just 1 attribute is left - making prediction of one attribute makes no sense
    res = elemno - 1

    for (i in 1:elemno) {

        # break moment
        if (i == res) break

        # that kfcv.subsets method generates subsets of provided data in every iteration 
        subsetslist = kfcv.subsets(elemno, i)
        subsetlen = length(subsetslist)
        print(subsetlen)

        # finding minimal delta before computation step
        prev.delta = min(all.delta)

        for (j in 1:subsetlen) {

            set = subsetslist[[j]]
            err = kfcv.error(data[, set], classifier, classTested, n, k)
            all.err = rbind(all.err, err)
            #cat(j, " iteration error: ", err, " \n")

            post.err = err * 100
            delta = abs(post.err - prev.err)

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
            break
        }

    # at the end we have to compute also first level of data (with not selected features)
    err = kfcv.error(data, classifier, classTested, n, k)

    # in every step we binding computed error with error list
    all.err = rbind(all.err, err)

    # omit not a number values
    all.err = na.omit(all.err)

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
classifier = classifier.naivebayes

# the class that we are trying get dependencies
classTested = 1

# the stop criterium (difference between errors gives us delta & then we are checking delta differences)
# if stopval is higher than delta difference - we are stopping wrapper. If you set 0 - than no stop criterium will be used. 
stopval = 0

kfcv.main(encData, classifier, classTested, n, k, stopval)

