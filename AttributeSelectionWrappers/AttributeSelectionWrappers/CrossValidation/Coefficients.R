## coefficent calculations https://en.wikipedia.org/wiki/Confusion_matrix

#' compute true positive rate, as an input we take confusion matrix
kfcv.computeTP = function(result) {

    result[1, 1]
}

#' compute true negative rate, as an input we take confusion matrix
kfcv.computeTN = function(result) {

    sum(result) - sum(result[1,]) - sum(result[, 1]) + sum(result[1, 1])
}

#' compute false positive rate, as an input we take confusion matrix
kfcv.computeFP = function(result) {

    sum(result[1,]) - result[1, 1]
}

#' compute false negative rate, as an input we take confusion matrix
kfcv.computeFN = function(result) {

    sum(result[, 1]) - result[1, 1]
}

#' compute sensivity/recall rate, as an input we take confusion matrix
kfcv.computeTPR = function(result) {

    kfcv.computeTP(result) / (kfcv.computeTP(result) + kfcv.computeFN(result))
}

#' compute true negative rate, as an input we take confusion matrix
kfcv.computeTNR = function(result) {

    kfcv.computeTN(result) / (kfcv.computeTN(result) + kfcv.computeFP(result))
}

#' compute positive predictive value, as an input we take confusion matrix
kfcv.computePPV = function(result) {

    kfcv.computeTP(result) / (kfcv.computeTP(result) + kfcv.computeFP(result))
}

#' compute negative predictive value, as an input we take confusion matrix
kfcv.computeNPV = function(result) {

    kfcv.computeTN(result) / (kfcv.computeTN(result) + kfcv.computeFN(result))
}

#' compute false negative/miss rate, as an input we take confusion matrix
kfcv.computeFNR = function(result) {

    kfcv.computeFN(result) / (kfcv.computeFN(result) + kfcv.computeTP(result))
}

#' compute fall out/false positive rate, as an input we take confusion matrix
kfcv.computeFPR = function(result) {

    kfcv.computeFP(result) / (kfcv.computeFP(result) + kfcv.computeTN(result))
}

#' compute false discovery rate, as an input we take confusion matrix
kfcv.computeFDR = function(result) {

    kfcv.computeFP(result) / (kfcv.computeFP(result) + kfcv.computeTP(result))
}

#' compute false omission rate, as an input we take confusion matrix
kfcv.computeFOR = function(result) {

    kfcv.computeFN(result) / (kfcv.computeFN(result) + kfcv.computeTN(result))
}

#' compute accuracy, as an input we take confusion matrix
kfcv.computeACC = function(result) {

    (kfcv.computeTP(result) + kfcv.computeTN(result)) / sum(result)
}

# coefficient calculations end
