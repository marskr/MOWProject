library(ROCR)
data(ROCR.simple)
head(cbind(ROCR.simple$predictions, ROCR.simple$labels), 5)

pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
class(pred)

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a = 0, b = 1)

AUC = performance(pred, measure = "auc")
result = AUC@y.values

ROC = 1 - result[[1]]


