library(pROC)

category <- c(1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0)
prediction <- rev(seq_along(category))
#prediction[9:10] <- mean(prediction[9:10])

roc_obj <- multiclass.roc(category, prediction)
auc(roc_obj)
