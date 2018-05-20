library("randomForest")
set.seed(1234)

ind = sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainData = iris[ind == 1,]
testData = iris[ind == 2,]

iris_rf = randomForest(Species ~ ., data = trainData, ntree = 100, proximity = T)
table(predict(iris_rf), trainData$Species)

iris_rf