source("DataNormalization/DataEncryption.R")

head(encData, 5)

set.seed(1234)

# classification
iris.sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.dtree <- rxDTree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
     data = iris[iris.sub,])

idtree <- rxDTree(Server ~ Licences + Continent, data = encData)


iris.dtree

table(rxPredict(iris.dtree, iris[-iris.sub,], type = "class")[[1]],
     iris[-iris.sub, "Species"])


