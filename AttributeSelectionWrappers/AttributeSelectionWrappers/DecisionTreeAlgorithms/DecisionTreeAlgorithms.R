source("DataNormalization/DataEncryption.R")
source("DecisionTreeAlgorithms/SystemParams.R")

# Classification Tree with rpart
library(rpart)

algoData = preprocessData(encData)

head(encData, 5)

# algoData$servers.num | algoData$OS.num | algoData$lics.num | algoData$countries.num | algoData$continents.num

fit = rpart(encData$Lics ~ encData$Count, method = "class", data = encData)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
windows(record = TRUE, width = 10, height = 10)
plot(fit, uniform = TRUE, main = "Classification Tree for Project")
text(fit, use.n = TRUE, all = TRUE, cex = .4)

# create attractive postscript plot of tree 
post(fit, file = filePath,
    title = "Classification Tree for Project")

