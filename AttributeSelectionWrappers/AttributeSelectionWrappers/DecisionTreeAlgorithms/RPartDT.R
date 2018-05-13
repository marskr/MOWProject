library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
source("DataNormalization/DataEncryption.R")
source("DecisionTreeAlgorithms/SystemParams.R")
source("DataNormalization/PreprocessData.R")
source("DataNormalization/WriteToFile.R")

write("", resFile, sep = separator, append = FALSE)
writeToFile("unique licences: ", unique(encData$Licences), resFile)
writeToFile("unique servers: ", unique(encData$Server), resFile)
writeToFile("unique OS: ", unique(encData$OS), resFile)

# Classification Tree with rpart

#algoData = preprocessData(encData)

head(encData, 5)

# encData$servers.num | encData$OS.num | encData$lics.num | encData$countries.num | encData$continents.num
myTree = rpart(encData$OS ~ encData$Licences + encData$Country + encData$LicencesUsage, method = "class", data = encData,
                                                                                   minsplit = 2,
                                                                                   minbucket = 1,
                                                                                   maxdepth = 2,
                                                                                   cp = -1)

myTree$variable.importance

printcp(myTree)

summary(myTree)

# myTree = prune(myTree, cp = 0.99) # calculate this value!!!

fancyRpartPlot(myTree)

encData$OSclass = predict(myTree, newdata = encData, type = "class") #Returns the predicted class

encData$OSprob = predict(myTree, newdata = encData, type = "prob")

head(encData, 100)

fancyRpartPlot(myTree)

plot(myTree, branch = 0.4, uniform = TRUE, compress = TRUE)
text(myTree, all = TRUE, use.n = TRUE)

## cross validation

#n = nrow(encData)
#k = 5
#part = n %/% k
#set.seed(5)

#ale = runif(n)
#rang = rank(ale)
#bloc = (rang - 1) %/% part + 1
#bloc = as.factor(bloc)
#print(summary(bloc))

#all.err = numeric(0)

#for (K in 1:k) {
    #arbre = rpart(encData$OS ~ encData$Licences, data = encData[bloc != k], method = "class")
    #pred = predict(arbre, newdata = encData[bloc == k,], type = "class")

    #mc = table(encData$OS[bloc != k], pred)

    #err = 1.0 - (mc[1, 1] + mc[2, 2]) / sum(mc)

    #all.err = rbind(all.err, err)
#}

#print(all.err)
