library(magrittr)
library(cluster);
source("ExtractDataModule.R")

transformedData = transformedData %>% na.omit()
head(transformedData,5)

# display unique classes in  row 1,2,3,4,5,6:
overallRows = length(unique(transformedData[, 1]))
typesServersNo = length(unique(transformedData[, 2]))
typesLicsNo = length(unique(transformedData[, 3]))
typesCountriesNo = length(unique(transformedData[, 4]))
typesContinentsNo = length(unique(transformedData[, 5]))
typesOSNo = length(unique(transformedData[, 6]))

uniqueServers = unique(transformedData[, 2])
uniqueLics = unique(transformedData[, 3])
uniqueCountries = unique(transformedData[, 4])
uniqueContinents = unique(transformedData[, 5])
uniqueOS = unique(transformedData[, 6])

minedDataFrame = data.frame(servers = c(transformedData[, 2]),
                            lics = c(transformedData[, 3]),
                            countries = c(transformedData[, 4]),
                            continents = c(transformedData[, 5]),
                            OS = c(transformedData[, 6]),
                            stringsAsFactors = FALSE)

minedDataFrame$servers.num = as.numeric(factor(minedDataFrame$servers, levels = c(uniqueServers))) 
minedDataFrame$lics.num = as.numeric(factor(minedDataFrame$lics, levels = c(uniqueLics)))
minedDataFrame$countries.num = as.numeric(factor(minedDataFrame$countries, levels = c(uniqueCountries)))
minedDataFrame$continents.num = as.numeric(factor(minedDataFrame$continents, levels = c(uniqueContinents)))
minedDataFrame$OS.num = as.numeric(factor(minedDataFrame$OS, levels = c(uniqueOS)))

head(minedDataFrame, 5)

droppedStringMDF = minedDataFrame[, 6:10]

head(droppedStringMDF, 5)

# determine number of clusters
wss <- (nrow(droppedStringMDF) - 1) * sum(apply(droppedStringMDF, 2, var))
for (i in 2:10)
    wss[i] <- sum(kmeans(droppedStringMDF, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# in this example we will try with 6 and 10 clusters!

# Set.seed for random number generator for predictability
set.seed(10);

# Generate clusters using rxKmeans and output key / cluster to a table in SQL Server called return_cluster
clust <- rxKmeans(~servers.num + lics.num + countries.num + continents.num + OS.num, droppedStringMDF, numClusters = 6
         , outFile = return_cluster, outColName = "cluster")

clusplot(droppedStringMDF, clust$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, plotchar = TRUE);
