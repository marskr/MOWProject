library(magrittr)
library(cluster)
source("ClusteringAlgorithms/LoadNormalizedData.R")

# saving to file settings
separator = '\t'
appending = TRUE

# method that saves cluster calculated data to a given file
writeToFile = function(resName, resPart) {
    write(resName, clusterResultFile, sep = separator, append = appending)
    write(resPart, clusterResultFile, sep = separator, append = appending)
}

writeAllCluster = function(clust) {
    write("\n*****CENTERS***** \n", clusterResultFile, sep = '\t', append = appending)
    write(clust$centers, clusterResultFile, sep = separator, append = appending)
    writeToFile("\n*****SIZE***** \n", clust$size)
    writeToFile("\n*****WITHINSS***** \n", clust$withinss)
    writeToFile("\n*****VALID.OBS***** \n", clust$valid.obs)
    writeToFile("\n*****MISSING.OBS***** \n", clust$missing.obs)
    writeToFile("\n*****NUM_ITERATIONS***** \n", clust$numIterations)
    writeToFile("\n*****TOT.WITHINSS***** \n", clust$tot.withinss)
    writeToFile("\n*****TOTSS***** \n", clust$totss)
    writeToFile("\n*****BETWEENSS***** \n", clust$betweenss)
    writeToFile("\n*****CLUSTER***** \n", clust$cluster)
}

preprocessData = function(transformedData) {
    transformedData = transformedData %>% na.omit()

    uniqueServers = unique(transformedData$Serv)
    uniqueLics = unique(transformedData$Lics)
    uniqueCountries = unique(transformedData$Count)
    uniqueContinents = unique(transformedData$Conti)
    uniqueOS = unique(transformedData$OS)

    minedDataFrame = data.frame(servers = c(transformedData$Serv),
                            lics = c(transformedData$Lics),
                            countries = c(transformedData$Count),
                            continents = c(transformedData$Conti),
                            OS = c(transformedData$OS),
                            stringsAsFactors = FALSE)

    minedDataFrame$servers.num = as.numeric(factor(minedDataFrame$servers, levels = c(uniqueServers)))
    minedDataFrame$lics.num = as.numeric(factor(minedDataFrame$lics, levels = c(uniqueLics)))
    minedDataFrame$countries.num = as.numeric(factor(minedDataFrame$countries, levels = c(uniqueCountries)))
    minedDataFrame$continents.num = as.numeric(factor(minedDataFrame$continents, levels = c(uniqueContinents)))
    minedDataFrame$OS.num = as.numeric(factor(minedDataFrame$OS, levels = c(uniqueOS)))

    return(minedDataFrame[, 6:10])
}

droppedStringMDF = preprocessData(transformedData)

#head(droppedStringMDF, 5)

# determine number of clusters
wss <- (nrow(droppedStringMDF) - 1) * sum(apply(droppedStringMDF, 2, var))
for (i in 2:10)
    wss[i] <- sum(kmeans(droppedStringMDF, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# in this example we will try with 3 clusters!

clusterResultFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/ClusteringAlgorithms/clusteringRXKMEANS.txt"

# Set.seed for random number generator for predictability
set.seed(10);

## Generate clusters using rxKmeans and output key 
#clustRXKMEANS <- rxKmeans(~servers.num + lics.num + countries.num + continents.num + OS.num, droppedStringMDF,
                  #numClusters = 6, outFile = NULL)
#clusplot(droppedStringMDF, clustRXKMEANS$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, plotchar = TRUE);

# some kmeans algorithm tests with formula expression

clustRXKMEANS = rxKmeans(~servers.num + lics.num, droppedStringMDF, numClusters = 3, outFile = NULL)

write("***~servers.num + lics.num***", clusterResultFile, append = FALSE)
writeAllCluster(clustRXKMEANS)

clustRXKMEANS = rxKmeans(~servers.num + countries.num, droppedStringMDF, numClusters = 3, outFile = NULL)

write("***~servers.num + countries.num", clusterResultFile, append = FALSE)
writeAllCluster(clustRXKMEANS)

clustRXKMEANS = rxKmeans(~servers.num + continents.num, droppedStringMDF, numClusters = 3, outFile = NULL)

write("***~servers.num + continents.num", clusterResultFile, append = FALSE)
writeAllCluster(clustRXKMEANS)

clustRXKMEANS = rxKmeans(~servers.num + OS.num, droppedStringMDF, numClusters = 3, outFile = NULL)

write("***~servers.num + OS.num", clusterResultFile, append = FALSE)
writeAllCluster(clustRXKMEANS)

#clusterResultFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/clusteringKMEANS.txt"

#set.seed(10);

#clustKMEANS <- kmeans(droppedStringMDF, 6)

#writeAllCluster(clustKMEANS)

#clusplot(droppedStringMDF, clustKMEANS$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, plotchar = TRUE);


