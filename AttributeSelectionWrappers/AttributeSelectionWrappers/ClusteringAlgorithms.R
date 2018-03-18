library(magrittr)
library(cluster);
source("ExtractDataModule.R")

# saving to file settings
separator = '\t'
appending = TRUE

# method that saves cluster calculated data to a given file
writeAllCluster = function(clust) {
    write("\n*****CENTERS***** \n", clusterResultFile, sep = '\t', append = !appending)
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

writeToFile = function(resName, resPart) {
    write(resName, clusterResultFile, sep = separator, append = appending)
    write(resPart, clusterResultFile, sep = separator, append = appending)
}

transformedData = transformedData %>% na.omit()
# head(transformedData,5)

# display unique classes in  row 1,2,3,4,5,6:
overallRows = length(unique(transformedData$CELL_ID))
typesServersNo = length(unique(transformedData$DSLS_SERVER))
typesLicsNo = length(unique(transformedData$LIC_TYPE))
typesCountriesNo = length(unique(transformedData$COUNTRY))
typesContinentsNo = length(unique(transformedData$CONTINENT))
typesOSNo = length(unique(transformedData$OPERATING_SYSTEM))

uniqueServers = unique(transformedData$DSLS_SERVER)
uniqueLics = unique(transformedData$LIC_TYPE)
uniqueCountries = unique(transformedData$COUNTRY)
uniqueContinents = unique(transformedData$CONTINENT)
uniqueOS = unique(transformedData$OPERATING_SYSTEM)

minedDataFrame = data.frame(servers = c(transformedData$DSLS_SERVER),
                            lics = c(transformedData$LIC_TYPE),
                            countries = c(transformedData$COUNTRY),
                            continents = c(transformedData$CONTINENT),
                            OS = c(transformedData$OPERATING_SYSTEM),
                            stringsAsFactors = FALSE)

minedDataFrame$servers.num = as.numeric(factor(minedDataFrame$servers, levels = c(uniqueServers))) 
minedDataFrame$lics.num = as.numeric(factor(minedDataFrame$lics, levels = c(uniqueLics)))
minedDataFrame$countries.num = as.numeric(factor(minedDataFrame$countries, levels = c(uniqueCountries)))
minedDataFrame$continents.num = as.numeric(factor(minedDataFrame$continents, levels = c(uniqueContinents)))
minedDataFrame$OS.num = as.numeric(factor(minedDataFrame$OS, levels = c(uniqueOS)))

# head(minedDataFrame, 5)
# rxGetInfo(minedDataFrame, getVarInfo = TRUE, numRows = 3)

# drop non numeric columns
droppedStringMDF = minedDataFrame[, 6:10]

# head(droppedStringMDF, 5)

# determine number of clusters
wss <- (nrow(droppedStringMDF) - 1) * sum(apply(droppedStringMDF, 2, var))
for (i in 2:10)
    wss[i] <- sum(kmeans(droppedStringMDF, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# in this example we will try with 6 and 10 clusters!

# Set.seed for random number generator for predictability
set.seed(10);

# Generate clusters using rxKmeans and output key 
clustRXKMEANS <- rxKmeans(~servers.num + lics.num + countries.num + continents.num + OS.num, droppedStringMDF,
                  numClusters = 6, outFile = NULL)

clusterResultFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/clusteringRXKMEANS.txt"

writeAllCluster(clustRXKMEANS)

clusplot(droppedStringMDF, clustRXKMEANS$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, plotchar = TRUE);

set.seed(10);

clustKMEANS <- kmeans(droppedStringMDF, 6)

clusterResultFile = "C:/GithubRepos/MOWProject/AttributeSelectionWrappers/AttributeSelectionWrappers/clusteringKMEANS.txt"

writeAllCluster(clustKMEANS)


