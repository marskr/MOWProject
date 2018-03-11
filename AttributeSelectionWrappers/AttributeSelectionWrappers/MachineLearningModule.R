library(ggplot2)
source("ExtractDataModule.R")

# filter the data

polandData = subset(transformedData, COUNTRY = "Poland")

# creation of data frames 

polandData = data.frame(polandData)

                                    # Sort by:
polandData[order(polandData[, 2],   # SERVER
                 polandData[, 3],   # L_TYPE
                 polandData[, 6])   # OS
         ,]

head(polandData, 5)

ggplot(transformedData, aes(DSLS_SERVER, COUNTRY, color = CONTINENT)) + geom_point()

#set.seed(20)
#dataCluster <- kmeans(transformedData, 1, nstart = 20)
#dataCluster

#table(dataCluster$cluster, transformedData$CONTINENT)

#dataCluster$cluster <- as.factor(dataCluster$cluster)
#ggplot(iris, aes(LIC_TYPE, COUNTRY, color = transformedData$cluster)) + geom_point()

