library(magrittr)

head(transformedData,5)

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