library(magrittr)
source("DataNormalization/ExtractDataModule.R")

createArtificalDataVec = function(salt, typesno) {

    vec = c()
    for (i in 1:typesno) {
        nameLine = paste(salt, i, sep = "")
        vec = c(vec, nameLine)
    }
    return(vec)
}

encryptCell = function(unique, vec, cellVal) {

    for (i in 1:length(unique)) { if (cellVal == unique[i]) return(vec[i]) }
}

getEncryptedVec = function(uniqueData, vecData, data) {

    vec = c()
    for (i in 1:length(data)) { vec = c(vec, encryptCell(uniqueData, vecData, data[i])) }
    return(vec)
}

# encryption of data & creation of the data frame, which will store future data
getDataFrame = function(data) {

    data = data %>% na.omit()

    typesServersno = length(unique(data$DSLS_SERVER))
    typesLicsno = length(unique(data$LIC_TYPE))
    typesCountriesno = length(unique(data$COUNTRY))
    typesContinentsno = length(unique(data$CONTINENT))
    typesOSno = length(unique(data$OPERATING_SYSTEM))

    uniqueServers = unique(data$DSLS_SERVER)
    uniqueLics = unique(data$LIC_TYPE)
    uniqueCountries = unique(data$COUNTRY)
    uniqueContinents = unique(data$CONTINENT)
    uniqueOS = unique(data$OPERATING_SYSTEM)

    saltServ = "server_"
    saltLics = "licence_"
    saltCount = "country_"
    saltConti = "continent_"
    saltOS = "os_"

    vecServ = createArtificalDataVec(saltServ, typesServersno)
    vecLics = createArtificalDataVec(saltLics, typesLicsno)
    vecCount = createArtificalDataVec(saltCount, typesCountriesno)
    vecConti = createArtificalDataVec(saltConti, typesContinentsno)
    vecOS = createArtificalDataVec(saltOS, typesOSno)

    Server = getEncryptedVec(uniqueServers, vecServ, data$DSLS_SERVER)
    Licences = getEncryptedVec(uniqueLics, vecLics, data$LIC_TYPE)
    Continent = getEncryptedVec(uniqueContinents, vecConti, data$CONTINENT)
    Country = getEncryptedVec(uniqueCountries, vecCount, data$COUNTRY)
    OS = getEncryptedVec(uniqueOS, vecOS, data$OPERATING_SYSTEM)

    dataFrame = data.frame(Server, Licences, Continent, Country, OS)

    dataFrame = getNumericToDF(dataFrame, data$LIC_USG)

    return(dataFrame)
}

# including integer data, which won't be encrypted in previous step!
getNumericToDF = function(dataFrame, LicencesUsage) {

    dataFrame = data.frame(dataFrame, LicencesUsage)

    return(dataFrame)
}

encData = getDataFrame(transformedData)
