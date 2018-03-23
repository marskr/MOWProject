library(magrittr)
source("DataNormalization\\ExtractDataModule.R")

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

    Serv = getEncryptedVec(uniqueServers, vecServ, data$DSLS_SERVER)
    Lics = getEncryptedVec(uniqueLics, vecLics, data$LIC_TYPE)
    Conti = getEncryptedVec(uniqueContinents, vecConti, data$CONTINENT)
    Count = getEncryptedVec(uniqueCountries, vecCount, data$COUNTRY)
    OS = getEncryptedVec(uniqueOS, vecOS, data$OPERATING_SYSTEM)

    dataFrame = data.frame(Serv, Lics, Conti, Count, OS)

    return(dataFrame)
}

encTransformedData = getDataFrame(transformedData)
