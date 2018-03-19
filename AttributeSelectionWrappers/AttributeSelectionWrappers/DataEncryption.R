library(magrittr)
source("ExtractDataModule.R")

transformedData = transformedData %>% na.omit()

typesServersno = length(unique(transformedData$DSLS_SERVER))
typesLicsno = length(unique(transformedData$LIC_TYPE))
typesCountriesno = length(unique(transformedData$COUNTRY))
typesContinentsno = length(unique(transformedData$CONTINENT))
typesOSno = length(unique(transformedData$OPERATING_SYSTEM))

uniqueServers = unique(transformedData$DSLS_SERVER)
uniqueLics = unique(transformedData$LIC_TYPE)
uniqueCountries = unique(transformedData$COUNTRY)
uniqueContinents = unique(transformedData$CONTINENT)
uniqueOS = unique(transformedData$OPERATING_SYSTEM)

saltServ = "server_"
saltLics = "licence_"
saltCount = "country_"
saltConti = "continent_"
saltOS = "os_"

createArtificalDataVec = function(salt, typesno) {

    vec = c()
    for (i in 1:typesno) {
        nameLine= paste(salt, i, sep = "")
        vec = c(vec, nameLine)
    }
    return(vec)
}

encryptCell = function(unique, vec, cellVal) {

    for (i in 1:typesServersno) {
        if (cellVal == unique[i]) return(vec[i])
        }
}

getEncryptedVec = function(uniqueData, vecData, data) {

    vec = c()
    for (i in 1:length(data)) {

        vec = c(vec, encryptCell(uniqueData, vecData, data[i]))
    }
    return(vec)
}

vecServ = createArtificalDataVec(saltServ, typesServersno)
vecLics = createArtificalDataVec(saltLics, typesLicsno)
vecCount = createArtificalDataVec(saltCount, typesCountriesno)
vecConti = createArtificalDataVec(saltConti, typesContinentsno)
vecOS = createArtificalDataVec(saltOS, typesOSno)

encVecServ = getEncryptedVec(uniqueServers, vecServ, transformedData$DSLS_SERVER)
encVecLics = getEncryptedVec(uniqueLics, vecLics, transformedData$LIC_TYPE)
encVecCounti = getEncryptedVec(uniqueContinents, vecConti, transformedData$CONTINENT)
encVecCount = getEncryptedVec(uniqueCountries, vecCount, transformedData$COUNTRY)
encVecOS = getEncryptedVec(uniqueOS, vecOS, transformedData$OPERATING_SYSTEM)

