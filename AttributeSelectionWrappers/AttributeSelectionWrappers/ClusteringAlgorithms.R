library(magrittr)
source("ExtractDataModule.R")

transformedData = transformedData %>% na.omit()
head(transformedData,5)

# display unique classes in  row 1,2,3,4,5,6:
overallRows <- length(unique(transformedData[, 1]))
typesServersNo <- length(unique(transformedData[, 2]))
typesLicsNo <- length(unique(transformedData[, 3]))
typesCountriesNo <- length(unique(transformedData[, 4]))
typesContinentsNo <- length(unique(transformedData[, 5]))
typesOSNo <- length(unique(transformedData[, 6]))

uniqueServers <- unique(transformedData[, 2])
uniqueLics <- unique(transformedData[, 3])
uniqueCountries <- unique(transformedData[, 4])
uniqueContinents <- unique(transformedData[, 5])
uniqueOS <- unique(transformedData[, 6])

df <- data.frame(servers = c(transformedData[, 2]), lics = c(transformedData[, 3]), countries = c(transformedData[, 4]),
                 continents = c(transformedData[, 5]), OS = c(transformedData[, 6]), stringsAsFactors = FALSE)

df$servers.num <- as.numeric(factor(df$servers, levels = c("Bg", "Df", "Ea", "Kb")))
df$lics.num <- as.numeric(factor(df$lics, levels = c("lic1", "lic2")))
df$countries.num <- as.numeric(factor(df$countries, levels = c("Finland", "England", "Poland")))
df$continents.num <- as.numeric(factor(df$continents, levels = c("EMEA", "FEMEA")))
df$OS.num <- as.numeric(factor(df$OS, levels = c("Windows 8", "Windows 10", "Windows 7")))

head(df, 5)
