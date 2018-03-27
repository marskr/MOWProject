library(sqldf)
library(odbc)
source("DataNormalization/DataEncryption.R")

connStr2 <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-8MRGK1B",
                 Database = "MASTER_2018",
                 UID = "mining_guest",
                 PWD = "Mining_18")

inputQuery <- "DELETE FROM [MASTER_2018].[dbo].[TB_MINING_ENC]"

dbGetQuery(connStr2, inputQuery)

dbWriteTable(conn = connStr2,
             name = "TB_MINING_ENC",
             value = encTransformedData[, 1:5],
             overwrite = FALSE, append = TRUE) ## x is any data frame

dbDisconnect(connStr2)
