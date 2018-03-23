connStr = paste("Driver=SQL Server;Server=", "DESKTOP-8MRGK1B", ";Database=", "MASTER_2018",
                ";uid=mining_guest;pwd=Mining_18;", sep = "");

inputQuery <- "SELECT [Serv]
                     ,[Lics]
                     ,[Conti]
                     ,[Count]
                     ,[OS]
                 FROM [MASTER_2018].[dbo].[TB_MINING_ENC]"

obtainedData <- RxSqlServerData(sqlQuery = inputQuery, colClasses = c(DSLS_SERVER = "string",
                                                                      LIC_TYPE = "string",
                                                                      COUNTRY = "string",
                                                                      CONTINENT = "string",
                                                                      OPERATING_SYSTEM = "string"
                                                                      ), connectionString = connStr);

transformedData <- rxDataStep(inData = obtainedData);