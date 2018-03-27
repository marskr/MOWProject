# Connection string to sql server
connStr = paste("Driver=SQL Server;Server=", "DESKTOP-8MRGK1B", ";Database=", "MASTER_2018", 
                ";uid=mining_guest;pwd=Mining_18;", sep = "");

inputQuery <- "SELECT [DSLS_SERVER]
                     ,[LIC_TYPE]
                     ,[COUNTRY]
                     ,[CONTINENT]
                     ,[OPERATING_SYSTEM]
                      FROM [MASTER_2018].[dbo].[TB_MINING]"

obtainedData <- RxSqlServerData(sqlQuery = inputQuery, colClasses = c(#CELL_ID = "integer",
                                                                      DSLS_SERVER = "string",
                                                                      LIC_TYPE = "string",
                                                                      COUNTRY = "string",
                                                                      CONTINENT = "string",
                                                                      OPERATING_SYSTEM = "string"
                                                                      ), connectionString = connStr);

transformedData <- rxDataStep(inData = obtainedData);

