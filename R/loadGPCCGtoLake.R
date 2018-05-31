
## Take this out of package as not appropriate to share externally.

library(RODBC)

# create a test data frame with the required fields
vLKP_GPPrac_CCG_Test <- data.frame(GPPracCDH = c("AA001","AA002","AA003"),
                                   GPPracNM  = c("Prac1","Prac2","Prac3"),
                                   CCGCDH    = c("N01","N01","N02"),
                                   CCGNM     = c("CCG1","CCG1","CCG2"),
                                   Date        = rep(c("2018-04-01"),3))


#Load data to data lake
#Set connection to lake and database
dbhandle <- odbcDriverConnect('driver={SQL Server};
                                server=SQLClusColLake.phe.gov.uk\\Lake;
                                database=z_LookupsShared_DEV;
                                Encrypt=true;
                                trusted_connection=true')

#Set table variable types
variabletypes<-c("GPPracCDH"="varchar(50)",
                 "GPPracNM"="varchar(120)",
                 "CCGCDH"="varchar(50)",
                 "CCGNM"="varchar(120)",
                 "Date" ="varchar(10)")

#Save data to the lake using connection, dataset and variable types
sqlSave(channel=dbhandle,dat=vLKP_GPPRAC15_Test,tablename="tLKP_GPPrac_Test_GA",rownames=FALSE,varTypes=variabletypes)


## CAN I ADD PRIMARY KEYS HERE?


#close the connection
odbcClose(dbhandle)
