
## Take this out of package as not appropriate to share externally.

library(RODBC)

# create a test data frame with the required fields
vLKP_GPPRAC15_Test <- data.frame(GPPRAC15CDH = c("AA001","AA002","AA003"),
                                 GPPRAC15NM  = c("Prac1","Prac2","Prac3"),
                                 CCG15CDH    = c("N01","N01","N02"),
                                 CCG15NM     = c("CCG1","CCG1","CCG2"))


#Load data to data lake
#Set connection to lake and database
dbhandle <- odbcDriverConnect('driver={SQL Server};
                                server=SQLClusColLake.phe.gov.uk\\Lake;
                                database=z_LookupsShared_DEV;
                                Encrypt=true;
                                trusted_connection=true')

#Set table variable types
variabletypes<-c("GPPRAC15CDH"="varchar(50)",
                 "GPPRAC15NM"="varchar(120)",
                 "CCG15CDH"="varchar(50)",
                 "CCG15NM"="varchar(120)")

#Save data to the lake using connection, dataset and variable types
sqlSave(channel=dbhandle,dat=vLKP_GPPRAC15_Test,tablename="tLKP_GPPRAC15_Test_GA",rownames=FALSE,varTypes=variabletypes)

#close the connection
odbcClose(dbhandle)
