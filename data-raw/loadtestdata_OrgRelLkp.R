# load the EPRACCUR testdata and save the output of the OrgRelLkp function (so data fixed for testing)

# get testfiles
testfiles <- list.files(path = "./tests/testthat/", pattern="^epraccur", full.names=TRUE, ignore.case=TRUE)

#### load EPRACCUR data
colnames <- c("OrganisationCode", "Name", "NationalGrouping", "HighLevelhealthGeography",
              "AddressLine1", "AddressLine2", "AddressLine3", "AddressLine4",
              "AddressLine5", "Postcode", "OpenDate", "CloseDate", "StatusCode",
              "OrganisationSubTypeCode", "Commissioner", "JoinProviderPurchaserDate",
              "LeftProviderPurchaserDate", "ContactTelephoneNumber", "Null1", "Null2",
              "Null3", "AmendedRecordIndicator", "Null4", "ProviderPurchaser", "Null5",
              "PrescribingSetting", "Null6")

testdata <- do.call("rbind", sapply(testfiles, read.csv, stringsAsFactors=FALSE,
                                    header=FALSE, col.names=colnames, simplify = FALSE)) %>%
    mutate(Effdate = sub(".csv.*","",
                     sub("./tests/testthat/epraccur_","",row.names(.), ignore.case=TRUE))) %>%
    filter(StatusCode != "C" & PrescribingSetting == 4) %>%
    select(OrganisationCode, Name, Postcode, OpenDate, CloseDate, StatusCode, Commissioner,
           JoinProviderPurchaserDate, LeftProviderPurchaserDate,
           ProviderPurchaser, PrescribingSetting, Effdate)

# reformat date fields
testdata$OpenDate   <- as.Date(as.character(testdata$OpenDate),format="%Y%m%d")
testdata$CloseDate  <- as.Date(as.character(testdata$CloseDate),format="%Y%m%d")


#### Create function output for saving
GP_CCG <- OrgRelLkp("RO177","RO76","RE4","RO98","2013-04-01")
#GP_CCG$RelStart <- as.Date(as.character(GP_CCG$RelStart),format="%Y-%m-%d")
#GP_CCG$RelEnd   <- as.Date(as.character(GP_CCG$RelEnd),format="%Y-%m-%d")


# use this devtools code to save testdata to package (hidden from users)
devtools::use_data(GP_CCG,testdata,
                     internal = TRUE, overwrite = TRUE)
