# get testfiles
testfiles <- list.files(path = "./tests/testthat/", pattern="^epraccur", full.names=TRUE, ignore.case=TRUE)

#### load data
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
    select(OrganisationCode, Name, OpenDate, CloseDate, StatusCode, Commissioner,
           JoinProviderPurchaserDate, LeftProviderPurchaserDate,
           ProviderPurchaser, PrescribingSetting, Effdate)

# reformat date fields
testdata$OpenDate   <- as.Date(as.character(testdata$OpenDate),format="%Y%m%d")
testdata$CloseDate  <- as.Date(as.character(testdata$CloseDate),format="%Y%m%d")

#### Create function output for comaprison
#allorgs <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId = "RO76")
#GP_CCG <- OrgRelLkp(allorgs,RelTypes = "RE4",RelPrimaryRoles = "RO98",FromDate = "2013-04-01")
GP_CCG <- lkup
GP_CCG$RelStart <- as.Date(as.character(GP_CCG$RelStart),format="%Y-%m-%d")
GP_CCG$RelEnd   <- as.Date(as.character(GP_CCG$RelEnd),format="%Y-%m-%d")


#### test lookup as at 2018-05-17

GP_CCG_20180517 <- GP_CCG %>%
    filter(RelStart <= "2018-05-17" &
           OrgRoleStart <= "2018-05-17" &
           (RelEnd >= "2018-05-17" | is.na(RelEnd)) &
           (OrgRoleEnd >= "2018-05-17" | is.na(OrgRoleEnd)))


#### Make comparisons

# records in both but discrepant - returns no records
QA_discreps_20180517 <- testdata %>%
    filter(Effdate == "20180517") %>%
    inner_join(GP_CCG_20180517,by = c("OrganisationCode" = "OrgId")) %>%
    filter(Commissioner != RelOrgId)


# records in epraccur only - returns no records
QA_epraccur_only_20180517 <- testdata %>%
    filter(Effdate == "20180517") %>%
    anti_join(GP_CCG_20180517,by = c("OrganisationCode" = "OrgId"))


# records in GP_CCG only - returns no records
QA_GP_CCG_only_20180517 <- GP_CCG_20180517 %>%
    anti_join(filter(testdata,Effdate == "20180517"),
              by = c("OrgId" = "OrganisationCode"))




#### test lookup as at 2015-05-26

GP_CCG_20150526 <- GP_CCG %>%
    filter(RelStart <= "2015-05-26" &
               OrgRoleStart <= "2015-05-26" &
               (RelEnd >= "2015-05-26" | is.na(RelEnd)) &
               (OrgRoleEnd >= "2015-05-26" | is.na(OrgRoleEnd)))


#### Make comparisons

# records in both but discrepant - returns 658 records
QA_discreps_20150526 <- testdata %>%
    filter(Effdate == "20150526") %>%
    inner_join(GP_CCG_20150526,by = c("OrganisationCode" = "OrgId")) %>%
    filter(Commissioner != RelOrgId)


# records in epraccur only - returns 2 records
QA_epraccur_only_20150526 <- testdata %>%
    filter(Effdate == "20150526") %>%
    anti_join(GP_CCG_20150526,by = c("OrganisationCode" = "OrgId"))


# records in GP_CCG only - returns 2 records
QA_GP_CCG_only_20150526 <- GP_CCG_20150526 %>%
    anti_join(testdata,by = c("OrgId" = "OrganisationCode"))


