context("test_GPLookup")

# Load EGPMEMAM files #######################################################################################

colnames <- c("OrganisationCode","ParentCode","OrganisationType","OpenDate","CloseDate","unknown")

egpmemam20180401 <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\egpmemam_20180401.csv",
                             stringsAsFactors=FALSE, header=FALSE, col.names=colnames) %>%
    filter(OrganisationType == "W") %>%
    mutate(EffDate = "20180401") %>%
    select(OrganisationCode, ParentCode, OpenDate, CloseDate, EffDate)

egpmemam20150401 <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\egpmemam_20150401.csv",
                             stringsAsFactors=FALSE, header=FALSE, col.names=colnames) %>%
    filter(OrganisationType == "W") %>%
    select(OrganisationCode, ParentCode, OpenDate, CloseDate)

# reformat date fields

egpmemam20180401$OpenDate   <- as.Date(as.character(egpmemam20180401$OpenDate),format="%Y%m%d")
egpmemam20180401$CloseDate  <- as.Date(as.character(egpmemam20180401$CloseDate),format="%Y%m%d")
egpmemam20150401$OpenDate   <- as.Date(as.character(egpmemam20150401$OpenDate),format="%Y%m%d")
egpmemam20150401$CloseDate  <- as.Date(as.character(egpmemam20150401$CloseDate),format="%Y%m%d")


# Create function output for comaprison

# GP_CCG <- OrgRelLkp("RO177","RO76","RE4","RO98","2012-04-01")
GP_CCG$RelStart <- as.Date(as.character(GP_CCG$RelStart),format="%Y-%m-%d")
GP_CCG$RelEnd   <- as.Date(as.character(GP_CCG$RelEnd),format="%Y-%m-%d")


# Create 20180401 and 20150401 snapshots

GP_CCG_20180401 <- GP_CCG %>%
    filter(RelStart <= "2018-04-01" &
               (RelEnd > "2018-04-01" | is.na(RelEnd)))

GP_CCG_20150401 <- GP_CCG %>%
    filter(RelStart <= "2015-04-01" &
               (RelEnd > "2015-04-01" | is.na(RelEnd)))


# records in both but discrepant
QA_discreps_20180401 <- egpmemam20180401 %>%
    inner_join(GP_CCG_20180401,by = c("OrganisationCode" = "OrgCD",
                                      "OpenDate" = "RelStart",
                                      "CloseDate" = "RelEnd")) %>%
    filter(ParentCode != RelOrgCD)

QA_discreps_20150401 <- egpmemam20150401 %>%
    inner_join(GP_CCG_20150401,by = c("OrganisationCode" = "OrgCD",
                                      "OpenDate" = "RelStart",
                                      "CloseDate" = "RelEnd")) %>%
    filter(ParentCode != RelOrgCD)


# records in egpmemam only
QA_egpmemam_only_20180401 <- egpmemam20180401 %>%
    filter(OpenDate <= "2018-04-01" &
               (CloseDate > "2018-04-01" | is.na(CloseDate))) %>%
    anti_join(GP_CCG_20180401,by = c("OrganisationCode" = "OrgCD",
                                     "OpenDate" = "RelStart"))
QA_egpmemam_only_20150401 <- egpmemam20150401 %>%
    filter(OpenDate <= "2015-04-01" &
               (CloseDate > "2015-04-01" | is.na(CloseDate))) %>%
    anti_join(GP_CCG_20150401,by = c("OrganisationCode" = "OrgCD",
                                     "OpenDate" = "RelStart"))

# records in function output only
QA_GP_CCG_20180401 <- GP_CCG_20180401 %>%
    anti_join(egpmemam20180401,by = c("OrgCD" = "OrganisationCode",
                                      "RelStart" = "OpenDate"))






# Using EPRACCUR files #######################################################################################

# extract records from epraccur where PrescribingSetting = 4 (GP Practices) and
# practice was open on 2018-04-01
epraccur <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\epraccur_20180517.csv",
                     stringsAsFactors=FALSE, header=FALSE) %>%
    rename(OrganisationCode   = V1,
           Name               = V2,
           NationalGrouping   = V3,
           HighLevelhealthGeography = V4,
           AddressLine1       = V5,
           AddressLine2       = V6,
           AddressLine3       = V7,
           AddressLine4       = V8,
           AddressLine5       = V9,
           Postcode           = V10,
           OpenDate           = V11,
           CloseDate          = V12,
           StatusCode         = V13,
           OrganisationSubTypeCode = V14,
           Commissioner       = V15,
           JoinProviderPurchaserDate = V16,
           LeftProviderPurchaserDate = V17,
           ContactTelephoneNumber = V18,
           Null1              = V19,
           Null2              = V20,
           Null3              = V21,
           AmendedRecordIndicator = V22,
           Null4              = V23,
           ProviderPurchaser  = V24,
           Null5              = V25,
           PrescribingSetting = V26,
           Null6              = V27) %>%
    select(OrganisationCode, Name, Commissioner, OpenDate, CloseDate, StatusCode, PrescribingSetting) %>%
    filter(PrescribingSetting == 4)

GPCurrent <- GPLookup %>%
    filter(RelStart<= "2018-04-01" & (RelEnd > "2018-04-01" | is.na(RelEnd)))


QA_discreps <- epraccur %>%
    inner_join(GPLookup,by = c("OrganisationCode" = "OrgCD")) %>%
    filter(Name != OrgNM | Commissioner != ParentCD)

# can't QA these until got all practice rows not just first 1000
QA_epraccur_only <- epraccur %>%
    anti_join(GPLookup,by = c("OrganisationCode" = "OrgCD"))

QA_lkup_only <- GPLookup %>%
    anti_join(epraccur,by = c("OrgCD" = "OrganisationCode"))


