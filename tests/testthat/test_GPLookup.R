context("test_GPLookup")

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
    filter(OpenDate <= "20180401" &
               (is.na(CloseDate)|CloseDate > "20180401"))
# & PrescribingSetting == 4


QA_discreps <- epraccur %>%
    inner_join(lkup,by = c("OrganisationCode" = "GPPracCD")) %>%
    filter(Name != GPPracNM | Commissioner != CCGCD)

# can't QA these until got all practice rows not just first 1000
QA_epraccur_only <- epraccur %>%
    anti_join(lkup,by = c("OrganisationCode" = "GPPracCD"))

QA_lkup_only <- lkup %>%
    anti_join(epraccur,by = c("GPPracCD" = "OrganisationCode"))




# Using EGPMEMAM files #######################################################################################

colnames <- c("OrganisationCode","ParentCode","OrganisationType","OpenDate","CloseDate","Unknown")

egpmemam20180401 <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\egpmemam_20180401.csv",
                             stringsAsFactors=FALSE, header=FALSE, col.names=colnames) %>%
    filter(OrganisationType == "W") %>%
    mutate(EffDate = "20180401") %>%
    select(OrganisationCode, ParentCode, OpenDate, CloseDate, EffDate)

egpmemam20170401 <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\egpmemam_20170401.csv",
                             stringsAsFactors=FALSE, header=FALSE, col.names=colnames) %>%
    filter(OrganisationType == "W") %>%
    mutate(EffDate = "20170401") %>%
    select(OrganisationCode, ParentCode, OpenDate, CloseDate, EffDate)

egpmemam20150401 <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\egpmemam_20150401.csv",
                             stringsAsFactors=FALSE, header=FALSE, col.names=colnames) %>%
    filter(OrganisationType == "W") %>%
    mutate(EffDate = "20150401") %>%
    select(OrganisationCode, ParentCode, OpenDate, CloseDate, EffDate)

Test3Dates <- bind_rows(egpmemam20180401,egpmemam20170401,egpmemam20150401)


