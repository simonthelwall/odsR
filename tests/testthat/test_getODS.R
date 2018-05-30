context("test_getODS")


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






#test calculations
#test_that("getODS - produces correct output when correct arguments specified",{
#
#    expect_equal(one thing,equals another thing,check.attributes=FALSE, check.names=FALSE, info="test1")
#})


# test error handling
test_that("getODS - errors are generated when invalid arguments are used",{
    expect_error(getODS(),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified1")
    expect_error(getODS(Limit=100,Offset=2,Format="xml"),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified2")
#    expect_error(getODS(Status = "Live"),
#                 "ERROR: Status is invalid - valid values are All (default), Active, Inactive", info="invalid Status")
#    expect_error(getODS(PrimaryRoleId = "XXX"),
#                 "ERROR: PrimaryRoleId is invalid - valid values are All (default), RO98 (CCG), RO101 (Care Home Site), RO177 (Prescribing Cost Centre)",
#                 info="invalid PrimaryRoleid")
#    expect_error(getODS(NonPrimaryRoleId = "XXX"),
#                 "ERROR: NonPrimaryRoleId is invalid - valid values are All (default), RO76 (GP Practice Prescribing Cost Centre), RO218 (Commissioning Hub)",
#                 info="invalid NonPrimaryRoleId")
#    expect_error(getODS(OrgRecordClass = "RC22"),
#                 "ERROR: OrgRecordClass is invalid - valid values are All (default), RC1 (Health and Social Care Organisation), RC2 (Health and Social Care Organisation Site)",
#                 info="invalid OrgRecordClass")
#    expect_error(getODS(Format = "xls"),
#                 "ERROR: Format is invalid - valid values are json, xml, text/json, text/xml, application/json, application/xml",
#                 info="invalid Format")
    expect_error(getODS(Status = "Active", Limit = "maximum"),
                 "ERROR: Limit must be a numeric integer", info="invalid Limit1")
    expect_error(getODS(Status = "Active", Limit = 2000),
                 "ERROR: Limit must be between 1 and 1000", info="invalid Limit2")
    expect_error(getODS(Status = "Active", Limit = 25.5),
                 "ERROR: Limit must be a numeric integer", info="invalid Limit3")
    expect_error(getODS(Status = "Active", Offset = "maximum"),
                 "ERROR: Offset must be a numeric integer", info="invalid Offset")
    expect_error(getODS(Status = "Active", Offset = 2.5),
                 "ERROR: Offset must be a numeric integer", info="invalid Limit3")

})

