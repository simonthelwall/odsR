context("test_getODS")


# extract practice codes, names and parent CCG codes from epraccur for records that are GP practices and were active on 2018-01-01
# move this section to package folder for test data later

epraccur <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\epraccur.csv") %>%
    filter(PrescribingSetting == 4 &
           OpenDate <= "20180101" &
           (is.na(CloseDate)|CloseDate > "20180101")) %>%
    select(OrganisationCode, Name, Commissioner, OpenDate, CloseDate, StatusCode)


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

