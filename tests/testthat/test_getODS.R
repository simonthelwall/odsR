context("test_getODS")

#test calculations
#test_that("byars_lower calculate correctly",{
#
#    expect_equal(one thing,equals another thing,check.attributes=FALSE, check.names=FALSE, info="test1")
#})


# test error handling
test_that("byars_lower - errors are generated when invalid arguments are used",{
    expect_error(getODS(),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified1")
    expect_error(getODS(Limit=100,Offset=2,Format="xml"),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified2")
    expect_error(getODS(Status = "Live"),
                 "ERROR: Status is invalid - valid values: All (default), Active, Inactive", info="invalid Status")
    expect_error(getODS(PrimaryRoleId = "XXX"),
                 "ERROR: PrimaryRoleId is invalid - valid values: All (default), RO98 (CCG), RO101 (Care Home Site), RO177 (Prescribing Cost Centre)",
                 info="invalid PrimaryRoleid")
    expect_error(getODS(NonPrimaryRoleId = "XXX"),
                 "ERROR: NonPrimaryRoleId is invalid - valid values: All (default), RO76 (GP Practice Prescribing Cost Centre), RO218 (Commissioning Hub)",
                 info="invalid NonPrimaryRoleId")
    expect_error(getODS(OrgRecordClass = "RC22"),
                 "ERROR: OrgRecordClass is invalid - valid values: All (default), RC1(Health and Social Care Organisation), RC2 (Health and Social Care Organisation Site)",
                 info="invalid OrgRecordClass")
    expect_error(getODS(Format = "xls"),
                 "ERROR: Format is invalid - valid values: json, xml, text/json, text/xml, application/json, application/xml",
                 info="invalid Format")
    expect_error(getODS(Limit = 2000),
                 "ERROR: Limit must be between 1 and 1000", info="invalid Limit1")
    expect_error(getODS(Limit = "maximum"),
                 "ERROR: Limit must be a numeric integer", info="invalid Limit2")
    expect_error(getODS(Offset = "maximum"),
                 "ERROR: Offset must be a numeric integer", info="invalid Offset")

})

