context("test_getODS")


url1 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Name=Woodseats"
url2 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?PostCode=S8"
url3 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Name=Woodseats&Status=Active"
url4 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Status=Active&PrimaryRoleid=RO197&OrgRecordClass=RC1"
url5 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Name=Woodseats"

#test function returns correct results
test_that("getODS - produces correct output when correct arguments specified",{

    expect_equal(getODS(Name="Woodseats"),
                 fromJSON(content(GET(url1, accept_json()), "text", encoding="UTF-8")),
                 check.attributes=FALSE, check.names=FALSE, info="test1")
    expect_equal(getODS(PostCode="S8"),
                 fromJSON(content(GET(url2, accept_json()), "text", encoding="UTF-8")),
                 check.attributes=FALSE, check.names=FALSE, info="test1")
    expect_equal(getODS(Name="Woodseats", Status="Active"),
                 fromJSON(content(GET(url3, accept_json()), "text", encoding="UTF-8")),
                 check.attributes=FALSE, check.names=FALSE, info="test1")
    expect_equal(getODS(Status="Active", PrimaryRoleId = "RO197", OrgRecordClass="RC1"),
                 fromJSON(content(GET(url4, accept_json()), "text", encoding="UTF-8")),
                 check.attributes=FALSE, check.names=FALSE, info="test1")
})


# test error handling
test_that("getODS - errors are generated when invalid arguments are used",{
    expect_error(getODS(),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified1")
    expect_error(getODS(LastChangeDate="AAAABBBB"),
                 "ERROR: LastChangeDate is not a valid date", info="error invalid LastChangeDate")
    expect_error(getODS(Status = "Live"),
                 "ERROR: Status is invalid - valid values are All (default), Active, Inactive", info="invalid Status")
    expect_error(getODS(PrimaryRoleId = "XXX"),
                 "ERROR: PrimaryRoleId is invalid - valid values begin with RO followed by a number, or specify All",
                 info="invalid PrimaryRoleid")
    expect_error(getODS(NonPrimaryRoleId = "XXX"),
                 "ERROR: NonPrimaryRoleId is invalid - valid values begin with RO followed by a number, or specify All",
                 info="invalid NonPrimaryRoleId")
    expect_error(getODS(OrgRecordClass = "RC22"),
                 "ERROR: OrgRecordClass is invalid - valid values are All (default), RC1 (Health and Social Care Organisation), RC2 (Health and Social Care Organisation Site)",
                 info="invalid OrgRecordClass")
})

