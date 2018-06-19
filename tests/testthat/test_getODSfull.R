context("test_getODSfull")


url1 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/RRF12"
url2 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/X25002AE"


# test function:
checksingle <- getODSfull(ODSCode="RRF12")




#test function returns correct results
test_that("getODS - produces correct output when correct arguments specified",{
    expect_equal(getODSfull("RRF12"),
                 fromJSON(content(GET(url1, accept_json()), "text", encoding="UTF-8")),
                 check.attributes=FALSE, check.names=FALSE, info="test1")
    expect_equal(getODSfull("X25002AE"),
                 fromJSON(content(GET(url2, accept_json()), "text", encoding="UTF-8")),
                 check.attributes=FALSE, check.names=FALSE, info="test2")
})


# test error handling
test_that("getODS - errors are generated when invalid arguments are used",{
    expect_error(getODS(),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified1")
})
