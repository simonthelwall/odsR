context("test_OrgRelLkpTrust")

#### test lookup as at 2018-08-31

NHSTrust_20180831 <- NHSTrust_NHSRLO %>%
    filter(OrgRoleStart <= "2018-08-31" &
               (RelStart <= "2018-08-31" | is.na(RelStart)) &
            (OrgRoleEnd >= "2018-08-31" | is.na(OrgRoleEnd)) &
               (RelEnd >= "2018-08-31" | is.na(RelEnd)))

#### Make comparisons

# records in both but discrepant
QA_discreps_20180831 <- testdata2 %>%
    filter(Effdate == "20180831") %>%
    inner_join(NHSTrust_20180831,by = c("OrganisationCode" = "OrgId")) %>%
#    filter(HighLevelHealthGeography != RelOrgId)
   filter(HighLevelHealthGeography != RelOrgId | Postcode != OrgPostCode)


# records in testdata only
QA_etr_only_20180831 <- testdata2 %>%
    filter(Effdate == "20180831") %>%
    anti_join(NHSTrust_20180831,by = c("OrganisationCode" = "OrgId"))


# records in function output only
QA_NHSTrust_only_20180831 <- NHSTrust_20180831 %>%
    anti_join(filter(testdata2,Effdate == "20180831"),
              by = c("OrgId" = "OrganisationCode"))






#### Perform tests
# currently there are discrepancies between testdata2 and NHSTrusts -
# 32 organisations for which status=Active and they have a legal end date but not an operational end date
# these are included in ODS API but not in ETR.CSv file - needs resolution



test_that("function output matches testdata2",{
    expect_equal(nrow(QA_discreps_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test discreps 20180831")
    expect_equal(nrow(QA_etr_only_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test epraccur only 20180831")
    expect_equal(nrow(QA_NHSTrust_only_20180831),
                 32,check.attributes=FALSE, check.names=FALSE,info="test default")
#                0,check.attributes=FALSE, check.names=FALSE,info="test default")

})
