context("test_GPLookup")

# Load EGPMEMAM file #######################################################################################

colnames <- c("OrganisationCode","ParentCode","OrganisationType","OpenDate","CloseDate","unknown")

egpmemam <- read.csv("C:\\Users\\Georgina.Anderson\\Documents\\R\\Projects\\odsR\\tests\\testthat\\egpmemam_20180401.csv",
                             stringsAsFactors=FALSE, header=FALSE, col.names=colnames) %>%
    filter(OrganisationType == "W") %>%
    mutate(EffDate = "20180401") %>%
    select(OrganisationCode, ParentCode, OpenDate, CloseDate, EffDate)


# reformat date fields

egpmemam$OpenDate   <- as.Date(as.character(egpmemam$OpenDate),format="%Y%m%d")
egpmemam$CloseDate  <- as.Date(as.character(egpmemam$CloseDate),format="%Y%m%d")


# Create function output for comaprison

# GP_CCG <- OrgRelLkp("RO177","RO76","RE4","RO98","2012-04-01")
GP_CCG$RelStart <- as.Date(as.character(GP_CCG$RelStart),format="%Y-%m-%d")
GP_CCG$RelEnd   <- as.Date(as.character(GP_CCG$RelEnd),format="%Y-%m-%d")


# Create 20180401 snapshot

GP_CCG <- GP_CCG %>%
    filter(RelStart <= "2018-04-01" &
           (RelEnd > "2018-04-01" | is.na(RelEnd)))


# Make comparisons

# records in both but discrepant
QA_discreps <- egpmemam %>%
    inner_join(GP_CCG,by = c("OrganisationCode" = "OrgCD",
                              "OpenDate" = "RelStart",
                              "CloseDate" = "RelEnd")) %>%
    filter(ParentCode != RelOrgCD)


# records in egpmemam only
QA_egpmemam_only <- egpmemam %>%
    filter(OpenDate <= "2018-04-01" &
           (CloseDate > "2018-04-01" | is.na(CloseDate))) %>%
    anti_join(GP_CCG,by = c("OrganisationCode" = "OrgCD",
                            "OpenDate" = "RelStart"))


# records in GP_CCG only
QA_GP_CCG_only <- GP_CCG %>%
    anti_join(egpmemam,by = c("OrgCD" = "OrganisationCode",
                              "RelStart" = "OpenDate"))


# show that output in egpmemam only does not meet relationship criteria specified in function
check <- data.frame()

for (i in 1:nrow(QA_egpmemam_only_20180401)) {
    getOrg <- getODSfull(QA_egpmemam_only_20180401[i,1])

    # check Primary & Non Primary Roles meets function specification
     Roles <- dplyr::bind_rows(getOrg$Organisation$Roles)  %>%
        select(-Date)

     PrimaryRole <- Roles %>%
        filter(id=="RO177" & primaryRole)

     NonPrimaryRole <- Roles %>%
         filter(id=="RO76" & is.na(primaryRole))


     addrow <- data.frame(OrgId = QA_egpmemam_only_20180401[i,1],
                          PrimaryRole = nrow(PrimaryRole),
                          NonPrimaryRole = nrow(NonPrimaryRole))




    # if relationships exist capture them
    if (is.list(getOrg$Organisation$Rels)) {

    RelDates <- dplyr::bind_rows(getOrg$Organisation$Rels$Rel$Date) %>%
        filter(Type == "Operational")

    RelTypeIds <- data.frame(typeid    = getOrg$Organisation$Rels$Rel$id,
                             stringsAsFactors=FALSE )
    RelRoles   <- data.frame(id        = getOrg$Organisation$Rels$Rel$Target$PrimaryRoleId$id,
                             stringsAsFactors=FALSE)
    RelOrgs    <- data.frame(extension = getOrg$Organisation$Rels$Rel$Target$OrgId$extension,
                             stringsAsFactors=FALSE)

    Rels       <- dplyr::bind_cols(RelTypeIds,RelDates,RelOrgs,RelRoles) %>%
        filter(extension==QA_egpmemam_only_20180401[i,2])

    check <- bind_rows(check, Rels)

    }
}





