
# retrieve org IDs and API URLs for all Active GP Practices (only retrives first 1000)

# Work In Progress - TO DO
# **********  NEED TO BIND PAGES so working with full GP list *************
# need to account for organisations with >1 start and end date record for the main practice ??



# retrieve all GP practices
allgps <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76")

# Create empty Lookup dataframe
lkup <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                 c("GPPRAC15CD","GPPRAC15NM","CCG15CD", "CCG15NM"))

# loop through each GP Practice record

for (i in (1:nrow(allgps))) {

    OrgExists <- NA
    MyDate <- "2015-12-31"
    getGPPrac <- getODSfull(allgps[i,2])

    # continue for English GP Practices
    if(getGPPrac$Organisation$GeoLoc$Location$Country == "ENGLAND") {

        # get Organisation dates
        OrgDates   <- dplyr::bind_rows(getGPPrac$Organisation$Date) %>%
            filter(Type=="Operational")


        # check if Org existed on specified date

        if ("End" %in% colnames(OrgDates)) {
            OrgExists <- OrgDates %>%
                filter(Start <= MyDate & (is.na(End) | End > MyDate))
        } else {
            OrgExists <- OrgDates %>%
                filter(Start <= MyDate)
        }

        # continue if organisation existed at specified date
        if (nrow(OrgExists) ==1) {

            # find parent dates
            RelDates <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Date) %>%
                filter(Type == "Operational")

            # append Roles and OrgIds to RelDates
            RelRoles <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Target$PrimaryRoleId)[1]
            RelOrgs  <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Target$OrgId)[3]
            Rels     <- dplyr::bind_cols(RelDates,RelOrgs,RelRoles)

            # check which parent has an operational relationship, is a CCG and existed on specified date
            if ("End" %in% colnames(Rels)) {
                CCG15CD <- filter(Rels, Rels$id    == "RO98" &
                                        Rels$Start <= MyDate &
                                        (is.na(Rels$End) | Rels$End > MyDate)) %>%
                        select(extension) %>%
                        rename(CCG15CD = extension)
            } else {
                CCG15CD <- filter(Rels, Rels$id    == "RO98" &
                                        Rels$Start <= MyDate) %>%
                        select(extension) %>%
                    rename(CCG15CD = extension)
            }


            # obtain CCGName for CCGCode
            CCG15NM <- getODSfull(CCG15CD)$Organisation$Name


            # build lookups record
            addrow <- data.frame(GPPRAC15CD = getGPPrac$Organisation$OrgId$extension,
                                GPPRAC15NM = getGPPrac$Organisation$Name,
                                CCG15CD = CCG15CD,
                                CCG15NM = CCG15NM, stringsAsFactors=FALSE)

        lkup <- bind_rows(lkup,addrow)

        }
    }
}








