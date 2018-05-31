


# retrieve all GP practices
allgps <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76")

# Create empty Lookup dataframe
lkup <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                 c("GPPracCD","GPPracNM","CCGCD", "CCGNM", "Date"))

# loop through each GP Practice record

for (i in (1:nrow(allgps))) {

    OrgExists <- NA
    MyDate <- "2018-04-01"
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
                CCGCD <- filter(Rels, Rels$id    == "RO98" &
                                        Rels$Start <= MyDate &
                                        (is.na(Rels$End) | Rels$End > MyDate)) %>%
                        select(extension) %>%
                        rename(CCGCD = extension)
            } else {
                CCGCD <- filter(Rels, Rels$id    == "RO98" &
                                      Rels$Start <= MyDate) %>%
                        select(extension) %>%
                    rename(CCGCD = extension)
            }


            # obtain CCGName for CCGCode
            CCGNM <- getODSfull(CCGCD)$Organisation$Name


            # build lookups record
            addrow <- data.frame(GPPracCD = getGPPrac$Organisation$OrgId$extension,
                                 GPPracNM = getGPPrac$Organisation$Name,
                                 CCGCD    = CCGCD,
                                 CCGNM    = CCGNM, stringsAsFactors=FALSE,
                                 Date     = MyDate)

        lkup <- bind_rows(lkup,addrow)

        }
    }
}








