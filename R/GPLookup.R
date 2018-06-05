


# retrieve all GP practices
allgps <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76")

# Create empty Lookup dataframe
lkup <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                 c("GPPracCD","GPPracNM","Start","End", "CCGCD", "CCGNM"))

# loop through each GP Practice record

for (i in (1:nrow(allgps))) {

    OrgExists <- NA
    getGPPrac <- getODSfull(allgps[i,2])

    # continue for English GP Practices
    if(getGPPrac$Organisation$GeoLoc$Location$Country == "ENGLAND") {

        # get Organisation dates
        OrgDates   <- dplyr::bind_rows(getGPPrac$Organisation$Date) %>%
            filter(Type=="Operational")

        # keep practices in operation after 01/04/2013 when CCGs introduced
        continue <- 0
        if(!("End" %in% colnames(OrgDates))) {
            continue <- 1
        } else if (OrgDates$End >= "2013-04-01") {
            continue <- 1
        }

        if (continue == 1) {

            # find parent dates
            RelDates <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Date) %>%
                filter(Type == "Operational")

            # append Roles and OrgIds to RelDates
            RelRoles <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Target$PrimaryRoleId)[1]
            RelOrgs  <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Target$OrgId)[3]
            Rels     <- dplyr::bind_cols(RelDates,RelOrgs,RelRoles) %>%
                filter(id == "RO98")

            # loop through each parent record
            for (j in 1:nrow(Rels)) {

                # find CCG Name for CCG Code
                CCGCD <- select(Rels,extension)[j,1]
                CCGNM <- getODSfull(CCGCD)$Organisation$Name

                # build lookups record
                if (!("End" %in% colnames(Rels))) {
                    Rels$End <- NA
                }

                addrow <- data.frame(GPPracCD = getGPPrac$Organisation$OrgId$extension,
                                    GPPracNM = getGPPrac$Organisation$Name,
                                    Start    = Rels$Start[j],
                                    End      = Rels$End[j],
                                    CCGCD    = Rels$extension[j],
                                    CCGNM    = CCGNM, stringsAsFactors=FALSE)

                lkup <- bind_rows(lkup,addrow)
            }
        }
    }
}








