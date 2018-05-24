
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

    # get Organisation dates
    OrgDates <- dplyr::bind_rows(getGPPrac$Organisation$Date)

    # check if Org existed on specified date
    if ("End" %in% colnames(OrgDates)) {
        OrgExists <- OrgDates %>%
            filter(Type == "Operational" & Start <= MyDate & (is.na(End) | End > MyDate))
    } else {
        OrgExists <- OrgDates %>%
            filter(Type == "Operational" & Start <= MyDate)
    }

    # continue if organisation existed at specified date
    if (nrow(OrgExists) ==1) {

        # find parent dates
        RelDates <- dplyr::bind_rows(getGPPrac$Organisation$Rels$Rel$Date)

        # check which parent existed on specified date
        if ("End" %in% colnames(RelDates)) {
            RelExists <- which(RelDates$Type == "Operational" &
                               RelDates$Start <= MyDate &
                               (is.na(RelDates$End) | RelDates$End > MyDate))
        } else {
            RelExists <- which(RelDates$Type == "Operational" &
                               RelDates$Start <= MyDate)
        }

        # return parent code for specified date
        CCG15CD <- getGPPrac$Organisation$Rels$Rel$Target$OrgId$extension[RelExists]

        # obtain CCGName for CCGCode
        CCG15NM <- getODSfull(CCG15CD)$Organisation$Name


        # build lookups record
        addrow <- data.frame(GPPRAC15CD = getGPPrac$Organisation$OrgId$extension,
                     GPPRAC15NM = getGPPrac$Organisation$Name,
                     CCG15CD = CCG15CD,
                     CCG15NM = CCG15NM)

    lkup <- bind_rows(lkup,addrow)

    }

}








