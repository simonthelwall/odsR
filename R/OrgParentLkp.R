#' GPLookup
#'
#' Generates a GP Practice to CCG Lookup Data Frame from the NHS Digitial ODS ORD API.
#'
#' @return returns a GP Practice to CCG Lookup data.frame including the start and end dates of each GP Practice >> CCG relationship.
#' There are no arguments to this function - it is a wrapper for the getODS function that will generate the lookup based on the latest data
#'
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # return full Organisation data for RRF12
#' GPLookup()
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------


# parameters to add
# PrimaryRole Id List
# Non PrimaryRoleId List
# Parent Roles
# FromDate
# Slice values and loops

PrimaryRole        = "RO177"
NonPrimaryRole     = "RO76"
ParentPrimaryRoles = c("RO98")
FromDate           = "2013-04-01"
slicerows <- as.numeric(2001-3000)


OrgLkp1 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(1:1000))
OrgLkp2 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(1001:2000))
OrgLkp3 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(2001:3000))
OrgLkp4 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(3001:4000))
OrgLkp5 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(4001:5000))
OrgLkp6 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(5001:6000))
OrgLkp7 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(6001:7000))
OrgLkp8 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(7001:8000))
OrgLkp9 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(8001:9000))

OrgLkp <- bind_rows(OrgLkp1,OrgLkp2) %>%
    bind_rows(OrgLkp3) %>%
    bind_rows(OrgLkp4) %>%
    bind_rows(OrgLkp5) %>%
    bind_rows(OrgLkp6) %>%
    bind_rows(OrgLkp7) %>%
    bind_rows(OrgLkp8) %>%
    bind_rows(OrgLkp9)



#allorgs <- getODS(PrimaryRoleId=PrimaryRole,NonPrimaryRoleId=NonPrimaryRole)


# below working but doesn't add parent names - do as separate function to avoid timeout errors??

# create function to generate GP Practice to CCG Lookup data.frame
OrgParentLkp <- function(PrimaryRole, NonPrimaryRole, ParentPrimaryRoles, FromDate) {  #}, Slicerows) {

    # retrieve all GP practices - moved outside function to see if helps timeout issue
    allorgs <- getODS(PrimaryRoleId=PrimaryRole,NonPrimaryRoleId=NonPrimaryRole)

# function works on 1000 records - use slice below.  memory errors if process full GP dataset in one go.
# allorgs <- slice(allorgs,slicerows)

    # Create empty Lookup dataframe
    lkup <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                     c("OrgCD","OrgNM","OrgStart","OrgEnd", "ParentCD", "ParentNM", "RelStart", "RelEnd"))

    # loop through each GP Practice record

    for (i in (1:nrow(allorgs))) {

        OrgExists <- NA
        getOrg <- getODSfull(allorgs[i,2])

        # continue for English GP Practices
        if(getOrg$Organisation$GeoLoc$Location$Country == "ENGLAND") {

            # get Organisation dates
            OrgDates   <- dplyr::bind_rows(getOrg$Organisation$Date) %>%
                filter(Type=="Operational")

            # add end column if missing
            if (!("End" %in% colnames(OrgDates))) {
                OrgDates$End <- NA
            }

            # keep practices in operation after specified FromDate
            addOrg <- 0
            if(!("End" %in% colnames(OrgDates))) {
                addOrg <- 1
            } else if (OrgDates$End >= FromDate | is.na(OrgDates$End)) {
                addOrg <- 1
            }

            if (addOrg == 1) {

                # find parent dates

                # check if any relationships
                if (!(is.list(getOrg$Organisation$Rels))) {       # if no Rels at all (eg GUE992)

                   addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                        OrgNM = getOrg$Organisation$Name,
                                        OrgStart = OrgDates$Start,
                                        OrgEnd   = OrgDates$End,
                                        ParentCD    = NA,
                                        ParentNM    = NA,
                                        RelStart    = NA,
                                        RelEnd      = NA)

                   lkup <- bind_rows(lkup,addrow)

                } else {

                    RelDates <- dplyr::bind_rows(getOrg$Organisation$Rels$Rel$Date) %>%
                        filter(Type == "Operational")

                    # append Roles and OrgIds to RelDates
                    RelRoles <- dplyr::bind_rows(getOrg$Organisation$Rels$Rel$Target$PrimaryRoleId)[1]
                    RelOrgs  <- dplyr::bind_rows(getOrg$Organisation$Rels$Rel$Target$OrgId)[3]
                    Rels     <- dplyr::bind_cols(RelDates,RelOrgs,RelRoles) %>%
                        filter(id %in% ParentPrimaryRoles)


                    # if Rels exists but none are correct role (eg P85619)
                    if (nrow(Rels) == 0) {
                        addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                             OrgNM = getOrg$Organisation$Name,
                                             OrgStart = OrgDates$Start,
                                             OrgEnd   = OrgDates$End,
                                             ParentCD    = NA,
                                             ParentNM    = NA,
                                             RelStart    = NA,
                                             RelEnd      = NA)

                        lkup <- bind_rows(lkup,addrow)

                    } else {

                        # if Rels with correct role exist - loop through each parent record
                        for (j in 1:nrow(Rels)) {

                            # find CCG Name for CCG Code
                            ParentCD <- select(Rels,extension)[j,1]
                      #      ParentNM <- getODSfull(ParentCD)$Organisation$Name    # see if getting parent name separately helps with timeout

                            # build lookups record
                            if (!("End" %in% colnames(Rels))) {
                                Rels$End <- NA
                            }

                            addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                                 OrgNM = getOrg$Organisation$Name,
                                                 OrgStart    = OrgDates$Start,
                                                 OrgEnd      = OrgDates$End,
                                                 ParentCD    = Rels$extension[j],
                                                 ParentNM    = NA, # ParentNM, stringsAsFactors=FALSE,
                                                 RelStart    = Rels$Start[j],
                                                 RelEnd      = Rels$End[j])

                            lkup <- bind_rows(lkup,addrow)
                        }
                    }
                }
            }
        }
    }
    return(lkup)
}








