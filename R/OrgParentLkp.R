#' OrgParentLkp
#'
#' Generates an Organisation to Parent Lookup Data Frame from the NHS Digitial ODS ORD API.
#'
#' @return returns a data.frame of  Organisation code, name, start date and end date plus the parent code and name
#' and associated start and end dates for the organisation >> parent relationship.
#'
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # return GP Practice to CCG Lookup to include all organisations effective on or after 01-04-2013
#' OrgParentLkp("RO177","RO76","RO98","2013-04-01")
#'
#' # return GP practice to CCG/PCT Lookup to include all organisations effective on or after 01-04-2012
#' OrgParentLkp("RO177","RO76",c("RO98","RO179"),"2012-04-01")
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

# PrimaryRole        = "RO177"
# NonPrimaryRole     = "RO76"
# ParentPrimaryRoles = c("RO98")
# FromDate           = "2013-04-01"
# slicerows <- as.numeric(2001-3000)

# library(dplyr)
# library(httr)
# library(jsonlite)

# Output <- OrgParentLkp("RO177","RO76","RO98","2014-04-01")

# create function to generate GP Practice to CCG Lookup data.frame
OrgParentLkp <- function(PrimaryRole, NonPrimaryRole, ParentPrimaryRoles, FromDate) {

    # retrieve all GP practices - moved outside function to see if helps timeout issue
    allorgs <- getODS(PrimaryRoleId=PrimaryRole,NonPrimaryRoleId=NonPrimaryRole)

    # Create empty Lookup dataframe
 #   lkup <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
 #                    c("OrgCD","OrgNM","OrgStart","OrgEnd", "ParentCD", "ParentNM", "RelStart", "RelEnd"))
    lkup <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
                     c("OrgCD","OrgNM","OrgStart","OrgEnd", "ParentCD", "RelStart", "RelEnd"))

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

            # keep only practices in operation after specified FromDate
            if(OrgDates$End >= FromDate | is.na(OrgDates$End)) {
                addOrg <- 1
            } else {
                addOrg <- 0
            }



            #addOrg <- 0
            #if(!("End" %in% colnames(OrgDates))) {
            #    addOrg <- 1
            #} else if (OrgDates$End >= FromDate | is.na(OrgDates$End)) {
            #    addOrg <- 1
            #}

            if (addOrg == 1) {

                # find parent dates

                # check if any relationships
                if (!(is.list(getOrg$Organisation$Rels))) {       # if no Rels at all (eg GUE992)

                   addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                        OrgNM = getOrg$Organisation$Name,
                                        OrgStart = OrgDates$Start,
                                        OrgEnd   = OrgDates$End,
                                        ParentCD    = NA,
                                       # ParentNM    = NA,
                                        RelStart    = NA,
                                        RelEnd      = NA, stringsAsFactors=FALSE)

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
                                           #  ParentNM    = NA,
                                             RelStart    = NA,
                                             RelEnd      = NA, stringsAsFactors=FALSE)

                        lkup <- bind_rows(lkup,addrow)

                    } else {

                        # if Rels with correct role exist - loop through each parent record
                        for (j in 1:nrow(Rels)) {

                            # find CCG Name for CCG Code
                            ParentCD <- select(Rels,extension)[j,1]
                      #      ParentNM <- getODSfull(ParentCD)$Organisation$Name    # see if getting parent name separately helps with timeout

                            # build lookups record
                            if (!("End" %in% colnames(Rels))) {
                                Rels$End[j] <- NA
                            }

                            addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                                 OrgNM = getOrg$Organisation$Name,
                                                 OrgStart    = OrgDates$Start,
                                                 OrgEnd      = OrgDates$End,
                                                 ParentCD    = Rels$extension[j],
                                              #   ParentNM    = NA, # ParentNM,
                                                 RelStart    = Rels$Start[j],
                                                 RelEnd      = Rels$End[j], stringsAsFactors=FALSE)

                            lkup <- bind_rows(lkup,addrow)
                        }
                    }
                }
            }
        }
    }
    return(lkup)
}








