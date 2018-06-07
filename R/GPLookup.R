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


# create function to generate GP Practice to CCG Lookup data.frame
GPLookup <- function() {

    # retrieve all GP practices
    allgps <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76")

allgps <- slice(allgps,1000:2000)

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

                # check if any relationships
                # need to edit if clause for this loop - P85619 has rels but none have an RO98 Primary Role ID (CCG)
                # so loops through the 'else' loop but finds no records when creates Rels  object

                if (!(is.list(getGPPrac$Organisation$Rels))) {
                   CCGCD <- NA
                   CCGNM <- NA

                   addrow <- data.frame(GPPracCD = getGPPrac$Organisation$OrgId$extension,
                                        GPPracNM = getGPPrac$Organisation$Name,
                                        Start    = Rels$Start[j],
                                        End      = Rels$End[j],
                                        CCGCD    = CCGCD,
                                        CCGNM    = CCGNM, stringsAsFactors=FALSE)

                   lkup <- bind_rows(lkup,addrow)


                } else {


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
    }
    return(lkup)
}








