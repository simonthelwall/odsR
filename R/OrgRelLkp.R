#' OrgRelLkp
#'
#' Generates an organisational lookup table from the NHS Digital ODS ORD API
#' based on organisational relationship data.
#'
#' @param PrimaryRole     The Primary Role code for organisations to be included in the lookup;
#'                        quoted string, no default
#' @param NonPrimaryRole  The Non Primary Role code for organisations to be included in the lookup;
#'                        quoted string, no default
#' @param RelTypes        The Named Relationship Types for related organisations to be included in the lookup;
#'                        character vector, no default
#' @param RelPrimaryRoles The Primary Role Ids for related organisations to be included in the lookup;
#'                        character vector, no default
#' @param FromDate        The effective date from which to include Organisations operational on or after;
#'                        character string in the format "yyyy-mm-dd", no default
#'
#' @return returns a data.frame of Organisation codes, names, start dates and end dates plus the
#' organisations (code & name) they are related to and associated start and end dates
#' for the relationship.
#'
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # return GP Practice to CCG Lookup to include all organisations effective on or after 01-04-2013
#' OrgRelLkp(PrimaryRole    = "RO177",
#'           NonPrimaryRole = "RO76",
#'           RelTypes       = "RE4",
#'           RelRoles       = "RO98",
#'           FromDate       = "2013-04-01")
#'
#' # return GP practice to CCG/PCT Lookup to include all organisations effective on or after 01-04-2012
#' OrgRelLkp("RO177","RO76","RE4",c("RO98","RO179"),"2012-04-01")
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

# PrimaryRole = "RO177"
# NonPrimaryRole = "RO76"
# RelTypes = "RE4"
# RelPrimaryRoles = "RO98"
# FromDate = "2013-04-01"


# create function to generate Organisation lookup data.frame
OrgRelLkp <- function(PrimaryRole, NonPrimaryRole, RelTypes, RelPrimaryRoles, FromDate) {

    # display experimental message
    message("Please Note that this function is experimental and all output should be independently QAd")

    # retrieve all organisations to include records for
    allorgs <- getODS(PrimaryRoleId=PrimaryRole,NonPrimaryRoleId=NonPrimaryRole) %>%
        unique()

    # Create empty Lookup dataframe
    lkup <- setNames(data.frame(matrix(ncol = 11, nrow = 0)),
                     c("OrgCD","OrgNM","OrgStart","OrgEnd","OrgRoleStart","OrgRoleEnd",
                       "RelOrgCD", "RelType","RelOrgPrimaryRole","RelStart", "RelEnd"))

    # loop through each Organisation record

    for (i in (1:nrow(allorgs))) {

        addOrg <- NA
        getOrg <- getODSfull(allorgs[i,2])

        # get Organisation Start and End dates
        OrgDates   <- dplyr::bind_rows(getOrg$Organisation$Date) %>%
            filter(Type=="Operational")

        # add end column if missing
        if (!("End" %in% colnames(OrgDates))) {
            OrgDates$End <- NA
        }


        # get dates when organisation was operational in the role specified
        RoleIds   <- data.frame(Role = getOrg$Organisation$Roles$Role$id, stringsAsFactors=FALSE)
        RoleDates <- dplyr::bind_rows(getOrg$Organisation$Roles$Role$Date) %>%
            filter(Type == "Operational")
        Roles     <- dplyr::bind_cols(RoleIds,RoleDates) %>%
            filter(Role %in% c(PrimaryRole, NonPrimaryRole))

        # add end column if missing and get period in specified role
        if (!("End" %in% colnames(Roles))) {
            RolePeriod <- data.frame(RoleStart = max(Roles$Start),
                                     RoleEnd   = NA,
                                     stringsAsFactors=FALSE)
        } else {
            RolePeriod <- data.frame(RoleStart = max(Roles$Start),
                                     RoleEnd = min(Roles$End, na.rm=TRUE),
                                     stringsAsFactors=FALSE)
        }

        # keep only organisations in operation in specified role after specified FromDate
        if(is.na(RolePeriod$RoleEnd)) {
            addOrg <- 1
        } else if(RolePeriod$RoleEnd >= FromDate) {
            addOrg <- 1
        } else {
            addOrg <- 0
        }

        # continue if Organisation record needs to be included in output
        if (addOrg == 1) {

            # find related organisation dates

            # if no relationships exist populate parent and rel columns with NA
            if (!(is.list(getOrg$Organisation$Rels))) {

               addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                    OrgNM = getOrg$Organisation$Name,
                                    OrgStart  = OrgDates$Start,
                                    OrgEnd    = OrgDates$End,
                                    OrgRoleStart = RolePeriod$RoleStart,
                                    OrgRoleEnd   = RolePeriod$RoleEnd,
                                    RelOrgCD  = NA,
                                    RelType   = NA,
                                    RelOrgPrimaryRole = NA,
                                    RelStart  = NA,
                                    RelEnd    = NA, stringsAsFactors=FALSE)

               lkup <- bind_rows(lkup,addrow)

            # otherwise find relationships
            } else {
                # find relationship dates and append RelTypes, Roles and Ids
                RelDates <- dplyr::bind_rows(getOrg$Organisation$Rels$Rel$Date) %>%
                    filter(Type == "Operational")

                RelTypeIds <- data.frame(typeid    = getOrg$Organisation$Rels$Rel$id,
                                         stringsAsFactors=FALSE )
                RelRoles   <- data.frame(id        = getOrg$Organisation$Rels$Rel$Target$PrimaryRoleId$id,
                                         stringsAsFactors=FALSE)
                RelOrgs    <- data.frame(extension = getOrg$Organisation$Rels$Rel$Target$OrgId$extension,
                                         stringsAsFactors=FALSE)

                Rels       <- dplyr::bind_cols(RelTypeIds,RelDates,RelOrgs,RelRoles) %>%
                    filter(id     %in% RelPrimaryRoles &
                           typeid %in% RelTypes)


                # if relationships exist but not of correct type populate parent and rel columns with NA
                if (nrow(Rels) == 0) {
                    addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                         OrgNM = getOrg$Organisation$Name,
                                         OrgStart = OrgDates$Start,
                                         OrgEnd   = OrgDates$End,
                                         OrgRoleStart = RolePeriod$RoleStart,
                                         OrgRoleEnd   = RolePeriod$RoleEnd,
                                         RelOrgCD    = NA,
                                         RelType     = NA,
                                         RelOrgPrimaryRole = NA,
                                         RelStart    = NA,
                                         RelEnd      = NA, stringsAsFactors=FALSE)

                    lkup <- bind_rows(lkup,addrow)

                } else {

                    # if Rels with correct type and roles exist - loop through each row to add related codes
                    for (j in 1:nrow(Rels)) {

                        # build lookups record
                        if (!("End" %in% colnames(Rels))) {
                            Rels$End[j] <- NA
                        }

                        addrow <- data.frame(OrgCD = getOrg$Organisation$OrgId$extension,
                                             OrgNM = getOrg$Organisation$Name,
                                             OrgStart    = OrgDates$Start,
                                             OrgEnd      = OrgDates$End,
                                             OrgRoleStart = RolePeriod$RoleStart,
                                             OrgRoleEnd   = RolePeriod$RoleEnd,
                                             RelOrgCD    = Rels$extension[j],
                                             RelType     = Rels$typeid[j],
                                             RelOrgPrimaryRole = Rels$id[j],
                                             RelStart    = Rels$Start[j],
                                             RelEnd      = Rels$End[j], stringsAsFactors=FALSE)

                        lkup <- bind_rows(lkup,addrow)
                    }
                }
            }
        }
    }
    return(lkup)
}








