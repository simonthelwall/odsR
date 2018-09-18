#' OrgRelLkp
#'
#' Generates an organisational lookup table from the NHS Digital ODS ORD API
#' based on organisational relationship data.
#'
#' @param PrimaryRole     The Primary Role code for organisations to be included in the lookup;
#'                        quoted string, no default
#' @param NonPrimaryRole  The Non Primary Role code for organisations to be included in the lookup;
#'                        quoted string, no default
#' @param RelTypes The Named Relationship Types for related organisations to be included in the lookup;
#'                 character vector, no default
#' @param RelPrimaryRoles The Primary Role Ids for related organisations to be included in the lookup;
#'                        character vector, no default
#' @param FromDate The effective date from which to include Organisations operational on or after;
#'                 character string in the format "yyyy-mm-dd", no default
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
#'
#' \dontrun{
#' OrgRelLkp(PrimaryRole    = "RO177",
#'           NonPrimaryRole = "RO76",
#'           RelTypes       = "RE4",
#'           RelPrimaryRoles       = "RO98",
#'           FromDate       = "2013-04-01")
#' }
#'
#' # return GP practice to CCG/PCT Lookup to include all organisations effective on or after 01-04-2012
#' \dontrun{
#' OrgRelLkp("RO177","RO76","RE4",c("RO98","RO179"),"2012-04-01")
#' }
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

# changes for testing:
# added postcode field
# changed dates to date format

# create function to generate Organisation lookup data.frame
OrgRelLkp <- function(PrimaryRole, NonPrimaryRole, RelTypes, RelPrimaryRoles, FromDate) {

    # display experimental message
    message("Please Note that this function is experimental and has not been thoroughly
             QAd for every possible set of arguments.")

    # retrieve all organisations to include records for
    allorgs <- getODS(PrimaryRoleId=PrimaryRole,NonPrimaryRoleId=NonPrimaryRole) %>%
        unique()

    # Create empty Lookup dataframe
    lkup <- data.frame(OrgId       = character(),
                       OrgName     = character(),
                       OrgPostCode = character(),
                       OrgStart    = as.Date(character()),
                       OrgEnd      = as.Date(character()),
                       OrgRoleStart = as.Date(character()),
                       OrgRoleEnd  = as.Date(character()),
                       RelOrgId    = character(),
                       RelType     = character(),
                       RelOrgPrimaryRole = character(),
                       RelStart    = as.Date(character()),
                       RelEnd      = as.Date(character()),
                       stringsAsFactors=FALSE)


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

        # find periods in primary and non primary roles
        RolesPrimary <- dplyr::bind_cols(RoleIds,RoleDates) %>%
            filter(Role == PrimaryRole)
        if (!("End" %in% colnames(RolesPrimary))) {
            RolesPrimary$End <- NA
        }

        RolesNonPrimary <- dplyr::bind_cols(RoleIds,RoleDates) %>%
            filter(Role == NonPrimaryRole)
        if (!("End" %in% colnames(RolesNonPrimary))) {
            RolesNonPrimary$End <- NA
        }
        colnames(RolesNonPrimary) <- c("RoleNP", "Type","StartNP", "EndNP")

        # Find periods org was in required role
        RolePeriods <- inner_join(RolesPrimary, RolesNonPrimary, by="Type") %>%
            mutate(RoleStart = pmax(Start,StartNP),
                   RoleEnd = pmin(End, EndNP, na.rm=TRUE)) %>%
            select(RoleStart,RoleEnd)

        # keep only organisations in operation with required role after specified FromDate
        if(all(is.na(RolePeriods$RoleEnd))) {
            addOrg <- 1
        } else if (any(RolePeriods$RoleEnd >= FromDate)) {
           addOrg <- 1
           RolePeriods <- RolePeriods %>%
               filter(is.na(RolePeriods$RoleEnd) | RolePeriods$RoleEnd >= FromDate)
        } else {
            addOrg <- 0
        }

        # continue if Organisation record needs to be included in output
        if (addOrg == 1) {

            # find related organisation dates

            # if no relationships exist populate parent and rel columns with NA
            if (!(is.list(getOrg$Organisation$Rels))) {

                # add row for each roleperiod
                for (k in 1:nrow(RolePeriods)) {
                    addrow <- data.frame(OrgId   = getOrg$Organisation$OrgId$extension,
                                    OrgName      = getOrg$Organisation$Name,
                                    OrgPostCode  = getOrg$Organisation$GeoLoc$Location$PostCode,
                                    OrgStart     = as.Date(OrgDates$Start, origin = "1900-01-01"),
                                    OrgEnd       = as.Date(OrgDates$End, origin = "1900-01-01"),
                                    OrgRoleStart = as.Date(RolePeriods$RoleStart[k], origin = "1900-01-01"),
                                    OrgRoleEnd   = as.Date(RolePeriods$RoleEnd[k], origin = "1900-01-01"),
                                    RelOrgId     = NA,
                                    RelType      = NA,
                                    RelOrgPrimaryRole = NA,
                                    RelStart     = NA,
                                    RelEnd       = NA, stringsAsFactors=FALSE)

                    lkup <- bind_rows(lkup,addrow)
                }

            # otherwise find relationships
            } else {
                # find relationship dates and append RelTypes, Roles and Ids
                RelDates <- dplyr::bind_rows(getOrg$Organisation$Rels$Rel$Date) %>%
                    filter(Type == "Operational")
                # add end column if missing
                if (!("End" %in% colnames(RelDates))) {
                    RelDates$End <- NA
                }

                RelTypeIds <- data.frame(typeid    = getOrg$Organisation$Rels$Rel$id,
                                         stringsAsFactors=FALSE )
                RelRoles   <- data.frame(id        = getOrg$Organisation$Rels$Rel$Target$PrimaryRoleId$id,
                                         stringsAsFactors=FALSE)
                RelOrgs    <- data.frame(extension = getOrg$Organisation$Rels$Rel$Target$OrgId$extension,
                                         stringsAsFactors=FALSE)

                # only keep Rels that existed during Role Period
                if(all(is.na(RolePeriods$RoleEnd))) {
                    Rels       <- dplyr::bind_cols(RelTypeIds,RelDates,RelOrgs,RelRoles) %>%
                        filter(id     %in% RelPrimaryRoles &
                               typeid %in% RelTypes &
                               (End >= min(RolePeriods$RoleStart) | is.na(End)))
                } else {
                    Rels       <- dplyr::bind_cols(RelTypeIds,RelDates,RelOrgs,RelRoles) %>%
                        filter(id     %in% RelPrimaryRoles &
                                   typeid %in% RelTypes &
                                   (Start  <= max(RolePeriods$RoleEnd, na.rm=TRUE) | all(is.na(RolePeriods$RoleEnd))) &
                                   (End >= min(RolePeriods$RoleStart) | is.na(End)))
                }


                # if relationships exist but not of correct type & period populate parent and rel columns with NA
                if (nrow(Rels) == 0) {
                    # add row for each roleperiod
                    for (k in 1:nrow(RolePeriods)) {

                        addrow <- data.frame(OrgId    = getOrg$Organisation$OrgId$extension,
                                         OrgName      = getOrg$Organisation$Name,
                                         OrgPostCode  = getOrg$Organisation$GeoLoc$Location$PostCode,
                                         OrgStart     = as.Date(OrgDates$Start, origin = "1900-01-01"),
                                         OrgEnd       = as.Date(OrgDates$End, origin = "1900-01-01"),
                                         OrgRoleStart = as.Date(RolePeriods$RoleStart[k],origin = "1900-01-01"),
                                         OrgRoleEnd   = as.Date(RolePeriods$RoleEnd[k],origin = "1900-01-01"),
                                         RelOrgId     = NA,
                                         RelType      = NA,
                                         RelOrgPrimaryRole = NA,
                                         RelStart     = NA,
                                         RelEnd       = NA, stringsAsFactors=FALSE)

                        lkup <- bind_rows(lkup,addrow)
                    }

                } else {

                    # if Rels with correct type and roles exist - loop through each row to add related codes
                    for (j in 1:nrow(Rels)) {

                        # build lookups record
                        if (!("End" %in% colnames(Rels))) {
                            Rels$End[j] <- NA
                        }

                        # add row for each roleperiod
                        for (k in 1:nrow(RolePeriods)) {

                            addrow <- data.frame(OrgId    = getOrg$Organisation$OrgId$extension,
                                             OrgName      = getOrg$Organisation$Name,
                                             OrgPostCode  = getOrg$Organisation$GeoLoc$Location$PostCode,
                                             OrgStart     = as.Date(OrgDates$Start, origin = "1900-01-01"),
                                             OrgEnd       = as.Date(OrgDates$End, origin = "1900-01-01"),
                                             OrgRoleStart = as.Date(RolePeriods$RoleStart[k],origin = "1900-01-01"),
                                             OrgRoleEnd   = as.Date(RolePeriods$RoleEnd[k],origin = "1900-01-01"),
                                             RelOrgId     = Rels$extension[j],
                                             RelType      = Rels$typeid[j],
                                             RelOrgPrimaryRole = Rels$id[j],
                                             RelStart     = as.Date(Rels$Start[j],origin = "1900-01-01"),
                                             RelEnd       = as.Date(Rels$End[j],origin = "1900-01-01"), stringsAsFactors=FALSE)

                            lkup <- bind_rows(lkup,addrow)
                        }
                    }
                }
            }
        }
    }
    return(lkup)
}




