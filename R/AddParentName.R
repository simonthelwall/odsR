#' AddParentName
#'
#' Adds Parent Organisation Name to a data.frame containing Parent Organisation Code using the NHS Digitial ODS ORD API.
#'
#' @return returns the original data.frame with Parent name column appended
#'
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # return full Organisation data for RRF12
#' AddParentName(lkup,"ParentOrgCD")
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

AddParentName <- function (df, parentCD) {

    # create ParentOrgNm column placeholder
 #   df$ParentOrgNm <- NA

    # loop through rows to populate ParentOrgNm
    for (i in 1:nrow(df)) {

        ParentOrgNm = getODSfull(parentCD)$Organisation$Name
        df$ParentOrgNm[i] <- ParentOrgNm

    }
}


