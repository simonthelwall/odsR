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

ParentCol <- "ParentCD"

AddParentName <- function (df, ParentCol) {

    # create list of unique parent codes
    codes <- df[ParentCol] %>%
        unique()
    names(codes) <- "codes"

    names <- data.frame()

    # loop through rows to populate ParentOrgNm
    for (i in 1:nrow(codes)) {

        if (is.na(slice(codes,i))) {
            thisname <- NA
            names(thisname) <- "names"
        } else {
            thiscode <- slice(codes,i)
            thisname = getODSfull(thiscode)$Organisation$Name
            names(thisname) <- "names"
        }
    names <- bind_rows(names,thisname)
    }
    all <- bind_cols(codes, names)

    # vector showing which column from df contains the parent codes
    which(match(names(df),ParentCol)) # is this useful? does it return 5 - position of ParentCD column in df?

    # vector showing which df rows to join to which all rows
#    matches <- match(df$ParentCD,all[colref])
    matches <- match(df$ParentCD,all$codes])
    df$ParentOrgNM <- all[matches,2]
    return(df)
}


