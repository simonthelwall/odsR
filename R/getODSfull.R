#' getODSfull
#'
#' Extracts full ODS data for a single organisation from the NHS Digitial ODS ORD API into a data frame.
#'
#' @param ODSCode The organisation to return details for; quoted string; no default
#' @inheritparams getODS
#'
#' @return returns a list of length 1 containing the full details for the Organisation including:
#'         Name, Date Start, Status, Last Change Date, Organisation Record Class,
#'         Address, Postcode, Primary Role ID, Non-primary Role ID,
#'         Non primary Role ID, Primary Role Description, Organisation Link (API endpoint URL for full organisation record)
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # return Organisation data for RRF12
#' getODSfull(ODSCode="RRF12")
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------


# create function to allow user to specify parameters to input to ODS API call
getODSfull <- function(ODSCode,Format="application/json") {

   # error checks
    if (is.null(ODSCode)) {
          stop("ERROR: Please specify an ODS Code to retrieve reference data for")
    } else if (!(tolower(Format) %in% c("json", "xml","text/json","text/xml","application/json","application/xml"))) {
          stop("ERROR: Format is invalid - valid values: json, xml, text/json, text/xml, application/json, application/xml")
    }

    urlfull <- paste0("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",ODSCode,"?_format=",Format,sep="")

    # better to set config elsewhere - not within function ??
    set_config(config(ssl_verifypeer = 0L))

    httpResponse <- GET(urlfull, accept_json())
    getODSfull <- fromJSON(content(httpResponse, "text", encoding="UTF-8"))

  return(getODSfull)
}

