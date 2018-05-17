#' getODSfull
#'
#' Extracts full ODS data for a single organisation from the NHS Digitial ODS ORD API into a data frame.
#'
#' @param ODSCode The organisation to return details for; quoted string; no default
#' @inheritparams getODS
#'
#' @return returns a data.frame containing the full details for the organisation including:
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
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------


# create function to allow user to specify parameters to input to ODS API call
getODSfull <- function(ODSCode,Format="json") {

   # error checks
    if (is.null(ODSCode)) {
          stop("ERROR: Please specify an ODS Code to retrieve reference data for")
    } else if (!(tolower(Format) %in% c("json", "xml","text/json","text/xml","application/json","application/xml"))) {
          stop("ERROR: Format is invalid - valid values: json, xml, text/json, text/xml, application/json, application/xml")
    }


    myQueryfull <- paste0("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",ODSCode,"?_format=",Format,sep="")
    cnxfull     <- url(c(myQueryfull),"rb")

    getODSfull <- fromJSON(cnxfull)  %>%
      unlist() %>%
      as.matrix()

   close(cnxfull)

  return(getODSfull)
}





