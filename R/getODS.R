#' getODS
#'
#' Extracts summary ODS data for multiple organisations from the NHS Digitial ODS ORD API into a data frame.
#'
#' @param Name     Search organisations based on name. Organisations that contain the argument string in their name are returned.;
#'                 quoted string; default "All" applies no filter
#' @param PostCode Search organisations based on postcode. Organisations that contain the argument string in their postcode are returned.;
#'                 quoted string; default "All" applies no filter
#' @param LastChangeDate Search for organisations based on their last changed date. Date must be in format "YYYY-MM-DD".
#'                       The search is greater than or equal to. Dates are restricted to 185 days from present.;
#'                 quoted string; default "All" applies no filter
#' @param Status   Search for organisations based on their active status. Arguments can be "Active" or "Inactive".;
#'                 quoted string; default "All" applies no filter
#' @param PrimaryRoleId Search for organisations based on their primary role codes.;
#'                 quoted string; default "All" applies no filter
#' @param NonPrimaryRoleId Search for organisations based on their non primary role codes.;
#'                 quoted string; default "All" applies no filter
#' @param OrgRecordClass Search for oganisations based on their record class. Arguments can be "RC1" or "RC2".;
#'                 quoted string; default "All" applies no filter
#' @param Limit    Limit the number of organisations returned per response. The argument can range from 1-1000.
#'                 numeric integer; default 1000
#' @param Offset   Offset the start point of the result set, by the value specified. The argument can range from 1-****.
#'                 numeric integer; default 1
#' @param Format   Specify the output format.  Arguments can be "xml", "json", "text/json", "text/xml",
#'                 "application/json" and "application/xml". quoted string, default "json"
#'#'
#' @return returns a data.frame containing the following details for the organisations that meet the filter specifications:
#'         Name, Organisation ID, Status, Organisation Record Class, Postcode, Last Change Date, Primary Role ID,
#'         Non primary Role ID, Primary Role Description, Organisation Link (API endpoint URL for full organisation record)
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # return Organisation data for all active GP practices
#' getODS(Status="Active", PrimaryRoleId = "RO177", NonPrimaryRoleId = "RO76")
#'
#' @import dplyr
#' @import jsonlite
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

# create function to allow user to specify parameters to input to ODS API call
getODS <- function(Name             = "All",
                    PostCode         = "All",
                    LastChangeDate   = "All",
                    Status           = "All",
                    PrimaryRoleId    = "All",
                    NonPrimaryRoleId = "All",
                    OrgRecordClass   = "All",
                    Limit            = 1000,
                    Offset           = 1,
                    Format           = "json") {
 # error checks
    if (Name           == "All" & PostCode         == "All" &
        LastChangeDate == "All" & Status           == "All" &
        PrimaryRoleId  == "All" & NonPrimaryRoleId == "All" &
        OrgRecordClass == "All") {
          stop("ERROR: at least one organisational parameter must be specified")
    } else if (!(tolower(Status) %in% c("all", "active","inactive"))) {
          stop("ERROR: Status is invalid - valid values are All (default), Active, Inactive")
    } else if (!(tolower(PrimaryRoleId) %in% c("all", "ro98","ro101","ro177"))) {
          stop("ERROR: PrimaryRoleId is invalid - valid values are All (default), RO98 (CCG), RO101 (Care Home Site), RO177 (Prescribing Cost Centre)")
    } else if (!(tolower(NonPrimaryRoleId) %in% c("all", "ro76","ro218"))) {
          stop("ERROR: NonPrimaryRoleId is invalid - valid values are All (default), RO76 (GP Practice Prescribing Cost Centre), RO218 (Commissioning Hub)")
    } else if (!(tolower(OrgRecordClass) %in% c("all", "rc1","rc2"))) {
          stop("ERROR: OrgRecordClass is invalid - valid values are All (default), RC1 (Health and Social Care Organisation), RC2 (Health and Social Care Organisation Site)")
    } else if (!(tolower(Format) %in% c("json", "xml","text/json","text/xml","application/json","application/xml"))) {
          stop("ERROR: Format is invalid - valid values are json, xml, text/json, text/xml, application/json, application/xml")
    } else if (!(is.numeric(Limit))) {
        stop("ERROR: Limit must be a numeric integer")
    } else if (!(Limit%%1==0)){
        stop("ERROR: Limit must be a numeric integer")
    } else if (Limit > 1000) {
        stop("ERROR: Limit must be between 1 and 1000")
    } else if (!(is.numeric(Offset))) {
        stop("ERROR: Offset must be a numeric integer")
    } else if (!(Offset%%1==0)) {
        stop("ERROR: Offset must be a numeric integer")
    }


# define organisation search endpoint URL
    myQuery <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?"

# complete URL using specified parameters
    if (!Name=="All") {
      myQuery <- paste0(myQuery,"&Name=", Name)
    }

    if (!PostCode=="All") {
      myQuery <- paste0(myQuery,"&PostCode=", PostCode)
    }

    if (!LastChangeDate=="All") {
      myQuery <- paste0(myQuery,"&LastChangeDate=", LastChangeDate)
    }

    if (!Status=="All") {
      myQuery <- paste0(myQuery,"&Status=", Status)
    }

    if (!PrimaryRoleId=="All") {
      myQuery <- paste0(myQuery,"&PrimaryRoleId=", PrimaryRoleId)
    }

    if (!OrgRecordClass=="All") {
      myQuery <- paste0(myQuery,"&OrgRecordClass=", OrgRecordClass)
    }

# append offset, limit and format to URL
  myQuery <- paste0(myQuery,"&Limit=", Limit,"&Offset=",Offset,"&_format=",Format)

# define read only URL connection
  cnx     <- url(c(myQuery),"rb")

# submit API request
  getODS <- fromJSON(cnx)$Organisations
#  subsetting from returned list works unless only 1 result in List then List of length 0 returned


  #rbind_pages()

# close URL connection
  close(cnx)

  return(getODS)
}




