#' Look-up table of role id codes and their meanings
#'
#' A data set containing lookups for role id codes
#'
#' @format A dataframe of 119 rows and 5 columns:
#' \describe{
#'   \item{role_id}{Role Code}
#'   \item{role_name}{Description of code}
#'   \item{role_start_date}{Date role started}
#'   \item{role_end_date}{Date role ended}
#'   \item{notes}{Notes}
#' }
#' @source HSCOrgRefData_DataItemCatalogue.pdf from \url{https://digital.nhs.uk/binaries/content/assets/website-assets/services/ods/xml-organisation-data-products/ancilliary.zip}
"role_primary_lut"

#' Look-up table of named relationships in ODS
#'
#' A data set containing the meanings of the relationship ids and examples of
#' possible relationships. For more details see Appendix 1 from
#' HSCOrgRefData_DataItemCatalogue.pdf in
#' \url{https://digital.nhs.uk/binaries/content/assets/website-assets/services/ods/xml-organisation-data-products/ancilliary.zip}
#'
#' @format A dataframe of 6 rows and 6 variables
#' \describe{
#'   \item{id}{A relationship id}
#'   \item{meaning}{The nature of the relationship}
#'   \item{ex_id_from}{An example role id}
#'   \item{ex_role_from}{An example role name}
#'   \item{ex_id_to}{An example role id to which the relationship points}
#'   \item{ex_role_to}{An example role name to which the relationship points}
#' }
#' @source HSCOrgRefData_DataItemCatalogue.pdf from \url{https://digital.nhs.uk/binaries/content/assets/website-assets/services/ods/xml-organisation-data-products/ancilliary.zip}
"named_relationships"

#' A lookup of non-primary roles
#'
#' @format A dataframe wiht 55 rows of four variables
#' \describe{
#'   \item{code}{A role id}
#'   \item{role_name}{A role name}
#'   \item{role_start_date}{Date role came into existence}
#'   \item{role_end_date}{Date role ceased to exist}
#' }
#' @source HSCOrgRefData_DataItemCatalogue.pdf from \url{https://digital.nhs.uk/binaries/content/assets/website-assets/services/ods/xml-organisation-data-products/ancilliary.zip}
"role_nonprimary_lut"
