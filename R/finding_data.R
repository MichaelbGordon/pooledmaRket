#' Information for 103 Findings included in Pooled Markets
#'
#' A dataset containing information on the 103 findings whose replication outcomes were
#' forecasted. See; (Camerer et al., 2016, 2018; Dreber et al., 2015; Forsell et al., 2018)
#'
#'
#' @format A data frame with 103 rows and 5 variables:
#' \describe{
#'   \item{project}{Project from which the prediction market came from. Options include RPP, EERP, SSRP & ML2}
#'   \item{finding_id}{Unqiue identifier for the finding}
#'   \item{cite}{Short citation for study. Format is: "authors (year)" }
#'   \item{doi}{Digital Object Identifier (DOI) for study}
#'   \item{replicated}{Outcome of Replication. 1 = Replicated, 0 =  Failed to Replicate}
#'   ...
#' }
#'
"finding_data"
