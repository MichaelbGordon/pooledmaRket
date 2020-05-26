#' Responses for 103 Surveys
#'
#' A dataset containing each response that was made in 103 surveys which
#' were attempting to predict the outcome of direct replications.
#' See; (Camerer et al., 2016, 2018; Dreber et al., 2015; Forsell et al., 2018)
#'
#' @format A data frame with 7885 rows and 10 variables:
#' \describe{
#'   \item{project}{Project from which the survey came from. Options include RPP, EERP, SSRP & ML2}
#'   \item{study_id}{Unique (within project), identifier for the study the survey is trading on}
#'   \item{user_id}{User which made trade, is consistent with market trades}
#'   \item{response}{Survey response, between 0 and 1 (0 being no chance to replicate, 1 being no chance to not replicate)}
#'   ...
#' }
#'
"survey_data"
