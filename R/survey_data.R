#' Responses for 103 Surveys
#'
#' A dataset containing each response that was made in 103 surveys which
#' were attempting to predict the outcome of direct replications. See; (Camerer et al., 2016, 2018; Dreber et al., 2015; Forsell et al., 2018)
#'
#'
#' @format A data frame with 7377 rows and 4 variables:
#' \describe{
#'   \item{project}{Project from which the survey came from. Options include RPP, EERP, SSRP & ML2}
#'   \item{study_id}{Unqiue (within project), identifier for the study the survey is trading on}
#'   \item{user_id}{User which made trade, is consistent with survey responses}
#'   \item{response}{Survey response, between 0 and 1 (0% and 100%)}
#'   ...
#' }
#'
"survey_data"