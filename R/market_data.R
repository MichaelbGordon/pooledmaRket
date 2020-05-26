#' Trading Data for 103 Prediction Markets
#'
#' A dataset containing each trade that was made in 103 prediction markets which
#' were attempting to predict the outcome of direct replications. See; (Camerer et al., 2016, 2018; Dreber et al., 2015; Forsell et al., 2018)
#'
#'
#' @format A data frame with 7885 rows and 10 variables:
#' \describe{
#'   \item{project}{Project from which the prediction market came from. Options include RPP, EERP, SSRP & ML2}
#'   \item{transaction_id}{Unqiue (within project) identifier for each prediction market transaction}
#'   \item{time_stamp}{Date and time of market transaction}
#'   \item{user_id}{User which made trade, is consistent with survey responses}
#'   \item{study_id}{Unqiue (within proiject), identifier for the study the market is trading on}
#'   \item{transaction_type}{Buy or sell (did the trade move the price up or down)}
#'   \item{transaction_cost}{Number of points (trading currency) used in trade (NA in SSRP due to methodology differences)}
#'   \item{transaction_shares}{Number of shares purchased in trade (NA in SSRP due to methodology differences)}
#'   \item{net_shares}{Total net shares for each study}
#'   ...
#' }
#'
"market_data"
