
## pooledmaRket

`PooledMarketR` is an R package built to provide the Pooled Market
Dataset.

This dataset pools together the data from four similar prediction market
on the results of systematic replication studies.

The systematic replication studies are:

  - Replication Project: Psychology (Open Science Collaboration.
    Estimating the reproducibility of psychological science. Science
    2015)
  - Experimental Economics Replication Project (Camerer et
    al. Evaluating replicability of laboratory experiments in
    economics. Science 2016)
  - Social Sciences Replication Project (Camerer et al. \_Evaluating the
    Replicability of Social Science Experiments in Nature and
    Science.\_Nature Human Behaviour (2018))
  - Many Labs 2 (Klein et al. Many Labs 2: Investigating Variation in
    Replicability Across Samples and Settings. Advances in Methods and
    Practices in Psychological Science 2018)

And the accompanying prediction markets are

*RPP - Dreber et al 2015 (PNAS) *EERP - Camerer et al 2016 (Science)
*SSRP - Camerer et al 2018 (Nat Human Behav) *ML2 - Forsell et al 2018

In the dataset the prediction markets are referred to as ‘projects’. I
would recommend reading the above studies before using the package. The
studies will explain the methodologies and provide context to the data.

### What this package contains

  - `study_data`. This data set includes information about the original
    study that was replicated, including study name and a binary
    variable where 0 indicates an unsuccessful replication and 1
    indicates successful replication.
  - `survey_data` . This includes individual responses to pre-market
    surveys. See studies for specific survey information, such as
    question wording.
  - `market_data`. This includes individual transactions for each study.
    Final market prices are taken as market beliefs. Note that user\_id
    in this data relates to the same user\_id in the survey data.

### How to use this package

The below example calculates the average survey belief and the final
market price for each of the 102 studies and the collates them into a
single table.

``` r
library(dplyr)
library(tidyr)
library(devtools)

install_github("michaelbgordon/pooledmaRket")
library(pooledmaRket)

aggregated_survey <- survey_data %>% group_by(project, study_id) %>% summarise(average_survey_response = mean(response))

aggregated_market <- market_data %>% group_by(project, study_id) %>% summarise(final_market_price = dplyr::last(price))


study_predictions <- study_data %>% left_join(aggregated_survey) %>% left_join(aggregated_market)
```
