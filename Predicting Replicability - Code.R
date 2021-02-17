

# This script provides the figures and statistical analysis for the paper:
#    " Predicting replicability - analysis of survey and prediction market data from large-scale forecasting projects"

# Author of this script: Michael Gordon (m.b.gordon@massey.ac.nz)
# Authors of the paper: Michael Gordon, Domenico Viganola, Anna Dreber, Magnus Johannesson, Thomas Pfeiffer
# 

# Standard Packages -------------------------------------------------------


library(dplyr)
library(tidyr)
library(devtools)
library(ggplot2)
library(ggsci)
library(lubridate)
library(stringr)


# Install Data Package: pooledmaRket --------------------------------------
install_github("michaelbgordon/pooledmaRket")
library(pooledmaRket)



# Aggregate Data From Package ---------------------------------------------

aggregated_survey <-  survey_data %>% 
  group_by(project,finding_id) %>% 
  summarise(average_survey_response = mean(response),
            number_survey_responses = n())

aggregated_market <- market_data %>% 
  group_by(project, finding_id) %>% 
  arrange(time_stamp) %>% 
  summarise(final_market_price = dplyr::last(price),
            number_market_trades = n())


study_predictions <- finding_data %>% 
  left_join(aggregated_survey) %>% 
  left_join(aggregated_market) %>% 
  mutate(`Replication Status` = ifelse(replicated == 0, 'Unsuccessful Replication', 'Successful Replication')) %>% 
  arrange(final_market_price) %>% 
  mutate(graph_order = c(1:103))


# Create Simple Graphs -----------------------------------------------------------

figure1 <- 
  ggplot(study_predictions) +
  geom_point(aes(x=final_market_price, y=graph_order, colour = `Replication Status`), size = 3) +
  scale_color_manual(values = c( "#3CB371","#EE5C42")) +
  theme_light() +
  geom_vline(xintercept = 0.5,linetype='dashed') +
  labs(y = "Findings (ordered by price)", x = "Final Market Price") +
  theme(legend.justification=c(1,0), 
        legend.position=c(0.95,0.02),
        panel.grid = element_blank(),
        axis.text.y=element_blank(),
        axis.text = element_text(colour = 'black', size = 15),
        axis.title = element_text(colour = 'black', size = 20), 
        legend.title = element_text(colour = 'black', size = 20), 
        legend.text = element_text(colour = 'black', size = 15), 
  )

figure1

figure3 <- 
  ggplot(study_predictions) + 
  geom_point(aes(x=final_market_price, y=average_survey_response, colour = `Replication Status`), size = 3) +
  scale_color_manual(values = c( "#3CB371","#EE5C42")) +
  labs(y = "Average Survey Response", x = "Final Market Price") +
  theme_light() +
  geom_vline(xintercept = 0.5,linetype='dashed') +
  geom_hline(yintercept = 0.5,linetype='dashed') +
  ggplot2::annotate("text", label = stringr::str_wrap("Predicted to replicate by survey",20), x =  0, y = 0.55, size = 5, colour = "#3CB371",hjust = 0) +
  ggplot2::annotate("text", label = stringr::str_wrap("Predicted to not replicate by survey",20), x =  0, y = 0.45, size = 5, colour = "#EE5C42",hjust = 0) +
  ggplot2::annotate("text", label = stringr::str_wrap("Predicted to replicate by market",20), x =  0.52, y = 1, size = 5, colour = "#3CB371",hjust = 0) +
  ggplot2::annotate("text", label = stringr::str_wrap("Predicted to not replicate by market",20), x =  0.48, y = 1, size = 5, colour = "#EE5C42",hjust = 1) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0,0.25,0.5,0.75,1)) +
  scale_y_continuous(limits=c(0, 1),breaks=c(0,0.25,0.5,0.75,1)) +
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0),
        panel.grid = element_blank(),
        axis.text = element_text(colour = 'black', size = 15),
        axis.title = element_text(colour = 'black', size = 20), 
        legend.title = element_text(colour = 'black', size = 20), 
        legend.text = element_text(colour = 'black', size = 15), 
  ) 

figure3
# Other aggregations ------------------------------------------------------

other_aggregations <- survey_data %>% 
  group_by(project,finding_id) %>% 
  summarise(Mean = mean(response),
            Median = median(response),
            Voting = mean(round(response,0))) %>% 
  left_join(aggregated_market) %>% 
  rename(`Final Market Price` = final_market_price) %>% 
  gather(Mean, Median, Voting, `Final Market Price`, key =measure, value = forecast)  %>% 
  group_by(measure) %>% 
  summarise(mean = mean(forecast),
            sd = sd(forecast))



# Accuracy Analysis ----------------------------------------------------

### Testing for differences in proportions

# Overall (i.e survey vs market)

M <- as.data.frame(cbind(c(75,68), c(28, 35)))

chisq.test(M, correct = F) 

## Comparing accuracy with when concluding that a study will not replicate rather than  will replicate

#Market 

M1 <- as.data.frame(cbind(c(3, 25), c(28, 48)))
chisq.test(M1, correct = F)


# Survey

S1 <- as.data.frame(cbind(c(2, 33), c(22, 81)))
chisq.test(S1, correct = F)


# Over estimation
mean(study_predictions$average_survey_response)
sd(study_predictions$average_survey_response)

mean(study_predictions$final_market_price)
sd(study_predictions$final_market_price)

t.test(study_predictions$replicated, study_predictions$average_survey_response, paired = T)
t.test(study_predictions$replicated, study_predictions$final_market_price, paired = T)

mean(study_predictions$replicated)

#### Correlation with Outcomes

cor.test(study_predictions$replicated, study_predictions$average_survey_response)
cor.test(study_predictions$replicated, study_predictions$final_market_price)

project_correlations <-  study_predictions     %>% 
  group_by(project) %>% 
  summarise(estimate = cor.test(average_survey_response, final_market_price, method = 'spearman')$estimate,
            p = cor.test(average_survey_response, final_market_price, method = 'spearman')$p.value,
  )

#### Paired t-test for error

abs_error <- study_predictions %>% 
  mutate(survey_error = abs(average_survey_response - replicated),
         market_error = abs(final_market_price - replicated))

t.test(abs_error$survey_error, abs_error$market_error, paired = T)


# Error Reduction Analysis ---------------------------------------------------------

#Time weighted Smoothing Algorithim 

smoothing_data <- market_data %>% 
  mutate(rounded_trade = lubridate::ceiling_date(time_stamp, "hour")) %>% 
  group_by(project, finding_id) %>% 
  summarise(max  = max(rounded_trade)) %>% 
  ungroup() %>% 
  distinct(project, max)


smoothing_data <- market_data %>% 
  mutate(rounded_trade = lubridate::floor_date(time_stamp, "hour")) %>% 
  mutate(year = lubridate::year(time_stamp)) %>% 
  mutate(starting_time = case_when(
    project == 'EERP' ~ '2015-04-23 02:00:00',
    project == 'ML2' ~ '2014-09-30 13:00:00',
    project == 'SSRP' ~ '2016-11-08 02:00:00',
    project == 'RPP' & year == '2012' ~ '2012-11-09 10:00:00',
    project == 'RPP' & year == '2014' ~ '2014-10-21 14:00:00')) %>% 
  mutate(ending_time = case_when(
    project == 'EERP' ~ '2015-05-04 02:00:00',
    project == 'ML2' ~ '2014-10-14 12:00:00',
    project == 'SSRP' ~ '2016-11-22 00:00:00',
    project == 'RPP' & year == '2012' ~ '2012-11-22 04:00:00',
    project == 'RPP' & year == '2014' ~ '2014-11-04 15:00:00')) %>% 
  mutate(lead_date = as_datetime(lead(time_stamp, n = 1)),
         starting_time = as_datetime(starting_time),
         ending_time = as_datetime(ending_time)) %>% 
  ungroup() %>% 
  mutate(seconds_up = time_length(as_datetime(lead_date) - time_stamp)) %>% 
  mutate(seconds_up = ifelse(is.na(seconds_up), time_length(ending_time - time_stamp), seconds_up)) %>% 
  #mutate(half_time = ((ending_time - starting_time)/2) + starting_time) %>%
  mutate(half_time = starting_time + days(7)) %>% 
  mutate(half = ifelse(time_stamp < half_time, 'first_half', 'second_half')) %>% 
  ungroup() %>% 
  group_by(finding_id, half, project) %>% 
  mutate(last_trade_of_half = ifelse(last(transaction_id)==transaction_id,1,0)) %>% 
  filter(half == 'second_half' | last_trade_of_half == 1) %>% 
  ungroup() %>% 
  group_by(finding_id, project) %>% 
  mutate(probs = price*seconds_up,
         total_seconds = sum(seconds_up)) %>% 
  ungroup() %>% 
  group_by(finding_id,project) %>% 
  summarise(smoothed_score = as.numeric(sum(probs)/as.numeric(sum(seconds_up)))) %>% 
  right_join(study_predictions) 

smooth_analysis <- smoothing_data %>% 
  select(project, finding_id, replicated, final_market_price, smoothed_score) %>% 
  mutate(mae_price = abs(replicated - final_market_price),
         mae_smooth = abs(replicated - smoothed_score))

mean(smooth_analysis$mae_price)
mean(smooth_analysis$mae_smooth)

### Error reduction (over number of trades)
error_reduction <- market_data %>% 
  left_join(finding_data) %>% 
  select(project, finding_id, time_stamp, price,replicated) %>% 
  group_by(project, finding_id) %>% 
  mutate(number_of_trades = 1,
         number_of_trades = cumsum(number_of_trades)) %>% 
  ungroup() %>% 
  mutate(error = abs(replicated -price))


grid <- error_reduction %>% 
  select(finding_id, number_of_trades) %>% 
  expand(finding_id, number_of_trades) 



average_error <- grid %>% 
  left_join(error_reduction) %>% 
  tidyr::fill(project:error) %>% 
  group_by(number_of_trades) %>% 
  summarise(mean_error = mean(error)) %>% 
  mutate(error_reduction_percent = min(mean_error)/mean_error) %>% 
  mutate(total_error_reduction = 0.5 - mean_error) %>% 
  mutate(total_error_reduction_percent = total_error_reduction/max(total_error_reduction))


g1 <- ggplot(average_error,aes(x= number_of_trades, y = mean_error)) +
  geom_line() +
  geom_smooth(se = T) +
  xlab('Number of Trades') +
  ylab('Mean Absolute Error') +
  theme_minimal() +
  ggtitle('a')


trades_loess <- loess(total_error_reduction_percent~number_of_trades, data=average_error)

trades_loess_results <- predict(trades_loess) %>% 
  as_tibble() %>% 
  mutate(number_of_trades = average_error$number_of_trades)

trades_summary <- error_reduction %>% 
  group_by(finding_id) %>% 
  summarise(count = n())

### Error reduction (over hours after market started)

rounded_trades <- market_data %>% 
  mutate(rounded_trade = lubridate::ceiling_date(time_stamp, "hour")) %>% 
  group_by(project, finding_id) %>% 
  mutate(year = lubridate::year(time_stamp)) %>% 
  mutate(starting_time = case_when(
    project == 'EERP' ~ '2015-04-23 02:00:00',
    project == 'ML2' ~ '2014-09-30 13:00:00',
    project == 'SSRP' ~ '2016-11-08 02:00:00',
    project == 'RPP' & year == '2012' ~ '2012-11-09 10:00:00',
    project == 'RPP' & year == '2014' ~ '2014-10-21 13:00:00')) %>% 
  mutate(rebase_time = as.numeric(as.difftime(rounded_trade - as_datetime(starting_time), units='hour')),
         rebase_time = ifelse(rebase_time < 0, 1, rebase_time)) %>% 
  ungroup() %>% 
  group_by(project, finding_id, rebase_time) %>% 
  summarise_all(last)


time_grid <- rounded_trades %>% 
  ungroup() %>% 
  select(finding_id, rebase_time) %>% 
  expand(finding_id, c(1:338))  %>% 
  rename(rebase_time = `c(1:338)`)

average_error_time <- time_grid %>% 
  left_join(rounded_trades) %>% 
  tidyr::fill(project:rounded_trade) %>% 
  left_join(finding_data)  %>% 
  select(project, finding_id, price, replicated, rebase_time) %>% 
  mutate(error = abs(price-replicated)) %>% 
  drop_na() %>% 
  group_by(rebase_time) %>%
  summarise(mean_error = mean(error)) %>% 
  mutate(error_reduction_percent = min(mean_error)/mean_error) %>% 
  mutate(total_error_reduction = 0.5 - mean_error) %>% 
  mutate(total_error_reduction_percent = total_error_reduction/max(total_error_reduction))

each_claim_time_error <-  time_grid %>% 
  left_join(rounded_trades) %>% 
  tidyr::fill(project:rounded_trade) %>% 
  left_join(finding_data)  %>% 
  select(project, finding_id, price, replicated, rebase_time) %>% 
  mutate(error = abs(price-replicated))



time_loess <- loess(total_error_reduction_percent~rebase_time, data=average_error_time )

trades_loess_results <- predict(time_loess) %>% 
  as_tibble() %>% 
  mutate(time = c(1:338))

g2<- ggplot(average_error_time,aes(x=rebase_time, y= mean_error)) +
  geom_line() +
  geom_smooth(se = T) +
  xlab('Hours after Market Start') +
  ylab('Mean Absolute Error') +
  theme_minimal() +
  ggtitle('b')


figure4 <- gridExtra::grid.arrange(g1,g2, ncol = 2)

# P value Analysis -----------------------------------------------------------------


p_val_data <-  finding_data %>% 
  mutate(p_value_numeric = str_remove_all(original_pval,'[[:alpha:]]| \\<|[[:blank:]]') %>% as.numeric) %>% 
  mutate(benjamin_cat = case_when(
    p_value_numeric > 0.005 ~ 'cat1>0.005',
    p_value_numeric <= 0.005 ~ 'cat2<0.005'))


lm_p_cat_b <- lm(replicated ~ benjamin_cat, data = p_val_data)
lm_p_cat_b %>% summary()

cor.test(p_val_data$replicated, as.numeric(as.factor(p_val_data$benjamin_cat)))

# Summary 

p_value_summary <- p_val_data %>% 
  group_by(replicated,benjamin_cat) %>% 
  summarise(count = n())


# Distance from 0.5 -------------------------------------------------------

dist_data <- study_predictions %>% 
  mutate(market_dist = abs(final_market_price - 0.5),
         survey_dist = abs(average_survey_response - 0.5))

t.test(dist_data$market_dist, dist_data$survey_dist, paired = T)


# Aggregations ------------------------------------------------------------

aggregations <-  survey_data %>% 
  group_by(forecaster_id) %>% 
  mutate(user_variance = var(response)) %>% 
  ungroup() %>% 
  group_by(project,finding_id) %>%
  summarise(mean_aggregation = mean(response),
            median_aggregation = median(response),
            weighted_mean = sum(user_variance*response)/sum(user_variance),
            simple_voting = mean(round(response)))

agg_summary <- aggregations %>% 
  ungroup() %>% 
  select(3:6) %>% 
  summarise_all(list(mean=mean,sd=sd)) %>% 
  gather()

aggregations_acc <- aggregations %>% 
  left_join(finding_data) %>% 
  ungroup() %>% 
  mutate(mean_error = abs(replicated-mean_aggregation),
         median_error = abs(replicated-median_aggregation),
         weighted_error =abs(replicated-weighted_mean ),
         voting_error = abs(replicated-simple_voting))

mean(aggregations_acc$mean_error)
mean(aggregations_acc$median_error)
mean(aggregations_acc$weighted_error)
mean(aggregations_acc$voting_error)



# Meta Analysis -----------------------------------------------------------

library(meta)

meta_correlation_data <- study_predictions %>% 
  group_by(project) %>% 
  summarise(cor = cor.test(replicated, final_market_price)$estimate, 
            n= n()) %>% 
  rename(Author = project)

m.cor <- metacor(cor, 
                 n, 
                 data = meta_correlation_data,
                 studlab = meta_correlation_data$Author,
                 sm = "ZCOR",
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 prediction = FALSE,
                 text.random = "Overall effect",
                 method.tau = "SJ", 
                 hakn= TRUE
)


m.cor

figure2 <- forest(m.cor,text.random ='Pooled Effect',
       prediction = FALSE, 
       smlab = 'Pearsons Correlation',
       leftcols = c('studlab','effect', 'n','ci'),
       leftlabs = c('Study', 'Cor', 'N', '[95% CI]'),
       rightcols = c("w.fixed"),
       digits.se  = 2, 
       fontsize = 18,
       squaresize = 1,
       plotwidth = '8cm'
)

# P Val Meta Analysis -----------------------------------------------------

p_meta_data_lm <- p_val_data %>% 
  group_by(project) %>% 
  summarise(TE = coef(summary(lm(replicated ~ benjamin_cat)))[2,1],
            Standard.Error = coef(summary(lm(replicated ~ benjamin_cat)))[2,2]) %>% 
  rename(Author = project)

p_meta_data_lm$n <- meta_correlation_data$n

meta_p <- metagen(TE,
             Standard.Error,
             n.e = n,
             data = p_meta_data_lm,
             studlab = paste(Author),
             comb.fixed = FALSE,
             comb.random = TRUE,
             method.tau = "SJ",
             hakn = TRUE,
             prediction = FALSE,
             sm = "MD")

meta_p
figure5 <- forest(meta_p,text.random ='Pooled Effect',
       prediction = FALSE, 
       smlab = 'Regression Coefficient',
       leftcols = c('studlab','effect', 'seTE', 'n.e','ci'),
       leftlabs = c('Study', 'Effect','S.E', 'N', '[95% CI]'),
       rightcols = c("w.fixed"),
       digits.se  = 2, 
       fontsize = 18,
       squaresize = 1,
       plotwidth = '8cm'
)
?forest
