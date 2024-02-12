install.packages("BTYDplus")
library(BTYDplus)

# we thankfully have a complete event log already created

event_log <- read.csv("/Users/khushishelat/Donor Lifetime Value/event_log_complete.csv")
head(event_log)

calibration_end <- as.POSIXct("1994-12-01")

# Convert date column to POSIXct format
event_log$date <- as.POSIXct(event_log$date)

# Verify the conversion
summary(event_log)

# create the sufficient statistic table CBS
(elog2cbs(event_log, T.cal = calibration_end) -> CBS)


# evaluate using visualizations the effectiveness of pareto/NBD analysis
op <- par(mfrow = c(1, 2))
k.wheat <- estimateRegularity(event_log, method = "wheat",
                              plot = TRUE, title = "Wheat & Morrison")

k.mle <- estimateRegularity(event_log, method = "mle",
                          plot = TRUE, title = "Maximum Likelihood")

# it is important to note that the above code resulted in an error, no minimum number of transactions

par(op)

#params.pnbd <- BTYD::pnbd.EstimateParameters(CBS)
#names(params.pnbd) <- c("r", "alpha", "s", "beta")
#round(params.pnbd, 3)

head(CBS)

# my thought is that this is not working due to the inclusion of customers who have only 1 purchase
# inter-purchase time estimates may not be working due to this. 

# Filter event_log to include only customers with at least one purchase
event_log_filtered_no_single_purchase <- event_log %>%
  group_by(cust) %>%
  filter(n() >= 1) %>%
  ungroup()

# View the first few rows of the filtered event_log
head(event_log_filtered_no_single_purchase)
summary(event_log_filtered_no_single_purchase)

#create new cbs
(elog2cbs(event_log, T.cal = calibration_end) -> CBS_no_single_donor)


# evaluate using visualizations the effectiveness of pareto/NBD analysis
op <- par(mfrow = c(1, 2))
k.wheat <- estimateRegularity(event_log_filtered_no_single_purchase, method = "wheat",
                              plot = TRUE, title = "Wheat & Morrison")

#k.mle <- estimateRegularity(event_log_filtered_no_single_purchase, method = "mle",
#                            plot = TRUE, title = "Maximum Likelihood")

# it is important to note that the above code resulted in an error, no minimum number of transactions

par(op)

#params.pnbd <- BTYD::pnbd.EstimateParameters(CBS_no_single_donor)
#names(params.pnbd) <- c("r", "alpha", "s", "beta")
#round(params.pnbd, 3)


