library(tidyverse)
library(dplyr)
library(BTYDplus)

transaction_level_givingtuesday_data <- read.csv("~/Downloads/fep_sample (1)/transaction_sample.csv")
org_level_giving_tuesday_data <- read.csv("~/Downloads/fep_sample (1)/organizations_sample.csv")
head(transaction_level_givingtuesday_data)


# Count unique values for each column
unique_counts <- sapply(transaction_level_givingtuesday_data, function(x) n_distinct(x, na.rm = TRUE))

# Print the unique counts
print(unique_counts)

# Find the organization with the highest number of transactions per donor
# Calculate the count of unique customers (donorid) for each orgid
org_donations_counts <- transaction_level_givingtuesday_data %>%
  group_by(orgid) %>%
  summarise(unique_customers = n_distinct(donorid))

# Find the organization with the highest count of unique customers
# Filter the data to include only donors with <= 250 transactions to their donorid
filtered_data <- transaction_level_givingtuesday_data %>%
  group_by(orgid, donorid) %>%
  summarise(num_transactions = n()) %>%
  filter(num_transactions <= 250)

# Count the number of transactions for each orgid
transactions_per_org <- filtered_data %>%
  group_by(orgid) %>%
  summarise(total_transactions = sum(num_transactions))

# Find the orgid with the greatest number of transactions per donor
org_with_max_transactions <- transactions_per_org %>%
  filter(total_transactions == max(total_transactions))

# Print the orgid with the greatest number of transactions per donor
print(org_with_max_transactions)

filtered_org_data <- org_level_giving_tuesday_data %>%
  filter(orgid == "E015A0C52A9F6C3CF38DE0C03EC74EE0")

head(filtered_org_data)

# create an event log with just this organization's data
public_benefit_elog <- transaction_level_givingtuesday_data %>%
  filter(orgid == "E015A0C52A9F6C3CF38DE0C03EC74EE0")

head(public_benefit_elog)


# Remove rows with NA values for date, amount, or donorid
cleaned_public_benefit_elog <- na.omit(public_benefit_elog[c("date", "amount", "donorid")])

# Check the first few rows of the cleaned dataframe
head(cleaned_public_benefit_elog)

nrow(public_benefit_elog)
nrow(cleaned_public_benefit_elog)

# Convert the date column to Date type
public_benefit_elog$date <- as.Date(public_benefit_elog$date)

# Check the structure of the dataframe to confirm the change
str(public_benefit_elog)

# now, create the variables we need to model with Pareto NBD
# Selecting and renaming columns
public_benefit_elog <- public_benefit_elog %>%
  select(cust = donorid, date, sale = amount)

# Display the modified dataframe
head(public_benefit_elog)

op <- par(mfrow = c(1, 2))
k.wheat <- estimateRegularity(public_benefit_elog, method = "wheat",
                              plot = TRUE, title = "Wheat & Morrison")

k.mle <- estimateRegularity(public_benefit_elog, method = "mle",
                            plot = TRUE, title = "Maximum Likelihood")

par(op)

# spread of the date column
summary(public_benefit_elog$date)

# let me try to change this to focus on a specific year. 
# Subset data for dates in 2019
public_benefit_elog_2019 <- subset(public_benefit_elog, format(date, "%Y") == "2019")
summary(public_benefit_elog_2019)

op <- par(mfrow = c(1, 2))
k.wheat <- estimateRegularity(public_benefit_elog_2019, method = "wheat",
                              plot = TRUE, title = "Wheat & Morrison")

k.mle <- estimateRegularity(public_benefit_elog_2019, method = "mle",
                            plot = TRUE, title = "Maximum Likelihood")

par(op)


# let's instead identify an organization that has both a high number of transactions and a high number of donors
# Step 1: Filter transactions for the year 2019
transaction_data_2019 <- transaction_level_givingtuesday_data %>%
  filter(year == 2019)

# Step 2-4: Group by orgid, calculate transaction and donor counts, and order by both counts
top_orgs <- transaction_data_2019 %>%
  group_by(orgid) %>%
  summarize(num_transactions = n(),
            num_donors = n_distinct(donorid)) %>%
  arrange(desc(num_transactions), desc(num_donors)) %>%
  top_n(5)

# Step 5: Print the top 5 organizations
print(top_orgs)


# now, let's go through these to try and find an org with a pattern that has an exponential underlying view
# first result
org1_attempted_wheat <- transaction_data_2019 %>%
  filter(orgid == "1DC727C6DB187CB9E8A76430D73F4FB1")

org1_attempted_wheat$date <- as.Date(org1_attempted_wheat$date)
str(org1_attempted_wheat)

org1_attempted_wheat <- org1_attempted_wheat %>%
  select(cust = donorid, date, sale = amount)

op <- par(mfrow = c(1, 2))
k.wheat <- estimateRegularity(org1_attempted_wheat, method = "wheat",
                              plot = TRUE, title = "Wheat & Morrison")

k.mle <- estimateRegularity(org1_attempted_wheat, method = "mle",
                            plot = TRUE, title = "Maximum Likelihood")

par(op)
# we have an MLE close to 2, but this still isn't the best

# we will continue. 
event_log_2019 <- org1_attempted_wheat
summary(event_log_2019)

# let's check to see what it looks like if we go ahead and use more of the data from this npo
org1_wheat <- transaction_level_givingtuesday_data %>%
  filter(orgid == "1DC727C6DB187CB9E8A76430D73F4FB1")
org1_wheat$date <- as.Date(org1_wheat$date)
str(org1_wheat)

org1_wheat <- org1_wheat %>%
  select(cust = donorid, date, sale = amount)

op <- par(mfrow = c(1, 2))
k.wheat <- estimateRegularity(org1_wheat, method = "wheat",
                              plot = TRUE, title = "Wheat & Morrison")

k.mle <- estimateRegularity(org1_wheat, method = "mle",
                            plot = TRUE, title = "Maximum Likelihood")

par(op)

# doesn't change much if we use the whole data, so let's do that. 
# Get the date range
date_range <- range(org1_wheat$date)

# Print the result
print(date_range)


event_log <- org1_wheat
# get the CBS

# Calculate the end date of the calibration period (3 months before the last date in 2019)
calibration_end <- calibration_end <- as.Date("2019-10-01")

str(calibration_end)

(elog2cbs(event_log) -> CBS)

params.pnbd <- BTYD::pnbd.EstimateParameters(CBS)
names(params.pnbd) <- c("r", "alpha", "s", "beta")
round(params.pnbd, 3)
