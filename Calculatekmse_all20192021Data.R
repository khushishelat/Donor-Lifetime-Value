library(tidyverse)
library(BTYDplus)
library(dplyr)
library(ggplot2)

transactions_df_all <- read.csv("~/Downloads/fep_sample (1)/transaction_sample.csv")
organisations_df_all <- read.csv("~/Downloads/fep_sample (1)/organizations_sample.csv")

# filter the large database for only transactions belonging to the 2019 - 2021 period. 
transactions_df_all$date <- as.Date(transactions_df_all$date)
filtered_transaction_df <- transactions_df_all %>%
  filter(amount > 0)

# create a function that calculates the k.mse vaue for an organization's data, given its id
calculate_k_mle <- function(org_id, transaction_data) {
  # Select all rows belonging to the specified orgid
  org_specific_data <- transaction_data %>%
    filter(orgid == org_id)
  
  org_specific_data <- org_specific_data %>%
    select(cust = donorid, date, sale = amount)
  
  # Try calculating k.mle, catching any errors
  k.mle <- tryCatch({
    estimateRegularity(org_specific_data, method = "mle", plot = FALSE, title = "Maximum Likelihood")
  }, error = function(e) {
    NA
  })
  
  return(k.mle)
}

plot_wheat <- function(org_id, transaction_data) {
  # Select all rows belonging to the specified orgid
  org_specific_data <- transaction_data %>%
    filter(orgid == org_id)
  
  org_specific_data <- org_specific_data %>%
    select(cust = donorid, date, sale = amount)
  
  op <- par(mfrow = c(1, 2))
  k.wheat <- estimateRegularity(org_specific_data, method = "wheat",
                                plot = TRUE, title = "Wheat & Morrison")
  
  k.mle <- estimateRegularity(org_specific_data, method = "mle",
                              plot = TRUE, title = "Maximum Likelihood")
  
  par(op)
  
  return(list(k.wheat = k.wheat, k.mle = k.mle))
}


create_event_log <- function(org_id, transaction_data, years) {
  # Select all rows belonging to the specified orgid
  org_specific_data <- transaction_data %>%
    filter(orgid == org_id) %>%
    filter(year(date) %in% c(year))
  
  org_specific_data <- org_specific_data %>%
    select(cust = donorid, date, sale = amount)
  
  return(org_specific_data)
}


# Example usage:
# this is the org with the highest number of transactions in the filtered df
org_id <- "1175686FA989329C622F070AB7D922F9"
k_mle_value <- calculate_k_mle(org_id, filtered_transaction_df)
print(k_mle_value)
#[1] 1.969209

# Apply the function to each org_id in filtered_transaction_df
k_mle_table <- filtered_transaction_df %>%
  distinct(orgid) %>%
  mutate(k_mle_result = map(orgid, ~ calculate_k_mle(., filtered_transaction_df))) %>%
  unnest(cols = c(k_mle_result))


# Plot the frequency distribution of rounded k_mle_result values
ggplot(k_mle_table, aes(x = k_mle_result)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Rounded k.mle Values", y = "Frequency", title = "Frequency Distribution of Rounded k.mle Values")

orgs_k_mle_1 <- k_mle_table %>%
  filter(round(k_mle_result) == 1)

# Show the resulting organizations
# because their k_mle is closest to 1, they are most likely to be good pnbd candidates. 
orgs_k_mle_1

# let's learn more about these organizations that have k_mle_1
# Calculate the number of unique transactions and unique donors for each organization in orgs_k_mle_1
orgs_k_mle_1_summary <- filtered_transaction_df %>%
  filter(orgid %in% orgs_k_mle_1$orgid) %>%
  group_by(orgid) %>%
  summarize(num_unique_transactions = n(),
            num_unique_donors = n_distinct(donorid))

# Merge orgs_k_mle_1 with the summary data
orgs_k_mle_1_with_summary <- left_join(orgs_k_mle_1, orgs_k_mle_1_summary, by = c("orgid" = "orgid")) %>%
  mutate(avg_trans_per_donor = num_unique_transactions / num_unique_donors)

# Show the resulting table
orgs_k_mle_1_with_summary <- orgs_k_mle_1_with_summary %>% 
    filter(avg_trans_per_donor > 2)

orgs_k_mle_1_with_summary

# there are only 2 orgs with k.mle close to 1 that have avg_trans_per_donor > 5. 
# let's select the one with the larger number of unique donors. 
# let's also expand the window to see if including 2018, 2019, 2020 and 2021 will help us.
# we can utilize the last 3 months of 2021 as holdout. 

plot_wheat("5DB61AE3432F3F26FCAFEBA698DAA9CD", filtered_transaction_df)
event_log_mle_1 <- create_event_log("2BBAD3E66E7CA6EE7AB94AEA86A6D971", filtered_transaction_df, 2020)
plotTimingPatterns(event_log_mle_1, n = 100, T.cal = "2020-10-01",
                   headers = c("Past", "Future"), title = "")
