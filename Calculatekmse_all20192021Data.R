library(tidyverse)
library(BTYDplus)
library(dplyr)
library(ggplot2)

transactions_df_all <- read.csv("~/Downloads/fep_sample (1)/transaction_sample.csv")

# filter the large database for only transactions belonging to the 2019 - 2021 period. 
transactions_df_all$date <- as.Date(transactions_df_all$date)
filtered_transaction_df <- transactions_df_all %>%
  filter(year(date) %in% c(2019, 2020, 2021))

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


# Round k_mle_result values to the nearest integer
k_mle_table$k_mle_result <- round(k_mle_table$k_mle_result)

# Plot the frequency distribution of rounded k_mle_result values
ggplot(k_mle_table, aes(x = k_mle_result)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Rounded k.mle Values", y = "Frequency", title = "Frequency Distribution of Rounded k.mle Values")

orgs_k_mle_1 <- k_mle_table %>%
  filter(k_mle_result == 1)

# Show the resulting organizations
# because their k_mle is closest to 1, they are most likely to be good pnbd candidates. 
orgs_k_mle_1
