library(tidyverse)
library(tidyr)

nonprofit_data <- read.csv("/Users/khushishelat/Downloads/nonprofitdata.csv")
original_nonprofit_data <- read.csv("/Users/khushishelat/Downloads/nonprofitdata.csv")
summary(nonprofit_data)
head(nonprofit_data)

# Display the type of the DATEFST column
class(nonprofit_data$DATEFST)

# Filter out rows where DATEFST is 0
nonprofit_data <- nonprofit_data[nonprofit_data$DATEFST != 0, ]

# Extract year from DATEFST (assuming DATEFST is already stored as integer)
years <- nonprofit_data$DATEFST %/% 100 + 1900

# Plot histogram of years
ggplot(data.frame(year = years), aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Year of First Transaction", y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("/Users/khushishelat/Donor Lifetime Value/year_first_transaction_histogram.png")

# Extract month from DATEFST (assuming DATEFST is already stored as integer)
months <- nonprofit_data$DATEFST %% 100

# Plot histogram of months
ggplot(data.frame(month = months), aes(x = factor(month))) +
  geom_bar(fill = "skyblue", color = "black") +
  scale_x_discrete(labels = month.name) +
  labs(x = "Month of First Transaction", y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("/Users/khushishelat/Donor Lifetime Value/month_first_transaction_histogram.png")


# Check if the sum of CNDOL1 to CNDOL10 equals CNTRLIF
sum_cndol <- rowSums(nonprofit_data[, c("CNDOL1", "CNDOL2", "CNDOL3", "CNDOL4", "CNDOL5", "CNDOL6", "CNDOL7", "CNDOL8", "CNDOL9", "CNDOL10")], na.rm = TRUE)
equal_cntrlif <- sum_cndol == nonprofit_data$CNTRLIF

# Print the result
cat("All rows where the sum of CNDOL1 to CNDOL10 equals CNTRLIF:", all(equal_cntrlif), "\n")


# Calculate the number of discrepant rows
num_discrepant_rows <- sum(sum_cndol != nonprofit_data$CNTRLIF)

# Print the number of discrepant rows
cat("Number of rows where the sum of CNDOL1 to CNDOL10 does not equal CNTRLIF:", num_discrepant_rows, "\n")

# Filter rows where the sum of the 10 transactions equals CNTRLIF
nonprofit_data <- nonprofit_data[sum_cndol == nonprofit_data$CNTRLIF, ]

# Print the number of remaining rows
cat("Number of rows remaining after filtering:", nrow(nonprofit_data), "\n")

nonprofit_data <- original_nonprofit_data

# Count the number of unique ACCNTNMB values
unique_count <- nonprofit_data %>% 
  summarise(unique_count = n_distinct(ACCNTNMB))

print(unique_count)

# we're working with a group of 99K customers. Let's filter this.
#first, we will filter for customers for whom 10 donations is the full extent of all their donations. 
filtered_nonprofit_data <- nonprofit_data

filtered_nonprofit_data <- filtered_nonprofit_data %>%
  mutate(sum_last_10_donations = rowSums(select(., starts_with("CNDOL"))))

filtered_nonprofit_data <- filtered_nonprofit_data %>%
  filter(CNTMLIF <= 10)

new_row_count <- nrow(filtered_nonprofit_data)
print(new_row_count)

# we are left with 87600 donors. 
# next, a column we need is the value for earliest donation. Remove all rows for which this column is null. 
# Filter for rows with non-null values in DATEFST column
filtered_nonprofit_data <- filtered_nonprofit_data %>%
  filter(!is.na(DATEFST))

new_row_count <- nrow(filtered_nonprofit_data)
print(new_row_count)
# the number of rows remains the same -- this is good because it indicates that we have a cleaner dataset. 

# now, we want to rotate the data to be in the format we require it. 
# rather than one row for each donor, we want an event log, one row for each donation
# each row should include information for account number, date, and value of the donation.

#first, we save the target variables
target_values <- filtered_nonprofit_data %>%
  select(ACCNTNMB, TARGDOL, TARGRESP)
write.csv(target_values, file = "target_values.csv", row.names = FALSE)

# next, we extract the needed columns for all rows 
donation_columns <- filtered_nonprofit_data %>%
  select(contains("CNDOL"), contains("CNDAT"))




data <- filtered_nonprofit_data

# Make data long format and create sales column
longdata <- gather(data, key = "transaction", value = "sales", CNDOL1:CNDOL10)

# Remove unnecessary characters from transaction column
longdata$transaction <- gsub("CNDOL", "", longdata$transaction)
longdata$transaction <- as.character(longdata$transaction)

# Ensure CNDAT1 to CNDAT10 columns are character type
longdata[, paste0("CNDAT", 1:10)] <- lapply(longdata[, paste0("CNDAT", 1:10)], as.character)

# Create date column
longdata <- mutate(longdata, 
                   date = case_when(
                     transaction == "1" ~ CNDAT1,
                     transaction == "2" ~ CNDAT2,
                     transaction == "3" ~ CNDAT3,
                     transaction == "4" ~ CNDAT4,
                     transaction == "5" ~ CNDAT5,
                     transaction == "6" ~ CNDAT6,
                     transaction == "7" ~ CNDAT7,
                     transaction == "8" ~ CNDAT8,
                     transaction == "9" ~ CNDAT9,
                     transaction == "10" ~ CNDAT10,
                     TRUE ~ NA_character_)
)

# Subset to only needed columns
longdata_short <- select(longdata, ACCNTNMB, date, sales)

# Rename ID column for consistency
longdata_short <- rename(longdata_short, cust = ACCNTNMB)

# Order longdata_short by cust
longdata_short <- longdata_short[order(longdata_short$cust), ]

# Order filtered_nonprofit_data by ACCNTNMB
filtered_nonprofit_data <- filtered_nonprofit_data[order(filtered_nonprofit_data$ACCNTNMB), ]

#we checked the head of both datasets to ensure the event log was correctly created

# next, we remove all rows for which date is null 
longdata_short <- filter(longdata_short, !is.na(date))

# Check the number of unique ACCNTNMB
num_unique_accntnmb <- longdata_short %>% 
  summarise(num_unique = n_distinct(cust))

print(num_unique_accntnmb)
# good, we haven't removed any customers. 

# Convert date column to POSIXt with first day of the month
longdata_short$date <- as.POSIXct(paste0(longdata_short$date, "01"), format = "%y%m%d")

# Print the structure of longdata_short to confirm the conversion
str(longdata_short)

longdata_short <- longdata_short %>%
  select(date, everything())

# let's use the first 75% of our data as our training set 
# the second 25% of the data (i.e. 1/4 of transactions) as our validation set
# we can't use the given target very easily because there's a gap from June to October that we have no view on
# Sort longdata_short by date in descending order
longdata_short <- longdata_short[order(longdata_short$date, decreasing = TRUE), ]

# Calculate the number of rows for testing dataset (25%)
num_testing_rows <- round(0.25 * nrow(longdata_short))

# Select the most recent 25% of rows for testing dataset
testing_data <- longdata_short[1:num_testing_rows, ]

# Select the remaining rows for training dataset
training_data <- longdata_short[-(1:num_testing_rows), ]

# Print the number of rows in training and testing datasets
cat("Number of rows in training dataset:", nrow(training_data), "\n")
cat("Number of rows in testing dataset:", nrow(testing_data), "\n")

# Define the cutoff date for December 1994
cutoff_date <- as.POSIXct("1994-12-01")

# Identify rows in the training dataset with dates in December 1994
december_1994_rows <- training_data$date >= cutoff_date

# Move the identified rows to the testing dataset
testing_data <- rbind(testing_data, training_data[december_1994_rows, ])

# Remove the moved rows from the training dataset
training_data <- training_data[!december_1994_rows, ]

head(training_data)
tail(training_data)
head(testing_data)
tail(testing_data)

write.csv(testing_data, file = "testing_transactions.csv", row.names = FALSE)
write.csv(training_data, file = "training_transactions.csv", row.names = FALSE)
