library(tidyverse)
library(dplyr)
library(ggplot2)

# the purpose of this file is to demonstrate an RFM segmentation of the training data transactions
# we will create a plot as suggested by Harrison Tietze (2008) to visually segment customers

training_data <- read.csv("/Users/khushishelat/Donor Lifetime Value/training_transactions.csv")
head(training_data)
summary(training_data)

# convert this table into an RFM table by grouping by cust
# Group data by 'cust' and calculate recency, frequency, and monetary value
summary_table <- training_data %>%
  group_by(cust) %>%
  summarise(
    recency = max(date),                  # Most recent transaction date
    frequency = n(),                      # Number of transactions
    monetary_value = mean(sales)          # Average transaction value
  )

# View the summary table
RFM_table <- summary_table


# Calculate quintiles for recency, frequency, and monetary_value
summary_table <- summary_table %>%
  mutate(
    recency_quintile = ntile(recency, 5),                  # Quintiles for recency
    frequency_quintile = ntile(frequency, 5),              # Quintiles for frequency
    monetary_value_quintile = ntile(monetary_value, 5)     # Quintiles for monetary value
  )


# Calculate average monetary value for each segment
segment_avg_monetary_value <- summary_table %>%
  group_by(recency_quintile, frequency_quintile) %>%
  summarise(avg_monetary_value = mean(monetary_value))

# Find the segment with the highest average monetary value
max_avg_monetary_value <- max(segment_avg_monetary_value$avg_monetary_value)
max_avg_monetary_segment <- segment_avg_monetary_value[segment_avg_monetary_value$avg_monetary_value == max_avg_monetary_value, ]

# Create the heatmap plot
heatmap_plot <- ggplot(segment_avg_monetary_value, aes(x = recency_quintile, y = frequency_quintile, fill = avg_monetary_value)) +
  geom_tile() +
  scale_fill_gradient(name = "Average\nMonetary Value", low = "lightblue", high = "darkblue", limits = c(0, max_avg_monetary_value)) +
  geom_text(data = max_avg_monetary_segment, aes(label = "Highest\nAvg Value"), color = "white") +
  labs(x = "Recency Quintile", y = "Frequency Quintile", title = "Heatmap of Recency vs. Frequency with Average Monetary Value") +
  theme_minimal()

# Print the heatmap plot
print(heatmap_plot)

head(summary_table)

ggsave("/Users/khushishelat/Donor Lifetime Value/heatmap_rfm.png", plot = heatmap_plot, width = 8, height = 6, dpi = 300)

ggplot(RFM_table, aes(x = frequency)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Frequency",
       x = "Frequency",
       y = "Frequency Count") +
  theme_minimal()
