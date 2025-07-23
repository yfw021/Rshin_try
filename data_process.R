# Load the necessary libraries
# install.packages("tidyverse")
library(tidyverse)

# Create a sample data frame with extra columns
your_data_frame <- tibble(
  L_R_Summary = c('1.1 a', '1.2 b', '1.1 a', '1.3 d', '2.1 y', '2.2 o', '1.1 a'),
  Years = c(2026, 2026, 2027, 2027, 2027, 2028, 2028),
  TX_LR_sum = c(100, 150, 110, 200, 250, 300, 125),
  First_year = c(2025, 2025, 2026, 2026, 2026, 2027, 2027),
  Last_year = c(2030, 2030, 2031, 2031, 2032, 2032, 2032)
)

final_table <- your_data_frame %>%
  # Step 1: Select only the columns you need
  select(L_R_Summary, Years, TX_LR_sum) %>%

  # Step 2: Create the 'LR_ID' column
  mutate(LR_ID = str_extract(L_R_Summary, "\\d+\\.\\d+")) %>%

  # Step 3: Reshape the data
  pivot_wider(
    id_cols = LR_ID,
    names_from = Years,
    values_from = TX_LR_sum
  )

# Print the final table
print(final_table)

final_table_summed <- your_data_frame %>%
  # Step 1: Create the 'LR_ID' column
  mutate(LR_ID = str_extract(L_R_Summary, "\\d+\\.\\d+")) %>%

  # Step 2: Group by the ID and the Year
  group_by(LR_ID, Years) %>%

  # Step 3: Calculate the sum for each group and name it TX_Code_sum
  summarise(TX_Code_sum = sum(TX_LR_sum, na.rm = TRUE), .groups = 'drop') %>%

  # Step 4: Reshape the summarized data into the final table
  pivot_wider(
    id_cols = LR_ID,
    names_from = Years,
    values_from = TX_Code_sum
  )

# Print the final table with the summed values
print(final_table_summed)



# Compare the two data frames after rounding all numeric columns to 0 decimal places
compare(
  table_original %>%
    select(-LR_ID) %>%
    mutate(across(where(is.numeric), ~round(.x, digits = 0))),

  table_new %>%
    select(-LR_ID) %>%
    mutate(across(where(is.numeric), ~round(.x, digits = 0)))
)



long_data <- wide_data %>%
  pivot_longer(
    cols = -`generation resources`, # Gathers all columns EXCEPT 'generation resources'
    names_to = "Years",             # Creates the new 'Years' column from the old column names
    values_to = "Value"             # Creates a 'Value' column to hold the cell values
  ) %>%
  mutate(Years = as.integer(Years)) # (Optional but good practice) Converts the Years column to a number

# Print the new long-format table



# Step 1: Ensure the column exists in df2.
# If it doesn't exist, this line adds it and fills it with NA.
# If it already exists, this line won't cause any problems.
if (!"projected_capacity" %in% names(df2)) {
  df2$projected_capacity <- NA_real_
}

# Step 2: Update df2 with values from df1 based on the keys
updated_df2 <- rows_update(
  df2,
  df1,
  by = c("Generation_Resources", "Years")
)

# Print a sample of the updated table
print(head(updated_df2, 10))



# --- Step 2: Create the summary table ---

# Group by year and type, sum the capacity, then pivot to the desired format
capacity_summary_table <- df %>%
  # Group by the columns we want to aggregate
  group_by(Years, type) %>%
  # Calculate the total capacity for each group
  summarise(total_capacity = sum(projected_capacity), .groups = 'drop') %>%
  # Pivot the data to a wide format
  pivot_wider(
    names_from = Years,
    values_from = total_capacity,
    values_fill = 0 # If a type has no capacity in a year, show 0
  )

# --- Step 3: Display the final table ---
print(capacity_summary_table)

