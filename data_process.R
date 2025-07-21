# Load the necessary libraries
# install.packages("tidyverse")
library(tidyverse)

# Create a sample data frame
your_data_frame <- tibble(
  L_R_Summary = c('1.1 a', '1.2 b', '1.1 a', '1.3 d', '2.1 y', '2.2 o', '1.1 a'),
  Years = c(2026, 2026, 2027, 2027, 2027, 2028, 2028),
  TX_LR_sum = c(100, 150, 110, 200, 250, 300, 125)
)

final_table <- your_data_frame %>%
  # Step 1: Create a new column 'LR_ID' by extracting the number from L_R_Summary
  mutate(LR_ID = str_extract(L_R_Summary, "\\d+\\.\\d+")) %>%
  
  # Step 2: Reshape the data from long to wide format
  pivot_wider(
    id_cols = LR_ID,                  # These will become the new rows
    names_from = Years,               # These will become the new columns
    values_from = TX_LR_sum           # These will be the values in the table
  )

# Print the final table
print(final_table)
