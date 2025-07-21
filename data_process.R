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



