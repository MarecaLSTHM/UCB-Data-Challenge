# I am adding a comment to test if I can commit

install.packages("usethis")

library(usethis)

usethis::use_git_config(
  user.name = "MarecaLSTHM",
  user.email = "marecasitholeuk2@gmail.com",
  github.user = "MarecaLSTHM")

# Another change done now


# Testing 123!

#leon was testing this 

# load dataset 
df <- read.csv('items for bisphosphonates and other drugs per.csv')

# Load package 
library(tidyverse)

# Stratification 
count(df, name) # 107 areas, each with 60 observations (dates)

# Subset the data
sub_df <- df %>%
  select(name, y_items, date, id)

# Long data to wide data
sub_df_wide <- sub_df %>%
  pivot_wider(names_from = date, values_from = y_items) %>%
  arrange(name)

# Test Plotting
options(repr.plot.width = 8, repr.plot.height = 4)
Test_plot_1 <- ggplot(sub_df, aes(x = as.Date(date), y = y_items, color = id)) +
  geom_line() +
  labs(title = "Values Over Time by Location",
       x = "Date",
       y = "Y Items") +
  theme_minimal()
Test_plot_1
dev.off() # change the plot setting back to normal
# Add a greater region separation to see the difference. 