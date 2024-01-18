library(tidyverse)
library(lubridate)
library(here)

setwd(here())
dat <- read.csv("data/By Region.csv")

# Create data subsets by regions
data_eoe <- subset(dat, name == "EAST OF ENGLAND COMMISSIONING REGION")
data_ldn <- subset(dat, name == "LONDON COMMISSIONING REGION")
data_mid <- subset(dat, name == "MIDLANDS COMMISSIONING REGION")
data_ney <- subset(dat, name == "NORTH EAST AND YORKSHIRE COMMISSIONING REGION")
data_nw <- subset(dat, name == "NORTH WEST COMMISSIONING REGION")
data_se <- subset(dat, name == "SOUTH EAST COMMISSIONING REGION")
data_sw <- subset(dat, name == "SOUTH WEST COMMISSIONING REGION")


# Extract year and months
# Combine the datasets
combined_data <- bind_rows(
  mutate(data_eoe, Region = "East of England"),
  mutate(data_ldn, Region = "London"),
  mutate(data_mid, Region = "Midlands"),
  mutate(data_ney, Region = "North East"),
  mutate(data_nw, Region = "North West"),
  mutate(data_se, Region = "South East"),
  mutate(data_sw, Region = "South West")
)

# Order the color categories by the highest to lowest value
combined_data$Region <- factor(combined_data$Region, 
                               levels = unique(combined_data$Region[order(combined_data$y_items, decreasing = TRUE)]))


# Create a time series plot using ggplot2
time_series_plot <- ggplot(combined_data, aes(x = date, y = y_items, color = Region, shape = Region)) +
  geom_line() +
  geom_point() + 
  labs(title = paste("Prescription of Bisphosphonates in", 
                     "regions of England in the last 5 years", sep = '\n'),
       x = "Date",
       y = "Prescriptions",
       color = "Region",
       shape = "Region") +
  scale_shape_manual(values = c("Midlands" = 16, 
                                "North East" = 17,
                                "South East" = 18, 
                                "North West" = 19,
                                "London" = 20,
                                "East of England" = 21,
                                "South West"= 22)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Display the plot
print(time_series_plot)


png("output/bisphosphonates_regions_by_time.png")
ggplot(combined_data, aes(x = date, y = y_items, color = Region, shape = Region)) +
  geom_line() +
  geom_point() + 
  labs(title = paste("Prescription of Bisphosphonates in", 
                     "regions of England in the last 5 years", sep = '\n'),
       x = "Date",
       y = "Prescriptions",
       color = "Region",
       shape = "Region") +
  scale_shape_manual(values = c("Midlands" = 16, 
                                "North East" = 17,
                                "South East" = 18, 
                                "North West" = 19,
                                "London" = 20,
                                "East of England" = 21,
                                "South West"= 22)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

dev.off()

