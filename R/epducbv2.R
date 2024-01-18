library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(kableExtra)
here()
setwd(here())


#data11=biphosphonates data from open prescribing
data11 <- read_csv("data/data11.csv")


#select needed columns
data11 <- data11%>%
              select(date, name,y_items, y_actual_cost)

#Date object
data11$date <- as.Date(data11$date, format = "%y%m%d")


# Plotting the time series for the entire country(1st try, not much inference)
ggplot(data11, aes(x = date, y = y_items, group = 1)) +
  geom_line() +
  labs(title = paste("Prescription trends the",
                     "last 5 years", 
                     sep = "\n"),
       x = "Year",
       y = "Prescriptions") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

#Group total number of items per year
prescription_data_grouped <- data11 %>%
  group_by(Year = lubridate::year(date)) %>%
  summarize(Total_Prescriptions = sum(y_items))


# Print the grouped data
print(prescription_data_grouped)

# Plotting the time series for the entire country(modified)
ggplot(prescription_data_grouped, aes(x = Year, y = Total_Prescriptions, group = 1)) +
  geom_line() +
  geom_point(shape = 2) +
  geom_text(aes(label = paste("(",Year, ";", Total_Prescriptions,")")), vjust = -0.5, hjust = 0.5) + # Add text labels
  labs(title = paste("Osteoporosis medicatication prescription trends over", "the last 5 years in England", sep="\n"),
       x = "Year",
       y = "Prescriptions") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title and remove background


# We can have this as a bar graph
ggplot(prescription_data_grouped, aes(y = Total_Prescriptions, x = factor(Year) )) +
  geom_bar(stat = "identity", fill = "red", width = 0.9) +  # Adjust fill color and width as needed
  geom_text(aes(label = paste(Total_Prescriptions, sep = "")), 
            vjust = -0.5, hjust = 0.5, color = "black", size = 3) +  # Add text labels with adjustments
  labs(title = paste("Osteoporosis medication prescription trends over", 
                     "the last 5 years in England", 
                     sep = "\n"),
       x = "Year",
       y = "Prescriptions") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))  # Remove grid lines and set background to white


# Horizontal bar graph
ggplot(prescription_data_grouped, aes(x = Total_Prescriptions, y = factor(Year))) +
  geom_bar(stat = "identity", fill = "maroon", height = 0.7) +
  geom_text(aes(label = paste("(", Total_Prescriptions, ";", Year, ")", sep = "")), 
            vjust = 0.5, hjust = -0.2, color = "black", size = 3) +
  labs(title = paste("Osteoporosis medication prescription trends over", "the last 5 years in England", sep = "\n"),
       x = "Prescriptions",
       y = "Year") +
  scale_x_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 1))  # Adjust y-axis text position if needed























