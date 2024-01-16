library(tidyverse)
install.packages("sf")
library(sf)
#data11=biphosphonates data from open prescribing
data11 <- read_csv("data11.csv")
#Number of regions
select(data11,name) %>% unique %>% nrow
#head/tail of data
head(data11)
tail(data11)

#select needed columns
data11 <- data11%>%
  select(date, name,y_items, y_actual_cost)
#may calculate monthly cost per item and compare to income(later)

#Date object
data11$date <- as.Date(data11$date, format = "%y%m%d")

# Plotting the time series for the entire country(1st try, not much inference)
ggplot(data11, aes(x = date, y = y_items_scaled, group = 1)) +
  geom_line() +
  labs(title = "Prescription Trends Over Time",
       x = "Date",
       y = "Number of Prescriptions(1000s)") +
  theme_minimal()

#group total number of items per year
prescription_data_grouped <- data11 %>%
  group_by(Year = lubridate::year(date)) %>%
  summarize(Total_Prescriptions = sum(y_items))

# Print the grouped data
print(prescription_data_grouped)

# Plotting the time series for the entire country(modified)
ggplot(prescription_data_grouped, aes(x = Year, y = Total_Prescriptions, group = 1)) +
  geom_line() +
  labs(title = "Prescription Trends Over Time",
       x = "Year",
       y = "Number of Prescriptions") +
  theme_minimal()
#Note:information shown just for bisphosphonates
#Trend explainable as 2018 has only data for two months, but generally, drug prescription can be seen to be on a steady decline from peak(2019)
#Comparatively, another plot should be done including all medications
#max 2-3 plots


#Shape file from ONS
regions <- st_read("NHSER_APR_2021_EN_BFC.shp")
#arrange alphabetically
regions <- regions%>% arrange(NHSER21NM)
#auto assign region_id(primary key for future merging)
regions <- regions %>%
  mutate(region_id = row_number())
#total prescriptions per region
total_prescriptions_by_region <- data11 %>%
  group_by(name) %>%
  summarise(total_prescriptions = sum(y_items)) 
#arrange alphabetically to match ONS format
total_prescriptions_by_region <- total_prescriptions_by_region%>% arrange(name) 
#auto assign region_id to match ONS
total_prescriptions_by_region <- total_prescriptions_by_region %>%
  mutate(region_id = row_number())

#merge ONS shapefile and our dataset
merged_data <- inner_join(regions, total_prescriptions_by_region, by = "region_id")
#Note this map not so important for Q1, just showing how to merge shapefiles and our transformed data
#Alternatively it can also show us total prescriptions and how it varies by region
#Simulation done only for bisphosphonates
#All medications can be computed as total and graphically represented
# Plotting the map
ggplot(merged_data) +
  geom_sf(aes(fill = total_prescriptions), color = "white", lwd = 0.1) +
  scale_fill_gradient(low = "orange", high = "red", name = "Total Prescriptions") +
  labs(title = "Bisphosphonate 5 years prescription") +
  theme_minimal() +
  theme(axis.text = element_blank(),   
        axis.ticks = element_blank())
#We can add labels on the regions too for proper identification