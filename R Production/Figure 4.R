library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(scales)
library(RColorBrewer)

here()
setwd(here())


#data11=biphosphonates data from open prescribing
data11 <- read_csv("data/data11.csv")

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


#Group total number of items per year
prescription_data_grouped <- data11 %>%
  group_by(Year = lubridate::year(date)) %>%
  summarize(Total_Prescriptions = sum(y_items))

# Print the grouped data
print(prescription_data_grouped)

# Data transformations
region_pop <- read.csv('data/Regional_pop.csv')
region_pop_copy<-region_pop
new_row<-region_pop_copy[region_pop_copy$Age %in% "90+", ]
new_row$Age <- "90"
region_pop_copy <- rbind(region_pop_copy, new_row)
region_pop_copy$Age<-as.numeric(region_pop_copy$Age)
region_pop_copy <- region_pop_copy[region_pop_copy$Age>50,]
region_pop_copy$patient_no<-region_pop_copy$Males*0.067 +region_pop_copy$Females* 0.219 
region_pop_copy <- region_pop_copy %>% group_by(Region) %>% summarise(sum = sum(patient_no))
region_pop_copy$sum <- as.numeric(region_pop_copy$sum)

new_row <- region_pop_copy[region_pop_copy$Region %in% c("Yorkshire & the Humber", "North East"), ]
new_row$sum <- sum(new_row$sum)
new_row$Region <- "NORTH EAST AND YORKSHIRE COMMISSIONING REGION"
region_pop_copy <- rbind(region_pop_copy, new_row)

new_row <- region_pop_copy[region_pop_copy$Region %in% c("East Midlands", "West Midlands"), ]
new_row$sum <- sum(new_row$sum)
new_row$Region <- "MIDLANDS COMMISSIONING REGION"
region_pop_copy <- rbind(region_pop_copy, new_row)

region_pop_copy$Region[region_pop_copy$Region == "East"] <- "EAST OF ENGLAND COMMISSIONING REGION"
region_pop_copy$Region[region_pop_copy$Region == "London"] <- "LONDON COMMISSIONING REGION"
region_pop_copy$Region[region_pop_copy$Region == "North West"] <- "NORTH WEST COMMISSIONING REGION"
region_pop_copy$Region[region_pop_copy$Region == "South East"] <- "SOUTH EAST COMMISSIONING REGION"
region_pop_copy$Region[region_pop_copy$Region == "South West"] <- "SOUTH WEST COMMISSIONING REGION"

region_pop_copy$name<-region_pop_copy$Region


#Shape file from ONS
regions <- st_read("data/NHSER_APR_2021_EN_BFC.shp")

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
region_id <- total_prescriptions_by_region[,c("name","region_id")]
#merge ONS shapefile and our dataset
total_prescriptions_by_region<- inner_join(total_prescriptions_by_region, region_pop_copy, by="name")
total_prescriptions_by_region$per_capita<- total_prescriptions_by_region$total_prescriptions/total_prescriptions_by_region$sum/5
merged_data <- inner_join(regions, total_prescriptions_by_region, by = "region_id")

# Plot the total prescriptions of bisphosphonates per region
# png(filename = "output/bisphosphates_total_by_UK_region.png", width = 800, height = 600, units = "px", pointsize = 12)
ggplot(merged_data) +
  geom_sf(aes(fill = total_prescriptions), 
          color = "white", 
          lwd = 0.1) +
  #scale_fill_gradient(low = "orange", high = "red", name = "Total prescriptions") +
  scale_fill_viridis_c(name = 'Total prescriptions') +
  # labs(title = "Bisphosphonates prescriptions over last 5 years") +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),   
        axis.ticks = element_blank()) 
# dev.off()

