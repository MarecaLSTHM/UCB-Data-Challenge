library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(scales)
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

# Plotting the time series for the entire country(1st try, not much inference)
ggplot(data11, aes(x = date, y = y_items, group = 1)) +
  geom_line() +
  labs(title = "Prescription trends over time",
       x = "Year",
       y = "Prescriptions") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
  theme_minimal()

##2nd this is a bit sus at the very le

#Group total number of items per year
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

#Note this map not so important for Q1, just showing how to merge shapefiles and our transformed data
#Alternatively it can also show us total prescriptions and how it varies by region
#Simulation done only for bisphosphonates
#All medications can be computed as total and graphically represented
# Plotting the map
png("output/yearly_bisphosphonate_prescriptions_per_patients.png")
ggplot(merged_data) +
  geom_sf(aes(fill = per_capita), 
          color = "white", 
          lwd = 0.1) +
  scale_fill_gradient(low = "orange", 
                      high = "red", 
                      name = "Total prescriptions") +
  labs(title = "Yearly Bisphosphonates Prescriptions Per Patients") +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),   
        axis.ticks = element_blank())
dev.off()

png(filename = "output/bisphosphates_total_by_UK_region.png", width = 800, height = 600, units = "px", pointsize = 12)
ggplot(merged_data) +
  geom_sf(aes(fill = total_prescriptions), 
          color = "white", 
          lwd = 0.1) +
  scale_fill_gradient(low = "orange", 
                      high = "red", 
                      name = "Total prescriptions") +
  labs(title = "Bisphosphonates prescriptions over last 5 years") +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),   
        axis.ticks = element_blank()) 
dev.off()

#We can add labels on the regions too for proper identification

df_alendronic_acid=read.csv('data/alendronic_acid.csv')
df_alendronic_acid <- df_alendronic_acid[, c(1,2,3,4)]
names(df_alendronic_acid)[names(df_alendronic_acid) == "y_items"] <- "alendronic_acid"

df_risedronate=read.csv('data/risedronate.csv')
df_risedronate <- df_risedronate[, c(1,2,3,4)]
names(df_risedronate)[names(df_risedronate) == "y_items"] <- "risedronate"


df_etidronate=read.csv('data/etidronate.csv')
df_etidronate <- df_etidronate[, c(1,2,3,4)]
names(df_etidronate)[names(df_etidronate) == "y_items"] <- "etidronate"


df_ibandronic_acid=read.csv('data/ibandronic_acid.csv')
df_ibandronic_acid <- df_ibandronic_acid[, c(1,2,3,4)]
names(df_ibandronic_acid)[names(df_ibandronic_acid) == "y_items"] <- "ibandronic_acid"



df_clodronate=read.csv('data/clodronate.csv')
df_clodronate <- df_clodronate[, c(1,2,3,4)]
names(df_clodronate)[names(df_clodronate) == "y_items"] <- "clodronate"



df_strontium=read.csv('data/strontium.csv')
df_strontium <- df_strontium[, c(1,2,3,4)]
names(df_strontium)[names(df_strontium) == "y_items"] <- "strontium"


df_all_bisphosphates=inner_join(df_alendronic_acid,df_risedronate, by=c("date","name","id"))
df_all_bisphosphates=inner_join(df_all_bisphosphates,df_etidronate, by=c("date","name","id"))
df_all_bisphosphates=inner_join(df_all_bisphosphates,df_ibandronic_acid, by=c("date","name","id"))
df_all_bisphosphates=inner_join(df_all_bisphosphates,df_clodronate, by=c("date","name","id"))
df_all_bisphosphates=inner_join(df_all_bisphosphates,df_strontium, by=c("date","name","id"))
df_all_bisphosphates$date=as.Date(df_all_bisphosphates$date)
df_all_bisphosphates_by_geography<-df_all_bisphosphates %>% group_by(id) %>% summarise(
  strontium = sum(strontium),
  clodronate = sum(clodronate),
  ibandronic_acid = sum(ibandronic_acid),
  etidronate = sum(etidronate),
  risedronate = sum(risedronate),
  alendronic_acid = sum(alendronic_acid),
)
df_all_bisphosphates_by_geography
write.csv(df_all_bisphosphates_by_geography,"output/df_all_bisphosphates_without_geographyy.csv")

df_all_bisphosphates_without_geography<-df_all_bisphosphates %>% group_by(date) %>% summarise(
  strontium = sum(strontium),
  clodronate = sum(clodronate),
  ibandronic_acid = sum(ibandronic_acid),
  etidronate = sum(etidronate),
  risedronate = sum(risedronate),
  alendronic_acid = sum(alendronic_acid),
)

df_all_bisphosphates_without_geography

write.csv(df_all_bisphosphates_without_geography,"output/df_all_bisphosphates_without_geographyy.csv")
df_all_bisphosphates_without_geography <- tidyr::gather(df_all_bisphosphates_without_geography, key = "drug", value = "count", -date)
png(filename="output/bisphosphonates_types.png")
ggplot(df_all_bisphosphates_without_geography, aes(x = date, y = count, color = drug)) +
  geom_line() +
  geom_point() +
  labs(title = "Prescriptions for different bisphosphonates",
       x = "Year",
       y = "Number of Prescriptions",
       color = "Bisphosphate type") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw()+
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold 
dev.off()


df_BIS=read.csv('data/BISPHOSPHATES_NHS_REGIONS.csv')
df_BIS <- df_BIS[, c(1,2,3,4)]
names(df_BIS)[names(df_BIS) == "y_items"] <- "BIS"

df_CA=read.csv('data/CA+VIT_D_NHS REGIONS.csv')
df_CA <- df_CA[, c(1,2,3,4)]
names(df_CA)[names(df_CA) == "y_items"] <- "CA"

df_PTH=read.csv("data/Calcitonins_PTH_NHS_REGIONS.csv")
df_PTH <- df_PTH[, c(1,2,3,4)]
names(df_PTH)[names(df_PTH) == "y_items"] <- "PTH"

df_deno=read.csv("data/deno_NHS_REGIONS.csv")
df_deno <- df_deno[, c(1,2,3,4)]
names(df_deno)[names(df_deno) == "y_items"] <- "deno"

df_drug=inner_join(df_BIS,df_CA, by=c("date","name","id"))
df_drug=inner_join(df_drug,df_deno, by=c("date","name","id"))
df_drug=inner_join(df_drug,df_PTH, by=c("date","name","id"))
df_drug$date<-as.Date(df_drug$date)
df_drug_by_geography<-df_drug %>% group_by(name) %>% summarise(
  BIS = sum(BIS),
  CA = sum(CA),
  deno = sum(deno),
  PTH = sum(PTH)
)
write.csv(df_drug_by_geography,"output/df_drug_by_geography.csv")

df_drug_date<-df_drug %>% group_by(date) %>% summarise(
  BIS = sum(BIS),
  CA = sum(CA),
  deno = sum(deno),
  PTH = sum(PTH)
)

df_drug_date_deno<-df_drug_date[, c("date", "deno")]

png("output/denosumab_trend.png")
ggplot(df_drug_date_deno, aes(x = date, y = deno)) +
  geom_line() +
  geom_point() +
  labs(title = "Presciptions of Denosumab last 5 years.",
       x = "Year",
       y = "Number of prescriptions",
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  theme_bw()+
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold 
dev.off()



png(filename = "output/df_drugs_with_time_without_geography.png")

# Plot Prescription Trends from 2019-2023
library(ggthemes)
colnames(df_drug_date) <- c('Date', 'Bisphosphonates', 'Calcium', 'Denosumab', 'Parathyroid Hormones & Analogues')
library(data.table)
df_drug_date_long <- melt(setDT(df_drug_date), id.vars = c("Date"), variable.name = 'Medication')
colnames(df_drug_date_long) <- c('Year', 'Medication', 'Values')
trend_plot <- ggplot(df_drug_date_long, aes(x = Year, y = Values)) + 
  geom_line(aes(colour = Medication, group = Medication))+
  theme_bw()+
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold 

print(trend_plot+labs(y= "Number of Prescriptions"))
dev.off()

df_selected<-select(df_drug_date, date, deno,PTH)

png(filename = "output/df_little_drugs_with_time_without_geography.png")

# Denosumab and Parathyroid Hormones & Analogues Trend Plot
df_drug_long_sub <- subset(df_drug_date_long, Medication == 'Denosumab' | Medication == 'Parathyroid Hormones & Analogues')
trend_plot <- ggplot(df_drug_long_sub, aes(x = Year, y = Values)) + 
  geom_line(aes(colour = Medication, group = Medication))+
  theme_bw()+
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold 
print(trend_plot+labs(y= "Number of Prescriptions"))

dev.off()

df_deno=read.csv("data/deno_NHS_REGIONS.csv")
df_deno <- df_deno[, c(1,2,3,4)]
names(df_deno)[names(df_deno) == "y_items"] <- "deno"

df_deno<- df_deno %>% group_by(name) %>% summarise(sum=sum(deno))
df_deno <- inner_join(df_deno, region_id, by = "name")
df_deno
df_deno<-inner_join(df_deno,region_pop_copy,by="name")
df_deno$per_capita<-df_deno$sum.x/df_deno$sum.y
df_deno<-inner_join(regions, df_deno,by="region_id")

sf_deno <- st_as_sf(df_deno, coords = c("LONG", "LAT"), crs = 4326)



png(file="output/denosumab prescription.png")
ggplot(sf_deno) +
  geom_sf(aes(fill = per_capita), 
          color = "white", 
          lwd = 0.1) +
  scale_fill_gradient(low = "orange", 
                      high = "red", 
                      name = "Denosumab prescriptions") +
  labs(title = "Yearly Denosumab prescriptions per pateint") +
  theme(axis.text = element_blank(),   
        axis.ticks = element_blank(), 
        panel.grid = element_blank())
dev.off()
###yoooo i also did it 


# Regional Level 
# Load Dataset
region_pop <- read.csv('data/Regional_pop.csv')

## 1. East
case_east_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'East' & region_pop$Age >= 50])
case_east_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'East' & region_pop$Age >= 50])
total_case_east <- case_east_F + case_east_M 
total_pop_east_F <- sum(region_pop$Females[region_pop$Region == 'East'])
total_pop_east_M <- sum(region_pop$Males[region_pop$Region == 'East'])  
total_pop_east <- total_pop_east_F + total_pop_east_M
east_F_pc <- round(case_east_F/total_pop_east_F, 2)
east_M_pc <- round(case_east_M/total_pop_east_M, 2)

## 2. London 
case_london_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'London' & region_pop$Age >= 50])
case_london_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'London' & region_pop$Age >= 50]) 
total_case_london <- case_london_F + case_london_M 
total_pop_london_F <- sum(region_pop$Females[region_pop$Region == 'London'])
total_pop_london_M <- sum(region_pop$Males[region_pop$Region == 'London'])  
total_pop_london <- total_pop_london_F + total_pop_london_M
london_F_pc <- round(case_london_F/total_pop_london_F, 2)
london_M_pc <- round(case_london_M/total_pop_london_M, 2)

## 3. Midlands (East AND West Midlands)
case_midlands_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'East Midlands' & region_pop$Age >= 50]) + 
  0.219 * sum(region_pop$Females[region_pop$Region == 'West Midlands' & region_pop$Age >= 50])
case_midlands_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'East Midlands' & region_pop$Age >= 50]) + 
  0.067 * sum(region_pop$Males[region_pop$Region == 'West Midlands' & region_pop$Age >= 50])
total_case_midlands <- case_midlands_F + case_midlands_M
total_pop_mid_F <- sum(region_pop$Females[region_pop$Region == 'East Midlands']) + sum(region_pop$Females[region_pop$Region == 'West Midlands'])
total_pop_mid_M <- sum(region_pop$Males[region_pop$Region == 'East Midlands'])  + sum(region_pop$Males[region_pop$Region == 'West Midlands'])
total_pop_mid <- total_pop_mid_F + total_pop_mid_M
mid_F_pc <- round(case_midlands_F/total_pop_mid_F, 2)
mid_M_pc <- round(case_midlands_M/total_pop_mid_M, 2)

## 4. North East AND Yorkshire
case_northeast_yorkshire_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'Yorkshire & the Humber' & region_pop$Age >= 50]) + 
  0.219 * sum(region_pop$Females[region_pop$Region == 'Northeast' & region_pop$Age >= 50])
case_northeast_yorkshire_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'Yorkshire & the Humber' & region_pop$Age >= 50]) + 
  0.067 * sum(region_pop$Males[region_pop$Region == 'Northeast' & region_pop$Age >= 50])
total_case_northeast_yorkshire <- case_northeast_yorkshire_F + case_northeast_yorkshire_M
total_pop_northeast_yorkshire_F <- sum(region_pop$Females[region_pop$Region == 'Yorkshire & the Humber']) + sum(region_pop$Females[region_pop$Region == 'Northeast'])
total_pop_northeast_yorkshire_M <- sum(region_pop$Males[region_pop$Region == 'Yorkshire & the Humber']) + sum(region_pop$Males[region_pop$Region == 'Northeast'])
total_pop_northeast_yorkshire <- total_pop_northeast_yorkshire_F + total_pop_northeast_yorkshire_M
ny_F_pc <- round(case_northeast_yorkshire_F/total_pop_northeast_yorkshire_F, 2)
ny_M_pc <- round(case_northeast_yorkshire_M/total_pop_northeast_yorkshire_M, 2)

## 5. North West
case_northwest_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'North West' & region_pop$Age >= 50]) 
case_northwest_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'North West' & region_pop$Age >= 50]) 
total_case_northwest <- case_northwest_F + case_northwest_M 
total_pop_northwest_F <- sum(region_pop$Females[region_pop$Region == 'North West'])
total_pop_northwest_M <- sum(region_pop$Males[region_pop$Region == 'North West'])  
total_pop_northwest <- total_pop_northwest_F + total_pop_northwest_M
nw_F_pc <- round(case_northwest_F/total_pop_northwest_F, 2)
nw_M_pc <- round(case_northwest_M/total_pop_northwest_M, 2)

## 6. South East 
case_southeast_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'South East' & region_pop$Age >= 50]) 
case_southeast_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'South East' & region_pop$Age >= 50]) 
total_case_southeast <- case_southeast_F + case_southeast_M 
total_pop_southeast_F <- sum(region_pop$Females[region_pop$Region == 'South East'])
total_pop_southeast_M <- sum(region_pop$Males[region_pop$Region == 'South East'])  
total_pop_southeast <- total_pop_southeast_F + total_pop_southeast_M
se_F_pc <- round(case_southeast_F/total_pop_southeast_F, 2)
se_M_pc <- round(case_southeast_M/total_pop_southeast_M, 2)

## 7. South West 
case_southwest_F <- 0.219 * sum(region_pop$Females[region_pop$Region == 'South West' & region_pop$Age >= 50]) 
case_southwest_M <- 0.067 * sum(region_pop$Males[region_pop$Region == 'South West' & region_pop$Age >= 50]) 
total_case_southwest <- case_southwest_F + case_southwest_M 
total_pop_southwest_F <- sum(region_pop$Females[region_pop$Region == 'South West'])
total_pop_southwest_M <- sum(region_pop$Males[region_pop$Region == 'South West'])  
total_pop_southwest <- total_pop_southwest_F + total_pop_southwest_M
sw_F_pc <- round(case_southwest_F/total_pop_southwest_F, 2)
sw_M_pc <- round(case_southwest_M/total_pop_southwest_M, 2)

# Create a data frame directly
region_case <- data.frame(
  Region = c("East of England", "London", "Midlands", "North East and Yorkshire", "North West", "South East", "South West"),
  case_Females = c(case_east_F, case_london_F, case_midlands_F, case_northeast_yorkshire_F, case_northwest_F, case_southeast_F, case_southwest_F),
  case_Males = c(case_east_M, case_london_M, case_midlands_M, case_northeast_yorkshire_M, case_northwest_M, case_southeast_M, case_southwest_M),
  Total_case = c(total_case_east, total_case_london, total_case_midlands, total_case_northeast_yorkshire, total_case_northwest, total_case_southeast, total_case_southwest),
  per_capita_F = c(east_F_pc, london_F_pc, mid_F_pc, ny_F_pc, nw_F_pc, se_F_pc, sw_F_pc), 
  per_capita_M = c(east_M_pc, london_M_pc, mid_M_pc, ny_M_pc, nw_M_pc, se_M_pc, sw_M_pc)
)

region_case

# Merge ONS shapefile and regional number of cases
region_case_merged <- cbind(regions, region_case, by.x = 'NHSER21NM', by.y = 'Region')

# Set limits for the color scale
scale_limits <- c(60000, 550000)

# Define breaks for the color scale
scale_breaks <- seq(from = scale_limits[1], to = scale_limits[2], length.out = 5)

# Plot number of cases for each region (Female)
plot_region_cases_F <- ggplot(region_case_merged) +
  geom_sf(aes(fill = case_Females), 
          color = 'white',
          lwd = 0.1) + 
  scale_fill_viridis_c(name = 'Number of Cases', limits = scale_limits, breaks = scale_breaks, labels = scales::label_comma()) +
  ggtitle('Number of Cases by Region') +
  labs(subtitle = 'Female') +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        text=element_text(family="Times", size=12))

library(RColorBrewer)
# Plot number of cases for each region (Male)
plot_region_cases_M <- ggplot(region_case_merged) +
  geom_sf(aes(fill = case_Males), 
          color = 'white',
          lwd = 0.1) + 
  scale_fill_viridis_c(name = 'Number of Cases', limits = scale_limits, breaks = scale_breaks, labels = scales::label_comma()) +
  labs(subtitle = '\n\nMale') +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        text=element_text(family="Times", size=12))

# Change the title position to the middle. 
ggarrange(plot_region_cases_F, plot_region_cases_M, 
          ncol = 2, nrow = 1, 
          common.legend = TRUE, legend = "right")

dev.off() # Set grid back to normal

# Set limits for the color scale
scale_limits <- c(0, 0.15)

library(RColorBrewer)
# Plot per capita number of cases for each region (Female)
plot_pc_region_cases_F <- ggplot(region_case_merged) +
  geom_sf(aes(fill = per_capita_F), 
          color = 'white',
          lwd = 0.1) + 
  scale_fill_viridis_c(name = 'Number of Cases per capita', limits = scale_limits, labels = scales::label_comma(), option = 'rocket') +
  ggtitle('Number of Cases by Region per Capita') +
  labs(subtitle = 'Female') +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA), 
        text=element_text(family="Times", size=12))

# Plot per capita number of cases for each region (Male)
plot_pc_region_cases_M <- ggplot(region_case_merged) +
  geom_sf(aes(fill = per_capita_M), 
          color = 'white',
          lwd = 0.1) + 
  scale_fill_viridis_c(name = 'Number of Cases per capita', limits = scale_limits, labels = scales::label_comma(), option = 'rocket') +
  labs(subtitle = '\n\nMale') +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA), 
        text=element_text(family="Times", size=12))

# Plot the two maps with a shared legend
ggarrange(plot_pc_region_cases_F, plot_pc_region_cases_M, 
          ncol = 2, nrow = 1, 
          common.legend = TRUE, legend = "right")

# Proportion of Estimated Prevalence Population (Female)
F_prop_mid <-  region_case$case_Females[region_case$Region == 'Midlands']/(sum(region_case$case_Females))*100
F_prop_EastofEngland <- region_case$case_Females[region_case$Region == 'East of England']/(sum(region_case$case_Females))*100
F_prop_London <- region_case$case_Females[region_case$Region == 'London']/(sum(region_case$case_Females))*100
F_prop_NE_Y <- region_case$case_Females[region_case$Region == 'North East and Yorkshire']/(sum(region_case$case_Females))*100
F_prop_NorthWest<- region_case$case_Females[region_case$Region == 'North West']/(sum(region_case$case_Females))*100
F_prop_SouthWest <- region_case$case_Females[region_case$Region == 'South West']/(sum(region_case$case_Females))*100
F_prop_SouthEast <- region_case$case_Females[region_case$Region == 'South East']/(sum(region_case$case_Females))*100

# Proportion of Estimated Prevalence Population (Male)
M_prop_mid <-  region_case$case_Males[region_case$Region == 'Midlands']/(sum(region_case$case_Males))*100
M_prop_EastofEngland <- region_case$case_Males[region_case$Region == 'East of England']/(sum(region_case$case_Males))*100
M_prop_London <- region_case$case_Males[region_case$Region == 'London']/(sum(region_case$case_Males))*100
M_prop_NE_Y <- region_case$case_Males[region_case$Region == 'North East and Yorkshire']/(sum(region_case$case_Males))*100
M_prop_NorthWest<- region_case$case_Males[region_case$Region == 'North West']/(sum(region_case$case_Males))*100
M_prop_SouthWest <- region_case$case_Males[region_case$Region == 'South West']/(sum(region_case$case_Males))*100
M_prop_SouthEast <- region_case$case_Males[region_case$Region == 'South East']/(sum(region_case$case_Males))*100

# Create a data frame for the proportion
region_case <- data.frame(
  Region = c("East of England", "London", "Midlands", "North East and Yorkshire", "North West", "South East", "South West"),
  case_Females = c(case_east_F, case_london_F, case_midlands_F, case_northeast_yorkshire_F, case_northwest_F, case_southeast_F, case_southwest_F),
  case_Males = c(case_east_M, case_london_M, case_midlands_M, case_northeast_yorkshire_M, case_northwest_M, case_southeast_M, case_southwest_M),
  Total_case = c(total_case_east, total_case_london, total_case_midlands, total_case_northeast_yorkshire, total_case_northwest, total_case_southeast, total_case_southwest),
  proportion_of_50_F = c(F_prop_EastofEngland, F_prop_London, F_prop_mid, F_prop_NE_Y, F_prop_NorthWest, F_prop_SouthEast, F_prop_SouthWest), 
  proportion_of_50_M = c(M_prop_EastofEngland, M_prop_London, M_prop_mid, M_prop_NE_Y, M_prop_NorthWest, M_prop_SouthEast, M_prop_SouthWest)
)


# Plot the estimated population change over the years by region. 
ggplot(df_long, aes(x = years, y = Risk, fill = region)) +
  geom_line(aes(colour = )) +
  labs(title = "Estimated Osteoporosis Populations Over the Years by Gender",
       x = "Year",
       y = "Population at Risk") +
  scale_fill_discrete(labels=c('Females at Risk', 'Males at Risk'))+
  theme_bw() + 
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt

