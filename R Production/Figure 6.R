library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(scales)
library(RColorBrewer)

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

# Conduct some data transformations
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


# Shape file from ONS
regions <- st_read("data/NHSER_APR_2021_EN_BFC.shp")

# Arrange alphabetically
regions <- regions%>% arrange(NHSER21NM)

# Auto assign region_id(primary key for future merging)
regions <- regions %>%
  mutate(region_id = row_number())

# Total prescriptions per region
total_prescriptions_by_region <- data11 %>%
  group_by(name) %>%
  summarise(total_prescriptions = sum(y_items)) 

# Arrange alphabetically to match ONS format
total_prescriptions_by_region <- total_prescriptions_by_region%>% arrange(name) 

# Auto assign region_id to match ONS
total_prescriptions_by_region <- total_prescriptions_by_region %>%
  mutate(region_id = row_number())
region_id <- total_prescriptions_by_region[,c("name","region_id")]


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


# png(file="output/denosumab prescription.png")
ggplot(sf_deno) +
  geom_sf(aes(fill = per_capita), 
          color = "white", 
          lwd = 0.1) +
  # scale_fill_gradient(low = "orange", high = "red", name = "Denosumab prescriptions") +
  scale_fill_viridis_c(name = 'Denosumab prescriptions') +
  # labs(title = "Denosumab prescriptions per patient per region") +
  theme_bw() + 
  theme(axis.text = element_blank(),   
        axis.ticks = element_blank(), 
        panel.grid = element_blank())
# dev.off()