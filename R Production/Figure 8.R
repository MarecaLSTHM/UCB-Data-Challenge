library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Regional Level 

#Shape file from ONS
regions <- st_read("data/NHSER_APR_2021_EN_BFC.shp")

#arrange alphabetically
regions <- regions%>% arrange(NHSER21NM)

#auto assign region_id(primary key for future merging)
regions <- regions %>%
  mutate(region_id = row_number())

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