library(ggthemes)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggthemes)

# Read in prescription data of Bisphosphonates
df_BIS=read.csv('data/BISPHOSPHATES_NHS_REGIONS.csv')
df_BIS <- df_BIS[, c(1,2,3,4)]
names(df_BIS)[names(df_BIS) == "y_items"] <- "BIS" # Rename the prescription counts for Bisphosphonates

# Read in prescription data of Calcium related medications
df_CA=read.csv('data/CA+VIT_D_NHS REGIONS.csv')
df_CA <- df_CA[, c(1,2,3,4)]
names(df_CA)[names(df_CA) == "y_items"] <- "CA" # Rename the prescription counts for Calcium medications

# Read in prescription data of PTH Medications 
df_PTH=read.csv("data/Calcitonins_PTH_NHS_REGIONS.csv")
df_PTH <- df_PTH[, c(1,2,3,4)]
names(df_PTH)[names(df_PTH) == "y_items"] <- "PTH" # Rename the prescription counts for PTH medications

# Read in data for NHS regions
df_deno=read.csv("data/deno_NHS_REGIONS.csv")
df_deno <- df_deno[, c(1,2,3,4)]
names(df_deno)[names(df_deno) == "y_items"] <- "deno"

# Merge all data sets
df_drug=inner_join(df_BIS,df_CA, by=c("date","name","id"))
df_drug=inner_join(df_drug,df_deno, by=c("date","name","id"))
df_drug=inner_join(df_drug,df_PTH, by=c("date","name","id"))
df_drug$date<-as.Date(df_drug$date)


# Summarise prescriptions over time
df_drug_date<-df_drug %>% group_by(date) %>% summarise(
  BIS = sum(BIS),
  CA = sum(CA),
  deno = sum(deno),
  PTH = sum(PTH)
)

# Plot Prescription Trends from 2019-2023
colnames(df_drug_date) <- c('Date', 
                            'Bisphosphonates', 
                            'Calcium', 
                            'Denosumab', 
                            'PTH')

df_drug_date_long <- melt(setDT(df_drug_date), 
                          id.vars = c("Date"), 
                          variable.name = 'Medication')

colnames(df_drug_date_long) <- c('Year', 
                                 'Medication', 
                                 'Values')

# Denosumab and Parathyroid Hormones & Analogues Trend Plot
df_drug_long_sub <- subset(df_drug_date_long, Medication == 'Denosumab' | Medication == 'PTH')

trend_plot <- ggplot(df_drug_long_sub, aes(x = Year, y = Values)) + 
  geom_line(aes(colour = Medication, group = Medication))+
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()+
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold 
print(trend_plot+labs(y= "Number of Prescriptions"))
