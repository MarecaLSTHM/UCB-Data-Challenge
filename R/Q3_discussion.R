library(here)
library(tidyr)
library(dplyr)
library(tseries)
library(forecast)
library(extrafont)
library(Kendall)
library(ggplot2)
here()
setwd(here())

data11 <- read.csv("data/BISPHOSPHATES_NHS_REGIONS.csv")
data11 <- data11%>%
  select(date, name,y_items)

ts_data<-data11 %>% group_by(date) %>% summarise(prescriptions=sum(y_items))
ts_data$date<-as.Date(ts_data$date)

result <- MannKendall(ts_data$prescriptions)
print(result)




data11 <- read.csv("data/deno_NHS_REGIONS.csv")
data11 <- data11%>%
  select(date, name,y_items)

ts_data<-data11 %>% group_by(date) %>% summarise(prescriptions=sum(y_items))
ts_data$date<-as.Date(ts_data$date)

result <- MannKendall(ts_data$prescriptions)
print(result)





