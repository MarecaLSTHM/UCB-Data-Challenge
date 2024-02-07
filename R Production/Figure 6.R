library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(scales)
library(RColorBrewer)

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
  # scale_fill_gradient(low = "orange", high = "red", name = "Denosumab prescriptions") +
  scale_fill_viridis_c(name = 'Denosumab prescriptions') +
  # labs(title = "Denosumab prescriptions per patient per region") +
  theme_bw() + 
  theme(axis.text = element_blank(),   
        axis.ticks = element_blank(), 
        panel.grid = element_blank())
dev.off()