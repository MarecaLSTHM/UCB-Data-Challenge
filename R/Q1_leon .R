library(here)
library(dplyr)
library(ggplot2)
setwd(here())
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

df_1=inner_join(df_BIS,df_CA, by=c("date","name","id"))
df_1=inner_join(df_1,df_deno, by=c("date","name","id"))
df_1=inner_join(df_1,df_PTH, by=c("date","name","id"))
df_1$date<-as.Date(df_1$date)
for (i in 1:nrow(df_1)) {
  df_1$total[i] = sum(df_1[i, c("BIS", "CA", "deno", "PTH")])
}

##note redunancy, a lot of patietn will get CA anyways .... 
for (i in 1:nrow(df_1)) {
  df_1$total[i] = sum(df_1[i, c("BIS", "CA", "deno", "PTH")])
  df_1$BIS_proportion[i] = df_1$BIS[i] / df_1$total[i]
  df_1$CA_proportion[i] = df_1$CA[i] / df_1$total[i]
  df_1$deno_proportion[i] = df_1$deno[i] / df_1$total[i]
  df_1$PTH_proportion[i] = df_1$PTH[i] / df_1$total[i]
}

df_SW<-df_1[df_1$name=="SOUTH WEST COMMISSIONING REGION",]
df_E<-df_1[df_1$name=="EAST OF ENGLAND COMMISSIONING REGION",] 
df_lon<-df_1[df_1$name=="LONDON COMMISSIONING REGION",]
df_NW<-df_1[df_1$name=="NORTH WEST COMMISSIONING REGION",]
df_SE<-df_1[df_1$name=="SOUTH EAST COMMISSIONING REGION",]
df_NE<-df_1[df_1$name=="NORTH EAST AND YORKSHIRE COMMISSIONING REGION",]
df_MID<-df_1[df_1$name=="MIDLANDS COMMISSIONING REGION",]

summary(df_SW)
ggplot(df_SW, aes(x = date,y=BIS_proportion)) +
  geom_line() +
  geom_line(aes(y = CA_proportion), color = "red") +
  geom_line(aes(y = deno_proportion), color = "blue") +
  geom_line(aes(y = PTH_proportion), color = "green") +
  labs(title = "Proportion of drugs Over Time in SOUTH WEST COMMISSIONING REGION",
       y = "Proportion",
       x = "Date") 
