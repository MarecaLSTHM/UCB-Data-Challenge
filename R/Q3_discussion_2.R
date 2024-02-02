library(here)
library(tidyr)
library(ggplot2)
library(extrafont)

here()
setwd(here())
font_import()
loadfonts(device="win")
here()
setwd(here())
df_male<-read.csv("data/male_population_predict.csv")
df_female<-read.csv("data/female_populaiton_predict.csv")
df_male$Age<-as.factor(df_male$Age)
summary(df_male)

df_previous<-read.csv("data/population_over_50.csv")

df_male[, -1] <- lapply(df_male[, -1], function(x) {
  as.numeric(gsub(",", "", as.character(x)))
})
df_male<-df_male[11:nrow(df_male),]
df_female[, -1] <- lapply(df_female[, -1], function(x) {
  as.numeric(gsub(",", "", as.character(x)))
})
df_female<-df_female[11:nrow(df_female),]
years <- c(2024:2030) 


df_over_50 <- data.frame(
  Year = years,
  M = numeric(length(years)),
  F = numeric(length(years))
)
df_over_50<-t(df_over_50)
df_over_50[2, ] <-colSums(df_male[, -1])* 0.067 
df_over_50[3, ] <-colSums(df_female[, -1])* 0.219 
df_over_50<-as.data.frame(df_over_50)
df_over_50<-t(df_over_50)


df_previous<-read.csv("data/population_over_50.csv")
df_previous$Males<-df_previous$Males* 0.067 /1000
df_previous$Females<-df_previous$Females* 0.219 /1000
colnames(df_previous) <- c("Year","M","F")

df_over_50<-rbind(df_previous, df_over_50)
rownames(df_over_50)<-c(1:12)

df_over_50$M_Percent_increase <- c(NA, diff(df_over_50$M) / df_over_50$M[1:11] * 100)
df_over_50$F_Percent_increase <- c(NA, diff(df_over_50$F) / df_over_50$F[1:11] * 100)

