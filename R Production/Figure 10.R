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


colnames(df_over_50) <- df_over_50[1,]
df_over_50 <- df_over_50[-1, ]
df_over_50_2<-df_over_50

df_over_50$Gender <- rownames(df_over_50)

df_long <- tidyr::pivot_longer(df_over_50, cols = -Gender, names_to = "Year", values_to = "Value")
png("output/predicted_osteoporosis.png")
ggplot(df_long, aes(x = Year, y = Value, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Year",
       y = "Population(thousands)",
       fill = "Gender") +
  scale_fill_manual(values = c("Pink", "Blue"), name = "Gender") + 
  theme_bw()+
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_y_continuous(breaks = seq(0,4000, by = 400)) 

dev.off()


total_sum<-colSums(df_over_50_2)
percentage_increase <- (total_sum[-1] - total_sum[-length(total_sum)]) / total_sum[-length(total_sum)] * 100

result_df <- data.frame(
  Year = names(percentage_increase),
  Percentage_Increase = percentage_increase
)





