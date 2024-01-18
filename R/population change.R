library(here)
library(ggplot2)
library(tidyr)
setwd(here())


df<-read.csv("data/population_over_50.csv")
df$Males_risk=df$Males* 0.067 
df$Females_risk=df$Females* 0.219 
df_long <- gather(df, key = "Gender", value = "Risk", Females_risk,Males_risk)



library(tidyr)

df_long <- gather(df, key = "Gender", value = "Risk", Males_risk, Females_risk)

png("output/osteoporosis_population_2018_2022.png")
ggplot(df_long, aes(x = years, y = Risk, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Male and Female Estimated osteoporosis populations Over the Years",
       x = "Year",
       y = "Population at Risk") +
  scale_fill_manual(values = c("pink", "blue"),  
                    labels = c("Female", "Male")) +  
  theme_minimal()

dev.off()
