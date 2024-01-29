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
options(scipen = 999)
ggplot(df_long, aes(x = years, y = Risk, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Estimated Osteoporosis Populations Over the Years by Gender",
       x = "Year",
       y = "Population at Risk") +
  scale_fill_discrete(labels=c('Females at Risk', 'Males at Risk'))+
  theme_bw() + 
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt
df_long$Total <- df_long$Female + df_long$Male
ggplot(df_long, aes(x = years, y = Risk, fill = Gender)) +
  geom_line(aes(color = Gender), stat = "identity", position = "stack") +
  geom_line(aes(y = Total, color = "Total"), linetype = "dashed", size = 1) + # Add the Total line
  labs(title = "Estimated Osteoporosis Populations Over the Years by Gender",
       x = "Year",
       y = "Population at Risk") +
  #  scale_fill_discrete(labels = c('Females at Risk', 'Males at Risk')) +
  # scale_color_manual(values = c('Female' = 'red', 'Male' = 'blue', 'Total' = 'purple')) +  # Set color for the Total line
  theme_bw() + 
  theme(text = element_text(family = "Times", size = 12))  # Times New Roman, 12pt

dev.off()

