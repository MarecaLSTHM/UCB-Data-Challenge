library(ggthemes)
library(data.table)

png(filename = "output/df_drugs_with_time_without_geography.png")

# Plot Prescription Trends from 2019-2023
colnames(df_drug_date) <- c('Date', 
                            'Bisphosphonates', 
                            'Calcium', 
                            'Denosumab', 
                            'PTH')

df_drug_date_long <- melt(setDT(df_drug_date), id.vars = c("Date"), variable.name = 'Medication')
colnames(df_drug_date_long) <- c('Year', 
                                 'Medication', 
                                 'Values')

trend_plot <- ggplot(df_drug_date_long, aes(x = Year, y = Values)) + 
  geom_line(aes(colour = Medication, group = Medication))+
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()+
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold 

print(trend_plot+labs(y= "Number of Prescriptions"))
dev.off()