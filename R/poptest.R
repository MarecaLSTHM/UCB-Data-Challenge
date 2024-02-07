library(here)
setwd(here())
pop <- read.csv("data/pop2.csv")
str(pop)
pop[] <- lapply(pop, function(x) {
  as.numeric(gsub(",", "", as.character(x)))
})

total_women_50_above <- sum(pop$Females[pop$Age >= 50])
total_men_50_above <- sum(pop$Males[pop$Age >= 50])
total_population <- sum(pop$All)

# Calculate 21.9% of the total number of women aged 50 and above
percentage_of_women <- 0.219 * total_women_50_above

# Print the result
cat("21.9% of total women aged 50 and above:", percentage_of_women, "\n")

# Calculate 6.7% of the total number of men aged 50 and above
percentage_of_men <- 0.067 * total_men_50_above

# Print the result
cat("6.7% of total men aged 50 and above:", percentage_of_men, "\n")

total_osteoporosis_population <- percentage_of_women + percentage_of_men
print(total_osteoporosis_population)

# comment test

png("output/population_at_risk.png")

percentage_data <- data.frame(
  category = c("Men", "Women"),
  percentage = c(percentage_of_men, percentage_of_women))
colnames(percentage_data) <- c('Gender', 'Counts')
library(ggplot2)
p <- ggplot(percentage_data, aes(x = Gender, y = percentage_data$Counts, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = 'Osteoporosis Population') +
  scale_color_manual()+
#  ylim(0, max(percentage_of_women) * 1.3) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12)) #Times New Roman, 12pt, Bold)
print(p+labs(y= "Osteoporosis Population"))

dev.off()
