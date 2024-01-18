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

png("output/population_at_risk.png")
barplot(height = c(percentage_of_men,percentage_of_women),
        names.arg = c("Men", "Women"),
        col = c("blue", "pink"),
        main = "Population at Risk of Osteoporosis",
        ylab = "population at risk")
dev.off()
