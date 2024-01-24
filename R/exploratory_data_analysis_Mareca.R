library(here)
library(dplyr)

setwd(here())
prescriptions_by_region <- read.csv("data/By Region.csv")


str(prescriptions_by_region)


# Formatting the Region names
# Transform sex as follows: Male = 1 and Female = 2
prescriptions_by_region$region <- if(prescriptions_by_region$name == 'SOUTH WEST COMMISSIONING REGION'){
  prescriptions_by_region$region <- "SW"
}

# Transform sex as follows: Male = 1 and Female = 2
prescriptions_by_region$region <- factor(prescriptions_by_region$name, 
                                         levels = c('SOUTH WEST COMMISSIONING REGION',
                                                    'EAST OF ENGLAND COMMISSIONING REGION', 
                                                    'LONDON COMMISSIONING REGION', 
                                                    'NORTH WEST COMMISSIONING REGION', 
                                                    'NORTH EAST AND YORKSHIRE COMMISSIONING REGION', 
                                                    'MIDLANDS COMMISSIONING REGION', 
                                                    'SOUTH EAST COMMISSIONING REGION'), 
                                         labels=c("SW", 
                                                  "EoE", 
                                                  "LDN",
                                                  "NW",
                                                  "NEY",
                                                  "MI",
                                                  "SE"))


boxplot(y_items ~ region, data = prescriptions_by_region, 
        ylab = "Prescriptions",
        xlab = "Region",
        main ="Prescriptions per region over time")

pop_data <- read.csv("data/pop2.csv")


