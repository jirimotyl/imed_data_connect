library(tidyverse)


treatments_current <- read_delim("240814_imed_TR.txt") 
treatments_list <- read_csv2("Medication_list.csv") 

treatments_current <- select(treatments_current, "Treatment Name")
treatments_list <- select(treatments_list, "Treatment.name")
treatments_current <- as_tibble(treatments_current, .name_repair = "universal")


unique_treatments <- setdiff(treatments_current$Treatment.Name, treatments_list$Treatment.name)

unique_treatments <- as_tibble(unique_treatments)
write_csv(unique_treatments, "new_treatments.csv")