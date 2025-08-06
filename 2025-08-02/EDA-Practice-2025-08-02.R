# Load relevant libraries
library(tidyverse)
library(readr)

# Load the data
person <- read_csv("2025-08-02/person.zip")

# Categorize vehicles by their type
person <- person %>% 
  # Case_when allows for multiple `if_else()` statements. 
  mutate(type = case_when(
    grepl("Motorcycle", VPICBODYCLASSNAME) ~ "Motorcycle", 
    grepl("SUV|Hatchback|Sedan|Coupe|Convertible|cUV|Wagon|Roadster", VPICBODYCLASSNAME) ~ "Passenger Cars",
    grepl("Pickup|SUT|Van|Motorhome|Minivan|Cutaway|Motor Home Chassis", VPICBODYCLASSNAME) ~ "Light Trucks and Vans",
    grepl("Bus", VPICBODYCLASSNAME) ~ "Bus",
    grepl("Truck|Fire Apparatus|Chassis Cab|Commercial Chassis", VPICBODYCLASSNAME) ~ "Medium and Heavy Trucks",
    grepl("Other|Enduro|Golf Cart|Farm Equipment|Construction Equipment|Go Kart|Street Sweeper|LSV", VPICBODYCLASSNAME) ~ "Other",
    .default = "Unknown or Incomplete"
  ))
person <- person %>% filter(type != "Unknown or Incomplete") %>% 
  mutate(type = factor(type, levels = c("Passenger Cars", "Motorcycle", "Light Trucks and Vans", "Medium and Heavy Trucks", "Bus", "Other")))

# Populate the vehiecle type dropdown options
vehicle_type <- unique(person$type)

# Leading cause of traffic fatalities in 2023
# Obtain total count
type_count <- person %>% group_by(type) %>% summarise(total_count = n())
# Obtain total fatality count and calculate the relative frequency of people who died from a crash
death_count <- person %>% filter(INJ_SEVNAME == "Fatal Injury (K)") %>% group_by(type) %>% 
  summarise(fatality_count = n()) %>% left_join(type_count) %>% mutate(percentage = round(100*(fatality_count/total_count),2)) %>% arrange(type) 
# Add a column of rank
death_count <- death_count %>% mutate(rank = case_when(
  type == "Passenger Cars" ~ 1,
  type == "Motorcycle" ~ 2,
  type == "Light Trucks and Vans" ~ 3,
  type == "Medium and Heavy Trucks" ~ 4,
  type == "Other" ~ 5,
  type == "Bus" ~ 6
))

# Separate df for rank in `pivot_wide` form
lead <- death_count %>% select(type, rank) %>% pivot_wider(names_from = type, values_from = rank)

# Separate df for percentage in `pivot_wide` form
percentage_death_by_vehicle <- death_count %>% select(type, percentage) %>% pivot_wider(names_from = type, values_from = percentage)

# Separate df containing information for pie-chart in `pivot_wide` form
percentage_death <- death_count %>% mutate(percentage = case_when(
  type == "Passenger Cars" ~ round(fatality_count[1]/sum(death_count$fatality_count),4)*100,
  type == "Motorcycle" ~ round(fatality_count[2]/sum(death_count$fatality_count),4)*100,
  type == "Light Trucks and Vans" ~ round(fatality_count[3]/sum(death_count$fatality_count),4)*100,
  type == "Medium and Heavy Trucks" ~ round(fatality_count[4]/sum(death_count$fatality_count),4)*100,
  type == "Bus" ~ round(fatality_count[5]/sum(death_count$fatality_count),4)*100,
  type == "Other" ~ round(fatality_count[6]/sum(death_count$fatality_count),4)*100
)) %>% select(type, percentage) %>% pivot_wider(names_from = type, values_from = percentage)
# Add another row of "other" traffic fatalities other than the vehicle type in question
percentage_death <- percentage_death %>% add_row(100 - percentage_death[1, ])
# Rename the rows to give an apporpriate label
row.names(percentage_death) <- c("Traffic Fatality(%)", "Other(%)")


# Motorcycle fatalities based on month
motorcycle_fatalities_by_month <- person %>% 
  filter(type == "Motorcycle" & INJ_SEVNAME == "Fatal Injury (K)") %>%
  group_by(INJ_SEVNAME, MONTHNAME) %>% summarise(fatality_count = n()) %>% ungroup() %>% select(-INJ_SEVNAME)
# Rearrange by correct month order
motorcycle_fatalities_by_month <- motorcycle_fatalities_by_month %>%
  mutate(MONTHNAME = factor(MONTHNAME, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) 


# Credits: Info-graphic - https://www.youtube.com/watch?v=RXdcHCPJRg8
# Credits: Data - NHTSA 2023 FARS data
# Credits: Vehicle type classification based on https://www.fhwa.dot.gov/policyinformation/tmguide/tmg_2013/vehicle-types.cfm