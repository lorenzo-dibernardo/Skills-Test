# DO NOT CHANGE THE PRE-WRITTEN CODE EXCEPT WHERE MARKED
# load package
library(tidyverse)
# Add any additional packages required here
library(dplyr)
library(lubridate)
library(ggplot2)


# set seed - PLEASE EDIT --------------------------------
# CHANGE THE VALUE OF 1 TO A NUMBER OF YOUR CHOICE
set.seed(12) 

################################################################################
# code to create two datasets to work with - DO NOT EDIT
# NOTE - comments have been removed
val_create <- function(date, loc, type, n) {
  val <- 1.3 + 
    0.1 * (1 - as.numeric(max(date) - date) / length(date)) + 
    0.1 * (loc %% 2) + 0.2 * type + 
    runif(length(date), -0.1, 0.1)
  
  val[sample(c(1:length(date)), size = 20)] <- NA
  
  return(val)
}
all_dates <- seq(ymd("2019-01-01"), ymd("2023-12-31"), by = "day")

fuel_data <- tibble(
  location_id = sample(c(1:5), length(all_dates), TRUE),
  date = all_dates
) %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    petrol_price = val_create(date, location_id, 0),
    diesel_price = val_create(date, location_id, 1)
  ) %>%
  select(-date) %>%
  slice_sample(prop = 1) 

rm(all_dates)

fuel_locations <- tibble(
  id = c(1:5),
  name = c("Motorway Service Station", "Supermarket 1", "Independent Retailer",
           "Supermarket 2", "City Service Station"),
  area = c("NC500", "Highlands", "Lothian", "Moray", "Grampian")
)
################################################################################
# the datasets fuel_data and fuel_locations should now exist
# please enter code below to complete the outlined tasks

fuel_data
fuel_locations

# join the datasets -----------------------------------------
# Mutual column needed to merge. Location is saved under different names 
# "location_id" and "id", merge executed by joining the two columns

fuel_data_new <- fuel_data %>%
  left_join(fuel_locations, by = c("location_id" = "id"))

fuel_data_new

# add a date column -----------------------------------------
# Date column created by merging year:day cols and made it
# to be first column visible in the new dataset

fuel_date <- fuel_data_new %>%
  mutate(date = make_date(year, month, day), .before = location_id) %>%
  select(-c(year:day)) 
  
fuel_date

# summarise the data in at least one way  --------------------

#### analyse average prices by fuel type

av_per_type <- fuel_date %>%
  group_by(name) %>% 
  summarise(avg_petrol = mean(petrol_price, na.rm = TRUE),
            avg_diesel = mean(diesel_price, na.rm = TRUE))

av_per_type

#### number of records per area
record_by_area <- fuel_date %>%
group_by(area) %>%
  summarise(count = n())

record_by_area


#### min & max prices per area
fuel_date %>%
group_by(area) %>%
  summarise(max_petrol = max(petrol_price, na.rm = TRUE),
            min_petrol = min(petrol_price, na.rm = TRUE),
            max_diesel = max(diesel_price, na.rm = TRUE),
            min_diesel = min(diesel_price, na.rm = TRUE))

#### average prices per area
av_per_area <- fuel_date %>%
  group_by(area) %>%
  summarise(avg_petrol_price = mean(petrol_price, na.rm = TRUE),
            avg_diesel_price = mean(diesel_price, na.rm = TRUE))

av_per_area

# plot the data in at least one way ---------------------------
#### price average per location by fuel type

av_per_type2 <- av_per_type %>%
pivot_longer(cols = c(avg_petrol, avg_diesel), 
             names_to = "fuel_type", 
             values_to = "avg_price")

ggplot(av_per_type2, aes(x = name, y = avg_price, fill = name)) +
  geom_point() +
  facet_wrap(~(fuel_type)) +
  labs(x = "Location Name", y = "Average Price", fill = "Location")


#### Visualisation of records by area

record_by_area%>%
  ggplot(aes(x = area, y = count, fill = area)) +
  geom_col() +
  labs(title = "Records by Area", 
       x = "Area", 
       y = "Count")

# critically evaluate your results  ---------------------------
# 
# Identifying the mutual column (location) allowed me to generate a new dataset
# (fuel_data_new), which enabled a more thorough analysis of the 2 datasets.
# Merging three columns into one (fuel_date) offered a clearer picture of the 
# dataset, alongside offering a quicker and tidier frame of reference if needed.
#The two processes allowed the data to be more reader friendly, readily available,
# and quicker to analyse in time-constrained situations.
#
# Given the nature of the data, its summary could be done in many ways.
# Analysing the average prices by fuel type offered a general overview into
# price analysis and price behaviour, while grouping the number of records per 
# area created an overview at the geographical location on the fuel usage and 
#demand, differences of which were enhanced by studying the minimum, maximum 
# and average prices per area. This allows a reader to grasp the variability of 
# prices according to the region.
#
# These last factors were included in the first plotting option,in which price
#averages per fuel type were plotted according to location and fuel typ; 
# allowing a visualisation into affordability of fuel according to location, 
#this can help customers or stakeholders to plan and understand where fuel
# might be more affordable. This visualization can support planning efforts 
#and improve market awareness.
#The other visualisation generated aids the understanding of pump usage by 
# location, which could help stakeholders or planners understand current and
# future demands for fuel stations, aiding decision for opening and/or closure
# of stations based on usage.









