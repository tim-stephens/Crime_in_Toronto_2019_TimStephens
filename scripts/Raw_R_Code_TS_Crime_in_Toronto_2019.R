### This is the code used to trim the dataset, create plots, and create tables. 

### WORKSPACE SETUP ###

library(tidyverse)
library(here)
library(sf)
library(knitr)
library(opendatatoronto)
library(tinytex)
library(gridExtra)
library(bibtex)



### GET THE DATASET ###

#Search opendatatoronto for dataset on Neighbourhood Crime Rates
neighbourhood_crime_rates_toronto <- search_packages("Neighbourhood Crime Rates")


#list the resources in the package
crime_packages <- 
  neighbourhood_crime_rates_toronto %>% 
  list_package_resources()

#pull the raw data out of the package 
raw_data <- 
  crime_packages %>% 
  get_resource()

#Save the raw data as a .csv file for safekeeping
write_csv(raw_data, here("inputs/data/Crime_Rates_Toronto_Raw.csv"))



### CLEAN THE DATASET ###

#load in the data from the local csv file
crime_rates_toronto_raw <- read_csv(here("inputs/data/Crime_Rates_Toronto_Raw.csv"))

#only keep columns of interest (major crimes in 2019 and their respective crime rates)
crime_in_toronto_2019 <- 
  tibble(neighbourhood = crime_rates_toronto_raw$Neighbourhood, 
         population = crime_rates_toronto_raw$Population,
         
         assault = crime_rates_toronto_raw$Assault_2019,
         car_theft = crime_rates_toronto_raw$AutoTheft_2019, 
         break_enter = crime_rates_toronto_raw$BreakandEnter_2019, 
         homicide = crime_rates_toronto_raw$Homicide_2019,
         robbery = crime_rates_toronto_raw$Robbery_2019,
         
         assault_rate = crime_rates_toronto_raw$Assault_Rate_2019,
         car_theft_rate = crime_rates_toronto_raw$AutoTheft_Rate_2019,
         break_enter_rate = crime_rates_toronto_raw$BreakandEnter_Rate_2019,
         homicide_rate = crime_rates_toronto_raw$Homicide_Rate_2019,
         robbery_rate = crime_rates_toronto_raw$Robbery_Rate_2019
         
  ) %>% 
  
  #add a column that is the sum of all major crimes in each neighbourhood
  mutate(total_crime = assault + car_theft + break_enter + homicide + robbery)


###PLOT1###

#Make plots of each major crime vs the population of the 140 neighbourhoods
#Every plot is a scatter plot with a line of best fit

#Assault
assault_plot <- 
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = assault)) + 
  theme_minimal() +
  geom_point() +
  geom_smooth() + 
  labs(subtitle = "Assaults",
       x = "Population",
       y = "Number of Assaults")

#Auto Theft
auto_theft_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = car_theft)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() + 
  labs(subtitle = "Car Theft",
       x = "Population",
       y = "Number of Thefts")

#Breaking and Entering 
break_and_enter_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = break_enter)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "Breaking and Entering",
       x = "Population",
       y = "Number of Break-Ins")


#Homicide
homicide_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = homicide)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "Homicide",
       x = "Population",
       y = "Number of Homicides")

#Robbery
robbery_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = robbery)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "Robberies",
       x = "Population",
       y = "Number of Robberies")


#Make a plot of the total number of crimes in each neighbourhood
total_crime_plot <- 
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = total_crime)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "All Major Crimes",
       x = "Population",
       y = "Total Number of Crimes")


#Display all 6 plots in one figure
grid.arrange(assault_plot, 
             auto_theft_plot, 
             break_and_enter_plot,
             homicide_plot,
             robbery_plot,
             total_crime_plot,
             ncol=3,
             top = "The Number of Major Crimes Commited Increases with Population
             ") #this is on a new line for formatting reasons. Increases whitespace.


### TABLE ###

# The purpose of this table is to determine the neighbourhood with the 
# highest amount of car theft - what is with that one outlier??

#Turns out...it is the neighbourhood by the airport.
#long term parking...lots of cars...makes sense. 

car_theft_table <- 
  crime_in_toronto_2019 %>% 
  select(neighbourhood, population, car_theft) %>% 
  arrange(-car_theft) %>% 
  slice(1:5)

#using kable, create a nicely designed table. 
knitr::kable(car_theft_table, 
             col.names = c(
               "Neighbourhood",
               "Population",
               "Number of Car Thefts"),
             caption = "*Toronto Neighbourhoods with the Highest Number of Car Thefts*")





### IMAGE ###

knitr::include_graphics(path = here("inputs/Images/West_Humber-Clairville_1.png"))


### PLOT 2 ###

#Make plots of each major crime rate vs the population of the 140 neighbourhoods
#Every plot is a scatter plot with a line of best fit

#Assault
assault_rate_plot <- 
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = assault_rate)) + 
  theme_minimal() +
  geom_point() +
  geom_smooth() + 
  labs(subtitle = "Assaults",
       x = "Population",
       y = "Assault Rate")

#Auto Theft
auto_theft_rate_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = car_theft_rate)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() + 
  labs(subtitle = "Car Theft",
       x = "Population",
       y = "Theft Rate")

#Breaking and Entering 
break_and_enter_rate_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = break_enter_rate)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "Breaking and Entering",
       x = "Population",
       y = "Rate of Break-Ins")


#Homicide
homicide_rate_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = homicide_rate)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "Homicide",
       x = "Population",
       y = "Homicide Rate")

#Robbery
robbery_rate_plot <-
  crime_in_toronto_2019 %>% 
  ggplot(mapping = aes(x = population, y = robbery_rate)) + 
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(subtitle = "Robberies",
       x = "Population",
       y = "Robbery Rate")


#Display all 6 plots in one figure
grid.arrange(assault_rate_plot, 
             auto_theft_rate_plot, 
             break_and_enter_rate_plot,
             homicide_rate_plot,
             robbery_rate_plot,
             ncol=3,
             top = "Comparing Crime Rates to Population in Toronto Neighbourhoods (2019)
             ") #this is on a new line for formatting reasons. Increased whitespace.
