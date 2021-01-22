library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(DT)

options(scipen=999)

censusdata <- read_csv('data/census_full.csv')
floods <- read_csv('data/floods.csv')

#Sorts the census data alphabetically by state and then county so as to display correctly in the UI dropdown lists
censusdata <- censusdata %>%
  arrange(state, county)

floods <- floods %>%
  arrange(State, Year, County) %>%
  mutate(Year = as.integer(Year),
         Damage_Nominal = as.integer(Damage_Nominal),
         CPI = as.integer(CPI),
         Damage_CPIAdj = as.integer(Damage_CPIAdj))

#Creates a variable tibble of states for use in the UI
state_choices <- censusdata %>% 
  distinct(state) %>% 
  deframe()

