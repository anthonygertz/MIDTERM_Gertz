library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)

censusdata <- read_csv('data/census_full.csv')
floods <- read_csv('data/floods.csv')

#Sorts the census data alphabetically by state and then county so as to display correctly in the UI dropdown lists
censusdata <- censusdata %>%
  arrange(state, county)

#Creates a variable tibble of states for use in the UI
state_choices <- censusdata %>% 
  distinct(state) %>% 
  deframe()

