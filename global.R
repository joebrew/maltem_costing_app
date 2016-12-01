library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggthemes)
library(DT)
library(scales)
library(RColorBrewer)
library(readr)
# library(rgeos)
# library(maptools)
# library(raster)
# library(rgdal)

#### MOZAMBIQUE
# Get a shapefile for Mozambique
# moz <- raster::getData("GADM", country = "MOZ", level = 3)

# #### MAGUDE
# # Read in shapefile of Magude
# magude <- readOGR(".", "Magude_adm3")
# # Ensure that only the relevant data
# # remains in the spatial object:
# magude <- magude[magude@data$NAME_2 == "Magude",]
# # Fortify magude (format for ggplot2)
# magude_fortified <- 
#   fortify(magude, region = 'NAME_3')
# # Get lng and lat in magude@data
# magude@data$lng <- coordinates(magude)[,1]
# magude@data$lat <- coordinates(magude)[,2]


# df <- read_excel('MALTEM_costing_nov 2016.xlsm',
#                  sheet = '5. Parameters Elimination',
#                  skip = 20)
df <- read_csv('MALTEM_costing_nov 2016.csv', skip = 20)
# Subset columns
df <- df[,c(1,2,6,7, 26, 27, 35, 36, 44, 45) +1]

# Remove empty rows
df <- df %>% filter(!is.na(`Elimination activity`))

# Clean up column names
names(df) <- tolower(gsub(' ', '_', names(df)))

# Gather
df <- gather(df, key = year, value = value, mzn_2015:usd_2017)
df$currency <- substr(df$year, 1, 3)
df$year <- substr(df$year, 5, 8)

# Remove periods from column names
names(df) <- gsub('.', '', names(df), fixed = TRUE)
names(df) <- tolower(gsub('-', '_', names(df)))

# convert to data frame
df <- data.frame(df)

# Make currency uppercase
df$currency <- toupper(df$currency)

# Read in the percentage data
projections <- 
  read_excel('MALTEM_costing_nov 2016.xlsm',
             sheet = 'percentage_projections') %>%
  mutate(p = p * 100) 
projections <- projections[,2:4]
projections <- projections[,nchar(names(projections)) >= 1]
projections$p[projections$year == 2032 &
                projections$elimination_activity == 'Entomology'] <- 60

# Create baseline
baseline <- df %>%
  mutate(keep = (elimination_activity == 'REACT' & year == 2017) |
           (elimination_activity != 'REACT' & year == 2015)) %>%
  filter(keep) %>%
  group_by(elimination_activity, currency) %>%
  summarise(value = sum(value))

# Make calculations for the the_future
the_future <- expand.grid(year = 2015:2035,
                      elimination_activity = sort(unique(baseline$elimination_activity)),
                      currency = sort(unique(baseline$currency))) %>%
  left_join(projections) %>%
  left_join(baseline) %>%
  mutate(value = value * (p/100))

# Get outcomes
outcomes <- read.csv('outcomes.csv')
