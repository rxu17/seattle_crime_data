###################################################################################
# Name of Script: data_cleaning.r
# Inputs: Seattle crime data:
#         https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5/data
#         Seattle police beats:
#         https://data.seattle.gov/Land-Base/Police-Beat-and-Precinct-Centerpoints/4khs-fz35
# Description: Data investigation into seattle crime data to find safest neighborhoods/present
# facts that will guide decidion for parents who are choosing what neighborhood to move in with their 
# kids
# Contributors: rxu17, yayinc, yiranl3
#
###################################################################################

################################
## Set up libraries and system #
################################
rm(list=ls())

# load in libraries
library(assertthat); library(ggplot2);
library(plotly); library(data.table); library(tidyr); library(here)

code_repo <-  paste0(here(), "/seattle_crime_data/")

################################
## Dataset Cleaning            #
################################
# read in datasets
data <- fread(paste0(code_repo, "/raw_data/Crime_Data.csv"))
beats <- fread(paste0(code_repo, "/raw_data/Police_Beat_and_Precinct_Centerpoints.csv"))
setnames(beats, old = "Name", new = "beat")
setnames(data, old = colnames(data), new = tolower(gsub(" ", "_", colnames(data))))
beat_data <- merge(data, beats, by = c('beat'), all.x = T)

# want rows with latitude and longitude
beat_data <- beat_data[beat != "" & !is.na(Latitude)]
neighbor <- beat_data[, list(beat, neighborhood)] %>% unique
neighbor[, .N, by = list(beat)]

# grab time data
beat_data[, month := strsplit(occurred_date, split = "/") %>%  sapply("[", 1)]
beat_data[, day := strsplit(occurred_date, split = "/") %>%  sapply("[", 2)]
beat_data[, year := strsplit(occurred_date, split = "/") %>%  sapply("[", 3)]

# read in severity ranking
severity <- fread(paste0(code_repo, "/raw_data/seattle_crime_severity.csv"))

# record number of crimes with weapon
beat_data[, has_weapon := ifelse(grepl("WEAPON|GUN", primary_offense_description), 1, 0)]
beat_data[, crime_subcategory_test := strsplit(crime_subcategory, split = "-") %>%  sapply("[", 1) %>% tolower]
beat_data[, crime_subcategory_test := gsub(" ", "_", crime_subcategory_test)]
beat_data$crime_subcategory <- NULL
beat_data$crime_subcategory <- beat_data$crime_subcategory_test

beat_data_new <- merge(beat_data, severity, by = c("crime_subcategory"))

# dataset to be used in interactive infographic in Power BI
fwrite(beat_data_new, paste0(code_repo, "/by_beat.csv"))

################################
## Dataset Exploration         #
################################
# aggregate stats by neighborhood
sample <- data[, .N, by = list(primary_offense_description, crime_subcategory, neighborhood)]
sample

# counts of crime for each beat grouped by crime category and severity
sample <- beat_data_new[, .N, by = list(crime_subcategory, severity, Latitude, Longitude)]
sample

ggplot(sample) + geom_bar(aes(x = crime_subcategory)) + facet_wrap(~neighborhood)
