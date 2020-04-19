library(shiny)
library(rgdal)
library(tidyverse)
library(maps)
library(maptools)
library(RColorBrewer)
library(tigris)
library(plyr)

truckdf <- read.table(file="Ref_Trucks.csv", header=TRUE, sep=",")

# MAPPINGS: ------------------------------------------------------------------------
# map mexico regions to mexico
mapping = data.frame(Region=c("Mexico-Arizona", "Mexico-Texas","Mexico-California"), 
                     Region=c("Mexico", "Mexico", "Mexico"))
# map each state to a more general Region
map_Region = data.frame(region=c("alabama", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "district of columbia", "florida", "georgia", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", 
                                 "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", 
                                 "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington","west virginia","wisconsin","wyoming","Mexico"),
                        Region=c("Southeast", "Arizona", "Southeast", "California", "Mountain", "Midatlantic", "New England", "Midatlantic", "Florida", "Southeast", "PNW", "Great Lakes", "Indiana", "Plains", "Plains", "Plains", "Southeast",
                                 "New England", "Midatlantic", "New England", "Great Lakes", "Great Lakes", "Southeast", "Plains", "Mountain", "Plains", "Mountain", "New England", "New England", "Arizona", "New York", "Southeast", "Plains",
                                 "Great Lakes", "Plains", "PNW", "Midatlantic", "New England", "Southeast", "Plains", "Plains", "Texas", "Mountain", "New England", "Midatlantic", "PNW", "Midatlantic", "Great Lakes", "Mountain", "Mexico"))

# merge mappings
truckdf <- full_join(truckdf, mapping, by=c("Region"))
# format strings better
truckdf$Destination <- str_to_title(truckdf$Destination)

# get some lists
destination_cities <- unique(truckdf$Destination)
origin_regions <- unique(truckdf$Region)

# GETTING SPATIAL DATA: -----------------------------------------------------------------
states <- map_data("state")
mexico <- map_data("world", regions="Mexico")
# change mexico's group number so it doesn't interfere with states
mexico[mexico$group == 16, ]$group = 75
truckdf[truckdf$Region == c("Mexico-Arizona", "Mexico-California", "Mexico-Texas"), ]$Region = "Mexico"

# merge the usa + mexico maps
merged <- rbind(states, mexico)
merged <- full_join(merged, map_Region, by=c("region"))

# merge the map data with truck data (TOO LARGE)
# merged_data <- full_join(merged, truckdf) #TO FIX: make a smaller modified truckdf with only the info we want

# save final outputs: -------------------------------------------------------------------
save(truckdf, destination_cities, states, mexico, merged, 
     file="ProcessedData.RData")






# TESTING THINGS OUT W/ FAKE INPUTS: ----------------------------------------------------
# for example purposes: we're looking for where boston gets cabbage from
input_dest <- "Boston"
input_item <- "TOMATO"

# find the item being searched for
itemdf <- truckdf[grep(input_item, truckdf$Commodity, ignore.case=TRUE), ]
# get only the data for destination city
item_dest_df <- itemdf[itemdf$Destination == input_dest, ]
# aggregate to count from origin region
item_dest_count <- count(item_dest_df, vars=c('region'))
item_dest_agg <- aggregate(item_dest_df, by = list(item_dest_df$Region), FUN = count)

# merge item_dest_df with spatial data
merged_item_dest <- left_join(merged, item_dest_count)

#  TESTING THINGS OUT 2:  -------------------------------------------------------
tomato1 <- truckdf[grep("TOMATO", truckdf$Commodity), ]
tomato2 <- tomato1[tomato1$Destination == "Boston", ]
tomato3 <- tomato2[tomato2$region == "Mexico", ]
nrow(tomato3)

tomatofilter <- filter(truckdf, grepl("TOMATO", Commodity), Destination == "Boston", region == "florida")
nrow(tomatofilter)

tomatotable <- as.data.frame(table(tomato))
nrow(table(tomato))
table(tomato)

count(tomato2$region)
count(tomato)