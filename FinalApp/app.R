#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#--------------------------------------------------------------------------------
# TO DO LIST:
# - (DONE) merge us and mexico spatialpolygonsdataframes
# - (DONE) work on choropleth plot
# - (DONE) create text search function for commodity
# - (DONE) create carbon emissions algorithm calculator
# - figure out submit/apply changes button
# - remove montreal

# QUESTIONS:
# - (DONE) what data is spplot using to assign color values by default?
# - (DONE) how to map a region (great lakes) to multiple states (michigan, wisconson, illinois, etc.)?

# FUTURE GOALS:
# - (DONE) figure out two side by side maps (to compare the same food for two cities)
# - clean up layout
# - figure out menu/autofill of foods to search for
# - (DONE) add key for color gradient
# - add star or highlight for destination city
# - (MAYBE) incorporate carbon emissions of production into calculations
#--------------------------------------------------------------------------------

library(shiny)
library(rgdal)
library(tidyverse)
library(maps)
library(maptools)
library(RColorBrewer)
library(tigris)
library(plyr)
library(tmap)
library(sf)
library(rsconnect)

# Load Preprocessed Data
load(file="ProcessedData.RData")

# Frontend UI: ------------------------------------------------------------------------------------
ui <- fluidPage(
  theme = "bootstrap1.css",
  
  # APPLICATION TITLE:
  titlePanel("Food Travel Tracker"),
  h5("Compare how two cities source the same fruit/vegetable by searching below"),
  
  hr(),
  
  # INPUTS ROW
  fluidRow(
    column(3,
           # input to select first destination city
           selectInput("destination_input1", "First Destination City:",
                       destination_cities, selected="Los Angeles"),
    ),
    column(3,
           # input to select first destination city
           selectInput("destination_input2", "Second Destination City:",
                       destination_cities, selected="Boston"),
    ),
    column(4,
           # OPTION 1: input to type in the commodity to search for
           textInput("commodity_input", "Food Item:",
                     placeholder="Ex: cabbage"),
           # OPTION 2: drop down menu to select item
           selectInput("commodity_input2", "Food Item:", food_itemsLIST, selected=NULL),
    ),
    column(2,
           # input to check whether or not to calculate/display carbon emissions
           checkboxInput("carbemission_input", "Calculate Average Carbon Emissions", 
                         value=FALSE, width=NULL),
    ),
  ),
  
  hr(),
  
  # OUTPUT PLOTS
  fluidRow(
    
    column(6,
           # output first map plot
           h3(textOutput("dest1")),
           plotOutput("mapPlot1"),
           # output calculated carbon emissions
           h4(textOutput("avgCarbonText1")),    
    ),
    
    column(6,
           # output second map plot
           h3(textOutput("dest2")),
           plotOutput("mapPlot2"),
           # output calculated carbon emissions
           h4(textOutput("avgCarbonText2")),    
    )
    
  )
)


# Backend Server Logic: ---------------------------------------------------------------------------
server <- function(input, output) {
  
  output$dest1 <- renderText({
    paste(input$destination_input1)
  })
  
  output$dest2 <- renderText({
    paste(input$destination_input2)
  })
  
  
  output$mapPlot1 <- renderPlot({
    # find the item being searched for
    itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
    # get only the data for destination city
    item_dest_df <- filter(itemdf, Destination == input$destination_input1)
    # aggregate to count from origin region
    item_dest_count <- count(item_dest_df, vars=c('Region'))
    
    # merge with spatial data
    merged_item_dest <- left_join(merged, item_dest_count)
    
    # plot merged data
    ggplot(data = merged_item_dest) +
      geom_polygon(aes(x = long, y = lat, fill = freq, group = group)) + 
      coord_fixed(1.3) +
      guides(fill = guide_legend(title = "Number of Trucks")) +
      scale_fill_gradient(merged_item_dest$freq, low = "#f5e558", high = "#8532a8", space = "Lab",
                          na.value = "grey50", guide = "colourbar", aesthetics = "fill") + 
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
  
  output$mapPlot2 <- renderPlot({
    # find the item being searched for
    itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
    # get only the data for destination city
    item_dest_df <- filter(itemdf, Destination == input$destination_input2)
    # aggregate to count from origin region
    item_dest_count <- count(item_dest_df, vars=c('Region'))
    
    # merge with spatial data
    merged_item_dest <- left_join(merged, item_dest_count)
    
    # plot merged data
    ggplot(data = merged_item_dest) + 
      geom_polygon(aes(x = long, y = lat, fill = freq, group = group)) + 
      coord_fixed(1.3) +
      guides(fill = guide_legend(title = "Number of Trucks")) +
      scale_fill_gradient(merged_item_dest$freq, low = "#f5e558", high = "#8532a8", space = "Lab",
                          na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
  
  
  output$avgCarbonText1 <- renderText({
    if (input$carbemission_input) {
      #same as in map, isolate data
      itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
      item_dest_df <- filter(itemdf, Destination == input$destination_input1)
      item_dest_count <- count(item_dest_df, vars=c('Region'))
      
      # merge item_dest_df with spatial data
      merged_item_dest <- left_join(merged, item_dest_count)
      
      # carbon emissions calculations
      distance <- mean(item_dest_df$Distance, na.rm=TRUE)
      avg_emissions_factor <- 161.8
      avg_weight_tons <- 27.5
      avg_carbon_emissions_grams <- distance * avg_emissions_factor * avg_weight_tons
      avg_carbon_emissions_kg <- round(avg_carbon_emissions_grams / 1000)
      
      paste("Average Carbon Emissions of ", input$commodity_input, " to ", 
            input$destination_input1, ": ", avg_carbon_emissions_kg, " kg")
    }
  })
  
  output$avgCarbonText2 <- renderText({
    if (input$carbemission_input) {
      # same as in map, isolate data
      itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
      item_dest_df <- filter(itemdf, Destination == input$destination_input2)
      item_dest_count <- count(item_dest_df, vars=c('Region'))
      
      # merge with spatial data
      merged_item_dest <- left_join(merged, item_dest_count)
      
      # carbon emissions calculations
      distance <- mean(item_dest_df$Distance, na.rm=TRUE)
      avg_emissions_factor <- 161.8
      avg_weight_tons <- 27.5
      avg_carbon_emissions_grams <- distance * avg_emissions_factor * avg_weight_tons
      avg_carbon_emissions_kg <- round(avg_carbon_emissions_grams / 1000)
      
      paste("Average Carbon Emissions of ", input$commodity_input, " to ", 
            input$destination_input2, ": ", avg_carbon_emissions_kg, " kg")
    }
  })
}

# Run the application: ----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
