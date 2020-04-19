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
# - styling to make things look nicer

# QUESTIONS:
# - (DONE) what data is spplot using to assign color values by default?
# - (DONE) how to map a region (great lakes) to multiple states (michigan, wisconson, illinois, etc.)?

# FUTURE GOALS:
# - two side by side maps (to compare the same food for two cities)
# - incorporate carbon emissions of production into calculations
#--------------------------------------------------------------------------------

library(shiny)
library(rgdal)
library(tidyverse)
library(maps)
library(maptools)
library(RColorBrewer)
library(tigris)
library(plyr)

# Load Preprocessed Data
load(file="ProcessedData.RData")

# Frontend UI: ------------------------------------------------------------------------------------
ui <- fluidPage(theme = "bootstrap1.css",
    
    # Application title:
    titlePanel("Where Does ___ Get Its ___ ?"),
    
    # Sidebar for Inputs:
    sidebarLayout(
        sidebarPanel(
          
            # input to select first destination city
            selectInput("destination_input1", "First Destination City:",
                        destination_cities, selected="Boston"),
            
            # input to select second destination city
            selectInput("destination_input2", "Second Destination City:",
                        destination_cities, selected="Los Angeles"),
            
            # input to type in the commodity to search for
            textInput("commodity_input", "Food Item:", width=NULL,
                      placeholder="Ex: Cabbage"),
            
            # input to check whether or not to calculate/display carbon emissions
            checkboxInput("carbemission_input", "Calculate Average Carbon Emissions", value=FALSE, width=NULL),
            
            # submit inputs
            actionButton("goButton", "Go")
        ),
        
        # Main Panel Output:
        mainPanel(
            
            # output first map plot
            h3(textOutput("dest1")),
            plotOutput("mapPlot1"),
            # output calculated carbon emissions
            h4(textOutput("avgCarbonText1")),
            
            # output second map plot
            plotOutput("mapPlot2"),
            # output calculated carbon emissions
            h4(textOutput("avgCarbonText2"))
        )
    )
)


# Backend Server Logic: ---------------------------------------------------------------------------
server <- function(input, output) {
    
    output$dest1 <- renderText({
      paste(input$destination_input1)
      })
  
    output$mapPlot1 <- renderPlot({
        
        # find the item being searched for
        itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
        # get only the data for destination city
        item_dest_df <- filter(itemdf, Destination == input$destination_input1)
        # aggregate to count from origin region
        item_dest_count <- count(item_dest_df, vars=c('Region'))
        
        # merge item_dest_df with spatial data
        merged_item_dest <- left_join(merged, item_dest_count)
        
        # plot merged data
        ggplot(data = merged_item_dest) + 
            geom_polygon(aes(x = long, y = lat, fill = freq, group = group)) + 
            coord_fixed(1.3) +
            guides(fill = guide_legend(title = "Number of Trucks")) +
            scale_fill_gradient(merged_item_dest$freq, low = "#dba842",high = "#8532a8", space = "Lab",
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
      
      # merge item_dest_df with spatial data
      merged_item_dest <- left_join(merged, item_dest_count)
      
      # plot merged data
      ggplot(data = merged_item_dest) + 
        geom_polygon(aes(x = long, y = lat, fill = freq, group = group)) + 
        coord_fixed(1.3) +
        guides(fill = guide_legend(title = "Number of Trucks")) +
        scale_fill_gradient(merged_item_dest$freq, low = "#dba842",high = "#8532a8", space = "Lab",
                            na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
    })
    
    output$avgCarbonText1 <- renderText({
        
        if (input$carbemission_input) {
            # find the item being searched for
            # using row selection: itemdf <- truckdf[grep(input$commodity_input, truckdf$Commodity, ignore.case=TRUE), ]
            itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
            # get only the data for destination city
            # using row selection: item_dest_df <- itemdf[Destination == input$destination_input, ]
            item_dest_df <- filter(itemdf, Destination == input$destination_input1)
            # aggregate to count from origin region
            item_dest_count <- count(item_dest_df, vars=c('Region'))
            
            # merge item_dest_df with spatial data
            merged_item_dest <- left_join(merged, item_dest_count)
            
            distance <- mean(item_dest_df$Distance, na.rm=TRUE)
            avg_emissions_factor <- 161.8
            avg_weight_tons <- 27.5
            
            avg_carbon_emissions_grams <- distance * avg_emissions_factor * avg_weight_tons
            avg_carbon_emissions_kg <- avg_carbon_emissions_grams / 1000
            
            paste("Average Carbon Emissions of ", input$commodity_input, " to ", 
                  input$destination_input1, ": ", avg_carbon_emissions_kg, " kg")
        }
  
    })
    
    output$avgCarbonText2 <- renderText({
      
      if (input$carbemission_input) {
        # find the item being searched for
        # using row selection: itemdf <- truckdf[grep(input$commodity_input, truckdf$Commodity, ignore.case=TRUE), ]
        itemdf <- filter(truckdf, grepl(input$commodity_input, Commodity, ignore.case=TRUE))
        # get only the data for destination city
        # using row selection: item_dest_df <- itemdf[Destination == input$destination_input, ]
        item_dest_df <- filter(itemdf, Destination == input$destination_input2)
        # aggregate to count from origin region
        item_dest_count <- count(item_dest_df, vars=c('Region'))
        
        # merge item_dest_df with spatial data
        merged_item_dest <- left_join(merged, item_dest_count)
        
        distance <- mean(item_dest_df$Distance, na.rm=TRUE)
        avg_emissions_factor <- 161.8
        avg_weight_tons <- 27.5
        
        avg_carbon_emissions_grams <- distance * avg_emissions_factor * avg_weight_tons
        avg_carbon_emissions_kg <- avg_carbon_emissions_grams / 1000
        
        paste("Average Carbon Emissions of ", input$commodity_input, " to ", 
              input$destination_input2, ": ", avg_carbon_emissions_kg, " kg")
      }
      
    })
}

# Run the application: ----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
