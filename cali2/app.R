#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# library(shinythemes)

 coasts <- read.csv("Coastal Borders_Migrated Data.csv")
cali_dat <- read.csv("california_housing.csv") 


 distance <- function(lat, long){
   dist <- sqrt( ( (lat - coasts$Latitude)*69 )^2 + ( (long - coasts$Longitude)*54.6 )^2)
   return(min(dist, na.rm = T))
 }


cali_dat$Distance <- 0
# 
for(i in 1:nrow(cali_dat)){
   cali_dat$Distance[i] <- distance(cali_dat$latitude[i], cali_dat$longitude[i])
 }

cali_dat <- cali_dat %>% 
  mutate(perc_br = totalBedrooms/totalRooms)

ui <- fluidPage(
  # theme = shinytheme("superhero"),
  
  titlePanel("California Housing Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("xval",
                  label = "Select X Values: ",
                  choices = colnames(cali_dat)),
      selectInput("yval",
                  label = "Select Y Values: ",
                  choices = colnames(cali_dat)),
      selectInput("col",
                  label = "Select Color: ",
                  choices = colnames(cali_dat)),
      selectInput("size",
                  label = "Select Size: ",
                  choices = colnames(cali_dat))
    ),
    
    mainPanel(
      plotOutput("caliDist")
      
      
      
      
    )
  )
)

server <- function(input, output) {
  
  
  output$caliDist <- renderPlot({
    
    str(input$xval)
    
    ggplot(cali_dat)+
      geom_point(aes_string(x = input$xval, 
                            y = input$yval, 
                            color = input$col, 
                            size = input$size), alpha = 0.5)
    
  },
  width = 500, height = 400)
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
