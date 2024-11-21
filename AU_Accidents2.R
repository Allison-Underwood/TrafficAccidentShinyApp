library(shiny)
library(shinydashboard) #for formatting a layout
library(rsconnect)
library(dplyr) #for rounding the latitude and longitude
library(ggplot2) 
library(giscoR) #for the Grey Map
library(maps) #to place the points in the map
library(DT) #for the data table

#test of color_change branch 

accident <- read.csv("accident.csv",
                     na.strings= c("NOT APPLICABLE","Not Applicable",
                                   "Unknown","None","Not Applicable (Not Transported)",
                                   "Unknown EMS Hospital Arrival Time", 
                                   "Unknown if Transported","Unknown if Arrived",
                                   "Unknown EMS Scene Arrival Minutes", 
                                   "Unknown EMS Scene Arrival Hour", "Unknown if Notified",
                                   "Reported as Unknown", "Other", "Not Reported",
                                   "Unknown Hours","Unknown Minutes"))

#round the latitude and longitude and rename to match with the data in maps,
# so you can join them
accident$lat = round(accident$LATITUDE, digits = 2)
accident$LATITUDE = NULL
accident$long = round(accident$LONGITUD, digits = 2)
accident$LONGITUD = NULL

#creating the map data for the plots 
USA = gisco_get_countries(country = "USA", resolution = 1)
data = world.cities %>% filter(country.etc == "USA")
data = left_join(accident,data, by = c("lat", "long"), relationship = 'many-to-many')

#Creates simplier names for using the app
column_aliases <- c(
  "Fatalities" = "FATALS",
  "State" = "STATENAME",
  "Type of Collision" = "MAN_COLLNAME",
  "Day of Accident" = "DAY_WEEKNAME",
  "Month of Accident" = "MONTHNAME",
  "Road Type" = "FUNC_SYSNAME",
  "Where Accident Occured" = "REL_ROADNAME",
  "Lighting Conditions" = "LGT_CONDNAME",
  "Weather Conditions" = "WEATHERNAME"
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Accident Data Map"),
  dashboardSidebar( #Creates a menu on the side to navigate between the map and table
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody( #Controls what each tab will do
    tabItems(
      # Map Tab
      tabItem(tabName = "map",
              fluidRow(
                #Allows for filtering the number of Fatalities using a slider
                box(title = "Filter by Fatalities", width = 4, 
                    sliderInput("fatalitySlider", "Number of Fatalities:",
                                min = 0, max = 13, value = c(0, 13))), 
                #Has a Column selection to look into different variables
                box(title = "Select Column to Visualize on Map", width = 4, 
                    selectInput("column", "Select Column:",
                                choices = names(column_aliases),
                                selected = "Type of Collision")),
                #Creates the map
                box(title = "Accident Map", width = 12,   
                    plotOutput("mapPlot"))
              )
      ),
      
      # Data Tab
      tabItem(tabName = "data",
              #Selects different columns from the alias list above to look at
              # Keeps the Filter of Fatalities established Previously
              box(title = "Select Columns to Display", width = 4, 
                  selectInput("dataColumns", "Select Columns:",
                              choices = names(column_aliases), 
                              selected = c("State", "Fatalities"),
                              multiple = TRUE)),
              #Prints the table
              DTOutput("dataTable")
      )
    )
  )
)


#Define server logic
server <- function(input, output, session) {
  
  #Filters the fatality data for both the map and table
  filteredData <- reactive({
    data %>% 
      filter(FATALS >= input$fatalitySlider[1] & FATALS <= input$fatalitySlider[2])
  })
  
  #Changes the alias to the column name
  getActualColumn <- function(alias) {
    column_aliases[alias]
  }
  
  #For creating the plot
  output$mapPlot <- renderPlot({
    actual_column <- getActualColumn(input$column)  # Get the actual column name
    
    ggplot() +
      #builds the grey 'background' map
      geom_sf(data = USA, fill = "grey", alpha = 0.3) +
      #adds the points/bubbles based on the filters
      geom_point(data = filteredData(),
                 aes(x = long, y = lat, colour = .data[[actual_column]], 
                     alpha = 0.001, size = FATALS)) +
      theme_void() +
      #limits the spacing to only show the relevtent area (the United States)
      ylim(c(20, 70)) +
      xlim(c(-175, -65)) +
      #gets rid of the alpha = 0.0001 legend
      guides( alpha = "none")
  })
  
  #For creating the data table
  output$dataTable <- renderDT({
    #Applies the filter
    selected_columns <- input$dataColumns
    actual_columns <- sapply(selected_columns, getActualColumn) 
    selectedData <- filteredData()[, actual_columns, drop = FALSE]
    #builds the table
    datatable(selectedData, options = list(pageLength = 50))
  })
}

#Run the application 
shinyApp(ui = ui, server = server)