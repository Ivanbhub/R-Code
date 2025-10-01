library(shiny)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(dplyr)


url_link <- 'https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv'

df <- read.csv(url(url_link))
#Cleaning 
#date
df$date <- as.Date(df$date)
df$year <- format(as.Date(df$date),"%Y")
#adding a description for the threat type, will use it for the tooltip later
df$desc <- NA
# set 'Desc' column value based on 'threat_type' column
df$desc[df$threat_type == 'shoot'] <- 'The victim fired a weapon'
df$desc[df$threat_type == 'point'] <- 'The victim pointed a weapon at another individual'
df$desc[df$threat_type == 'attack'] <- 'The victim attacked with other weapons or physical force'
df$desc[df$threat_type == 'threat'] <- 'The victim had some kind of weapon visible to the officers on the scene'
df$desc[df$threat_type == ' move'] <- 'The victim was moving in a threatening way'
df$desc[df$threat_type == 'undetermined'] <- 'The threat type could not be determined from available evidence'
df$desc[df$threat_type == 'accident'] <- 'It was an accident'
df$desc[df$threat_type == 'flee'] <- 'The victim was fleeing'

# after creating a map plot I noticed that Harrison township in Ohio 
# had the wrong longitude and latitude
df[df$city=='Harrison Township',][,c('longitude')]=-84.191
df[df$city=='Harrison Township',][,c('latitude')] = 39.808

# Filter out rows with invalid latitude and longitude
df <- df[!((df$latitude < -90 | df$latitude > 90) | (df$longitude < -180 | df$longitude > 180)), ]






# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Fatal police shooting"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      
      pickerInput("year",
                  "Select Year:",
                  choices = levels(factor(format(as.Date(df$date),"%Y"))),
                  options = list(
                    'actions-box' = TRUE,
                    size=10,
                    'selected-text-format'="count > 4"
                  ),
                  multiple = TRUE,
                  selected=max(levels(factor(format(as.Date(df$date),"%Y"))))
      )
      
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      navbarPage( "Police Shooting",
                  
                  tabPanel("Map",leafletOutput("map_plot",height = "600px"),verbatimTextOutput("Map_comment")),
                  tabPanel("Table",DT::dataTableOutput('table1')),

                  
      ),
      
    )
  )
)

#------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------ 
server <- function(input, output) {
  
  
  #filter data based on year selection
  
  datashow <- reactive({
    req(input$year)
    data <- df
    data <- data[data$year %in% input$year,]
    
    data
  })
  
  #display 
  output$table1 <- DT::renderDataTable({
    DT::datatable(datashow())
  })
  
  #------------------------------------------------------------------------------  
  # Map
  
  output$map_plot <-  renderLeaflet({
    
    df1 = datashow() #applies the year filter to the map
    
    # add a tooltip columnn 
    df1<- df1 %>% 
      mutate(tootlip = paste0('<B>Name:</B> ',name,"<br/>",
                              "<B>Age:</B> ", age,"<br/>",
                              '<B>armed_with:</B> ',armed_with,"<br/>",
                              '<B>Date:</B> ',date,"<br/>")) 
    
    
    leaflet()%>% 
      addTiles%>% 
      addMarkers(lng = df1$longitude,
                 lat = df1$latitude,
                 popup = df1$tootlip,
                 clusterOptions = markerClusterOptions())%>%
      setView(lng = -98.555183,lat = 39.809860,zoom = 4)
    
    
    
  })  
  
  output$Map_comment <-renderText({
    "Zoom in to explore the map and access additional points for interaction. 
To filter by year, simply use the filter tab located on the left side of the screen.
  "
  })
  
}
#------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
