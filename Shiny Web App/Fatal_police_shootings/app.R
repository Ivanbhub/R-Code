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
                  selected='2023'
      )
      
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      navbarPage( "Police Shooting",
                  
                  tabPanel("Map",leafletOutput("map_plot",height = "600px")),
                  tabPanel("Threat Level",plotlyOutput("threat_type_plot",height = "600px"),verbatimTextOutput("Threat_comment")),
                  tabPanel("Gender",plotOutput("gender_plot",height = "600px"),verbatimTextOutput("Gender_comment")),
                  tabPanel("Table",DT::dataTableOutput('table1')),
                  navbarMenu("More",
                             tabPanel("Race",plotlyOutput("race_plot",height = "600px"),verbatimTextOutput("Race_comment")),
                             tabPanel("flee Status",plotlyOutput("flee_status_plot",height = "600px"),verbatimTextOutput("flee_status_comment")),
                             tabPanel("Age",plotlyOutput("age_plot",height = "600px"),verbatimTextOutput("Age_comment")),
                             tabPanel("Armed",plotlyOutput("armed_with_plot",height = "600px"),verbatimTextOutput("armed_with_comment")),
                             tabPanel("City",plotlyOutput("city_plot",height = "600px"),verbatimTextOutput("City_comment")),
                  )
                  
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
    if (input$year != 0) {
      data <- data[data$year %in% input$year,]
    }
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
  
  
  #------------------------------------------------------------------------------ 
  #race 
  
  output$race_plot<-renderPlotly({
    
    race_filtered_data <-as.data.frame(
      table(datashow()['race'], dnn =list("Race") ),
      responseName = "Count"
    )
    
    race_plot <- ggplot(data = race_filtered_data, aes(x=Race,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')
    
    race_plot <- ggplotly(race_plot)
    race_plot
  })
  
  output$Race_comment <-renderText({
    "
  Date: 8/5/2022
There is a clear disparity in the number of white, black, and Hispanic 
individuals killed by police officers in the past eight years. 
While 3180 white people were killed, 1674 black people and 1127 Hispanics were 
also killed. It is important to consider the population percentages of these
races, but it is also crucial to recognize that every life lost to police
violence is significant and should be treated with equal gravity.
  "
  })
  
  
  #------------------------------------------------------------------------------ 
  
  # Gender 
  output$gender_plot<-renderPlot({
    
    gender_filtered_data <-as.data.frame(
      table(subset(datashow(),gender %in% c("male","female") )['gender'], dnn =list("Gender") ),
      responseName = "Count"
    )
    
    gender_plot <- ggplot(data = gender_filtered_data, aes(x = "", y = Count, fill = Gender)) +
      geom_bar(stat = "identity", width=1) +
      coord_polar("y", start=0)
    
    #sgender_plot <- ggplotly(gender_plot)
    gender_plot
  })
  
  output$Gender_comment <-renderText({
    "
  Date: 8/5/2022
  
 According to the data, men are significantly more likely to be shot by police 
 than women. This alarming statistic may be due to the perception that males
 are more prone to violence and aggression, leading the police to view them 
 as a greater threat. Further investigation is necessary to fully understand
 the reasons behind this significant gender disparity in police shootings.
  "
  })
  
  
  #------------------------------------------------------------------------------
  # threat_type
  
  output$threat_type_plot <- renderPlotly({
    threat_type_filtered_data <- datashow() %>%
      group_by(threat_type, desc) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      arrange(desc, count)
    
    max_count <- max(threat_type_filtered_data$count)
    
    threat_type_plot <- ggplot(data = threat_type_filtered_data, aes(x=threat_type, y=count, tooltip=desc, fill = count)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "deepskyblue1", high = "deepskyblue4", limits = c(0, max_count))
    
    ggplotly(threat_type_plot,tooltip = c("desc"))
  })
  
  output$Threat_comment <- renderText({
    "
  Date: 8/5/2022  
  The graph above shows that in 63% of cases, police officers were responding 
  to a threat and had to act to stop it. 
  "
  })
  
  #------------------------------------------------------------------------------ 
  # flee_status_plot
  output$flee_status_plot<-renderPlotly({
    
    flee_status_filtered_data <-as.data.frame(
      table(datashow()['flee_status'], dnn =list("flee_status") ),
      responseName = "Count"
    )
    
    flee_status_plot <- ggplot(data = flee_status_filtered_data, aes(x=flee_status,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')
    
    flee_status_plot <- ggplotly(flee_status_plot)
    
    flee_status_plot
    
    
  })
  
  output$flee_status_comment <-renderText({
    "
  
The graph above shows that the majority police shootings victims were not fleeing. 
  "
  })
  
  
  #------------------------------------------------------------------------------
  # age_plot
  
  output$age_plot<-renderPlotly({
    
    Age_plot <- ggplot(data = datashow(), aes(x=age,))+
      geom_histogram(bins = 10,fill='deepskyblue4',col='red4')
    
    Age_plot <- ggplotly(Age_plot)
    
    Age_plot
    
    
  })
  
  output$Age_comment <-renderText({
    "
  
The data displayed on the chart illustrates that the majority of individuals
who are fatally shot by law enforcement officers are within the ages of 20 and 
40. It is likely that this statistic is influenced by the tendency of officers
to view younger individuals as more dangerous and potentially aggressive 
compared to older individuals. This perceived threat may lead to a higher
incidence of police shootings in this age range.
  "
  })
  
  
  #------------------------------------------------------------------------------
  # armed_with_plot
  output$armed_with_plot<-renderPlotly({
    
    armed_with_filtered_data <-as.data.frame(
      table(datashow()['armed_with'], dnn =list("armed_with") ),
      responseName = "Count"
    )
    
    armed_with_filtered_sorted_data <- tail(armed_with_filtered_data[
      order(armed_with_filtered_data$Count),], 5)
    
    armed_with_plot <- ggplot(data = armed_with_filtered_sorted_data, aes(x=armed_with,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')+
      
      #theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),plot.title = element_text(hjust = 0.5))+
      labs(title='Most used weapon')+
      xlab('')
    
    armed_with_plot <- ggplotly(armed_with_plot)
    
    armed_with_plot
    
    
  })
  
  output$armed_with_comment <-renderText({
    "
  
Unsurprisingly, the weapon of choice use by the victims who were armed
is a gun (more than 70% of all incidents).   
  "
  })
  
  
  #------------------------------------------------------------------------------
  # city_plot
  
  output$city_plot<-renderPlotly({
    
    city_filtered_data <-as.data.frame(
      table(datashow()['city'], dnn =list("city") ),
      responseName = "Count"
    )
    
    city_filtered_sorted_data <- tail(city_filtered_data[
      order(city_filtered_data$Count),], 10)
    
    city_plot <- ggplot(data = city_filtered_sorted_data, aes(x=city,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')+
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),
            plot.title = element_text(hjust = 0.5))+
      labs(title='Top 10 Cities')+
      xlab('')
    
    
    city_plot <- ggplotly(city_plot)
    
    city_plot
    
    
  })
  
  
  output$City_comment <-renderText({
    "
  
Date: 8/5/2022  
The data depicted in the graph demonstrates the top 10 cities with the highest
number of police shooting victims over the past 8 years. Los Angeles holds the
top spot, with a total of 125 victims, averaging 15 per year and at least one
per month. In fact, Los Angeles has not experienced a single year with single 
digit police shooting victims in this time period. Phoenix holds the second
highest number of victims at 101, with a record high of 23 victims in 2018,
the highest of any city in the United States during this time frame. 
  "
  })
  
  
  
}
#------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
