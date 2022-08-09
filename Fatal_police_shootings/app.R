#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(dplyr)

url_link = 'https://github.com/washingtonpost/data-police-shootings/releases/download/v0.1/fatal-police-shootings-data.csv'

df = read.csv(url(url_link))
#Cleaning 
#date
df$date = as.Date(df$date)
df$year = format(as.Date(df$date),"%Y")

# after creating a map plot I noticed that Harrison township in Ohio 
# had the wrong longitude and latitude
df[df$city=='Harrison Township',][,c('longitude')]=-84.191
df[df$city=='Harrison Township',][,c('latitude')] = 39.808





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
                      selected='2015'
          )
          

        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          
          navbarPage( "Police Shooting",
                       tabPanel("Table",DT::dataTableOutput('table1')),
                       tabPanel("Map",leafletOutput("map_plot",height = "600px")),
                       tabPanel("Race",plotlyOutput("race_plot",height = "600px"),verbatimTextOutput("Race_comment")),
                       tabPanel("Gender",plotlyOutput("gender_plot",height = "600px"),verbatimTextOutput("Gender_comment")),
                      navbarMenu("More",
                       tabPanel("Threat Level",plotlyOutput("threat_level_plot",height = "600px"),verbatimTextOutput("Threat_comment")),
                       tabPanel("Flee",plotlyOutput("flee_plot",height = "600px"),verbatimTextOutput("Flee_comment")),
                       tabPanel("Age",plotlyOutput("age_plot",height = "600px"),verbatimTextOutput("Age_comment")),
                       tabPanel("Armed",plotlyOutput("armed_plot",height = "600px"),verbatimTextOutput("Armed_comment")),
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
                              '<B>Manner of Death:</B> ',manner_of_death,"<br/>",
                              '<B>Armed:</B> ',armed,"<br/>",
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
 There is no doubt that the police are more likely to shoot white people than 
 other race. As you can see from the graph above, in the last 8 years, 3180 
 white people were killed by police officers. In comparison, 1674 black people
 and 1127 Hispanics were killed by police officers over this same period of time.
 One can make a claim that these numbers would tell a different story if we were
 to compare them in terms of racial and ethnic population percentage.
 However, I believe these deaths shouldn't mean less just because they represent
 a smaller percentage of the race they're associated with.
  "
})


#------------------------------------------------------------------------------ 
  
 # Gender 
  output$gender_plot<-renderPlotly({
    
    gender_filtered_data <-as.data.frame(
      table(datashow()['gender'], dnn =list("Gender") ),
      responseName = "Count"
    )
    
    gender_plot <- ggplot(data = gender_filtered_data, aes(x=Gender,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')
    
    gender_plot <- ggplotly(gender_plot)
    gender_plot
  })

output$Gender_comment <-renderText({
  "
  Date: 8/5/2022
  
 The statistics show that men are 21 times more likely to get shot by police 
 than women. One reason might be that the police see males as being more likely
 to be violent and aggressive, so they may perceive them as a greater threat. 
 A little more digging is necessary to understand why there is such a big
 disparity.
  "
})
  
  
#------------------------------------------------------------------------------
  # threat_level
  
  output$threat_level_plot<-renderPlotly({
    
    threat_level_filtered_data <-as.data.frame(
      table(datashow()['threat_level'], dnn =list("threat_level") ),
      responseName = "Count"
    )
    
    threat_level_plot <- ggplot(data = threat_level_filtered_data, aes(x=threat_level,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')
    
    threat_level_plot <- ggplotly(threat_level_plot)
    threat_level_plot
  })

output$Threat_comment <-renderText({
  "
  Date: 8/5/2022  
 The graph above shows that in 63% of cases, police officers were responding 
 to a threat and had to act to stop it. 
  "
})

  
#------------------------------------------------------------------------------ 
  # flee_plot
  output$flee_plot<-renderPlotly({
    
    flee_filtered_data <-as.data.frame(
      table(datashow()['flee'], dnn =list("Flee") ),
      responseName = "Count"
    )
    
    flee_plot <- ggplot(data = flee_filtered_data, aes(x=Flee,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')
    
    flee_plot <- ggplotly(flee_plot)
    
    flee_plot
    
    
  })

output$Flee_comment <-renderText({
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
  
The chart above shows that most victims of police shootings are between the
ages of 20 and 40. I think this is because officers are more likely to perceive 
a young person as a greater threat than an older person, and therefore perceive 
them as more aggressive.
  "
})


#------------------------------------------------------------------------------
  # armed_plot
  output$armed_plot<-renderPlotly({
    
    armed_filtered_data <-as.data.frame(
      table(datashow()['armed'], dnn =list("Armed") ),
      responseName = "Count"
    )
    
    armed_filtered_sorted_data <- tail(armed_filtered_data[
      order(armed_filtered_data$Count),], 15)
    
    armed_plot <- ggplot(data = armed_filtered_sorted_data, aes(x=Armed,y=Count))+
      geom_bar(stat = "identity",fill='deepskyblue4')+
      labs(title='Most used weapons')+
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
      xlab('')
    
    armed_plot <- ggplotly(armed_plot)
    
    armed_plot
    
    
  })

output$Armed_comment <-renderText({
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
The graph above shows the top 10 cities with the most police shooting victims.
Los Angeles comes in first position with 125 total police shooting victims the 
last 8 years, that's an average of 15 per year and at least one per month.
As of today, Los Angeles has 10 victims and it's only August. Going back to each
of the last 8 years, I noticed that Los Angeles has never had a year with single
digit police shooting victims. 
Phoenix comes is second place with 101 police shooting victims, and in 2018 it 
had a record high of 23 victims in a single year, that's more than any other
city in the US in the same span of time. 
  "
})



}
#------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)




