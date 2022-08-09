

library(shiny)
library(plotly)
library(ggplot2)

# Make a dataset to get you started: The dataset "diamonds" is available from ggplots. The following will save it on your computer
#write.csv(diamonds, file="~/Downloads/diamonds.csv", row.names = FALSE) # On your computer, you may save D in a different folder

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title = "Uploading Your File"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      ## Create a file upload control
      fileInput(inputId = "file",
                label = "Choose Your File:",
                accept = c(".txt", ".csv")),
      ## Use html tag hr (horizontal rule) to make a horizontal separator
      hr(),
      ## Make a h5 heading
      h5("Max file size is 2M"),
      ## Create a checkbox that can be used to specify logical values.
      checkboxInput(inputId = "header",
                    label = "Header",
                    value = TRUE),
      ## Create a set of radio buttons used to select an item from a list.
      radioButtons(inputId = "sep",
                   label = "Separator",
                   choices = c(Comma = ",", Space = " ", Tab = "\t")),
      
      uiOutput("variable")
    ),
    
    
    # Generate Outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("table")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", plotlyOutput("plot", height = "700px"))
      )
      
    )
  )
)

# Define server logic required to generate desired outputs
server <- function(input, output, session) {
  
  myData <- reactive({ 
    # input$file will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the path to the local file.
    f = input$file
    if (is.null(f)){
      return(NULL)
      
    } else {
      read.table(f$datapath, header = input$header, sep = input$sep)
      
    }
  })
  
  # Create a drop-down menu to choose a variable
  output$variable <- renderUI({
    
    #Code you need to write (a)

    selectInput("var", "Choose variables to use:",
                choices = names(myData())
                
                )
                
                
                
   
    
  })
  
  # Display the whole table
  output$table <- renderTable({
    
  
    head(myData(),200)
    
  })
  
  # Summarize the whole table
  output$summary <- renderPrint({
    
    
    summary(myData())
    
    
    
    
    
  })
  
  # Plot only the selected variable.
  
  # The code needs to handle both a categorical and numeric variables
  output$plot <- renderPlotly({
    
    #Code you need to write (d)
   value= myData()[,input$var]
   plot_1= qplot(data =myData(), value , main=input$var,
                 xlab = input$var)

   plot_1 <- ggplotly(plot_1)
   
  plot_1
    
    
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
