source("main.R")
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FR Nationality Follow-Up"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
       
            actionButton(inputId = "reprocessButton", "Reprocess"),
            selectInput(inputId = "countryInput", "Country", choices = c("All", "France", "Brésil", "Argentine") )
        ),
        


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           dataTableOutput("dataTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    if(hasTidyDataFile()){
        outputData <- getTidyData()
        fullData <- reactiveValues(data=outputData)
        
    }
    


    
    output$dataTable <- renderDataTable(
        
        if(hasTidyDataFile()){
            outputData<-fullData$data
            if(input$countryInput!="All")outputDataShow <- outputData[outputData$"Birth Country"==input$countryInput,]
            else outputDataShow <- outputData
        }
        
        )
    
    output$distPlot <- renderPlot({
           })

    observeEvent(input$reprocessButton, {
        
        reprocess()
        if(hasTidyDataFile()){
            fullData$data <- getTidyData()
        }
        
    })

    observe({
        outputData<-fullData$data
        listCountries <- levels(outputData$"Birth Country")
        updateSelectInput(session, input$countryInput, choices = c("1","2","3"))
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
