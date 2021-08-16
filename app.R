source("main.R")
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plyr)
library(dplyr)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FR Nationality Follow-Up"),

    navbarPage("FR Nat.",
               tabPanel("Summary",
                        plotOutput("distPlot"),
                        dataTableOutput("dataTable")
                        ),
               tabPanel("Data upload",
                        fileInput("file_input", "Choose pdf File",
                                  multiple = FALSE,
                                  accept = c("pdf")),
                        actionButton(inputId = "reprocessButton", "Reprocess"),
                        
                        )
               
    )
    #,
    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #    
    #         actionButton(inputId = "reprocessButton", "Reprocess"),
    #         
    #     ),
    #     
    # 
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot"),
    #        dataTableOutput("dataTable")
    #     )
    # )
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

        }
        
        )
    
    output$distPlot <- renderPlot({
        
        chartData <-fullData$data
        chartData$year <- chartData$"App. Year"
        chartData$serie <- chartData$"App. Serie"
        

        ggplot(chartData, aes(x = serie, fill = year)) + geom_bar(position = position_dodge()) + theme_classic()
        
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
    
    observe({
        req(input$file_input)
        filePath<-str_c("data/pdf_toprocess",input$file_input$name, sep="/")
        print(filePath)
        file.copy(input$file_input$datapath,filePath, overwrite = T)
        })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
