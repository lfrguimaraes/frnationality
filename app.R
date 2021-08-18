
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("functions.R")
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
                        mainPanel(
                            selectizeInput("selectDepartment", "Department", choices = NULL, multiple = TRUE),
                            selectizeInput("selectYear", "Year", choices = NULL, multiple = TRUE)
     
                            
                        ),
                        
                        dataTableOutput("dataTable")
                        ),
               tabPanel("Data upload",
                        
                        sidebarPanel(
                            fileInput("file_input", "Choose JORF .pdf file to upload",
                                      multiple = FALSE,
                                      accept = c("pdf")),
                            actionButton(inputId = "reprocessButton", "Reprocess")
                        ),
                        mainPanel(
                            
                            verbatimTextOutput("outPutFilesToProcess"),
                            
                            verbatimTextOutput("outPutFilesProcessed")
                            
                        )

                        
                        )
               
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #initialize data
    if(hasTidyDataFile()){
        outputData <- getTidyData()
        fullData <- reactiveValues(data=outputData)
        tempFullData <- reactiveValues(data=outputData)
    }
    
    filesToProcess <- getFilesToProcess()
    reactFilesToProcess <- reactiveValues(data=filesToProcess)
    
    filesProcessed <- getFilesProcessed()
    reactFilesProcessed <- reactiveValues(data=filesProcessed)
    
    
    

    #Outputs

    
    output$outPutFilesToProcess <- renderText({
        filesToProcess <- reactFilesToProcess$data
        
        if(length(filesToProcess)>0) filesToProcess <- paste(c("Files waiting proccesing: \n", filesToProcess), collapse="\n")
        else filesToProcess <- "No files to process"
        
        
        
    }
    )
    output$outPutFilesProcessed <- renderText({
        filesProcessed <- reactFilesProcessed$data
        
        filesProcessed <- paste(filesProcessed$filename, " (" ,filesProcessed$processDate,")")
        
        if(length(filesProcessed)>0) paste(c("Files processed: \n", filesProcessed), collapse="\n")
        else filesProcessed <- "No files processed"
        
        
        
    }
    
    )
    
    output$dataTable <- renderDataTable(
        
        if(hasTidyDataFile()){
 
            tableData <- tempFullData$data
            
      

        }
        
        )
    
    output$distPlot <- renderPlot({
        
        chartData <-tempFullData$data
        chartData$year <- chartData$Year
        chartData$serie <- chartData$Serie
        

        ggplot(chartData, aes(x = serie, fill = year)) + geom_bar(position = position_dodge()) + theme_classic()
        
           })

    observeEvent(input$reprocessButton, {
        
        reprocess()
        if(hasTidyDataFile()){
            fullData$data <- getTidyData()
        }
        reactFilesToProcess$data <- getFilesToProcess()
        reactFilesProcessed$data <- getFilesProcessed()
    })
    
    observe({
        tempFullData <- reactiveValues(data=fullData$data)
    })
    

    observe({
        
        outputData <- data.frame(fullData$data)
        outputDataDepartments <- unique(outputData[,3])
        outputDataYears <- unique(outputData[,4])
        
        updateSelectizeInput(session,"selectDepartment",choices = outputDataDepartments)
        updateSelectizeInput(session,"selectYear",choices = outputDataYears)
       
    })
    
    observe({
        
        outputData <- fullData$data
        
        selectedDepartments <- input$selectDepartment
        selectedYears <- input$selectYear
        
        if(length(selectedDepartments)>0){
            outputData <- filter(outputData, Department %in% selectedDepartments)
        }
        
        if(length(selectedYears)>0){
            outputData <- filter(outputData, Year %in% selectedYears)
        }
        
        tempFullData$data <- outputData
        

        
    })
    

    
    observe({
        req(input$file_input)
        filePath<-str_c("data/pdf_toprocess",input$file_input$name, sep="/")
        print(filePath)
        file.copy(input$file_input$datapath,filePath, overwrite = T)
        reactFilesToProcess$data <- getFilesToProcess()
        reactFilesProcessed$data <- getFilesProcessed()
        
        })
    
    


    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
