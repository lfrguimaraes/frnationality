
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
library(DT)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FR Nationality Follow-Up"),

    navbarPage("Menu",
               tabPanel("Summary",
                        
                        verticalLayout(
                            wellPanel(
                                
                               
                                    selectizeInput("selectDepartment", "Department", choices = NULL, multiple = TRUE),
                                    selectizeInput("selectYear", "Year", choices = NULL, multiple = TRUE),
                                    selectizeInput("selectBirthCountry", "Country", choices = NULL, multiple = TRUE),
                                    selectizeInput("selectSerie", "Serie", choices = NULL, multiple = TRUE)
                                    
                                    
                                
                                
                                
                            ),
                            plotOutput("distPlotBar"),
                            plotOutput("distPlotLine"),
                            wellPanel(
                                dataTableOutput("dataTable")
                            
                            )
                        ),
                        

                        
                        
                        ),
               tabPanel("Data upload",
                        
                        wellPanel(
                            
                            fileInput("file_input", "Choose JORF .pdf file to upload",
                                      multiple = FALSE,
                                      accept = c("pdf")),
                            #actionButton(inputId = "reprocessButton", "Reprocess"),                        
                            
                        ),

                            
                            #verbatimTextOutput("outPutFilesToProcess"),
                            
                            verbatimTextOutput("outPutFilesProcessed")
                            
                        

                        
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
 
            DT::datatable(tempFullData$data, options = list(searching = FALSE))
            
             
            
      

        }
        
        )
    
    output$distPlotBar <- renderPlot({
        
        chartData <-tempFullData$data

        

        ggplot(chartData, aes(x = Serie, fill = Year)) + geom_bar(position = position_dodge()) + theme_classic()
        
           })
    
    output$distPlotLine <- renderPlot({
      
      chartData <-tempFullData$data
      
      
      
      chartDataCountSeriesJournal <- chartData %>%
        dplyr::count(Journal, Serie)
      

      ggplot(data = chartDataCountSeriesJournal, aes(x = Journal, y = n, group = Serie, color=Serie)) +
        geom_line() + theme_classic()
      

      
    })

    observeEvent(input$reprocessButton, {
        
        withProgress(message = "Processing Files...", min = 1, max = 4, {
            
            incProgress(amount = 1)
            reprocess()
            incProgress(amount = 2)
            if(hasTidyDataFile()){
                fullData$data <- getTidyData()
            }
            incProgress(amount = 3)
            reactFilesToProcess$data <- getFilesToProcess()
            reactFilesProcessed$data <- getFilesProcessed()
            incProgress(amount = 4)
        })
        
        

    })
    
    observe({
        tempFullData <- reactiveValues(data=fullData$data)
    })
    

    observe({
        
        outputData <- data.frame(fullData$data)
        outputDataDepartments <- unique(outputData[,3])
        outputDataYears <- unique(outputData[,4])
        outputDataBirthCountries <- unique(outputData[,1])
        outputDataSeries <- unique(outputData[,5])
        
        
        updateSelectizeInput(session,"selectDepartment",choices = outputDataDepartments)
        updateSelectizeInput(session,"selectYear",choices = outputDataYears)
        updateSelectizeInput(session,"selectBirthCountry",choices = outputDataBirthCountries)
        updateSelectizeInput(session,"selectSerie",choices = outputDataSeries)
        
       
    })
    
    #filter definition
    observe({
        
        outputData <- fullData$data
        
        selectedDepartments <- input$selectDepartment
        selectedYears <- input$selectYear
        selectedBirthCountries <- input$selectBirthCountry
        selectedSeries <- input$selectSerie
        
        if(length(selectedDepartments)>0){
            outputData <- filter(outputData, Department %in% selectedDepartments)
        }
        
        if(length(selectedYears)>0){
            outputData <- filter(outputData, Year %in% selectedYears)
        }
        
        if(length(selectedBirthCountries)>0){
          
          outputData <- filter(outputData, Country %in% selectedBirthCountries)
        }
        
        if(length(selectedSeries)>0){
          outputData <- filter(outputData, Serie %in% selectedSeries)
        }
        
        
        tempFullData$data <- outputData
        

        
    })
    

    
    observe({
        req(input$file_input)
        filePath<-str_c("data/pdf_toprocess",input$file_input$name, sep="/")
        print(filePath)
        file.copy(input$file_input$datapath,filePath, overwrite = T)
        withProgress(message = "Processing Files...", min = 1, max = 4, {
          
          incProgress(amount = 1)
          reprocess()
          incProgress(amount = 2)
          if(hasTidyDataFile()){
            fullData$data <- getTidyData()
          }
          incProgress(amount = 3)
          reactFilesToProcess$data <- getFilesToProcess()
          reactFilesProcessed$data <- getFilesProcessed()
          incProgress(amount = 4)
        })
        
        })
    
    


    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
