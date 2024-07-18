#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("CSV Data Analysis"),

    # Sidebar with a file input and options
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Horizontal line
            tags$hr(),
            # Checkbox if file has header
            checkboxInput("header", "Header", TRUE),
            # Input: Select separator
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            # Input: Select quotes
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            # Button to model the data
            actionButton("modelBtn", "Model Data"),
            # Horizontal line
            tags$hr(),
            # Input: Select number of rows to display
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            # Download button
            downloadButton("downloadData", "Download CSV")
        ),

        # Show outputs
        mainPanel(
           plotOutput("scatterPlot"),
           plotOutput("lmPlot"),
           verbatimTextOutput("modelSummary"),
           tableOutput("contents")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # Observe button click to model data
    modelData <- eventReactive(input$modelBtn, {
        req(dataInput())
        df <- dataInput()
        
        # Check if the data has at least two columns
        if (ncol(df) >= 2) {
            lm_model <- lm(df[[2]] ~ df[[1]], data = df)
            return(lm_model)
        }
        return(NULL)
    })
    
    output$scatterPlot <- renderPlot({
        req(dataInput())
        df <- dataInput()
        plot(df[[1]], df[[2]], xlab = names(df)[1], ylab = names(df)[2], 
             main = "Scatter Plot of Data", col = 'blue', pch = 19)
    })

    output$lmPlot <- renderPlot({
        lm_model <- modelData()
        req(lm_model)
        df <- dataInput()
        
        plot(df[[1]], df[[2]], xlab = names(df)[1], ylab = names(df)[2], 
             main = "Linear Model Overlay", col = 'blue', pch = 19)
        abline(lm_model, col = 'red', lwd = 2)
    })

    output$modelSummary <- renderPrint({
        lm_model <- modelData()
        req(lm_model)
        
        slope <- coef(lm_model)[2]
        intercept <- coef(lm_model)[1]
        r_squared <- summary(lm_model)$r.squared
        
        cat("Slope:", slope, "\n")
        cat("Intercept:", intercept, "\n")
        cat("R-squared:", r_squared, "\n")
    })

    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            "regrex1.csv"
        },
        content = function(file) {
            write.csv(dataInput(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
