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
                         selected = "head")
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
    
    # Model the data when button is clicked
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
    
    # Render scatter plot
    output$scatterPlot <- renderPlot({
        req(dataInput())
        df <- dataInput()
        plot(df[[1]], df[[2]], 
             xlab = names(df)[1], 
             ylab = names(df)[2], 
             main = "Scatter Plot of Data", 
             col = 'blue', pch = 19)
    })

    # Render linear model plot
    output$lmPlot <- renderPlot({
        lm_model <- modelData()
        req(lm_model)
        df <- dataInput()
        
        plot(df[[1]], df[[2]], 
             xlab = names(df)[1], 
             ylab = names(df)[2], 
             main = "Linear Model Overlay", 
             col = 'blue', pch = 19)
        abline(lm_model, col = 'red', lwd = 2)
    })

    # Output model summary: slope, intercept, and correlation coefficient
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

    # Display the contents of the uploaded data
    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server