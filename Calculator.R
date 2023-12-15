# Define UI for application of a simple calculator
ui <- fluidPage(
  # Application title
  titlePanel("A Very Simple Calculator"),
  # This is a Simple Calculator where two numbers are received as input. 
  # Enter the first two numbers
  mainPanel(
    numericInput("Num1", "Enter the First Number", 0),
    numericInput("Num2", "Enter the Second Number", 0),
    selectInput("Operators", "Choose the Operator", 
                choices = c("ADD", "SUB", "MUL", "DIV", "LOG", "LN", "SIN", "COS", "TAN", "SQRT")),
    checkboxInput("Shift", "Shift", FALSE),
    checkboxInput("Alpha", "Alpha", FALSE),
    actionButton("RecallBtn", "RCL"),  # Added RCL button
    textOutput("Output")
  )
)

# Define server logic required to perform the operations of the calculator
server <- function(input, output) {
  # Variable to store the recalled value
  recalled_value <- reactiveVal(0)
  
  # Update recalled_value when RecallBtn is clicked
  observeEvent(input$RecallBtn, {
    recalled_value(input$Num1)
  })
  
  output$Output <- renderText({
    operator <- input$Operators
    shift <- input$Shift
    alpha <- input$Alpha
    
    # Modify operations based on the shift and alpha states
    if (shift) {
      operator <- paste0("SHIFT_", operator)
    }
    
    if (alpha) {
      operator <- paste0("ALPHA_", operator)
    }
    
    # Perform the selected operation
    result <- switch(operator,
                     "ADD" = input$Num1 + input$Num2,
                     "SUB" = input$Num1 - input$Num2,
                     "MUL" = input$Num1 * input$Num2,
                     "DIV" = input$Num1 / input$Num2,
                     "LOG" = log(input$Num1, base = 10),
                     "LN"  = log(input$Num1),
                     "SIN" = sin(input$Num1),
                     "COS" = cos(input$Num1),
                     "TAN" = tan(input$Num1),
                     "SQRT" = sqrt(input$Num1),
                     "SHIFT_ADD" = input$Num1 + input$Num2 + 10,
                     "SHIFT_SUB" = input$Num1 - input$Num2 + 10,
                     "SHIFT_MUL" = input$Num1 * input$Num2 + 10,
                     "SHIFT_DIV" = input$Num1 / input$Num2 + 10,
                     "ALPHA_ADD" = input$Num1 + input$Num2 - 5,
                     "ALPHA_SUB" = input$Num1 - input$Num2 - 5,
                     "ALPHA_MUL" = input$Num1 * input$Num2 - 5,
                     "ALPHA_DIV" = input$Num1 / input$Num2 - 5,
                     "RCL" = recalled_value()  # Recall stored value
    )
    
    # Check for NaN result and return an error message if necessary
    if (is.nan(result)) {
      return("Error: Invalid operation")
    } else {
      return(result)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)