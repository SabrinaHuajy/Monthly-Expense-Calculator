library(shiny)

# Define UI
ui <- fluidPage(
    titlePanel("Monthly Expense Calculator"),
    sidebarLayout(
        sidebarPanel(
            numericInput("income", "Monthly Income:", value = 0, min = 0),
            numericInput("rent", "Rent/Mortgage:", value = 0, min = 0),
            numericInput("utilities", "Utilities:", value = 0, min = 0),
            numericInput("food", "Food/Groceries:", value = 0, min = 0),
            numericInput("transportation", "Transportation:", value = 0, min = 0),
            numericInput("entertainment", "Entertainment:", value = 0, min = 0)
        ),
        mainPanel(
            h4("Monthly Summary:"),
            verbatimTextOutput("summary")
        )
    )
)

# Define server
server <- function(input, output) {

    # Calculate total expenses
    expenses <- reactive({
        input$rent + input$utilities + input$food + input$transportation + input$entertainment
    })

    # Calculate amount left over
    left_over <- reactive({
        input$income - expenses()
    })

    # Output summary
    output$summary <- renderText({
        paste("Total Income: $", input$income, "\n",
              "Total Expenses: $", expenses(), "\n",
              "Amount Left Over: $", left_over())
    })
}

# Run the app
shinyApp(ui, server)
