library(shiny)
library(ggplot2)

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
            numericInput("entertainment", "Entertainment:", value = 0, min = 0),
            selectInput("month", "Select Month:", choices = month.name, selected = month.name[1]),
            actionButton("add_expenses", "Add Expenses"),
            actionButton("reset", "Reset")
        ),
        mainPanel(
            h4("Monthly Summary:"),
            verbatimTextOutput("summary"),
            plotOutput("expenses_chart")
        )
    )
)

# Define server
server <- function(input, output) {

    # Initialize data frame to store expenses
    expenses_data <- reactiveValues(data = data.frame(month = character(),
                                                      rent = numeric(),
                                                      utilities = numeric(),
                                                      food = numeric(),
                                                      transportation = numeric(),
                                                      entertainment = numeric()))

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

    # Add expenses for selected month
    observeEvent(input$add_expenses, {
        expenses_data$data[nrow(expenses_data$data) + 1, ] <- list(input$month, input$rent, input$utilities, input$food, input$transportation, input$entertainment)
    })

    # Reset expenses
    observeEvent(input$reset, {
        expenses_data$data <- data.frame(month = character(),
                                         rent = numeric(),
                                         utilities = numeric(),
                                         food = numeric(),
                                         transportation = numeric(),
                                         entertainment = numeric())
    })

    # Plot total expenses over time
    output$expenses_chart <- renderPlot({
        ggplot(data = expenses_data$data, aes(x = month)) +
            geom_col(aes(y = rent, fill = "Rent/Mortgage"), width = 0.7) +
            geom_col(aes(y = utilities, fill = "Utilities"), width = 0.7) +
            geom_col(aes(y = food, fill = "Food/Groceries"), width = 0.7) +
            geom_col(aes(y = transportation, fill = "Transportation"), width = 0.7) +
            geom_col(aes(y = entertainment, fill = "Entertainment"), width = 0.7) +
            labs(x = "Month", y = "Total Expenses", title = "Total Expenses Over Time") +
            scale_x_discrete(limits = month.name, expand = c(0, 0)) +
            scale_fill_manual(values = c("red", "green", "blue", "orange", "purple"),
