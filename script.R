library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI
ui <- fluidPage(

    # App title
    titlePanel("Monthly Expenses Calculator"),

    # Sidebar layout
    sidebarLayout(

        # Sidebar panel
        sidebarPanel(

            # Select month input
            selectInput("month", "Select month:", choices = month.name),

            # Expenses input fields
            numericInput("rent", "Rent/mortgage payment:", value = 0, min = 0),
            numericInput("utilities", "Utilities:", value = 0, min = 0),
            numericInput("groceries", "Groceries:", value = 0, min = 0),
            numericInput("transportation", "Transportation:", value = 0, min = 0),
            numericInput("entertainment", "Entertainment:", value = 0, min = 0),

            # Submit and Reset buttons
            fluidRow(
                column(6, offset = 3,
                       actionButton("submit", "Submit", class = "btn-primary")
                )
            ),
            br(),
            fluidRow(
                column(6, offset = 3,
                       actionButton("reset", "Reset", class = "btn-outline-secondary")
                )
            )
        ),

        # Main panel
        mainPanel(

            # Total monthly expenses
            h4("Monthly Expenses:"),
            div(style = "text-align: center; font-size: 2em; font-weight: bold; color: #482A5A",
                textOutput("total")),

            # Bar plot of monthly expenses breakdown
            plotOutput("expenses_plot", height = "300px"),

            # Table of monthly expenses by category
            tableOutput("expenses_table")
        )
    )
)

# Define server
server <- function(input, output, session) {

    # Reactive value to store expenses data
    expenses <- reactiveVal(data.frame(month = character(), rent = numeric(), utilities = numeric(), groceries = numeric(), transportation = numeric(), entertainment = numeric()))

    # Update expenses data on submit button click
    observeEvent(input$submit, {
        new_row <- data.frame(
            month = input$month,
            rent = input$rent,
            utilities = input$utilities,
            groceries = input$groceries,
            transportation = input$transportation,
            entertainment = input$entertainment
        )
        expenses_data <- expenses()
        expenses_data <- bind_rows(expenses_data, new_row)
        expenses_data <- expenses_data %>% arrange(month)
        expenses(expenses_data)
    })

    # Reset expenses data on reset button click
    observeEvent(input$reset, {
        expenses(data.frame(month = character(), rent = numeric(), utilities = numeric(), groceries = numeric(), transportation = numeric(), entertainment = numeric()))
    })

    # Render total monthly expenses
    output$total <- renderText({
        expenses_sum <- sum(expenses() %>% select(-month) %>% unlist())
        paste("£", round(expenses_sum, 2))
    })

    # Render bar plot of monthly expenses breakdown
    output$expenses_plot <- renderPlot({
        expenses_melted <- expenses() %>%
            pivot_longer(cols = rent:entertainment, names_to = "category", values_to = "expenses") %>%
            mutate(month = factor(month, levels = month.name))
        ggplot(expenses_melted, aes(x = month, y = expenses, fill = category)) +
            geom_col() +
            labs(x = "Month", y = "Expenses", title = "Monthly Expenses Breakdown") +
            scale_fill_manual(values = c("#ffb3ba", "#ffdfba", "#ffffba", "#baffc9", "#bae1ff")) +
            theme_minimal()
    })

    output$expenses_table <- renderTable({
        expenses() %>%
            pivot_longer(cols = rent:entertainment, names_to = "category", values_to = "expenses") %>%
            select(month, category, expenses) %>%
            group_by(month, category) %>%
            summarise(expenses = sum(expenses)) %>%
            pivot_wider(names_from = category, values_from = expenses)
    })
}

shinyApp(ui = ui, server = server)
