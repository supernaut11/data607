library('curl')
library('shiny')
library('tidyverse')

# Set up the user interface for the shiny app
ui <- fluidPage(
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    tableOutput("table")
)

# Set up the server functionality and input/output mappings for the shiny app
server <- function(input, output, session) {
    wine_data <- reactive({
        read_csv(curl('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'), col_names = FALSE)
    })

    output$table <- renderTable({
        wine_data()
    })
}

# Launch the shiny app
shinyApp(ui, server)
