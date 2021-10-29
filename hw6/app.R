library('curl')
library('shiny')
library('tidyverse')

COLUMN_LABELS = c('Cultivar', 'Alcohol', 'Malic Acid', 'Ash', 'Alcalinity of Ash', 'Magnesium',
                  'Total Phenols', 'Flavanoids', 'Nonflavanoid Phenols', 'Proanthocyanins',
                  'Color Intensity', 'Hue', 'OD280/OD315 of Diluted Wines', 'Proline')

# Set up the user interface for the shiny app
ui <- fluidPage(
    titlePanel('Welcome to Wine-O!'),
    p('The best place to go for analysis of the latest wines!'),
    h1('Our Latest Analysis'),
    p('The interactive dashboards below represent our latest wine analysis. Interact with the plots and learn more about the wines you love!'),
    h1('Data Explorer'),
    p('At Wine-O, transparency is one of our founding principles. For this reason, our users have full access to the data that drive our analytics. Please use the table below to view our data for yourself!'),
    selectInput('cultivar', 'Select a cultivar:', choices = c('1', '2', '3')),
    dataTableOutput("table")
)

# Set up the server functionality and input/output mappings for the shiny app
server <- function(input, output, session) {
    wine_data <- reactive({
        read_csv(curl('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'),
	         col_names = COLUMN_LABELS) %>%
	    mutate(Cultivar = as.integer(Cultivar))
    })

    output$pretable_text <- renderText('At Wine-O, we value transparency in our reporting. For this reason, we allow users to view the data that drive our analytics. Please use the table below to view our data yourself!')

    output$table <- renderDataTable({
        wine_data() %>%
            filter(Cultivar == input$cultivar) %>%
	    select(Alcohol:Proline)
    }, options = list(pageLength = 5))
}

# Launch the shiny app
shinyApp(ui, server)
