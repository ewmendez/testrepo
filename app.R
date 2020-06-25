library(DT)
collist <- c('Alcoholic.Beverages', 'Animal.Products', 'Animal.fats', 'Aquatic.Products..Other',
             'Cereals...Excluding.Beer', 'Eggs', 'Fish..Seafood', 'Fruits...Excluding.Wine',
             'Meat', 'Milk...Excluding.Butter', 'Miscellaneous', 'Offals', 'Oilcrops',
             'Pulses', 'Spices', 'Starchy.Roots', 'Stimulants', 'Sugar.Crops', 'Sugar...Sweeteners',
             'Treenuts', 'Vegetal.Products', 'Vegetable.Oils', 'Vegetables')
collist2 <- c('Obesity', 'Undernourished','Confirmed', 'Deaths', 'Recovered', 'Active', 'Population')

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
    
    # Application title
    headerPanel("Covid ~ Food Intake Relationship App"),
    sidebarPanel(
        selectInput('x', 'Variable', collist)
    ),
    mainPanel(
        plotOutput('plot1'),
        DT::dataTableOutput("mytable")
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    library(shiny)
    library(tidyverse)
    library(ggplot2)
    library(rworldmap)
    library(maps)
    
    df <- read.csv('https://raw.githubusercontent.com/ewmendez/testrepo/master/datasets_618335_1272299_Food_Supply_kcal_Data.csv')
    
    output$plot1 <- renderPlot({
        
        df2 <- df[c('Country', input$x)]
        
        test<- joinCountryData2Map(
            df2, joinCode='NAME', nameJoinColumn='Country'
        )
        mapCountryData(test, nameColumnToPlot = input$x)
    })
    
    output$mytable = DT::renderDataTable({
        df[c('Country', input$x)]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
