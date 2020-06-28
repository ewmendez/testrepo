library(DT)
library(plotly)
collist <- c('Alcoholic.Beverages', 'Animal.Products', 'Animal.fats',
             'Cereals...Excluding.Beer', 'Eggs', 'Fish..Seafood', 'Fruits...Excluding.Wine',
             'Meat', 'Milk...Excluding.Butter', 'Miscellaneous', 'Offals', 'Oilcrops',
             'Pulses', 'Spices', 'Starchy.Roots', 'Stimulants', 'Sugar.Crops', 'Sugar...Sweeteners',
             'Treenuts', 'Vegetal.Products', 'Vegetable.Oils', 'Vegetables')
collist2 <- c('Obesity','Confirmed', 'Deaths', 'Recovered', 'Active', 'Population')

ui <- navbarPage("Covid ~ Food Intake Relationship",
           tabPanel("World Map View",
                        sidebarPanel(
                            selectInput('x', '% of Daily Caloric Intake by Type - SELECTION', collist)
                            )
                        ,
                        mainPanel(
                            plotOutput("plot1"), DT::dataTableOutput("mytable")
                        )
                    
                    ),
           tabPanel("Plotting Against Covid Metric", 
                    mainPanel(plotOutput("scatterplot", click='plot_click'),
                              verbatimTextOutput("info")),
                    verbatimTextOutput("click"), 
                        sidebarPanel(selectInput(
                            inputId = "xaxis", 
                            label = "Choose a Food Intake Variable for the X-axis", 
                            choices = collist
                        ),
                        selectInput(
                            inputId = "yaxis", 
                            label = "Choose a Covid Related Variable for the Y-axis", 
                            choices = collist2
                        )))
           )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    library(shiny)
    library(tidyverse)
    library(ggplot2)
    library(dplyr)
    library(rworldmap)
    library(maps)
    
    df <- read.csv('https://raw.githubusercontent.com/ewmendez/testrepo/master/datasets_618335_1272299_Food_Supply_kcal_Data.csv')
    
    df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
    
    output$plot1 <- renderPlot({
        
        df2 <- df[c('Country', input$x)]
        
        test<- joinCountryData2Map(
            df2, joinCode='NAME', nameJoinColumn='Country'
        )
        mapCountryData(test, nameColumnToPlot = input$x, colourPalette = "heat", 
                       mapTitle='% of Daily Caloric Intake by Type',missingCountryCol = 'grey')
    })
    
    output$mytable = DT::renderDataTable({
        df[c('Country', input$x)]
    })
    output$scatterplot <- renderPlot({
        req(input$xaxis)
        req(input$yaxis)
        ggplot(df, aes_string(x = paste0("`", input$xaxis, "`"), 
                                  y = paste0("`", input$yaxis, "`"))) + geom_point() + geom_smooth(method=lm) + ggtitle("Food to Covid Metric Relationship", subtitle="*each dot represents a single country*") + theme_classic()
    })
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
