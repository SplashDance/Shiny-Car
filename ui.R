proj_path <- paste("~/Desktop/MOOCs/Coursera/Data Science",
                   "9. Developing Data Products/Project/shiny-project", sep='/')
library(shiny)


shinyUI(fluidPage(
    titlePanel("Predicting Resale Value"),

    sidebarLayout(

        sidebarPanel(
            # Select Brand
            selectInput('brand',
                label = 'Choose a Brand:',
                choices = list("Chevy", "Buick", "Cadillac", "Pontiac",
                               "Saab", "Saturn"),
                selected = "Chevy"),

            selectInput('cartype',
                label='Model Type:',
                choices = list("convertible", "coupe", "hatchback",
                               "sedan", "wagon"),
                selected = 'sedan'),

            selectInput('doors',
                label='Number of Doors:',
                choices = list(2, 4),
                selected = 2),

            selectInput('cylinders',
                label = 'Number of Cylinders',
                choices = list(4, 6, 8),
                selected = 4),

            sliderInput('mileage',
                label = 'Mileage:',
                min = 0, max = 60000, value = 0),
            br(),
            br(),
            checkboxGroupInput('addons',
                label = 'Additional Features:',
                choices = list('Upgraded Sound System'='Premium',
                               'Leather Seats' = 'Leather',
                               'Cruise Control' = 'Yes'))),

        mainPanel(
            plotOutput('my.graph'),
            br(),
            h4("Predicted Re-Sale Value:"),
            h1(textOutput("my.pred"))
        )
    )
))
