#
#
# Лабораторна робота 5
# author: "Юрій Харченко"
#
# date: "11/30/2020"
#

library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Лабораторна робота 5"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("selectCountry",
                        label = "Виберіть країну",
                        choices = c("ukr", "pol", "rus"),
                        selected = "ukr"
                    ),
            selectInput("selectType",
                        label = "Виберіть тип графіку",
                        choices = c("l", "p", "b", "c", "o", "s", "h", "n"),
                        selected = "l"
            )
        ),


        mainPanel(
           plotOutput("plot"),
           plotOutput("hist"),
           plotOutput("boxplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    pop_totl <- read.csv("pop_totl.csv")

    a <- reactive({
        list(
            xy = pop_totl %>%
                select(year, input$selectCountry) %>%
                    filter(year > 1990, rm.na = TRUE)
        )
    })

    countryName <- function(code) {
        switch(code,
        "ukr" = "Україна",
        "pol" = "Польща",
        "rus" = "Росія"
        )
    }

    countryColor <- function(code) {
        switch(code,
               "ukr" = "green",
               "pol" = "red",
               "rus" = "blue"
        )
    }

    output$plot <- renderPlot({
        country <- input$selectCountry
        params <- a()
        d <- params$xy
        x <- d$year
        y <- d[[country]]

        plot(
            x,
            y/1000000,
            main = paste("Населення: ", countryName(country)),
            ylab = "млн.чол",
            xlab = "роки",
            type = input$selectType,
            col = countryColor(country)
        )
    })

    output$hist <- renderPlot({
        country <- input$selectCountry
        params <- a()
        d <- params$xy
        y <- d[[country]]

        hist(
            y,
            main = paste("Гістограма частот: ", countryName(country)),
            col = countryColor(country)
        )
    })

    output$boxplot <- renderPlot({
        country <- input$selectCountry
        params <- a()
        d <- params$xy
        y <- d[[country]]

        boxplot(
            y/1000000,
            main = paste("Діаграма розмаху: ", countryName(country)),
            col = countryColor(country)
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
