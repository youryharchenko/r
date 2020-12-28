#
#
# Самостійна робота 6
# author: "Юрій Харченко"
#
# date: "12/01/2020"
#


library(shiny)
library(dplyr)

vars <- c("SP.POP.TOTL","NY.GDP.PCAP.PP.CD","AG.LND.TOTL.K2","AG.LND.AGRI.K2","SP.RUR.TOTL.ZS")
varNames <- c("Населення загалом","ВВП на душу","Площа загалом","Площа с.г","Сільське населення, %")

getName <- function(code) {
    varNames[match(code, vars)]
}

getCode <- function(name) {
    vars[match(name, varNames)]
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Самостійна робота 6"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("selectX",
                        label = "Виберіть змінну X",
                        choices = varNames,
                        selected = varNames[1]
            ),
            selectInput("selectY",
                        label = "Виберіть змінну Y",
                        choices = varNames,
                        selected = varNames[2]
            ),
            selectInput("selectType",
                        label = "Виберіть тип графіку",
                        choices = c("l", "p", "b", "c", "o", "s", "h", "n"),
                        selected = "p"
            )
        ),


        mainPanel(
            plotOutput("plot"),
            textOutput("text"),
            plotOutput("histX"),
            plotOutput("histY"),
            plotOutput("boxplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    all3 <- read.csv("all3.csv")

    a <- reactive({
        list(
            xy = all3 %>%
                select(getCode(input$selectX), getCode(input$selectY))
        )
    })


    # countryColor <- function(code) {
    #     switch(code,
    #            "ukr" = "green",
    #            "pol" = "red",
    #            "rus" = "blue"
    #     )
    # }

    output$plot <- renderPlot({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        d <- params$xy
        x <- d[[getCode(x_var)]]
        y <- d[[getCode(y_var)]]

        plot(
            x,
            y,
            main = paste("Графік,", "x:", x_var,"->", "y:", y_var),
            ylab = y_var,
            xlab = x_var,
            type = input$selectType,
            col = "red"
        )
    })

    output$text <- renderPrint({
        "Помітна зворотня залежність ВВП на душу населення від відсотка сільского населення"
    })

    output$histX <- renderPlot({

        params <- a()
        d <- params$xy
        x <- d[[getCode(input$selectX)]]

        hist(
            x,
            main = paste("Гістограма частот X: ", input$selectX),
            col = "green"
        )
    })

    output$histY <- renderPlot({

        params <- a()
        d <- params$xy
        y <- d[[getCode(input$selectY)]]

        hist(
            y,
            main = paste("Гістограма частот Y: ", input$selectY),
            col = "blue"
        )
    })


    output$boxplot <- renderPlot({

        d <- all3 %>% select(SP.POP.TOTL, region)

        boxplot(
            SP.POP.TOTL ~ region,
            d,
            main = paste("Діаграма розмаху населення"),
            varwidth = TRUE,
            ylab = "Населення",
            xlab = "Регіони"
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
