#
#
# Лабораторна робота 6
#
# Для виконання потрібен доступ в Інтернет
#
# author: "Юрій Харченко"
#
# date: "12/03/2020"
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
    titlePanel("Лабораторна робота 6"),

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
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot"),
            verbatimTextOutput("textCor"),
            verbatimTextOutput("textSum"),
            verbatimTextOutput("textAnova")
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

    output$plot <- renderPlot({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        d <- params$xy
        x <- d[[getCode(x_var)]]
        y <- d[[getCode(y_var)]]

        model <- lm(y ~ x)

        plot(
            x,
            y,
            main = paste("Графік,", "x:", x_var,"->", "y:", y_var),
            ylab = y_var,
            xlab = x_var,
            type = "p",
            col = "red"
        )

        abline(model,lwd=2)
    })

    output$textCor <- renderPrint({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        d <- params$xy
        x <- d[[getCode(x_var)]]
        y <- d[[getCode(y_var)]]

        model <- lm(x ~ y)

        paste(
            "Кореляція:", cor(x, y, use = "complete.obs")
        )

    })

    output$textSum <- renderPrint({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        d <- params$xy
        x <- d[[getCode(x_var)]]
        y <- d[[getCode(y_var)]]

        model <- lm(y ~ x)

        summary(model)


    })

    output$textAnova <- renderPrint({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        d <- params$xy
        x <- d[[getCode(x_var)]]
        y <- d[[getCode(y_var)]]

        model <- lm(y ~ x)

        anova(model)


    })
}

# Run the application
shinyApp(ui = ui, server = server)
