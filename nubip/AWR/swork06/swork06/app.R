#
#
# Самостійна робота 6
# author: "Юрій Харченко"
#
# ! Для виконання потрібен доступ в Інтернет !
#
# date: "12/03/2020"
#


library(shiny)
library(dplyr)
library(WDI)
library(DT)

vars <- c(
    "NY.GDP.PCAP.CD",
    "NY.GDP.PETR.RT.ZS",
    "NY.GDP.TOTL.RT.ZS",
    "NY.GDP.NGAS.RT.ZS",
    "NY.GDP.FRST.RT.ZS",
    "NY.GDP.COAL.RT.ZS"
)

varNames <- c(
    "GDP per capita (current US$)",
    "Oil rents (% of GDP)",
    "Total natural resources rents (% of GDP)",
    "Natural gas rents (% of GDP)",
    "Forest rents (% of GDP)",
    "Coal rents (% of GDP)"
)

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
            textInput("year", "Year",value = 2015),
            selectInput("selectX",
                        label = "Select  X",
                        choices = varNames,
                        selected = varNames[2]
            ),
            selectInput("selectY",
                        label = "Select Y",
                        choices = varNames,
                        selected = varNames[3]
            ),
        ),

        # Show
        mainPanel(
            DT::dataTableOutput("dt"),
            plotOutput("plot"),
            verbatimTextOutput("textCor"),
            verbatimTextOutput("textSum"),
            verbatimTextOutput("textAnova"),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    a <- reactive({
        x <- WDI(country="all", indicator=getCode(input$selectX), start=input$year, end=input$year, extra=TRUE, cache=NULL) %>%
            filter(region != "Aggregates") %>%
            arrange(iso2c)
        y <- WDI(country="all", indicator=getCode(input$selectY), start=input$year, end=input$year, extra=TRUE, cache=NULL) %>%
            filter(region != "Aggregates") %>%
            arrange(iso2c)
        list(
            xy = inner_join(x, select(y, iso2c, getCode(input$selectY)), by = c("iso2c" = "iso2c"))
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
            main = paste("X:", x_var,"->", "Y:", y_var),
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
            "Correlation:", cor(x, y, use = "complete.obs")
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

    output$dt <- DT::renderDataTable({
        params <- a()
        d <- params$xy %>% select(iso2c, country,
                                  getCode(input$selectX),
                                  getCode(input$selectY))
        names(d) <- c("ISO", "Country", input$selectX, input$selectY)
        datatable(d)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
