#
#
# Самостійна робота 7
# author: "Юрій Харченко"
#
# ! Для виконання потрібен доступ в Інтернет !
#
# date: "12/06/2020"
#

library(shiny)
library(tidymodels)
library(WDI)
library(DT)

vars <- c(
    "AG.LND.TOTL.K2",
    "AG.LND.AGRI.ZS",
    #"AG.LND.IRIG.AG.ZS",
    "AG.LND.ARBL.ZS",
    "AG.LND.ARBL.HA.PC",
    "AG.YLD.CREL.KG",
    "SP.RUR.TOTL",
    "SP.RUR.TOTL.ZS"
)
varNames <-c(
    "Land area (sq km)",
    "Agricultural land (% of land area)",
    #  "Agricultural irrigated land (% of total agricultural land)",
    "Arable land (% of land area)",
    "Arable land (hectares per person)",
    "Cereal yield (kg per hectare)",
    "Rural population",
    "Rural population (% of total population)"
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
    titlePanel("Самостійна робота 7. Кластеринг"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("year", "Year",value = 2014),
            sliderInput("n", "N centers",
                        value = 9,
                        min = 1,
                        max = 20
            ),
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
            verbatimTextOutput("text"),
            DT::dataTableOutput("dt"),
            plotOutput("plot"),
            DT::dataTableOutput("textAugment"),
            DT::dataTableOutput("textTidy")
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

    output$text <- renderText({

        paste(

            "Для кластеризації використано функцію 'kmeans'",
            "Результати оформлено за допомогою пакетів 'tidymodels' та 'DT'",
            "Дані світового банку завантажено за допомогою пакету 'WDI'",
            "",
            "Інтерфейс дозволяє вибирити рік спостережень та змінні для аналізу.",
            "Встановлювати кількість центрів кластеризації",
            sep = "\n"
            )
    })

    output$plot <- renderPlot({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        df <- params$xy

        dfc <- select(df, country, getCode(x_var), getCode(y_var)) %>%
            na.omit()

        cl <- dfc %>%
            select(getCode(x_var), getCode(y_var)) %>%
            kmeans(input$n)
        plot(select(dfc,  getCode(x_var), getCode(y_var)),
             xlab = x_var,
             ylab = y_var,
             col = cl$cluster)
        points(cl$centers,
               col = 1:length(cl$cluster), pch = 8, cex = 2)

    })

    output$textAugment <- DT::renderDataTable({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        df <- params$xy

        dfc <- select(df, country, getCode(x_var), getCode(y_var)) %>%
            na.omit()

        cl <- dfc %>%
            select(getCode(x_var), getCode(y_var)) %>%
            kmeans(input$n)

        augment(cl, dfc)
    })

    output$textTidy <- DT::renderDataTable({
        x_var <- input$selectX
        y_var <- input$selectY
        params <- a()
        df <- params$xy

        dfc <- select(df, country, getCode(x_var), getCode(y_var)) %>%
            na.omit()

        cl <- dfc %>%
            select(getCode(x_var), getCode(y_var)) %>%
            kmeans(input$n)
        tidy(cl)
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
