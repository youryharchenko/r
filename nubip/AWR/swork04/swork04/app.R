#
#
# Самостійна робота 4
# author: "Юрій Харченко"
#
# date: "11/28/2020"
#

library(shiny)
library(queueing)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Імітаційне моделювання роботи АТС"),

    # Sidebar with sliders input for parameters
    sidebarLayout(
        sidebarPanel(
            sliderInput("c",
                        "Каналів(c):",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("lambda",
                        "Вхідний потік(lambda):",
                        min = 1,
                        max = 40,
                        value = 10),
            sliderInput("mu",
                        "Час обслуговування(mu):",
                        min = 1,
                        max = 15,
                        value = 5)
        ),

        # Show reports
        mainPanel(
           verbatimTextOutput("textReport"),
           plotOutput("plotInpReport"),
           plotOutput("plotProcReport")
        )
    )
)

# Define server logic
server <- function(input, output) {

    a <- reactive({
        # Вхідний потік M/M/с/K
        lambda <- input$lambda
        c <- input$c
        mu <- input$mu
        input_mmck <- NewInput.MMCK(lambda = lambda, mu = mu, c = c, k = c)

        CheckInput(input_mmck)

        # Create queue class object
        QueueingModel(input_mmck)

    })

    print_report <- function(param) {
        pander(Report(param))
    }

    plot_inp_report <- function(param) {

        curve(dpois(x, input$lambda),
              from = 0,
              to = 60,
              type = "l",
              ylim = c(0, 0.3),
              lwd = 2,
              xlab = "Число дзвінків",
              ylab = "Імовірність",
              main = paste("Розподіл Пуасона для вхідних дзвінків, lambda:", input$lambda),
              n = 61)
        abline(h = 0)
    }

    plot_proc_report <- function(param) {

        curve(dexp(x, input$mu),
              from = 0,
              to = 2,
              type = "l",
              ylim = c(0, 1.2),
              lwd = 2,
              xlab = "Тривалість дзвінків",
              ylab = "Імовірність",
              main = paste("Експоненціальний розподіл тривалості дзвінків, mu:", input$mu)
              )
        abline(h = 0)
    }

    output$textReport <- renderPrint({
        print_report(a())
    })

    output$plotInpReport <- renderPlot({
        plot_inp_report(a())
    })

    output$plotProcReport <- renderPlot({
        plot_proc_report(a())
    })
}

# Run the application
shinyApp(ui = ui, server = server)
