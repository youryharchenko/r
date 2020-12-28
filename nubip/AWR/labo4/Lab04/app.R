#
# Лабораторна робота 4
# author: "Юрій Харченко"
#
# date: "11/27/2020"
#


library(shiny)


# Визначаємо графічний користувацький інтерфецс
ui <- fluidPage(

    # Заголовок
    titlePanel("Лабораторна робота 4"),

    # Бокова панель з інструментами
    sidebarLayout(
        sidebarPanel(
            sliderInput("N",
                        "N",
                        min = 1,
                        max = 10,
                        value = 5),

        ),

        # Головна панель
        mainPanel(
            plotOutput("unifPlotCum"),
            plotOutput("unifPlotDens"),
            plotOutput("normPlotCum"),
            plotOutput("normPlotDens"),
            verbatimTextOutput("chisqTest"),
            plotOutput("poisPlotCum"),
            plotOutput("poisPlotDens")
        )
    )
)

# Серверна логіка програми
server <- function(input, output) {

    output$unifPlotCum <- renderPlot({
        # Генеруємо рівноміриний розподіл [0; N+10]
        x_unif <- runif(30, min = 0, max = input$N + 10)
        p <- ecdf(x_unif)
        # Виводимо функцію рівноміриного розподілу
        plot(p, main = 'Кумулятивна функція рівноміриного розподілу [0; N + 10]')
    })

    output$unifPlotDens <- renderPlot({
        # Генеруємо рівноміриний розподіл [0; N+10]
        x_unif <- runif(30, min = 0, max = input$N + 10)

        # Виводимо гістограму: Щільність рівноміриного розподілу
        hist(x_unif, freq = FALSE, xlab = 'x', main = 'Щільність рівноміриного розподілу [0; N + 10]', density = 10)
    })

    output$normPlotCum <- renderPlot({
        # Генеруємо нормальний розподіл [0; N+10]
        x_norm <- rnorm(240, mean = input$N, sd = input$N / 10)
        p <- ecdf(x_norm)
        # Виводимо функцію нормального розподілу
        plot(p, main = 'Кумулятивна функція нормального розподілу: mean - N, sd - N/ 10')
    })

    output$normPlotDens <- renderPlot({
        # Генеруємо нормальний розподіл: mean - N, sd - N/ 10
        x_norm <- rnorm(240, mean = input$N, sd = input$N / 10)

        # Виводимо гістограму: Щільність нормального розподілу: mean - N, sd - N/ 10
        hist(x_norm, freq = FALSE, xlab = 'x', main = 'Щільність нормального розподілу: mean - N, sd - N/ 10', density = 10)
    })

    output$chisqTest <- renderPrint({
        k <- 6
        x_norm <- rnorm(240, mean = input$N, sd = input$N / 10)
        x_norm_q <- quantile (x_norm, probs = seq (0,1,0.1))

        x_q <- c (-10, -1.0, 0.5, 2.0, 3.5, 5.0, 12.0)
        x_norm_hist <-hist(x_norm, breaks = x_q, plot = FALSE)

        x_q[1] <- (- Inf)
        x_q[k + 1] <- (+ Inf)

        x_norm_p_theor <- pnorm(x_q, mean = mean(x_norm), sd = sd(x_norm))
        x_norm_p_theor <- (x_norm_p_theor[2:(k + 1)]-x_norm_p_theor[1: k])

        chisq.test (x_norm_hist$counts, p = x_norm_p_theor)

    })

    output$poisPlotCum <- renderPlot({
        # Генеруємо розподіл Пуассона: lambda = N / 2
        x_pois <- rpois(240, lambda = input$N / 2)
        p <- ecdf(x_pois)
        # Виводимо функцію розподілу Пуассона
        plot(p, main = 'Кумулятивна функція розподілу Пуассона: lambda = N / 2')
    })

    output$poisPlotDens <- renderPlot({
        # Генеруємо розподіл Пуассона: mean - N, sd - N/ 10
        x_pois <- rpois(240, input$N / 2)

        # Виводимо гістограму: Щільність розподілу Пуассона:  lambda = N / 2
        hist(x_pois, freq = FALSE, xlab = 'x', main = 'Щільність розподілу Пуассона: lambda = N / 2', density = 10)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
