#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DiagrammeR)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Probe DiagrammeR"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("nr",
                        "Number of rows",
                        min = 2,
                        max = 10,
                        value = 5),
            sliderInput("nc",
                        "Number of columns",
                        min = 2,
                        max = 10,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            grVizOutput("graph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$graph <- renderGrViz({
        nr <- input$nr
        nc <- input$nc
        n <- nr*nc
        all_nodes <- vector("character", n)
        from_to_edges <- matrix("integer", nrow = nr, ncol = nc)
        from_edges <- vector("integer", 0)
        to_edges <- vector("integer", 0)

        k <- 0
        for(i in 1:nr) {
            for(j in 1:nc) {
              k <- k+1
              all_nodes[k] = paste(i,':',j, sep = '')
              from_to_edges[i, j] <- k
            }
        }

        for(i in 1:nr) {
            for(j in 2:nc) {
                from_edges <- c(from_edges, from_to_edges[i, j-1])
                to_edges <- c(to_edges, from_to_edges[i, j])
            }
        }

        for(j in 1:nc) {
            for(i in 2:nr) {
                from_edges <- c(from_edges, from_to_edges[i-1, j])
                to_edges <- c(to_edges, from_to_edges[i, j])
            }
        }

        ndf <-
            create_node_df(
                n = n,
                label = all_nodes,
                type  = "upper",
                style = "filled",
                color = "aqua",
                shape = rep("rectangle", n),
                data = rep(0, n),
                height = 1.2,
                width = 1.2            )

        edf <-
            create_edge_df(
                from = from_edges,
                to = to_edges,
                arrowhead = "none",
                arrowtail = "none",
                dir = "both",
                minlen = 1
            )

        graph <-
            create_graph(
                nodes_df = ndf,
                edges_df = edf
            ) %>%
            set_node_attrs(
                node_attr = "fontname",
                values = "Helvetica"
            ) %>%
            set_edge_attrs(
                edge_attr = "color",
                values = "blue"
            ) %>%
            set_edge_attrs(
                edge_attr = "arrowsize",
                values = 2
            )

            render_graph(graph, layout = 'nicely')

    })
}

# Run the application
shinyApp(ui = ui, server = server)
