
ui <- fluidPage(
  setBackgroundColor("pink"),
  titlePanel("RShiny Gen I Pokedex!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xcol", "X Variable", choices = names(pokemon)),
      selectInput("ycol", "Y Variable", choices = names(pokemon))
    ),
    mainPanel(
      img(src='myImage.png', align = "right")
      tabsetPanel(type = "tab",
                  tabPanel("Poke-Stat Scatterplots", plotlyOutput(outputId = "scatterplots")),
                  tabPanel("Boxplots", plotOutput(outputId = "boxplots")),
                  tabPanel("Data Table", dataTableOutput("dynamic"))
                  
      ))))


server <- function(input, output) {
  x <- reactive({
    pokemon[,input$xcol]
  })
  
  y <- reactive({
    pokemon[,input$ycol]
  })
  output$scatterplots <- renderPlotly({
    plot_ly(pokemon, x = x(), y = y(), color = Name,  type = "scatter", mode = "markers")
  })
  output$boxplots <- renderPlot({
    ggplot(diamonds, mapping = aes(x = .data[[input$DVvar]], fill = cut)) +
      geom_boxplot(color = "black") +
      coord_flip() +
      ggtitle("boxplot")
  })
  output$dynamic <- renderDataTable({
    diamonds1
  })
}

shinyApp(ui = ui, server = server) 

