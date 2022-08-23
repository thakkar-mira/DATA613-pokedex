ui <- fluidPage(
  theme = bs_theme(version = version_default(), "vapor"),
  titlePanel("Generation I Pokédex"),
  img(src = "haunter.gif", height = 100, width = 100),
  img(src = "bulbasaur.gif", height = 100, width = 100, align = "right"),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Welcome!",
                           img(src = "empty2.png", height = 50, width = 50),
                           uiOutput("markdown"),
                           img(src = "pikachu.png", align = "bottom", height = 100, width = 100)),
                  tabPanel("Pokédex",
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput("markdown4")  
                             ),
                             mainPanel(
                               img(src = "empty2.png", height = 50, width = 50),
                               dataTableOutput("dynamic"))
                           )),
                  tabPanel("Poké-Pics",
                           img(src = "empty2.png", height = 50, width = 50),
                           uiOutput("markdown2"),
                           img(src = "empty2.png", height = 50, width = 50),
                           fluidRow(
                             column(5,
                                    selectInput("ppokemon", "Normal Pokémon", 
                                                choices = c("select a pokémon..." = "", pokemon$Name)),
                                    uiOutput("normalimage")),
                             
                             column(5,
                                    selectInput("spokemon", "Shiny Pokémon", 
                                                choices = c("select a pokémon..." = "", pokemon$Name)),
                                    uiOutput("shinyimage"))
                           ),
                  ),
                  tabPanel("Poké-Plots",
                           img(src = "empty2.png", height = 50, width = 50),  
                        sidebarLayout(
                           sidebarPanel(
                           selectInput("xcol", "X Variable", choices = names(pokemon)),
                           selectInput("ycol", "Y Variable", choices = names(pokemon)),
                           img(src = "mewyy.gif", height = 200, width = 200, align = "center")
                           ),
                        mainPanel(
                           plotlyOutput(outputId = "scatterplots"),
                           img(src = "empty.png", align = "bottom", height = 50, width = 50)
                           ))),
                
                  tabPanel("Resources",
                           uiOutput("markdown3"),
                           img(src = "charmeleon.gif", align = "bottom", height = 90, width = 90))
                  
      )))


server <- function(input, output) {
  
  output$markdown <- renderUI({
    includeMarkdown("markdown.md")
  })
  
  output$markdown2 <- renderUI({
    includeMarkdown("markdown2.md")
  })
  
  output$markdown3 <- renderUI({
    includeMarkdown("markdown3.md")
  })
  
  output$markdown4 <- renderUI({
    includeMarkdown("markdown4.md")
  })
  
  x <- reactive({
    pokemon[,input$xcol]
  })
  
  y <- reactive({
    pokemon[,input$ycol]
  })
  output$scatterplots <- renderPlotly({
    plot_ly(pokemon, x = x(), y = y(), color = Name,  type = "scatter", mode = "markers")
  })
  
  n_id <- reactive({
    shiny::validate(
      shiny::need(input$ppokemon, "Select a Pokémon to see its normal form!")
    )
    pokemon[pokemon$Name == tolower(input$ppokemon), "Name"]
  })
  
  n_url <- reactive({
    paste0("https://img.pokemondb.net/sprites/home/normal/2x/", n_id(), ".jpg")
  })
  
  output$normalimage <- renderUI({
    tags$img(src = n_url())
  })
  s_id <- reactive({
    shiny::validate(
      shiny::need(input$spokemon, "Select a Pokémon to see its shiny form!")
    )
    pokemon[pokemon$Name == tolower(input$spokemon), "Name"]
  })
  
  s_url <- reactive({
    paste0("https://img.pokemondb.net/sprites/home/shiny/2x/", s_id(), ".jpg")
  })
  
  output$shinyimage <- renderUI({
    tags$img(src = s_url())
  })
  output$dynamic <- renderDataTable({
    pokemon1
  })
}


shinyApp(ui = ui, server = server) 
