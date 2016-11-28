library(shiny)
library(shinythemes)

shinyApp(
  ui = navbarPage(
    theme = shinytheme("united"),
    title = 'MALTEM costing applicaiton',
    tabPanel('Home',
             textInput('some_text', 'Some text input'),
             fluidRow(column(10,
                             p('Some very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very wide text.'),
                             plotOutput('plot1')),
                      column(2,
                             p('Some less wide text. Not wide at all.')))),
    tabPanel('Visualizations',
             fluidRow(plotOutput('plot2'))),
    tabPanel('Data')
  ), 
  server = function(input, output) {
    output$plot1 <- renderPlot({
      plot((1:30)^4, type = 'l', main = input$some_text)
    })    
    output$plot2 <- renderPlot({
      barplot((1:30)^3)
    })
  }
)
