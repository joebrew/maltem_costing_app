library(shiny)
source('global.R')

shinyApp(
  ui = navbarPage(
    theme = shinytheme("united"),
    title = 'MALTEM costing applicaiton',
    tabPanel('Home',
             textInput('some_text', 'Some text input'),
             sliderInput('year',
                         'Year',
                         min = 2015,
                         max = 2035,
                         value = 2015,
                         step = 1,
                         animate = animationOptions(interval = 400, 
                                                    loop = FALSE, 
                                                    playButton = NULL,
                                                    pauseButton = NULL)),
             fluidRow(column(8,
                             plotOutput('g'),
                             htmlOutput('motion_chart'),
                             plotOutput('plot1')),
                      column(4,
                             p('Some less wide text. Not wide at all.')))),
    tabPanel('Visualizations',
             fluidRow(plotOutput('plot2'))),
    tabPanel('Data')
  ), 
  server = function(input, output) {
    df <- reactive({
      x <- 
        data.frame(year = 2015:2035,
                       val = jitter(rnorm(n = 21), 10),
                       other_val = jitter(1:21, 20),
                       x = jitter((21:1)^2),
                       id = sample(c('a', 'b', 'c'), 21, replace = TRUE)) %>%
        mutate(x = x * (1 + (2 *(as.numeric(factor(id))))))
      # Forward fill
      x <- expand.grid(year = 2015:2035,
                       id = c('a', 'b', 'c')) %>%
        left_join(x,
                  by = c('year', 'id')) %>%
        tidyr::fill(val, other_val, x,
                    .direction = 'down') %>%
        tidyr::fill(val, other_val, x,
                    .direction = 'up') 
      for (j in c('val', 'other_val', 'x')){
        x[,j] <- jitter(x[,j], 20)
      }
      x
    })
    output$plot1 <- renderPlot({
      plot((1:30)^4, type = 'l', main = input$some_text)
    })    
    output$plot2 <- renderPlot({
      barplot((1:30)^3)
    })
    output$motion_chart <- renderGvis({
      gvisMotionChart(data = df(),
                      idvar = 'id',
                      timevar = 'year',
                      xvar = 'other_val',
                      yvar = 'x',
                      sizevar = 'val')
    })
    output$g <- renderPlot({
      ggplot(data = df() %>%
               filter(year <= input$year),
             aes(x = year,
                 y = x,
                 group = id,
                 color = id)) +
        geom_line(alpha = 0.3) +
        geom_point(alpha = 0.3) + 
        geom_smooth(se = FALSE) +
        xlim(2015, 2035) +
        ylim(0, max(df()$x))
      
    })
  }
)
