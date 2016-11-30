source('global.R')
options(scipen = 999)

function(input, output, session) {
  
  output$filter <- renderUI({
    checkboxGroupInput('elimination_activity',
                       label = 'Filter elimination activities',
                       choices = c(sort(unique(df$elimination_activity))),
                       selected = c(sort(unique(df$elimination_activity))))
  })
  
  output$cost_per_output_indicator_input <- renderUI({
    
    if(input$intervention == 'MDA'){
      the_choices <- c('Cost per person tested per round',
                       'Cost per person reached per round',
                       'Cost per person treated per round')  
    } else if(input$intervention == 'REACT'){
      the_choices <- c('Cost per person covered')
    } else if(input$intervention == 'LLIN'){
      the_choices <- c('Cost per net',
                       'Cost per person protected')
    } else if(input$intervention == 'IRS'){
      the_choices <- c('Cost per sprayed structure',
                       'Cost per sprayed house',
                       'Cost per person protected')
    }
    
    
    selectInput('cost_per_output_indicator',
                'Cost per output indicator',
                choices = the_choices,
                selected = the_choices[1])
    
  })
  
  future <- reactive({
    x <- the_future
    # Inflation rate
    x$value <- x$value * (1 + (input$inflation_rate/100))^(x$year - 2015)
    # Discount rate
    x$value <- x$value * (1 - (input$discount_rate/100))^(x$year - 2015)
    x
  })
  
  # Value for outcome
   output$text1<- renderText({
    if(input$cost_per_output_indicator ==
       'Cost per person tested per round'){
        val <- ((df %>% 
          filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
          summarise(x = sum(value)) %>% .$x -
          # DHA PQP cost 2015
          182800) / 2) / 
          28713 # number of people tested
    } else if(input$cost_per_output_indicator ==
              'Cost per person reached per round'){
      val <- ((df %>% 
        filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
        summarise(x = sum(value)) %>% .$x -
        # drug and supplies 2015
        229340.29)/ 2) /
        # number of people reached
        outcomes$value[outcomes$indicator == 'reached']
    } else if(input$cost_per_output_indicator ==
              'Cost per person treated per round'){
      val <- (df %>% 
                filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
                summarise(x = sum(value)) %>% .$x /2)/ outcomes$value[outcomes$indicator == 'treated']
    } else if(input$cost_per_output_indicator ==
              'Cost per person covered'){
      val <- (df %>% 
        filter(elimination_activity == 'REACT', year == 2017, currency == 'USD') %>% 
        summarise(x = sum(value)) %>% .$x) / 
        outcomes$value[outcomes$indicator == 'inhabitants']
    } else if(input$cost_per_output_indicator ==
              'Cost per net'){
      val <- 5.86
    } else if(input$cost_per_output_indicator ==
              'Cost per person protected'){
      val <- (df %>% 
                filter(elimination_activity == 'LLIN', year == 2017, currency == 'USD') %>% 
                summarise(x = sum(value)) %>% .$x) /
        outcomes$value[outcomes$indicator == 'inhabitants']
    } else if(input$cost_per_output_indicator ==
              'Cost per sprayed structure'){
      val <- (df %>% 
                filter(elimination_activity == 'IRS', year == 2015, currency == 'USD') %>% 
                summarise(x = sum(value)) %>% .$x) / 
        outcomes$value[outcomes$indicator == 'structures_sprayed']
    } else if(input$cost_per_output_indicator ==
              'Cost per sprayed house'){
      val <- (df %>% 
                filter(elimination_activity == 'IRS', year == 2015, currency == 'USD') %>% 
                summarise(x = sum(value)) %>% .$x) / 
        outcomes$value[outcomes$indicator == 'houses_sprayed']
    } else if(input$cost_per_output_indicator ==
              'Cost per person protected'){
      val <- (df %>% 
                filter(elimination_activity == 'IRS', year == 2015, currency == 'USD') %>% 
                summarise(x = sum(value)) %>% .$x) / 
        outcomes$value[outcomes$indicator == 'people_irs']
    }  
    val <- round(val, digits = 2)
    val
  })
   
   output$simple_plot <-
     renderPlot({
       # waffle(c('a' = 5, 'b' = 3, 'x' = 12))
       if(input$cost_per_output_indicator ==
          'Cost per person tested per round'){
         val <- ((df %>% 
                    filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
                    summarise(x = sum(value)) %>% .$x -
                    # DHA PQP cost 2015
                    182800) / 2) / 
           28713 # number of people tested
       } else if(input$cost_per_output_indicator ==
                 'Cost per person reached per round'){
         val <- ((df %>% 
                    filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
                    summarise(x = sum(value)) %>% .$x -
                    # drug and supplies 2015
                    229340.29)/ 2) /
           # number of people reached
           outcomes$value[outcomes$indicator == 'reached']
       } else if(input$cost_per_output_indicator ==
                 'Cost per person treated per round'){
         val <- (df %>% 
                   filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
                   summarise(x = sum(value)) %>% .$x /2)/ outcomes$value[outcomes$indicator == 'treated']
       } else if(input$cost_per_output_indicator ==
                 'Cost per person covered'){
         val <- (df %>% 
                   filter(elimination_activity == 'REACT', year == 2017, currency == 'USD') %>% 
                   summarise(x = sum(value)) %>% .$x) / 
           outcomes$value[outcomes$indicator == 'inhabitants']
       } else if(input$cost_per_output_indicator ==
                 'Cost per net'){
         val <- 5.86
       } else if(input$cost_per_output_indicator ==
                 'Cost per person protected'){
         val <- (df %>% 
                   filter(elimination_activity == 'LLIN', year == 2017, currency == 'USD') %>% 
                   summarise(x = sum(value)) %>% .$x) /
           outcomes$value[outcomes$indicator == 'inhabitants']
       } else if(input$cost_per_output_indicator ==
                 'Cost per sprayed structure'){
         val <- (df %>% 
                   filter(elimination_activity == 'IRS', year == 2015, currency == 'USD') %>% 
                   summarise(x = sum(value)) %>% .$x) / 
           outcomes$value[outcomes$indicator == 'structures_sprayed']
       } else if(input$cost_per_output_indicator ==
                 'Cost per sprayed house'){
         val <- (df %>% 
                   filter(elimination_activity == 'IRS', year == 2015, currency == 'USD') %>% 
                   summarise(x = sum(value)) %>% .$x) / 
           outcomes$value[outcomes$indicator == 'houses_sprayed']
       } else if(input$cost_per_output_indicator ==
                 'Cost per person protected'){
         val <- (df %>% 
                   filter(elimination_activity == 'IRS', year == 2015, currency == 'USD') %>% 
                   summarise(x = sum(value)) %>% .$x) / 
           outcomes$value[outcomes$indicator == 'people_irs']
       }  
       val <- round(val, digits = 2)
       
       barplot(val, ylim = c(0,30),
               col = adjustcolor('darkorange', alpha.f = 0.6))
     })
   
   output$text2 <-
     renderText({
       paste0('dollars ',
              gsub('Cost', '', input$cost_per_output_indicator))
     })
  
  output$bars <- renderPlot({
    group_variable <- input$group_variable
    x <- df
    x$x <- x[,group_variable]
    parts <- x %>%
      filter(year %in% input$year) %>%
      filter(elimination_activity %in% input$elimination_activity) %>%
      filter(currency == input$currency) %>%
      group_by(x) %>%#,
      summarise(value = sum(value)) %>%
      mutate(p = value / sum(value) * 100)
    
    ggplot(data = parts,
           aes(x = x,
               y = value)) +
      geom_bar(stat = 'identity',
               fill = 'darkgreen',
               alpha = 0.6) +
      xlab(toupper(gsub('_', ' ', group_variable))) +
      theme_fivethirtyeight() +
      geom_label(aes(label = round(p, digits = 1))) +
      ggtitle(paste0('Breakdown by ', (gsub('_', ' ', group_variable)), ' in ', input$currency),
              '(Percentages on each bar)') +
      ylab(input$currency) +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  # output$motion_chart <- renderGvis({
  #   x <- future() %>% filter(currency == 'USD') #%>%
  #     # mutate(year = as.Date(paste0(year, '-01-01')))
  #   gvisMotionChart(data = x,
  #                   idvar = 'elimination_activity',
  #                   # colorvar = 'elimination_activity',
  #                   # # xvar = 'year',
  #                   timevar = 'year')#,
  #                   # yvar = 'value')
  # })
  output$time_chart <-
    renderPlot({
      x <- future() %>%
        filter(currency == input$currency_for_time) %>%
        filter(year <= input$year_animate)
      if(input$cumulative){
        x <- x %>%
          arrange(year) %>%
          group_by(elimination_activity) %>%
          mutate(value = cumsum(value))
      }
      y <- x %>%
        group_by(year) %>%
        summarise(value = sum(value))
      cols <- colorRampPalette(brewer.pal(n = 9, name = 'Spectral'))(length(unique(x$elimination_activity)))
      ggplot(data = x,
             aes(x = year,
                 y = value)) +
        geom_bar(aes(fill = elimination_activity),
                 stat = 'identity', position = 'stack') +
        # geom_point() +
        # geom_smooth() +
        # geom_line() +
        xlim(2015, 2035) +
        theme_fivethirtyeight() +
        ggtitle('Cost projections') +
        scale_fill_manual(name = 'Activity',
                          values = cols)
        # geom_label(data = y,
        #            aes(x = year,
        #                y = value,
        #                label = round(value)),
        #            size = 2)
    })
  
  output$time_table <-
    DT::renderDataTable({
      x <- future() %>%
        filter(currency == input$currency_for_time) %>%
        group_by(elimination_activity) %>%
        summarize(value = round(sum(value)))
        x$value <- scales::comma(x$value)
        names(x) <- c('Elimination activity',
                      input$currency_for_time)
      x
      })
  
  output$time_table_yearly <-
    DT::renderDataTable({
      x <- future() %>%
        filter(currency == input$currency_for_time) %>%
        group_by(year, elimination_activity) %>%
        summarize(value = round(sum(value)))
      x$value <- scales::comma(x$value)
      names(x) <- c('Year', 'Elimination activity',
                    input$currency_for_time)
      x
    })
  
  output$bars_table <- 
    DT::renderDataTable({
      group_variable <- input$group_variable
      x <- df
      x$x <- x[,group_variable]
      parts <- x %>%
        filter(year %in% input$year) %>%
        filter(elimination_activity %in% input$elimination_activity) %>%
        filter(currency == input$currency) %>%
        group_by(x) %>%#,
        summarise(value = sum(value)) %>%
        mutate(p = value / sum(value) * 100)
      parts$value <- round(parts$value)
      parts$p <- round(parts$p, digits = 2)
      parts$value <- scales::comma(parts$value)
      parts$p <- scales::comma(parts$p)
      names(parts) <- c(gsub('_', ' ', input$group_variable),
                        input$currency,
                        'Percentage')
      parts
    })
  output$g <- renderPlot({
    ggplot(data = df %>%
             filter(year <= input$year),
           aes(x = year,
               y = x,
               group = id,
               color = id)) +
      geom_line(alpha = 0.3) +
      geom_point(alpha = 0.3) + 
      geom_smooth(se = FALSE) +
      xlim(2015, 2035) +
      ylim(0, max(df$x))
    
  })
  
  output$plot1 <-
    renderPlot({
      plot(magude)
      # cm <- coordinates(magude)
      # for(i in 1:nrow(cm)){
      #   points(x = rnorm(1000, mean = cm[i,1]),
      #          y = rnorm(1000, mean = cm[i,2]),
      #          color = adjustcolor('darkred', alpha.f = 0.2))
      # }
      
      # plot(rnorm(1000),
      #      rnorm(1000))
    })
  
  output$table3 <- renderTable({
    if(input$intervention == 'MDA'){
      result <- 
        data_frame(`Inhabitants Magude` = outcomes$value[outcomes$indicator == 'inhabitants'],
                       `People reached by MDA` = outcomes$value[outcomes$indicator == 'reached'],
                       `People tested` = 28713,
                       `People treated` = outcomes$value[outcomes$indicator == 'treated'],
                   `Total cost MDA per round` = (df %>% 
                                                   filter(elimination_activity == 'MDA', year == 2015, currency == 'USD') %>% 
                                                   summarise(x = sum(value)) %>% .$x /2))
    
    } else if(input$intervention == 'REACT'){
      result <- data_frame(`Inhabitants Magude` = outcomes$value[outcomes$indicator == 'inhabitants'],
                           `Total cost for 2017` = (df %>% 
                                                      filter(elimination_activity == 'REACT', year == 2017, currency == 'USD') %>% 
                                                      summarise(x = sum(value)) %>% .$x))
      
    } else if(input$intervention == 'LLIN'){
      result <- data_frame(`Inhabitants Magude` = outcomes$value[outcomes$indicator == 'inhabitants'])
      result$`Universal LLIN` <- result$`Inhabitants Magude` / 2
      
    } else if(input$intervention == 'IRS'){
      result <- data_frame(
        `Houses sprayed` = outcomes$value[outcomes$indicator == 'houses_sprayed'],
        `Structures sprayed` = outcomes$value[outcomes$indicator == 'structures_sprayed'],
        `People protected` = outcomes$value[outcomes$indicator == 'people_irs'])
      
    }
    for(j in 1:ncol(result)){
      result[,j] <- scales::comma(round(result[,j]))
    }
    result
  })
}

