# UI Elements -------------------------------------------------------------
output$conc = renderUI({
  vars <- get_vars()
  selectInput('conc', 'Concentration', vars)
})

output$y = renderUI({
  vars <- get_vars()
  selectInput('y', 'Response', vars)
})

output$total = renderUI({
  vars <- get_vars()
  selectInput('total', 'Total (binomial data)', vars)
})
observe({
  toggleState(id = "total", condition = input$type == 'binomial')
})


output$group = renderUI({
  vars <- get_vars()
  vars <- c(None = '__none__', vars)
  selectInput('group', 'Group (optional)', vars)
})



# Reactive Functions ------------------------------------------------------


get_pdata <- reactive({
  df <- get_data()
  
  # transform if binomial
  if (input$type == 'binomial') {
    df[['y_trans']] <- df[[input$y]] / df[[input$total]]
  } else {
    df[['y_trans']] <- df[[input$y]]
  }
  
  if (input$group != '__none__') {
    df[[input$group]] <- factor(df[[input$group]])
  }
  df
})


prep_data <- reactive({
  df <- get_pdata()
  
  # shift 0 xvalues
  if (min(df[[input$conc]]) == 0) {
    newmin <- min(df[[input$conc]] + sort(unique(df[[input$conc]]))[2] / 10)
    df[df[[input$conc]] == 0, input$conc] <- newmin
  }
  df
})


plot_raw <- reactive({
  p <- ggplot(prep_data(), aes_string(x = input$conc, y =  'y_trans')) +
    scale_x_log10() +
    theme_edi() +
    labs(y = 'Response', x = 'Concentration')
  
  if (input$group != '__none__') {
    p <- p + geom_point(aes_string(col = input$group))
  } else {
    p <- p + geom_point()
  }
  p
})

# Plot output -------------------------------------------------------------


output$plot_settings <- renderPlot({
  print(plot_raw())
})
