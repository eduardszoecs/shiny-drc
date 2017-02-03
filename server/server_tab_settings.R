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
  selectInput('total', 'Total', vars)
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
  pdata <- get_data()
  
  # transform if binomial
  if (input$type == 'binomial') {
    pdata[['y_trans']] <- pdata[[input$y]] / pdata[[input$total]]
  } else {
    pdata[['y_trans']] <- pdata[[input$y]]
  }
  
  if (input$group != '__none__') {
    pdata[[input$group]] <- factor(pdata[[input$group]])
  }
  pdata
})


# Auto-select model -------------------------------------------------------

auto_fct <- reactive({
  pdata <- get_pdata()
  fla <- as.formula(paste0('y_trans ~ ', input$conc))
  
  # set vars to NULL
  if (input$group == '__none__') {
    curveid <- NULL
  } else {
    curveid <- pdata[[input$group]]
  }
  
  if (input$type == 'binomial') {
    wght <-  pdata[[input$total]]
  } else {
    wght <- NULL
  }
  
  # fit model
  mod <- drm(fla, 
             curveid = curveid,
             weights = wght,
             data = pdata,
             fct = LL.2(),
             type = input$type
  )
  # fit all models
  all_mods <- mselect(mod, fctList = list(LL.2(), LL.3(), LL.3u(), LL.4(), LL.5(),
                              W1.2(),  W1.3(), W1.4(), W2.2(), W2.3(), W2.4(),
                              BC.4(), BC.5(),
                              LL2.2(), LL2.3(), LL2.3u(), LL2.4(), LL2.5(),
                              AR.2(), AR.3(),
                              MM.2(), MM.3()))
  
  rownames(all_mods)[which.min(all_mods[ , 2])]
})



# Fit model ---------------------------------------------------------------

model <- reactive({
  pdata <- get_pdata()
  fla <- as.formula(paste0('y_trans ~ ', input$conc))
  
  # set vars to NULL
  if (input$group == '__none__') {
    curveid <- NULL
  } else {
    curveid <- pdata[[input$group]]
  }
  
  if (input$type == 'binomial') {
    wght <-  pdata[[input$total]]
  } else {
    wght <- NULL
  }
  
  if (input$fct == auto) {
    fctt <- paste0(auto_fct(), '()')
  } else {
    fctt <- paste0(input$fct, '()')
  }
  
  # fit model
  mod <- drm(fla, 
             curveid = curveid,
             weights = wght,
             data = pdata,
             fct = eval(parse(text = fctt)),
             type = input$type
  )
  mod
})


# Plot output -------------------------------------------------------------


output$plot_settings <- renderPlot({
  df <- get_pdata()
  
  # shift 0 xvalues
  if (min(df[[input$conc]]) == 0) {
    newmin <- min(df[[input$conc]] + sort(unique(df[[input$conc]]))[2] / 10)
    df[df[[input$conc]] == 0, input$conc] <- newmin
  }
  
  p <- ggplot(df, aes_string(x = input$conc, y =  'y_trans')) +
    scale_x_log10()
  
  if (input$group != '__none__') {
    p <- p + geom_point(aes_string(col = input$group))
  } else {
    p <- p + geom_point()
  }
  
  p
})
