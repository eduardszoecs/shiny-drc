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
  
  # select fct
  if (input$fct == 'auto') {
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

get_pred <- reactive({
  df <- prep_data()
  
  # new dose levels as support for the line
  pconc <- exp(seq(log(min(df[[input$conc]] + sort(unique(df[[input$conc]]))[2] / 10)), 
                   log(max(df[[input$conc]])), 
                   length = 100))
  
  if (input$group != '__none__') {
    newdata <- expand.grid(pconc ,
                           unique(df[[input$group]]))
    names(newdata) <- c(input$conc, 'curveid')
    newdata[['curveid']] <- factor(newdata[['curveid']])
  } else {
    newdata <- expand.grid(pconc)
    names(newdata) <- input$conc
  }
  
  # predictions and confidence intervals
  pm <- predict(model(), newdata = newdata, interval = "confidence")
  # new data with predictions
  newdata$p <- pm[ , 1]
  newdata$pmin <- pm[ , 2]
  newdata$pmax <- pm[ , 3]
  newdata
})


plot_model <- reactive({
  p <- ggplot(prep_data(), aes_string(x = input$conc, y =  'y_trans')) +
    scale_x_log10() +
    theme_edi()
  
  pdat <- get_pred()
  
  if (input$group != '__none__') {
    p <- p + 
      geom_point(aes_string(col = input$group)) +
      geom_ribbon(data = pdat, aes_string(x = input$conc, y = 'p',
                                             ymin = 'pmin', ymax = 'pmax',
                                             fill = 'curveid'),
                  alpha = 0.2) +
      geom_line(data = pdat, aes_string(x = input$conc, y = 'p',
                                           col = 'curveid')) +
      guides(col = 'none')
    
  } else {
    p <- p + geom_point() +
      geom_ribbon(data = pdat, aes_string(x = input$conc, y = 'p', 
                                             ymin = 'pmin', ymax = 'pmax'), 
                  alpha = 0.2) +
      geom_line(data = pdat, aes_string(x = input$conc, y = 'p'))
  }
  
  p
})

# get ECx -----------------------------------------------------------------


output$model_ecx <- renderPrint({
  ecx <- numextractall(input$ecx)
  ED(model(), ecx, interval = 'delta')
})


# Plot --------------------------------------------------------------------

output$model_plot <- renderPlot({
 print(plot_model())
})


output$model_summary <- renderPrint({
  summary(model())
})
