tabPanel(
  title = '3. Model',
  id = 'tab_results',
  value = 'tab_results',
  icon = icon('play'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('fct', 'Model', 
                  c(`auto: Select by lowest AIC` = 'auto', select_mod)),
      textInput('ecx', 'ECx (separated by comma): ',
                '10, 50')
    ),
    mainPanel(
      plotlyOutput('model_plot'), 
      verbatimTextOutput(outputId = 'model_summary'),
      verbatimTextOutput(outputId = 'model_ecx')
    )
  )
)
