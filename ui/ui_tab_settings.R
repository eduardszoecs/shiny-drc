tabPanel(
  title = 'Settings',
  id = 'tab_settings',
  value = 'tab_settings',
  icon = icon('wrench'),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput('conc'),
      uiOutput('y'),
      selectInput('type', 'Response type', 
                  c("binomial", "continuous", "Poisson", "quantal", "event")),
      uiOutput('total'),
      uiOutput('group')
    ),
    mainPanel(
      plotOutput('plot_settings')
    )
  )
)
