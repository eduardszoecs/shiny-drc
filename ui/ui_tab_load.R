tabPanel(
  title = 'Load data',
  id = 'tab_load',
  value = 'tab_load',
  icon = icon('table'),
  
  sidebarLayout(
    sidebarPanel(
      title = 'Upload dataset',
      id = 'tab_load_upload',
      
      # read table
      fileInput(
        inputId = 'upload_data',
        label = 'File',
        multiple = FALSE,
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv'
        )
      ),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma = ',',
                     Semicolon = ';',
                     Tab = '\t'),
                   ',')
      
      
      # use example dataset
      , selectInput('sample_data',
                  'Choose a dataset',
                  c(
                    'Selenium' = 'selenium',
                    'None' = 'none'))
      

    ),
    mainPanel(
      dataTableOutput('data_table')
    )
  )
)
