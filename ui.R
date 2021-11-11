ui <- fluidPage(
  theme = shinytheme("lumen"),
  tags$head(tags$style(
    HTML(
      "
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
      font-family: 'Lobster', cursive;
      font-weight: 500;
      line-height: 1.1;
      color: #000000;
      }
      
      "
    )
    )),
  
  headerPanel("Event Study"),
  h3(),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        'datos_eventos',
        label = tags$b('Event Data'),
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt"),
        buttonLabe = 'Open',
        placeholder = 'Event Data filename'
      ),
      fileInput(
        'datos_muestra',
        label = tags$b('Sample Data'),
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt"),
        buttonLabe = 'Open',
        placeholder = 'Sample Data filename'
      ),
      fileInput(
        'datos_mercados',
        label = tags$b('Market Data'),
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt"),
        buttonLabe = 'Open',
        placeholder = 'Market Data filename'
      ),
      fileInput(
        'Riskfree',
        label = tags$b('Risk Free'),
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt"),
        buttonLabe = 'Open',
        placeholder = 'Risk Free filename'
      ),
      shinyDirButton(
        "Directory",
        "Choose a stocks sample" ,
        title = "Please select a folder:",
        buttonType = "default",
        class = NULL
      ),
      tags$hr(),
      selectInput(
        'Function',
        label = tags$b('Analysis:'),
        choices = c(
          'Volume events analysis',
          'Abnormal volumes',
          'Return events analysis',
          'Market model',
          '3 Factors',
          '4 Factors',
          '5 Factors', 
          'Global'
        ),
        selected = 1,
        multiple = FALSE
      ),
      tags$hr(),
      conditionalPanel(
        condition = "input.Function == 'Volume events analysis'",
        numericInput('LSPE1', label = 'Upper Limit Estimate 1', value = 55),
        numericInput('LSPE2', label = 'Upper Limit Estimate 2', value = 55)
      ),
      conditionalPanel(
        condition = "input.Function == 'Abnormal volumes'",
        numericInput('LSPE11', label = 'Upper Limit Estimate 1', value = 55),
        numericInput('LIPE11', label = 'Lower Limit Estimate 1', value = 11),
        numericInput('LSPE22', label = 'Upper Limit Estimate 2', value = 55),
        numericInput('LIPE22', label = 'Lower Limit Estimate 2', value = 11),
        numericInput('LVE', label = 'Limit Event Window', value = 10)
      ),
      conditionalPanel(
        condition = "input.Function == 'Return events analysis'",
        numericInput('LIE1', label = 'Lower Limit Event', value = 99),
        numericInput('LVE1', label = 'Limit Event Window', value = 10)
      ),
      conditionalPanel(
        condition = "input.Function == 'Market model'",
        numericInput('LIE2', label = 'Lower Limit Event', value = 99),
        numericInput('LVE2', label = 'Limit Event Window', value = 10),
        numericInput('VPE', label = 'Pre-Event Window', value = 10)
      ),
      conditionalPanel(
        condition = "input.Function == '3 Factors'",
        numericInput('LIE3', label = 'Lower Limit Event', value = 99),
        numericInput('LVE3', label = 'Limit Event Window', value = 10),
        numericInput('DNE', label = 'Calendar Days Event', value=146),
        numericInput('DNV', label = 'Calendar Days Window', value=21)
      ),
      conditionalPanel(
        condition = "input.Function == '4 Factors'",
        numericInput('LIE4', label = 'Lower Limit Event', value = 99),
        numericInput('LVE4', label = 'Limit Event Window', value = 10),
        numericInput('DNE1', label = 'Calendar Days Event', value=146),
        numericInput('DNV1', label = 'Calendar Days Window', value=21)
      ),
      conditionalPanel(
        condition = "input.Function == '5 Factors'",
        numericInput('LIE5', label = 'Lower Limit Event', value = 99),
        numericInput('LVE5', label = 'Limit Event Window', value = 10),
        numericInput('DNE2', label = 'Calendar Days Event', value=146),
        numericInput('DNV2', label = 'Calendar Days Window', value=21)
      ),
      conditionalPanel(
        h3("Volume events analysis"),
        condition = "input.Function == 'Global'",
        numericInput('LSPE1G', label = 'Upper Limit Estimate 1', value = 55),
        numericInput('LSPE2G', label = 'Upper Limit Estimate 2', value = 55),
        h3("Abnormal volumes"),
        numericInput('LSPE11G', label = 'Upper Limit Estimate 1', value = 55),
        numericInput('LIPE11G', label = 'Lower Limit Estimate 1', value = 11),
        numericInput('LSPE22G', label = 'Upper Limit Estimate 2', value = 55),
        numericInput('LIPE22G', label = 'Lower Limit Estimate 2', value = 11),
        numericInput('LVEG', label = 'Limit Event Window', value = 10),
        h3("Return events analysis"),
        numericInput('LIE1G', label = 'Lower Limit Event', value = 99),
        numericInput('LVE1G', label = 'Limit Event Window', value = 10),
        h3("Market model"),
        numericInput('LIE2G', label = 'Lower Limit Event', value = 99),
        numericInput('LVE2G', label = 'Limit Event Window', value = 10),
        numericInput('VPEG', label = 'Pre-Event Window', value = 10),
        h3("3 Factors"),
        numericInput('LIE3G', label = 'Lower Limit Event', value = 99),
        numericInput('LVE3G', label = 'Limit Event Window', value = 10),
        numericInput('DNEG', label = 'Calendar Days Event', value=146),
        numericInput('DNVG', label = 'Calendar Days Window', value=21),
        h3("4 Factors"),
        numericInput('LIE4G', label = 'Lower Limit Event', value = 99),
        numericInput('LVE4G', label = 'Limit Event Window', value = 10),
        numericInput('DNE1G', label = 'Calendar Days Event', value=146),
        numericInput('DNV1G', label = 'Calendar Days Window', value=21),
        h3("5 Factors"),
        numericInput('LIE5G', label = 'Lower Limit Event', value = 99),
        numericInput('LVE5G', label = 'Limit Event Window', value = 10),
        numericInput('DNE2G', label = 'Calendar Days Event', value=146),
        numericInput('DNV2G', label = 'Calendar Days Window', value=21)
      ),
      tags$hr(),
      actionButton("run", "Analyze"),
      downloadButton('dwn', label = 'Save'),
      actionButton("stop", "Close")
    ),
    mainPanel(
      tags$style(
        "#text1 {
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
        
        font-size:20px;
        color:back;
        font-family: 'Lobster', cursive;
        }"),
      tags$style(
        "#text2 {
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
        
        font-size:20px;
        color:back;
        font-family: 'Lobster', cursive;
        }"),
      tags$style(
        "#text3 {
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
        
        font-size:20px;
        color:back;
        font-family: 'Lobster', cursive;
        }"),
      tabsetPanel(
        tabPanel('Instructions',
                 htmlOutput("text2")),
        tabPanel('Console',
                 verbatimTextOutput("text3"))
    )
    )
  )
)
