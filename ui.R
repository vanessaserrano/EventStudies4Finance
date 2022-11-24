ui <- fluidPage(
  theme = shinytheme("lumen"),
  tags$head(tags$style(
    HTML(
      "
      h1 {
      font-family:'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
      font-weight: 500;
      line-height: 1.1;
      color: #000000;
      }
      body {
      font-family:'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
      color:back;
      }
       .modal1 .modal-header {background-color:white; text-align:center; font-weight:bold
       }"
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
          '5 Factors'
        ),
        selected = 1,
        multiple = FALSE
      ),
      tags$hr(),
      conditionalPanel(
        condition = "input.Function == 'Volume events analysis'",
        numericInput('LSPE1', label = 'Upper Limit Estimate 1', value = 35),
        numericInput('LSPE2', label = 'Upper Limit Estimate 2', value = 35)
      ),
      conditionalPanel(
        condition = "input.Function == 'Abnormal volumes'",
        numericInput('LSPE11', label = 'Upper Limit Estimate 1', value = 35),
        numericInput('LIPE11', label = 'Lower Limit Estimate 1', value = 11),
        numericInput('LSPE22', label = 'Upper Limit Estimate 2', value = 35),
        numericInput('LIPE22', label = 'Lower Limit Estimate 2', value = 11),
        numericInput('LVE', label = 'Limit Event Window', value = 2)
      ),
      conditionalPanel(
        condition = "input.Function == 'Return events analysis'",
        numericInput('LIE1', label = 'Lower Limit Event', value = 75),
        numericInput('LVE1', label = 'Limit Event Window', value = 2)
      ),
      conditionalPanel(
        condition = "input.Function == 'Market model'",
        numericInput('LIE2', label = 'Lower Limit Event', value = 75),
        numericInput('LVE2', label = 'Limit Event Window', value = 2),
        numericInput('VPE', label = 'Pre-Event Window', value = 11)
      ),
      conditionalPanel(
        condition = "input.Function == '3 Factors'",
        numericInput('LIE3', label = 'Lower Limit Event', value = 75),
        numericInput('LVE3', label = 'Limit Event Window', value = 2),
        numericInput('DNE', label = 'Calendar Days Event', value=90),
        numericInput('DNV', label = 'Calendar Days Window', value=5)
      ),
      conditionalPanel(
        condition = "input.Function == '4 Factors'",
        numericInput('LIE4', label = 'Lower Limit Event', value = 75),
        numericInput('LVE4', label = 'Limit Event Window', value = 2),
        numericInput('DNE1', label = 'Calendar Days Event', value=90),
        numericInput('DNV1', label = 'Calendar Days Window', value=5)
      ),
      conditionalPanel(
        condition = "input.Function == '5 Factors'",
        numericInput('LIE5', label = 'Lower Limit Event', value = 75),
        numericInput('LVE5', label = 'Limit Event Window', value = 2),
        numericInput('DNE2', label = 'Calendar Days Event', value=90),
        numericInput('DNV2', label = 'Calendar Days Window', value=5)
      ),
      tags$hr(),
      actionButton("run", "Analyze"),
      downloadButton('dwn', label = 'Save'),
      actionButton("stop", "Close")
    ),
    mainPanel(
      tags$style(
        "body {
        font-size:15px;
        }"),
      tabsetPanel(
        tabPanel('Instructions',
                 htmlOutput("text2")),
        tabPanel('Files format and dates',
                 htmlOutput("text3")),
        tabPanel('Console',
                 verbatimTextOutput("text4"))
    )
    )
  )
)
