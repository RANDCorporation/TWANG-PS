ui <- tagList(
  useShinyjs(),
  
  navbarPage(
    title = "TWANG", id = "navbar", collapsible = TRUE,
    
    #
    # these buttons control the app ---
    
    header = column(
      12,
      fluidRow(
        column(6, actionButton("prevBtn", "Prev")),
        column(6, align = "right", actionButton("nextBtn", "Next"))
      )
    ),
    
    #
    # introduction page ---
    
    tabPanel(
      "Introduction", 
      value = "intro",
      fluidRow(
        includeHTML("html/introduction.html")
      )
    ),
    
    # 
    # propensity score model page ---
    
    tabPanel(
      "Propensity Score Model", 
      value = "model",
      br(),
      fluidRow(
        sidebarPanel(
          width = 4,
          shinydashboard::box(
            width = NULL,
            title = "Twang options",
            selectInput("treatment", "Treatment", ""),
            selectInput("outcome", "Outcome", "", multiple = TRUE),
            selectInput("covariates", "Covariates", "", multiple = TRUE),
            numericInput("n.trees", "gbm iterations", 5000),
            numericInput("interaction.depth", "Interaction depth", 3),
            textInput("shrinkage", "Shrinkage", "0.01"),
            numericInput("print.level", "Print level", 2),
            selectInput("estimand", "Estimand", choices = c("ATE", "ATT")),
            selectInput("stop.method", "Stop method", choices = c("es.mean","es.max","ks.mean", "ks.max"), selected = c("es.mean"), multiple = TRUE ),
            actionButton("run", "Run Analysis")
          )
        ),
        mainPanel(
          width = 8,
          shinydashboard::box(
            width = NULL,
            title = "Propensity Score Model",
            tableOutput("psm")
          )
        )
      )
    ),
    
    # 
    # eval page ---
    
    tabPanel(
      "Model Evaluation", 
      value = "eval",
      br(),
      fluidRow(
        navlistPanel(
          tabPanel(
            "Diagnostic Plots",
            selectInput("diag.plot.select", "Plot", choices = plot.types),
            plotOutput("diag.plot"),
            downloadButton("diag.plot.save", "save")
          ),
          tabPanel(
            "Balance Tables",
            selectInput("bal.stopmethod", "Stop method", "", multiple = FALSE),
            h3("Unweighted balance statistics"),
            tableOutput("balance.table.unw"),
            h3("Weighted balance statistics"),
            tableOutput("balance.table")
          )
        )
      )
    ),
    
    # 
    # effect estimation page ---
    
    tabPanel(
      "Effect Estimation",
      value = "effects",
      br(),
      sidebarPanel(
        width = 4,
        shinydashboard::box(
          width = NULL,
          title = "Effect estiamtion",
          selectInput("ee.outcome", "Outcome", ""),
          selectInput("ee.type", "Outcome Type", choices = list(Binary="binomial", Continuous="gaussian") , selected="gaussian"),
          selectInput("ee.covariates", "Covariates", "", multiple = TRUE),
          selectInput("ee.stopmethod", "Stop method", "", multiple = FALSE),
          actionButton("out.run", "Run Analysis")
        )
      ),
      mainPanel(
        width = 8,
        shinydashboard::box(
          width = NULL,
          title = "Treatment Effect",
          tableOutput("out.model")
        )
      )
    )
  )
)