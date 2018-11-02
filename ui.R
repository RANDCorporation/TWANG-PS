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
            numericInput("interaction.depth", "Interaction depth", 2),
            textInput("shrinkage", "Shrinkage", "0.01"),
            textInput("bag.fraction", "Bag fractions", "1.0"),
            numericInput("perm.test.iters", "Permutation test iterations", 0),
            numericInput("print.level", "Print level", 2),
            numericInput("interlim", "Max interations for direct optimizaiton", 1000),
            checkboxInput("verbose", "Verbose", value = FALSE),
            selectInput("estimand", "Estimand", choices = c("ATE", "ATT")),
            selectInput("stop.method", "Stop method", choices = c("es.mean", "ks.max"), selected = c("es.mean", "ks.max"), multiple = TRUE),
            checkboxInput("mulitnom", "Multinom", FALSE),
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
            plotOutput("diag.plot")
          ),
          tabPanel(
            "Balance Tables",
            tableOutput("balance.table.unw"),
            tableOutput("balance.table.es"),
            tableOutput("balance.table.ks")
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
          selectInput("ee.type", "Outcome Type", choices = c("", "binary", "categorical", "continuous")),
          selectInput("ee.covariates", "Covariates", "", multiple = TRUE)
        )
      ),
      mainPanel(
        width = 8,
        shinydashboard::box()
      )
    )
  )
)