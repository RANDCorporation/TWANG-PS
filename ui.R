suppressWarnings(library(shiny))

ui <- tagList(
  useShinyjs(),
  
  navbarPage(
    title = "TWANG", id = "main", collapsible = TRUE,
    
    #
    # introduction page --
    
    tabPanel(
      "Introduction", 
      value = "intro",
      fluidRow(
        column(1, offset = 8, actionButton('goToModelFwd', 'Next: Propensity Score Model', style="color: #ffff; background-color: #663399;"))
      ),
      fluidRow(
        includeHTML("html/introduction.html")
      )
    ),
    
    # 
    # propensity score model page ---
    
    tabPanel(
      "Propensity Score Model", 
      value = "model",
      fluidRow(
        column(1, offset = 1, actionButton('goToIntro', 'Back: Introduction', icon("arrow-circle-left"), style="color: #fff; background-color: #663399;")),
        column(1, offset = 8, actionButton('goToDiagnostics', 'Next: Model Diagnosticcs', icon("arrow-circle-right"), style="color: #fff; background-color: #663399;"))
      ),
      fluidRow(
        column(
          width = 4,
          box(
            width = NULL,
            title = "Twang options",
            uiOutput("treatment"),
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
            actionButton("run", "Run Analysis", icon("paper-plane"), style="color: #fff; background-color: #663399;")
          )
        ),
        column(
          width = 8,
          box(
            width = NULL,
            "Propensity Score Model",
            tableOutput("psm")
          )
        )
      )
    ),
    
    # 
    # diagnostics page ---
    
    tabPanel(
      "Model Diagnostics", 
      value = "diagnostics",
      fluidRow(
        column(1, offset = 1, actionButton('goToModelBck', 'Back: Propensity Score Model', style="color: #fff; background-color: #663399;"))
      ),
      fluidRow(
        tabBox(
          width = NULL,
          id = "ps.diagnostics", 
          tabPanel(
            "Diagnostic Plots",
            plotOutput("ps.plot1"),
            plotOutput("ps.plot2"),
            plotOutput("ps.plot3"),
            plotOutput("ps.plot4"),
            plotOutput("ps.plot5")
          ),
          tabPanel(
            "Balance Tables"
          ),
          tabPanel(
            "Effect Estimation"
          )
        )
      )
    )
  )
)