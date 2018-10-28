suppressWarnings(library(shiny))

ui <- tagList(
  useShinyjs(),
  
  navbarPage(
    title = "TWANG", id = "main", collapsible = TRUE,
    
    header = column(
      12, offset = 10,
      actionButton("prevBtn", "< Prev"),
      actionButton("nextBtn", "Next >")
    ),
    
    #
    # introduction page --
    
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
        mainPanel(
          width = 8,
          box(
            width = NULL,
            title = "Propensity Score Model",
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
      br(),
      fluidRow(
        navlistPanel(
          tabPanel(
            "Diagnostic Plots",
            plotOutput("ps.plot1"),
            plotOutput("ps.plot2"),
            plotOutput("ps.plot3"),
            plotOutput("ps.plot4"),
            plotOutput("ps.plot5")
          ),
          tabPanel(
            "Balance Tables",
            tableOutput("balance.table.unw"),
            tableOutput("balance.table.es"),
            tableOutput("balance.table.ks")
          ),
          tabPanel(
            "Effect Estimation"
          )
        )
      )
    )
  )
)