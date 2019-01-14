ui <- tagList(
  useShinyjs(),
  
  fixedPage(
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
              selectInput("stop.method", "Stop method", choices = stop.methods, selected = c("es.mean", "ks.max"), multiple = TRUE),
              selectInput("estimand", "Estimand", choices = c("ATE", "ATT"), selected = "ATT"),
              numericInput("seed", "Seed", 1),
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
              "Relative Influence Graph",
              plotOutput("rel.inf.plot")
            ),
            tabPanel(
              "Diagnostic Plots",
              selectInput("diag.plot.select", "Plot", choices = plot.types),
              selectInput("diag.plot.stopmethod", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              plotOutput("diag.plot"),
              downloadButton("diag.plot.save", "save")
            ),
            tabPanel(
              "Balance Tables",
              h3("Unweighted balance statistics"),
              tableOutput("balance.table.unw"),
              h3("Weighted balance statistics"),
              selectInput("bal.stopmethod", "Stop method", ""),
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
            title = "Effect Estimation",
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
            tableOutput("out.model"),
            tableOutput("out.model.summary")
          )
        )
      ),
      
      #
      # weights ---
      
      tabPanel(
        "Weights",
        value = "weights",
        br(),
        fluidRow(
          column(
            width = 12,
            textInput("weight.var", "Weight column name", value = "w"),
            DT::dataTableOutput("weights.tbl"),
            downloadButton("weights.save", "save")
          )
        )
      )
    )
  )
)