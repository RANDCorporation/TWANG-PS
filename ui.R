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
      # file upload ---
      
      # source: https://shiny.rstudio.com/articles/upload.html
      tabPanel(
        "File Upload",
        value = "upload",
        br(),
        fluidRow(
          sidebarPanel(
            width = 4,
            shinydashboard::box(
              width = NULL,
              
              # select a file
              fileInput("file1", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              
              # horizontal line
              tags$hr(),
              
              # checkbox if file has header 
              checkboxInput("header", "Header", TRUE),
              
              # select separator 
              radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
              
              # select quotes 
              radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
            )
          ),
          mainPanel(
            width = 8,
            shinydashboard::box(
              id = "contents.box",
              width = NULL,
              DT::dataTableOutput("contents")
            )
          )
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

              selectInput("treatment", "Treatment", ""),
              
              selectInput("outcome", "Outcome", "", multiple = TRUE),
              
              selectInput("covariates", "Covariates", "", multiple = TRUE),
              
              numericInput("n.trees", "gbm iterations", 5000),
              
              numericInput("interaction.depth", "Interaction depth", 2),
              
              textInput("shrinkage", "Shrinkage", "0.01"),
              
              selectInput("stop.method", "Stop method", choices = stop.methods, selected = c("es.mean", "ks.max"), multiple = TRUE),
              
              selectInput("estimand", "Estimand", choices = c("ATE", "ATT"), selected = "ATT"),
              
              numericInput("seed", "Seed", 1),
              
              actionButton("run", "Run", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
          ),
          mainPanel(
            width = 8,
            shinydashboard::box(
              id = "prop.score.box",
              width = NULL,
              h3("Propensity Score Model"),
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
            actionButton("out.run", "Run", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        mainPanel(
          width = 8,
          shinydashboard::box(
            id = "effect.est.box",
            width = NULL,
            h3("Treatment Effect"),
            tableOutput("out.model"),
            tableOutput("out.model.summary"),
            h3("Propensity Score Weights"),
            includeHTML("html/weights.html"),
            textInput("weight.var", "Enter name for new column", value = "w"),
            downloadButton("weights.save", "save")
          )
        )
      )
      
    )
  )
)