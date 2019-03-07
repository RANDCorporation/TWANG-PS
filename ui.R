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
          # prev button
          column(6, actionButton("prevBtn", "Prev")),
          
          # next button
          column(6, align = "right", actionButton("nextBtn", "Next"))
        )
      ),
      
      #
      # introduction page ---
      
      tabPanel(
        title = "Introduction", 
        value = "intro",
        fluidRow(
          # intro material was written by beth ann
          includeHTML("html/introduction.html")
        )
      ),
      
      #
      # file upload ---
      
      # source: https://shiny.rstudio.com/articles/upload.html
      tabPanel(
        title = "File Upload",
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
              
              # show contents
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
              
              selectInput("estimand", "Estimand", choices = c("ATE", "ATT"), selected = "ATE"),
              
              selectInput("stop.method", "Stop method", choices = stop.methods, selected = c("es.mean", "ks.max"), multiple = TRUE),
              
              selectInput("sampw", "Sampling weights", ""),
              
              numericInput("seed", "Seed", 42),
              
              actionButton("run", "Run", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
          ),
          mainPanel(
            width = 8,
            shinydashboard::box(
              id = "prop.score.box",
              width = NULL,
              
              h3("Propensity Score Model"),
              
              # show the psm summary table
              tableOutput("psm.summary"),
              
              # save the summary table
              downloadButton("psm.summary.save", "save")
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
              title = "Relative Influence Graph",
              
              h3("Relative influence graph"),
              
              # show the relative influence graph
              plotOutput("rel.inf.plot", height = "600px"),
              
              # save the plot
              downloadButton("rel.inf.plot.save", "save")
            ),
            tabPanel(
              title = "Diagnostic Plots",
              
              # choose plot type (this is an argument passed to the twang plot function)
              selectInput("diag.plot.select", "Plot", choices = plot.types),
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("diag.plot.stopmethod", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("diag.plot"),
              
              # save the plot
              downloadButton("diag.plot.save", "save")
            ),
            tabPanel(
              title = "Balance Tables",
              
              h3("Unweighted balance table"),
              
              # show the unweighted balance table
              tableOutput("unweighted.balance.table"),
              
              # save the table
              downloadButton("unweighted.balance.table.save", "save"),
              
              h3("Weighted balance table"),
              
              # choose the stop method (this is an argument passed to the twang plot function)
              selectInput("bal.stopmethod", "Stop method", ""),
              
              # show the weighted balance table
              tableOutput("weighted.balance.table"),
              
              # save the table 
              downloadButton("weighted.balance.table.save", "save")
            )
          )
        )
      ),
      
      # 
      # effect estimation page ---
      
      tabPanel(
        "Treatment Effect Estimation",
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
            
            tableOutput("out.model.summary")
          )
        )
      ),
      
      #
      # weights page ---
      
      tabPanel(
        title = "Weights", 
        value = "weights",
        fluidRow(
          includeHTML("html/weights.html"),
          
          textInput("weight.var", "Enter name for new column", value = "w"),
          
          downloadButton("weights.save", "save")
        )
      )
      
    )
  )
)