ui <- tagList(
  useShinyjs(),
  
  fixedPage(
    navbarPage(
      title = "TWANG", id = "navbar", collapsible = TRUE,
      
      #
      # these buttons control the app ---
      
      header = fluidRow(
        # prev button
        column(6, actionButton("prevBtn", "Prev")),
        
        # next button
        column(6, align = "right", actionButton("nextBtn", "Next"))
      ),
      
      #
      # introduction page ---
      
      tabPanel(
        title = "Introduction", 
        value = "intro",
        br(),
        includeHTML("html/introduction.html")
      ),
      
      #
      # file upload ---
      
      # source: https://shiny.rstudio.com/articles/upload.html
      tabPanel(
        title = "File Upload",
        value = "upload",
        br(),
        sidebarPanel(
          width = 3,
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
          width = 9,
          shinydashboard::box(
            id = "contents.box",
            width = NULL,
            
            # show contents
            DT::dataTableOutput("contents")
          )
        )
      ),
      
      # 
      # propensity score model page ---
      
      tabPanel(
        title = "Propensity Score Model", 
        value = "model",
        br(),
        sidebarPanel(
          width = 3,
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
          width = 9,
          shinydashboard::box(
            id = "prop.score.box",
            width = NULL,
            
            h3("Propensity Score Model"),
            
            # show the psm summary table
            div(dataTableOutput("psm.summary"), style = "font-size: 85%; width: 100%")#,
            
            # save the summary table
            #downloadButton("psm.summary.save", "save")
          )
        )
      ),
      
      # 
      # eval page ---
      
      tabPanel(
        title = "Model Evaluation", 
        value = "eval",
        br(),
        navlistPanel(
          widths = c(2,10),
          tabPanel(
            title = "Diagnostic Plots",
            
            column(
              # width
              width = 8,
              
              # choose plot type (this is an argument passed to the twang plot function)
              selectInput("diag.plot.select", "Plot", choices = plot.types[-3]),
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("diag.plot.stopmethod", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("diag.plot"),
              
              # save the plot
              downloadButton("diag.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/diagnostic-plots.html")
              )
            )
          ),
          tabPanel(
            title = "Balance Plots",
            
            column(
              # width 
              width = 8,
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("bal.plot.stopmethod", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("bal.plot"),
              
              # save the plot
              downloadButton("bal.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/balance-plots.html")
              )
            )
          ),
          tabPanel(
            title = "Balance Tables",
            
            column(
              # width 
              width = 8,
              
              h3("Unweighted Balance Table"),
              
              # show the unweighted balance table
              div(dataTableOutput("unweighted.balance.table"), style = "font-size: 85%; width: 100%"),
              
              # save the table
              #downloadButton("unweighted.balance.table.save", "save"),
              
              h3("Weighted Balance Table"),
              
              # choose the stop method (this is an argument passed to the twang plot function)
              selectInput("bal.stopmethod", "Stop method", ""),
              
              # show the weighted balance table
              div(dataTableOutput("weighted.balance.table"), style = "font-size: 85%; width: 100%")
              
              # save the table 
              #downloadButton("weighted.balance.table.save", "save")
            ),
            
            column(
              # width
              width = 4,
              
              # description of tables
              wellPanel(
                includeHTML("html/balance-tables.html")
              )
            )
          ),
          tabPanel(
            title = "Relative Influence",
            
            column(
              # width
              width = 8,
              
              h3("Relative Influence"),
              
              # show the relative influence graph
              plotOutput("rel.inf.plot"),
              
              # save the plot
              downloadButton("rel.inf.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/relative_influence-plots.html")
              )
            )
          )
        )
      ),
      
      # 
      # effect estimation page ---
      
      tabPanel(
        title = "Treatment Effect Estimation",
        value = "effects",
        br(),
        sidebarPanel(
          width = 3,
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
          width = 9,
          shinydashboard::box(
            id = "effect.est.box",
            width = NULL,
            
            h3("Treatment Effect"),
            
            div(dataTableOutput("out.model"),style = "font-size: 85%; width: 100%"),
            #tableOutput("out.model"),
            
            div(dataTableOutput("out.model.summary"),style = "font-size: 85%; width: 100%")
            #tableOutput("out.model.summary")
          )
        )
      ),
      
      #
      # weights page ---
      
      tabPanel(
        title = "Weights", 
        value = "weights",
        br(),
        includeHTML("html/weights.html"),
        textInput("weight.var", "Enter name for new column", value = "w"),
        downloadButton("weights.save", "save")
      )
      
    )
  )
)