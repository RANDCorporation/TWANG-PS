ui <- tagList(
  # why are we using this again?
  useShinyjs(),
  
  # why are we using this again?
  use_bs_popover(),
  
  # remove the increment/decrement buttons from numericInput()
  tags$style(
    HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")
  ),
  
  # create the page
  fixedPage(
    # create the app
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
          radioButtons(
            "file.type",
            label = h4("Choose File type"),
            choices = list("CSV/Text" = 1, "Excel" = 2, "SAS" = 3),
            selected = 1,
            inline = FALSE
          ),
          shinydashboard::box(
            id = "csv.box",
            width = NULL,
            
            # select a file
            fileInput("file.name.csv", "Choose CSV File", 
                      accept = c(
                        "text/csv", 
                        "text/comma-separated-values,text/plain", 
                        ".csv"
                      )
            ),
            
            # horizontal line
            tags$hr(),
            
            # select separator 
            radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
            
            # select quotes 
            radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
          ),
          shinydashboard::box(
            id = "excel.box",
            width = NULL,
            
            # select a file
            fileInput("file.name.excel", "Choose Excel File", 
                      accept = c(
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                        ".xlsx"
                      )
            ),
            
            # horizontal line
            tags$hr(),
            
            # get sheet number
            textInput("excel.sheet", "Sheet (String or Integer)", "1")
          ),
          shinydashboard::box(
            id = "sas.box",
            width = NULL,
            
            # select a file
            fileInput("file.name.sas", "Choose SAS File", 
                      accept = c(
                        "application/x-sas-data", 
                        ".sas7bdat"
                      )
            )
          )
        ),
        mainPanel(
          width = 9,
          shinydashboard::box(
            id = "contents.box",
            width = NULL,
            
            # show contents of the uploaded file
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
            
            selectInput("treatment", "Treatment", "") %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Treatment", content = "The 0/1 treatment indicator.", placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("outcome", "Outcomes", "", multiple = TRUE) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Outcomes", content = "The outcome(s) of interest.", placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("covariates", "Numerical Covariates", "", multiple = TRUE) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Covariates", content = "The observed binary (0/1) and continuous pretreatment covariates.", placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("categorical", "Categorical Covariates", "", multiple = TRUE) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Covariates", content = "The observed categorical pretreatment covariates.", placement = "top", trigger = "hover"
                  )
              ),
            
            numericInput("n.trees", "GBM iterations", 5000) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "GBM Iterations", content = n.trees.text, placement = "top", trigger = "hover"
                  )
              ),
            
            numericInput("interaction.depth", "Interaction depth", 2) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Interaction depth", content = interaction.depth.text, placement = "top", trigger = "hover"
                  )
              ),
            
            numericInput("shrinkage", "Shrinkage", 0.01) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Shrinkage", content = shrinkage.text, placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("estimand", "Estimand", choices = c("ATE", "ATT"), selected = "ATE") %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Estimand", content = estimand.text, placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("stop.method", "Stop method", choices = stop.methods, selected = c("es.max", "ks.max"), multiple = TRUE) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Stop method", content = stop.method.text, placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("sampw", "Sampling weights", "") %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Sampling weights", content = "Optional sampling weights", placement = "top", trigger = "hover"
                  )
              ),
            
            actionButton("run", "Run", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        mainPanel(
          width = 9,
          shinydashboard::box(
            id = "prop.score.box",
            width = NULL,
            
            h3("Propensity Score Model Summary Table"),
            
            # show the psm summary table
            div(dataTableOutput("psm.summary"), style = "font-size: 85%; width: 100%")
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
            title = "Convergence",
            
            column(
              # width
              width = 8,
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("conv.plot.stop", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("conv.plot"),
              
              # save the plot
              downloadButton("conv.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/convergence-plot.html")
              )
            )
          ),
          tabPanel(
            title = "Propensity Score",
            
            column(
              # width
              width = 8,
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("ps.plot.stop", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("ps.plot"),
              
              # save the plot
              downloadButton("ps.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/propensity-score-plot.html")
              )
            )
          ),
          tabPanel(
            title = "Balance Plot",
            
            column(
              # width 
              width = 8,
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("bal.plot.stop", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
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
                includeHTML("html/balance-plot.html")
              )
            )
          ),
          tabPanel(
            title = "ES p-values",
            
            column(
              # width
              width = 8,
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("es.plot.stop", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("es.plot"),
              
              # save the plot
              downloadButton("es.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/es-p_values-plot.html")
              )
            )
          ),
          tabPanel(
            title = "KS p-values",
            
            column(
              # width
              width = 8,
              
              # choose stop method (this is an argument passed to the twang plot function)
              selectInput("ks.plot.stop", "Stop method", choices = stop.methods, selected = "", multiple = TRUE),
              
              # show the plot
              plotOutput("ks.plot"),
              
              # save the plot
              downloadButton("ks.plot.save", "save")
            ),
            
            column(
              # width 
              width = 4,
              
              # description of plots
              wellPanel(
                includeHTML("html/ks-p_values-plot.html")
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
              
              h3("Weighted Balance Table"),
              
              # choose the stop method (this is an argument passed to the twang plot function)
              selectInput("bal.table.stop", "Stop method", ""),
              
              # show the weighted balance table
              div(dataTableOutput("weighted.balance.table"), style = "font-size: 85%; width: 100%"),
              
              br()
            ),
            
            column(
              # width
              width = 4,
              
              # description of tables
              wellPanel(
                includeHTML("html/balance-table.html")
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
                includeHTML("html/relative_influence-plot.html")
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
            
            selectInput("ee.outcome", "Outcome", "") %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Outcome", content = "The outcome of interest. Can only fit one at a time.", placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("ee.type", "Outcome Type", choices = list(Binary="quasibinomial", Continuous="gaussian") , selected="gaussian") %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Outcome Type", content = outcome.type.text, placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("ee.covariates", "Covariates", "", multiple = TRUE) %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Covariates", content = outcome.covariates.text, placement = "top", trigger = "hover"
                  )
              ),
            
            selectInput("ee.stopmethod", "Stop method", "") %>%
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Stop method", content = outcome.stop.method.text, placement = "top", trigger = "hover"
                  )
              ),
            
            actionButton("out.run", "Run", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        mainPanel(
          width = 9,
          shinydashboard::box(
            id = "effect.est.box",
            width = NULL,
            
            h3("Treatment Effect"),
            
            div(dataTableOutput("out.model"), style = "font-size: 85%; width: 100%"),
            
            br(),
            
            h3(textOutput("te.title")),
            
            div(dataTableOutput("out.model.summary"), style = "font-size: 85%; width: 100%")
          )
        )
      ),
      
      #
      # weights page ---
      
      tabPanel(
        title = "Weights", 
        value = "weights",
        br(),
        sidebarPanel(
          width = 3,
          shinydashboard::box(
            width = NULL,
            
            title = "Propensity Score Weights",
            
            selectInput("wt.stopmethod", "Stop method", "") %>% 
              shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_popover(
                    title = "Stop method", content = weight.stop.method.text, placement = "top", trigger = "hover"
                  )
              ),
            
            textInput("wt.var", "Enter name for new column", value = "w"),
            
            downloadButton("weights.save", "save")
          )
        ),
        mainPanel(
          width = 9,
          shinydashboard::box(
            width = NULL,
            
            includeHTML("html/weights.html")
          )
        )
      )
      
    )
  )
)