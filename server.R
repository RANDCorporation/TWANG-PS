shinyServer(function(input, output, session) {
  
  # hide some boxes
  shinyjs::hide(id = "contents.box")
  shinyjs::hide(id = "prop.score.box")
  shinyjs::hide(id = "effect.est.box")
  
  
  #
  # navbar controls ---
  
  observe({
    hide(selector = "#navbar li a[data-value=model]")
  })
  
  observe({
    hide(selector = "#navbar li a[data-value=eval]")
  })
  
  observe({
    hide(selector = "#navbar li a[data-value=effects]")
  })
  
  observe({
    hide(selector = "#navbar li a[data-value=weights]")
  })
  
  observeEvent(input$run, {
    shinyjs::show(selector = "#navbar li a[data-value=eval]")
    shinyjs::show(selector = "#navbar li a[data-value=effects]") 
    tab$max = 5
  })
  
  observeEvent(input$out.run, {
    shinyjs::show(selector = "#navbar li a[data-value=weights]")
    tab$max = 6
  })
  
  
  #
  # button controls ----
  
  # see: https://github.com/daattali/advanced-shiny/blob/master/multiple-pages/app.R
  tab <- reactiveValues(page = 1, min = 1, max = 2)
  
  observe({
    toggleState(id = "prevBtn", condition = tab$page > tab$min)
    toggleState(id = "nextBtn", condition = tab$page < tab$max)
  })
  
  observe({
    if (input$navbar == "intro") tab$page = 1
    if (input$navbar == "upload") tab$page = 2
    if (input$navbar == "model") tab$page = 3
    if (input$navbar == "eval") tab$page = 4
    if (input$navbar == "effects") tab$page = 5
    if (input$navbar == "weights") tab$page = 6
  })
  
  navPage <- function(direction) {
    tab$page <- tab$page + direction
  }
  
  observeEvent(input$prevBtn, {
    navPage(-1)
    updateTabsetPanel(session, "navbar", tab.names[tab$page])
  })
  
  observeEvent(input$nextBtn, {
    navPage(1)
    updateTabsetPanel(session, "navbar", tab.names[tab$page])
  })
  
  
  #
  # file upload ---
  
  # source: https://shiny.rstudio.com/articles/upload.html
  
  df <- reactive({
    req(input$file1)
    
    # when reading semicolon separated files, having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # show the box
    shinyjs::show(id = "contents.box")
    
    # update the navbar
    shinyjs::show(selector = "#navbar li a[data-value=model]")
    shinyjs::hide(selector = "#navbar li a[data-value=eval]")
    shinyjs::hide(selector = "#navbar li a[data-value=effects]") 
    tab$max = 3
    
    # reset the model panel
    shinyjs::hide(id = "prop.score.box")
    
    # reset the effect panel
    shinyjs::hide(id = "effect.est.box")
    
    # return the data
    df
  })
  
  output$contents <- renderDT({
    df()
  }, options = list(dom = "tip", pageLength = 100 , scrollX = T ,scrollY= 300) ,rownames=F )
  
  vars <- reactive({
    names(df())
  })
  
  
  #
  # propensity score model ---
  
  # select the treatment variable
  observeEvent(vars(), {
    updateSelectInput(session, inputId = "treatment", choices = c("", vars()))
  })
  
  # list of outcome variables
  outcomes <- reactive({
    vars()[!(vars() %in% c(input$treatment, input$covariates, input$sampw))]
  })
  
  # select the outcome variable
  observeEvent(outcomes(), {
    updateSelectInput(session, inputId = "outcome", choices = c("", outcomes()), selected = input$outcome)
  })
  
  # select the outcome variable
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "outcome", choices = c("", outcomes()), selected = "")
  })
  
  # list of covariates
  covariates <- reactive({
    vars()[!(vars() %in% c(input$treatment, input$outcome, input$sampw))]
  })
  
  # select the covariates
  observeEvent(covariates(), {
    updateSelectInput(session, inputId = "covariates", choices = c("", covariates()), selected = input$covariates)
  })
  
  # select the covariates
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "covariates", choices = c("", covariates()), selected = "")
  })
  
  # for non integer parameters
  shrinkage <- reactive({as.numeric(input$shrinkage)})
  
  # list of sampling weights variables
  sampw <- reactive({
    vars()[!(vars() %in% c(input$treatment, input$outcome, input$covariates))]
  })
  
  # select the sampling weights variable
  observeEvent(sampw(), {
    updateSelectInput(session, inputId = "sampw", choices = c("", sampw()), selected = input$sampw)
  })
  
  # select the covariates
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "sampw", choices = c("", sampw()), selected = "")
  })
  
  # store the results of the ps command
  m <- reactiveValues()
  
  # let the user know something is happening
  observeEvent(input$run, {
    if (input$treatment == "") {
      showNotification("Please select a treatment variable", type = "error")
      return()
    }
    
    if (length(input$outcome) == 0) {
      showNotification("Please select outcome variable", type = "error")
      return()
    }
    
    if (length(input$covariates) == 0) {
      showNotification("Please select covariates", type = "error")
      return()
    }
    
    sampling.weights <- NULL
    if (input$sampw != "") {
      sampling.weights <- df() %>% pull(input$sampw)
      print(sampling.weights)
    }
    
    # TODO: do we need to validate all inputs?
    
    tryCatch(
      {
    
        # pop-up a message to show that twang is running
        showModal(modalDialog(title = "TWANG", "Calculating propensity scores. Please wait.", footer = NULL, easyClose = FALSE))
        
        # generate the formula
        formula <- as.formula(paste0(input$treatment, "~" , paste0(input$covariates, collapse = "+")))
        
        # run propensity score
        
        m$ps <- ps(
          formula = formula,
          data = df(),
          n.trees = input$n.trees,
          interaction.depth = input$interaction.depth,
          shrinkage = shrinkage(),
          estimand = input$estimand,
          stop.method = input$stop.method,
          sampw = sampling.weights,
          verbose = FALSE)
        
        # save the balance table
        m$bal <- bal.table(m$ps)
        
        # show the box
        shinyjs::show(id = "prop.score.box")
        
        # close the modal
        removeModal()
      },
      error = function(e) {
        # close the modal
        removeModal()
        
        # open the error modal
        showModal(
          modalDialog(
            title = "Error During Analysis",
            HTML(
              paste(
                "There was an error while running your analysis:",
                "<br><br>",
                "<a style=color:red>", e, "</a>")
            )
          )
        )
      })
  })
  
  # write the output of summary()
  output$psm.summary <- renderDataTable({ 
    req(m$ps)
    
    # column names
    cols <- c(
      "Sample Size Treated", 
      "Sample Size Control", 
      "Effective Sample Size Treated", 
      "Effective Sample Size Control", 
      "Maximum Standardized Difference", 
      "Mean Standardized Difference", 
      "Maximum Kolmogorov–Smirnov", 
      "Mean Kolmogorov–Smirnov", 
      "Optimal Iteration"
      )
    
    # row names
    rows = rownames(as.table(summary(m$ps)))[-1]
    rows = gsub( "ks.max.*" , "Maximum Kolmogorov–Smirnov" , rows)
    rows = gsub( "ks.mean.*" , "Mean Kolmogorov–Smirnov" , rows)
    rows = gsub( "es.max.*" , "Maximum Standardized Difference" , rows)
    rows = gsub( "es.mean.*" , "Mean Standardized Difference" , rows)
    
    rows <- c(
       "None (Unweighted)",
       rows
    )
    
    # table
    tab = as.data.frame(summary(m$ps)[,-8])
    setDT(tab)
    colnames(tab) = cols
    tab[ , "Stop Method":=rows]
    setcolorder(tab, c("Stop Method", cols))
    datatable(tab, 
              options = list(pageLength = 10 , "dom" = 'Brtip',buttons = list('copy', 'csv', 'excel') , scrollX = T),
              extensions = 'Buttons' , rownames=F ) %>% 
       formatRound(c(4:5), 1) %>% formatRound(c(6:9), 3) 
  } )
  
  # save the output of summary
  output$psm.summary.save <- downloadHandler(
    filename = function() {"psm-summary.csv"},
    content = function(file) {
      write.csv(summary(m$ps), file, row.names = TRUE)
    }
  )
  
  # update dropdown with valid stop method choices
  observeEvent(input$stop.method, {
    updateSelectInput(session, inputId = "diag.plot.stopmethod", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "bal.plot.stopmethod", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "bal.stopmethod", choices = input$stop.method)
  })
  
  #
  # model evaluation/outputs ---
  
  # diagnostic plots --
  
  # create plot 
  diag.plot <- reactive({
    req(m$ps)
    validate(need(input$diag.plot.stopmethod, message = "Please select stopping method"))
    plot(m$ps, plots = which(plot.types == input$diag.plot.select), subset = which(input$stop.method == input$diag.plot.stopmethod))
  })
  
  # render plot
  output$diag.plot <- renderPlot({
    print(diag.plot())
  })
  
  # save plot
  output$diag.plot.save <- downloadHandler(
    filename = "diagnostic.png",
    content = function(file) {
      png(file)
      print(diag.plot())
      dev.off()
    }
  )
  
  # balance plots --
  
  # create plot 
  bal.plot <- reactive({
    req(m$ps)
    validate(need(input$bal.plot.stopmethod, message = "Please select stopping method"))
    plot(m$ps, plots = 3, subset = which(input$stop.method == input$bal.plot.stopmethod))
  })
  
  # render plot
  output$bal.plot <- renderPlot({
    print(bal.plot())
  })
  
  # save plot
  output$bal.plot.save <- downloadHandler(
    filename = "balance.png",
    content = function(file) {
      png(file)
      print(bal.plot())
      dev.off()
    }
  )
  
  # balance tables --
  
  # render unweighted balance table
  output$unweighted.balance.table <- renderDataTable({
    req(m$bal)
    
    unw=m$bal$unw 
    unw$Variable = rownames(unw)
    
    cols.bal = c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","t","p-value","Kolmogorov–Smirnov","KS p-value")
    colnames(unw) = c(cols.bal,"Variable")
    
    datatable(
      unw[,c("Variable",c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","Kolmogorov–Smirnov"))], 
      options = 
        list(
          pageLength = 50, 
          "dom" = 'Brtip', 
          buttons = list('copy', 'csv', 'excel'), 
          scrollX = TRUE, 
          scrollY = 300, 
          scrollCollapse = TRUE
        ),
      extensions = 'Buttons', 
      rownames = FALSE
    ) 
  })
  
  # save unweighted balance table
  output$unweighted.balance.table.save <- downloadHandler(
    filename = function() {"unweighted-balance-table.csv"},
    content = function(file) {
      write.csv(m$bal$unw, file, row.names = TRUE)
    }
  )
  
  # create weighted balance table
  weighted.balance.table <- reactive({
    req(m$bal)
    
    w.tab = m$bal[[paste0(input$bal.stopmethod,".",input$estimand)]]
    w.tab$Variable = rownames(w.tab)
    
    cols.bal = c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","t","p-value","Kolmogorov–Smirnov","KS p-value")
    colnames(w.tab) = c(cols.bal,"Variable")
    
    datatable(
      w.tab[,c("Variable",c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","Kolmogorov–Smirnov"))], 
      options = 
        list(
          pageLength = 50, 
          "dom" = 'Brtip', 
          buttons = list('copy', 'csv', 'excel'), 
          scrollX = TRUE, 
          scrollY= 300, 
          scrollCollapse = TRUE
        ),
      extensions = 'Buttons', 
      rownames = FALSE
    ) 
  })
  
  # render weighted balance table 
  output$weighted.balance.table <- renderDataTable({
    weighted.balance.table() 
  })
  
  # save unweighted balance table
  output$weighted.balance.table.save <- downloadHandler(
    filename = function() {"weighted-balance-table.csv"},
    content = function(file) {
      write.csv(weighted.balance.table(), file, row.names = TRUE)
    }
  )
  
  # relative influence --
  
  # render plot
  output$rel.inf.plot <- renderPlot(height = 600, width = 400, {
    req(m$ps)
    summary(m$ps$gbm.obj, plot = TRUE)
  })
  
  # save plot
  output$rel.inf.plot.save <- downloadHandler(
    filename = "relative-influence.png",
    content = function(file) {
      png(file)
      summary(m$ps$gbm.obj, plot = TRUE)
      dev.off()
    }
  )
  
  
  #
  # effect estimation ---
  
  # select the outcome variable
  # NOTE: this is limited to variables listed as outcomes in the twang options
  observeEvent(input$outcome, {
    if (!is.null(input$outcome)) {
      updateSelectInput(session, inputId = "ee.outcome", choices = c("", input$outcome))
    }
  })
  
  # list of treatment effects covariates
  te.covariates <- reactive({
    vars()[!(vars() %in% c(input$treatment, input$outcome))]
  })
  
  # select the covariates
  # NOTE: this can be any variable that is not the specified outcome or the treatment
  observeEvent(te.covariates(), {
    updateSelectInput(session, inputId = "ee.covariates", choices = te.covariates())
  })
  
  # select the stopmethod to use in effect estimation
  observeEvent(input$stop.method, {
    updateSelectInput(session, inputId = "ee.stopmethod", choices = input$stop.method)
  })
  
  observeEvent(input$out.run, {
    tryCatch({
      # extract weights and set up svy
      m$wt = get.weights(m$ps, stop.method = input$ee.stopmethod, estimand=input$estimand)
      Dsvy = svydesign(id=~1, weights = m$wt, data=df())
      
      # generate the formula
      formula <- as.formula(paste0(input$ee.outcome, "~" , paste0(c(input$treatment, input$ee.covariates), collapse = "+")))
      
      # run propensity score
      m$out.model <- svyglm(formula, design = Dsvy, family = input$ee.type)
      
      # find marginal effects
      if (input$ee.type != "gaussian") {
        m$out = margins(m$out.model, variables=input$treatment, design=Dsvy)
      }
      
      if (input$ee.type == "gaussian") {
        # construct table from regression model
        tab.ate = as.data.frame(summary(m$out.model)$coef[input$treatment,,drop=F])
        tab.ate[,"Treatment"] = rownames(tab.ate)
        tab.ate[,"95% CI"] = confint(m$out.model)[input$treatment,] %>% myround(d=3) %>% 
          paste0(collapse=", ") %>% (function(x) paste0("(",x,")"))
        tab.ate = tab.ate[ ,c("Treatment","Estimate","Std. Error","t value","Pr(>|t|)","95% CI")]
        colnames(tab.ate) = c("Treatment",input$estimand,"Standard Error","Test Statistic","p-vaue","95% Confidence Interval")
        tab.ate[,2:5] = apply(tab.ate[,2:5],1:2,myround,d=3)
        
        # save to the reactive variable
        m$ate.tbl <- tab.ate
        
        # save to the reactive variable
        m$te.title <- "Propensity Score Weighted Linear Regression Results"
      }
      
      if (input$ee.type == "binomial") {
        # construct output table from marginal estimation
        tab.ate = summary(m$out)[,c("factor","AME","SE","z","p","lower","upper")]
        tab.ate[,"95% CI"] = paste0("(",signif(tab.ate[,"lower"],3) , ", ", signif(tab.ate[,"upper"],3) , ")")
        tab.ate = tab.ate[ ,c("factor","AME","SE","z","p","95% CI")]
        colnames(tab.ate) = c("Treatment",input$estimand,"Standard Error","Test Statistic","p-vaue","95% Confidence Interval") 
        tab.ate[,2:5] = apply(tab.ate[,2:5],1:2,myround,d=3)
        
        # save to the reactive variable
        m$ate.tbl <- tab.ate
        
        # save to the reactive variable
        m$te.title <- "Propensity Score Weighted Logistic Regression Results"
      }
      
      tab.reg = as.data.frame(summary(m$out.model)$coef)
      tab.reg[,"Variable"] = rownames(tab.reg)
      tab.reg[,"95% CI"] = apply(myround(confint(m$out.model), d=3) , 1 , function(x) paste0("(", paste0(x,collapse=", "),")") ) 
      tab.reg = tab.reg[ ,c("Variable","Estimate","Std. Error","t value","Pr(>|t|)","95% CI")]
      colnames(tab.reg) = c("Variable","Coefficient","Standard Error","Test Statistic","p-vaue","95% Confidence Interval") 
      tab.reg[,2:5] = apply(tab.reg[,2:5],1:2,myround,d=3)
      
      # save to the reactive variable
      m$reg.tbl <- tab.reg
      
      # show the box
      shinyjs::show(id = "effect.est.box")
    },
    error = function(e) {
      # open the error modal
      showModal(
        modalDialog(
          title = "Error During Analysis",
          HTML(
            paste(
              "There was an error while running your analysis:",
              "<br><br>",
              "<a style=color:red>", e, "</a>")
          )
        )
      )
    })
  })
  
  # summary
  output$out.model <- renderDataTable({ 
    req(m$ate.tbl)
    
    datatable(
      m$ate.tbl, 
      options = list(
        pageLength = 50, 
        "dom" = 'Brtip',
        buttons = list('copy', 'csv', 'excel'), 
        scrollX = TRUE, 
        scrollY = 300, 
        scrollCollapse = TRUE,
        columnDefs = list(list(className = 'dt-right', targets = 5))
      ),
      extensions = 'Buttons', 
      rownames = FALSE
    )
  })
  
  # coefficients
  output$out.model.summary <- renderDataTable({
    req(m$reg.tbl)
    
    datatable(
      m$reg.tbl, 
      options = 
        list(
          pageLength = 50, 
          "dom" = 'Brtip',
          buttons = list('copy', 'csv', 'excel'), 
          scrollX = TRUE, 
          scrollY = 300, 
          scrollCollapse = TRUE,
          columnDefs = list(list(className = 'dt-right', targets = 5))
        ),
      extensions = 'Buttons', 
      rownames = FALSE
    )
  })
  
  # append weights as the right-most column
  df.w <- reactive({
    df() %>%
      mutate(!!input$weight.var := m$wt)
  })
  
  # allow the user to download this table
  output$weights.save <- downloadHandler(
    filename = function() {"data_with_weights.csv"},
    content = function(file) {
      write.csv(df.w(), file, row.names = FALSE)
    }
  )
})
