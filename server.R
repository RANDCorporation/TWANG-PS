shinyServer(function(input, output, session) {
  
  # hide some boxes
  shinyjs::hide(id = "excel.box")
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
  
  observeEvent(input$file.type, {
    if (input$file.type == 1) {
      shinyjs::show(id = "csv.box")
      shinyjs::hide(id = "excel.box")
      shinyjs::hide(id = "sas.box")
    }
    
    if (input$file.type == 2) {
      shinyjs::hide(id = "csv.box")
      shinyjs::show(id = "excel.box")
      shinyjs::hide(id = "sas.box")
    }
    
    if (input$file.type == 3) {
      shinyjs::hide(id = "csv.box")
      shinyjs::hide(id = "excel.box")
      shinyjs::show(id = "sas.box")
    }
  })
  
  # source: https://shiny.rstudio.com/articles/upload.html
  
  # data
  df <- reactiveValues()
  
  # open csv file
  observeEvent(input$file.name.csv, {
    if (input$file.type == 1) {
      # open file
      tryCatch({
        df.tmp <- read_delim(input$file.name.csv$datapath, delim = input$sep, quote = input$quote)
        
        # if you don't do this, then ps() won't work!!
        df.tmp <- as.data.frame(df.tmp)
        
        # convert strings to factors
        df.tmp <- df.tmp %>%
          mutate_if(is.character, as.factor)
        
        # save to reactive value
        df$data <- df.tmp
        df$vars <- names(df.tmp)
        
        # show the box
        shinyjs::show(id = "contents.box")
        
        # update the navbar
        shinyjs::show(selector = "#navbar li a[data-value=model]")
        
        # update the navbar
        shinyjs::hide(selector = "#navbar li a[data-value=eval]")
        shinyjs::hide(selector = "#navbar li a[data-value=effects]")
        shinyjs::hide(selector = "#navbar li a[data-value=weights]")
        tab$max = 3
        
        # reset the model panel
        shinyjs::hide(id = "prop.score.box")
        
        # reset the effect panel
        shinyjs::hide(id = "effect.est.box")
      },
      error = function(e) {
        # open the error modal
        showModal(
          modalDialog(
            title = "Error Reading File",
            HTML(
              paste(
                "There was an error while reading the file:",
                "<br><br>",
                "<a style=color:red>", e, "</a>")
            )
          )
        )
      })
    }
  })
  
  # open xlsx file
  observeEvent(input$file.name.excel, {
    if (input$file.type == 2) {
      # check if the sheet is a string or numeric
      sheet = input$excel.sheet
      if (!is.na(as.numeric(sheet))) sheet = as.numeric(sheet)
      
      # open file
      tryCatch({
        df.tmp <- read_excel(input$file.name.excel$datapath, sheet = sheet)
        
        # if you don't do this, then ps() won't work!!
        df.tmp <- as.data.frame(df.tmp)
        
        # save to reactive value
        df$data <- df.tmp
        df$vars <- names(df.tmp)
        
        # show the box
        shinyjs::show(id = "contents.box")
        
        # update the navbar
        shinyjs::show(selector = "#navbar li a[data-value=model]")
        
        # update the navbar
        shinyjs::hide(selector = "#navbar li a[data-value=eval]")
        shinyjs::hide(selector = "#navbar li a[data-value=effects]")
        shinyjs::hide(selector = "#navbar li a[data-value=weights]")
        tab$max = 3
        
        # reset the model panel
        shinyjs::hide(id = "prop.score.box")
        
        # reset the effect panel
        shinyjs::hide(id = "effect.est.box")
      },
      error = function(e) {
        # open the error modal
        showModal(
          modalDialog(
            title = "Error Reading File",
            HTML(
              paste(
                "There was an error while reading the file:",
                "<br><br>",
                "<a style=color:red>", e, "</a>")
            )
          )
        )
      })
    }
  })
  
  # open sas7bdat file
  observeEvent(input$file.name.sas, {
    if (input$file.type == 3) {
      # open file
      tryCatch({
        df.tmp <- read_sas(input$file.name.sas$datapath)
        
        # if you don't do this, then ps() won't work!!
        df.tmp <- as.data.frame(df.tmp)
        
        # save to reactive value
        df$data <- df.tmp
        df$vars <- names(df.tmp)
        
        # show the box
        shinyjs::show(id = "contents.box")
        
        # update the navbar
        shinyjs::show(selector = "#navbar li a[data-value=model]")
        
        # update the navbar
        shinyjs::hide(selector = "#navbar li a[data-value=eval]")
        shinyjs::hide(selector = "#navbar li a[data-value=effects]")
        shinyjs::hide(selector = "#navbar li a[data-value=weights]")
        tab$max = 3
        
        # reset the model panel
        shinyjs::hide(id = "prop.score.box")
        
        # reset the effect panel
        shinyjs::hide(id = "effect.est.box")
      },
      error = function(e) {
        # open the error modal
        showModal(
          modalDialog(
            title = "Error Reading File",
            HTML(
              paste(
                "There was an error while reading the file:",
                "<br><br>",
                "<a style=color:red>", e, "</a>")
            )
          )
        )
      })
    }
  })
  
  # show table
  output$contents <- renderDataTable({
    req(df$data)
    
    # custom header
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(sapply(df$data, class), th)
        ),
        tr(
          lapply(names(df$data), th)
        )
      )
    ))
    
    # data table
    datatable(
      df$data,
      options = 
        list(
          pageLength = 100,
          dom = "tip", 
          scrollX = TRUE, 
          scrollY= 300,
          scrollCollapse = TRUE
        ), 
      container = sketch,
      rownames = FALSE
    )
  })
  
  #
  # propensity score model ---
  
  # select the treatment variable
  observeEvent(df$vars, {
    updateSelectInput(session, inputId = "treatment", choices = c("", df$vars))
  })
  
  # list of outcome variables
  outcomes <- reactive({
    df$vars[!(df$vars %in% c(input$treatment, input$covariates, input$categorical, input$sampw))]
  })
  
  # select the outcome variable
  observeEvent(outcomes(), {
    updateSelectInput(session, inputId = "outcome", choices = c("", outcomes()), selected = input$outcome)
  })
  
  # list of covariates
  covariates <- reactive({
    df$vars[!(df$vars %in% c(input$treatment, input$outcome, input$categorical, input$sampw))]
  })
  
  # select the covariates
  observeEvent(covariates(), {
    updateSelectInput(session, inputId = "covariates", choices = c("", covariates()), selected = input$covariates)
  })
  
  # list of categorical covariates
  categorical <- reactive({
    df$vars[!(df$vars %in% c(input$treatment, input$outcome, input$covariates, input$sampw))]
  })
  
  # select the categorical covariates
  observeEvent(categorical(), {
    updateSelectInput(session, inputId = "categorical", choices = c("", categorical()), selected = input$categorical)
  })
  
  # iterations must be an integer
  observeEvent(input$n.trees, {
    if (!is.integer(input$n.trees) | !(input$n.trees > 0)) {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Number of GBM iterations must be a positive integer!")
        )
      )
    }
  })
  
  # interaction depth must be an integer
  observeEvent(input$interaction.depth, {
    if (!is.integer(input$interaction.depth) | !(input$interaction.depth > 0)) {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Interaction depth must be a positive integer!")
        )
      )
    }
  })
  
  # shrinkage must be numeric
  observeEvent(input$shrinkage, {
    if (!is.numeric(input$shrinkage) | !(input$shrinkage > 0)) {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Shrinkage must be a positive numeric value!")
        )
      )
    }
  })
  
  # list of sampling weights variables
  sampw <- reactive({
    df$vars[!(df$vars %in% c(input$treatment, input$outcome, input$covariates, input$categorical))]
  })
  
  # select the sampling weights variable
  observeEvent(sampw(), {
    updateSelectInput(session, inputId = "sampw", choices = c("", sampw()), selected = input$sampw)
  })
  
  # store the results of the ps command
  m <- reactiveValues()
  
  # app info: e.g., warnings
  app.info <- reactiveValues()
  
  # let the user know something is happening
  observeEvent(input$run, {
    if (input$treatment == "") {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Please select a treatment variable!")
        )
      )
      return()
    }
    
    # NOTE: TWANG should do this check
    if (any(is.na(df$data %>% pull(input$treatment)))) {
      # filter out NA values in treatment var
      df$data <- filter_na(df$data, input$treatment)
      
      # send warning to the modal
      na.values.warning <- sprintf('%s, %s', na.values.warning, input$treatment)
      running.message <- 
        HTML(
          paste(
            running.message,
            "<br><br>",
            "<a style=color:blue>", na.values.warning, "</a>")
        )
    }
    
    # NOTE: TWANG should do this check
    if (length(unique(df$data %>% pull(input$treatment))) != 2) {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Treatment variable should be a 0/1 indicator.")
        )
      )
      return()
    }
    
    if (length(input$outcome) == 0) {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Please select an outcome variable!")
        )
      )
      return()
    }
    
    if (length(input$covariates) + length(input$categorical) == 0) {
      showModal(
        modalDialog(
          title = "Input Error",
          HTML("Please select one or more covariate variables!")
        )
      )
      return()
    }
    
    sampling.weights <- NULL
    if (input$sampw != "") {
      sampling.weights <- df$data %>% pull(input$sampw)
    }
    
    # update the navbar
    shinyjs::hide(selector = "#navbar li a[data-value=eval]")
    shinyjs::hide(selector = "#navbar li a[data-value=effects]") 
    shinyjs::hide(selector = "#navbar li a[data-value=weights]")
    tab$max = 3
    
    # reset the model panel
    shinyjs::hide(id = "prop.score.box")
    
    # reset the effect panel
    shinyjs::hide(id = "effect.est.box")
    
    # clear the warning messages
    app.info$messages <- NULL

    # let the user know that ps() is running
    running.message <- "Calculating propensity scores. Please wait."
    
    # pop-up a message to show that twang is running
    showModal(
      modalDialog(
        title = "TWANG", 
        running.message, 
        footer = NULL, 
        easyClose = FALSE
      )
    )
    
    # generate the formula
    formula <- as.formula(paste0(input$treatment, "~" , paste0(c(input$covariates, input$categorical), collapse = "+")))
    
    # warning/error handling
    tryCatch(
      withCallingHandlers(
        {
          # don't reset modals on error
          m$error <- FALSE
          
          # convert categorical covariates to factors
          tmp <- df$data %>%
            mutate_at(input$categorical, as.factor)
          
          # run propensity score
          m$ps <- ps(
            formula = formula,
            data = tmp,
            n.trees = input$n.trees,
            interaction.depth = input$interaction.depth,
            shrinkage = input$shrinkage,
            estimand = input$estimand,
            stop.method = input$stop.method,
            sampw = sampling.weights,
            verbose = FALSE)
          
          # save the balance table
          m$bal <- bal.table(m$ps)
          
          # show the box
          shinyjs::show(id = "prop.score.box")
          
          # update the UI
          shinyjs::show(selector = "#navbar li a[data-value=eval]")
          shinyjs::show(selector = "#navbar li a[data-value=effects]") 
          shinyjs::show(selector = "#navbar li a[data-value=weights]")
          tab$max = 6
        },
        warning = function(w) {
          if (any(str_detect(deparse(w$call), "^ps\\("))) {
            app.info$messages <- c(app.info$messages, as.character(w))
          }
        }
      ),
      error = function(e) {
        # don't reset modals on error
        m$error <- TRUE
        
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
      }
    )
    
    # close the modal
    if (!m$error) { removeModal() }
    
    # open a warning modal
    n.warnings <- length(app.info$messages)
    modal.title <- sprintf("Warning(s): %i", n.warnings)
    if (n.warnings > 0) {
      modal.text <- ""
      for (message in app.info$messages) {
        modal.text <- ifelse(modal.text == "", message, paste(modal.text, message, sep = "<br/><br/>"))
      }
      
      showModal(
        modalDialog(
          title = modal.title,
          HTML(modal.text),
          easyClose = TRUE
        )
      )
    }
    
    # scroll to top
    # https://stackoverflow.com/questions/49137032/r-shiny-dashboard-scroll-to-top-on-button-click
    shinyjs::runjs("window.scrollTo(0, 0);")
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
              options = 
                list(
                  pageLength = 20, 
                  "dom" = 'Bt', 
                  buttons = list('copy', 'csv', 'excel'), 
                  scrollX = TRUE,
                  scrollY = 300,
                  scrollCollapse = TRUE,
                  fixedColumns = list(leftColumns = 1)
                ),
              extensions = c("Buttons", "FixedColumns"),
              rownames = FALSE) %>% 
       formatRound(c(4:5), 1) %>% formatRound(c(6:9), 3) 
  })
  
  # update dropdown with valid stop method choices
  observeEvent(input$stop.method, {
    updateSelectInput(session, inputId = "conv.plot.stop", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "ps.plot.stop", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "bal.plot.stop", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "bal.table.stop", choices = input$stop.method)
    updateSelectInput(session, inputId = "es.plot.stop", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "ks.plot.stop", choices = input$stop.method, selected = input$stop.method)
  })
  
  #
  # model evaluation/outputs ---
  
  # convergence plot --
  
  convergence.plot <- reactive({
    req(m$ps)
    validate(need(input$conv.plot.stop, message = "Please select stopping method"))
    plot(m$ps, plots = 1, subset = which(input$stop.method == input$conv.plot.stop))
  })
  
  # render plot
  output$conv.plot <- renderPlot({
    print(convergence.plot())
  })
  
  # save plot
  output$conv.plot.save <- downloadHandler(
    filename = "convergence.png",
    content = function(file) {
      png(file)
      print(convergence.plot())
      dev.off()
    }
  )
  
  # propensity score plot --
  
  propensity.plot <- reactive({
    req(m$ps)
    validate(need(input$ps.plot.stop, message = "Please select stopping method"))
    plot(m$ps, plots = 2, subset = which(input$stop.method == input$ps.plot.stop))
  })
  
  # render plot
  output$ps.plot <- renderPlot({
    print(propensity.plot())
  })
  
  # save plot
  output$ps.plot.save <- downloadHandler(
    filename = "propensity-score.png",
    content = function(file) {
      png(file)
      print(propensity.plot())
      dev.off()
    }
  )
  
  # balance plots --
  
  balance.plot <- reactive({
    req(m$ps)
    validate(need(input$bal.plot.stop, message = "Please select stopping method"))
    plot(m$ps, plots = 3, subset = which(input$stop.method == input$bal.plot.stop))
  })
  
  # render plot
  output$bal.plot <- renderPlot({
    print(balance.plot())
  })
  
  # save plot
  output$bal.plot.save <- downloadHandler(
    filename = "balance.png",
    content = function(file) {
      png(file)
      print(balance.plot())
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
    
    # if there are fewer than 20 rows, don't show pagination
    options = ifelse(nrow(unw) > 20, "Btip", "Bt")
    
    # create table
    datatable(
      unw[,c("Variable",c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","Kolmogorov–Smirnov", "KS p-value"))], 
      options = 
        list(
          pageLength = 20, 
          "dom" = options, 
          buttons = list('copy', 'csv', 'excel'), 
          scrollX = TRUE, 
          scrollY = 300, 
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 1)
        ),
      extensions = c("FixedColumns", "Buttons"), 
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
    
    w.tab = m$bal[[paste0(input$bal.table.stop,".",input$estimand)]]
    w.tab$Variable = rownames(w.tab)
    
    cols.bal = c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","t","p-value","Kolmogorov–Smirnov","KS p-value")
    colnames(w.tab) = c(cols.bal,"Variable")
    
    # if there are fewer than 20 rows, don't show pagination
    options = ifelse(nrow(w.tab) > 20, "Btip", "Bt")
    
    datatable(
      w.tab[,c("Variable",c("Treatment Mean","Treatment Standard Deviation","Control Mean","Control Standard Deviation","Standardized Difference","Kolmogorov–Smirnov" , "KS p-value"))], 
      options = 
        list(
          pageLength = 20, 
          "dom" = options, 
          buttons = list('copy', 'csv', 'excel'), 
          scrollX = TRUE, 
          scrollY= 300, 
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 1)
        ),
      extensions = c("FixedColumns", "Buttons"),
      rownames = FALSE
    ) 
  })
  
  # render weighted balance table 
  output$weighted.balance.table <- renderDataTable({
    weighted.balance.table() 
  })
  
  # ES p-values plot --
  
  es.plot <- reactive({
    req(m$ps)
    validate(need(input$es.plot.stop, message = "Please select stopping method"))
    plot(m$ps, plots = 4, subset = which(input$stop.method == input$es.plot.stop))
  })
  
  # render plot
  output$es.plot <- renderPlot({
    print(es.plot())
  })
  
  # save plot
  output$es.plot.save <- downloadHandler(
    filename = "es-p_values.png",
    content = function(file) {
      png(file)
      print(es.plot())
      dev.off()
    }
  )
  
  # KS p-values plot --
  
  ks.plot <- reactive({
    req(m$ps)
    validate(need(input$ks.plot.stop, message = "Please select stopping method"))
    plot(m$ps, plots = 5, subset = which(input$stop.method == input$ks.plot.stop))
  })
  
  # render plot
  output$ks.plot <- renderPlot({
    plot(ks.plot())
  })
  
  # save plot
  output$ks.plot.save <- downloadHandler(
    filename = "ks-p_value.png",
    content = function(file) {
      png(file)
      print(ks.plot())
      dev.off()
    }
  )
  
  # relative influence --
  
  rel.inf.plot <- reactive({
    req(m$ps)
    tmp <- summary(m$ps$gbm.obj)
    ggplot(tmp, aes(x = reorder(var, rel.inf), y = rel.inf)) + 
      geom_bar(stat = "identity") + 
      coord_flip() + 
      xlab("Relative influece") +
      ylab("") + 
      theme_minimal()
  })
  
  # render plot
  output$rel.inf.plot <- renderPlot({
    plot(rel.inf.plot())
  })
  
  # save plot
  output$rel.inf.plot.save <- downloadHandler(
    filename = "relative-influence.png",
    content = function(file) {
      png(file)
      print(rel.inf.plot())
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
    df$vars[!(df$vars %in% c(input$treatment, input$outcome))]
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
  
  # select the stopmethod to use in effect estimation
  observeEvent(input$stop.method, {
    updateSelectInput(session, inputId = "wt.stopmethod", choices = input$stop.method)
  })
  
  # NOTE: for help with this section of code email Matthew Cefalu
  observeEvent(input$out.run, {
    tryCatch({
      # convert categorical covariates to factors
      tmp <- df$data %>%
        mutate_at(input$categorical, as.factor)
      
      # get weights using the selected stopping criteria
      # NOTE: By default, get.weights will use the estimand used to fit the ps object.
      weights = get.weights(m$ps, stop.method = input$ee.stopmethod)
      
      # sepcify the survey design using the extracted weights
      Dsvy = svydesign(id=~1, weights = weights, data=tmp)
      
      # generate the formula
      formula <- as.formula(paste0(input$ee.outcome, "~" , paste0(c(input$treatment, input$ee.covariates), collapse = "+")))
      
      # run survey-weighted generalied linear model
      m$out.model <- svyglm(formula, design = Dsvy, family = input$ee.type)
      
      # find marginal effects
      if (input$ee.type != "gaussian") {
        m$out = margins(m$out.model, variables=input$treatment, design=Dsvy)
      }
      
      if (input$ee.type == "gaussian") {
        # construct table from regression model
        tab.ate = as.data.frame(summary(m$out.model)$coef[input$treatment,,drop=F])
        tab.ate[,"Treatment"] = rownames(tab.ate)
        tab.ate[,"95% CI"] = confint(m$out.model)[input$treatment,] %>% 
          myround(d=3) %>% 
          paste0(collapse=", ") %>% 
          (function(x) paste0("(",x,")"))
        tab.ate = tab.ate[ ,c("Treatment","Estimate","Std. Error","t value","Pr(>|t|)","95% CI")]
        colnames(tab.ate) = c("Treatment",input$estimand,"Standard Error","Test Statistic","p-value","95% Confidence Interval")
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
        colnames(tab.ate) = c("Treatment",input$estimand,"Standard Error","Test Statistic","p-value","95% Confidence Interval") 
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
      colnames(tab.reg) = c("Variable","Coefficient","Standard Error","Test Statistic","p-value","95% Confidence Interval") 
      tab.reg[,2:5] = apply(tab.reg[,2:5],1:2,myround,d=3)
      
      # save to the reactive variable
      m$reg.tbl <- tab.reg
      
      # show the box
      shinyjs::show(id = "effect.est.box")
    }, 
    warning = function(w) {
      # open the error modal
      showModal(
        modalDialog(
          title = "Warning During Analysis",
          HTML(
            paste(
              "There was a warning while running your analysis:",
              "<br><br>",
              "<a style=color:red>", w, "</a>")
          )
        )
      )
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
    
    # if there are fewer than 20 rows, don't show pagination
    options = ifelse(nrow(m$ate.tbl) > 20, "Btip", "Bt")
    
    # create table
    datatable(
      m$ate.tbl, 
      options = list(
        pageLength = 20, 
        "dom" = options,
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
    
    # if there are fewer than 20 rows, don't show pagination
    options = ifelse(nrow(m$reg.tbl) > 20, "Btip", "Bt")
    
    # create table
    datatable(
      m$reg.tbl, 
      options = 
        list(
          pageLength = 50, 
          "dom" = options,
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
  
  # allow the user to download this table
  output$weights.save <- downloadHandler(
    filename = function() {"data_with_weights.csv"},
    content = function(file) {
      # get weights
      weights = get.weights(m$ps, stop.method = input$wt.stopmethod)
      
      # append to the table
      df$data[, input$wt.var] <- weights
      
      # save
      write.csv(df$data, file, row.names = FALSE)
    }
  )

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
})
