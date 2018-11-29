shinyServer(function(input, output, session) {
  #
  # navbar controls ---
    
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
    if (tab$max == 2)
    {
      toggle(selector = "#navbar li a[data-value=eval]")
      toggle(selector = "#navbar li a[data-value=effects]") 
      toggle(selector = "#navbar li a[data-value=weights]")
    }
    tab$max = length(tab.names)
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
    if (input$navbar == "model") tab$page = 2
    if (input$navbar == "eval") tab$page = 3
    if (input$navbar == "effects") tab$page = 4
    if (input$navbar == "weights") tab$page = 5
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
  # propensity score model ---
  
  # TODO: this needs is where the uploaded data will go
  df <- lalonde
  vars <- names(df)
  
  # select the treatment variable
  # TODO: this needs to be reactive
  updateSelectInput(session, inputId = "treatment", choices = c("", vars))
  
  # list of outcome variables
  outcomes <- reactive({
    vars[!(vars %in% c(input$treatment, input$covariates))]
  })
  
  # select the outcome variable
  observeEvent(outcomes(), {
    updateSelectInput(session, inputId = "outcome", choices = c("", outcomes()), selected = input$outcome)
  })
  
  # list of covariates
  covariates <- reactive({
    vars[!(vars %in% c(input$treatment, input$outcome))]
  })
  
  # select the covariates
  observeEvent(covariates(), {
    updateSelectInput(session, inputId = "covariates", choices = covariates(), selected = input$covariates)
  })
  
  # for non integer parameters
  shrinkage <- reactive({as.numeric(input$shrinkage)})
  bag.fraction <- reactive({as.numeric(input$bag.fraction)})
  
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
    
    # TODO: do we need to validate all inputs
    
    # set seed
    set.seed(input$seed)
    
    # pop up a message so the user knows the code is running
    showModal(modalDialog(title = "TWANG", "Calculating propensity scores. Please wait.", footer = NULL, easyClose = FALSE))
    
    # generate the formula
    formula <- as.formula(paste0(input$treatment, "~" , paste0(input$covariates, collapse = "+")))
    
    # run propensity score
    m$ps <- ps(
      formula = formula,
      data = df,
      n.trees = input$n.trees,
      interaction.depth = input$interaction.depth,
      shrinkage = shrinkage(),
      stop.method = input$stop.method,
      estimand = input$estimand,
      verbose = FALSE
    )
        
    # save the balance table
    m$bal <- bal.table(m$ps)
    
    # close the modal
    removeModal()
  })
  
  # write the output of summary()
  output$psm <- renderText({ 
    req(m$ps)
    summary(m$ps) %>%
      as.table() %>%
      kable("html") %>%
      kable_styling("striped", full_width = FALSE)
  })
  
  
  #
  # model evaluation/outputs ---
  
  # update stop method choices
  observeEvent(input$stop.method, {
    updateSelectInput(session, inputId = "diag.plot.stopmethod", choices = input$stop.method, selected = input$stop.method)
    updateSelectInput(session, inputId = "bal.stopmethod", choices = input$stop.method)
  })
  
  # plot function
  diag.plot <- reactive({
    req(m$ps)
    plot(m$ps, plots = which(plot.types == input$diag.plot.select), subset = which(input$stop.method == input$diag.plot.stopmethod))
  })
  
  # plot
  output$diag.plot <- renderPlot({
    print(diag.plot())
  })
  
  # save plot
  output$diag.plot.save <- downloadHandler(
    filename = "diagnostic-plot.png",
    content = function(file) {
      png(file)
      print(diag.plot())
      dev.off()
    }
  )
  
  # balance table: unw
  output$balance.table.unw <- renderText({
    req(m$bal)
    tmp <- m$bal$unw
    if (!is.null(tmp)) {
      tmp %>%
        kable("html") %>%
        kable_styling("striped", full_width = FALSE)
    }
  })
  
  # balance table
  output$balance.table <- renderText({
    req(m$bal)
    tmp <- m$bal[[paste0(input$bal.stopmethod,".",input$estimand)]]
    if (!is.null(tmp)) {
      tmp %>%
        kable("html") %>%
        kable_styling("striped", full_width = FALSE)
    }
  })
  
  
  #
  # effect estimation ---
  
  # select the outcome variable
  # NOTE: this is limited to variables listed as outcomes in the twang options
  observeEvent(input$outcome, {
    if (!is.null(input$outcome)) {
      updateSelectInput(session, inputId = "ee.outcome", choices = c("", input$outcome))
    }
  })
  
  # select the covariates
  # NOTE: this can be any variable that is not the specified outcome or the treatment
  observeEvent(covariates(), {
    updateSelectInput(session, inputId = "ee.covariates", choices = covariates())
  })
  
  # select the stopmethod to use in effect estimation
  observeEvent(input$stop.method, {
    updateSelectInput(session, inputId = "ee.stopmethod", choices = input$stop.method)
  })
  
  observeEvent(input$out.run, {
    # pop-up message indicating that twang is running
    showModal(modalDialog(title = "TWANG", "Estimating treatment effects. Please wait.", footer = NULL, easyClose = FALSE))
    
    # extract weights and set up svy
    m$wt = get.weights(m$ps, stop.method = input$ee.stopmethod, estimand=input$estimand)
    Dsvy = svydesign(id=~1, weights = m$wt, data=df)
    
    # generate the formula
    formula <- as.formula(paste0(input$ee.outcome, "~" , paste0(c(input$treatment,input$covariates), collapse = "+")))
    
    # run propensity score
    m$out.model <- svyglm(formula, design = Dsvy, family = input$ee.type)
    
    # find marginal effects
    m$out = margins(m$out.model, variables=input$treatment, design=Dsvy)
    
    # close the modal
    removeModal()
  })
  
  # write the output of summary()
  output$out.model <- renderText({ 
    req(m$out)
    summary(m$out) %>%
      kable("html", caption = "TABLE TITLE") %>%
      kable_styling("striped", full_width = TRUE)
  })
  
  # coefficients
  output$out.model.summary <- renderText({
    req(m$out.model)
    summary(m$out.model)$coefficient %>%
      kable("html", caption = "TABLE TITLE") %>%
      kable_styling("striped", full_width = TRUE)
  })
  
  #
  # weights ---
  
  df.w <- reactive({
    df %>%
      mutate(!!input$weight.var := m$wt)
  })
  
  output$weights.tbl = DT::renderDataTable({
    req(m$wt)
    df.w()
  })
  
  output$weights.save <- downloadHandler(
    filename = function() {"data_with_weights.csv"},
    content = function(file) {
      write.csv(df.w(), file, row.names = FALSE)
    }
  )
})
