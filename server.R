shinyServer(function(input, output, session) {
  # TODO: this needs is where the uploaded data will go
  df <- lalonde
  vars <- names(df)
  
  #
  # button controls ----
  
  # see: https://github.com/daattali/advanced-shiny/blob/master/multiple-pages/app.R
  tabs <- c("intro", "model", "eval", "effects")
  tabs.rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = tabs.rv$page > 1)
    toggleState(id = "nextBtn", condition = tabs.rv$page < 4)
    hide(selector = ".page")
  })
  
  observe({
    if (input$navbar == "intro") tabs.rv$page = 1
    if (input$navbar == "model") tabs.rv$page = 2
    if (input$navbar == "eval") tabs.rv$page = 3
    if (input$navbar == "effects") tabs.rv$page = 4
  })
  
  navPage <- function(direction) {
    tabs.rv$page <- tabs.rv$page + direction
  }
  
  observeEvent(input$prevBtn, {
    navPage(-1)
    updateTabsetPanel(session, "navbar", tabs[tabs.rv$page])
  })
  
  observeEvent(input$nextBtn, {
    navPage(1)
    updateTabsetPanel(session, "navbar", tabs[tabs.rv$page])
  })
  
  #
  # propensity score controls ---
  
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
  
  # write the output of summary()
  output$psm <- renderText({ 
    req(m$ps)
    summary(m$ps) %>%
      as.table() %>%
      kable("html") %>%
      kable_styling("striped", full_width = F)
  })
  
  # for non integer parameters
  shrinkage <- reactive({as.numeric(input$shrinkage)})
  bag.fraction <- reactive({as.numeric(input$bag.fraction)})
  
  # store the results of the ps command
  m <- reactiveValues()
  
  # let the user know something is happening
  observeEvent(input$run, {
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
      bag.fraction = bag.fraction(),
      perm.test.iters = input$perm.test.iters,
      print.level = input$print.level,
      interlim = input$interlim,
      verbose = FALSE,
      estimand = input$estimand,
      stop.method = input$stop.method,
      multinom = input$mulitnom
    )
    
    # save the balance table
    m$bal <- bal.table(m$ps)
    
    # close the modal
    removeModal()
  })
  
  #
  # model evaluation/outputs
  
  # plot
  output$diag.plot <- renderPlot({
    req(m$ps)
    plot(m$ps, plots = which(plot.types == input$diag.plot.select))
  })
  
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
  
  # balance table: es mean ATT
  output$balance.table.es <- renderText({
    req(m$ps)
    tmp <- m$bal$es.mean.ATT
    if (!is.null(tmp)) {
      tmp %>%
        kable("html") %>%
        kable_styling("striped", full_width = FALSE)
    }
  })
  
  # balance table: ks mean ATT
  output$balance.table.ks <- renderText({
    req(m$ps)
    tmp <- m$bal$ks.mean.ATT
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
  
})
