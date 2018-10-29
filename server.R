shinyServer(function(input, output, session) {
  # TODO: this needs is where the uploaded data will go
  df <- lalonde
  vars <- names(df)
  
  #
  # button controls ----
  
  # see: https://github.com/daattali/advanced-shiny/blob/master/multiple-pages/app.R
  tabs <- c("intro", "model", "eval", "output")
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
    if (input$navbar == "output") tabs.rv$page = 4
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
  # application control ---
  
  # select the treatment variable
  output$treatment <- renderUI({
    selectInput(
      inputId = "sel_treatment",
      label = "Treatment variable",
      choices = c("", vars)
    )
  })
  
  # select the covariates
  observeEvent(input$sel_treatment, {
    if (input$sel_treatment != "") {
      choices <- vars[vars != input$sel_treatment]
      updateSelectInput(session, inputId = "covariates", choices = choices)
    }
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
    
    # load data for debug
    m$ps <- ps.lalonde
    
    # # generate the formula
    # formula <- as.formula(paste0(input$sel_treatment, "~" , paste0(input$covariates, collapse = "+")))
    # 
    # # run propensity score
    # m$ps <- ps(
    #   formula = formula,
    #   data = df,
    #   n.trees = input$n.trees,
    #   interaction.depth = input$interaction.depth,
    #   shrinkage = shrinkage(),
    #   bag.fraction = bag.fraction(),
    #   perm.test.iters = input$perm.test.iters,
    #   print.level = input$print.level,
    #   interlim = input$interlim,
    #   verbose = FALSE,
    #   estimand = input$estimand,
    #   stop.method = input$stop.method,
    #   multinom = input$mulitnom
    # )
    
    # save the balance table
    m$bal <- bal.table(m$ps)
    
    # close the modal
    removeModal()
  })
  
  # plot 1
  output$ps.plot1 <- renderPlot({
    req(m$ps)
    plot(m$ps, plots = 1)
  })
  
  # plot 2
  output$ps.plot2 <- renderPlot({
    req(m$ps)
    plot(m$ps, plots = 2)
  })
  
  # plot 3
  output$ps.plot3 <- renderPlot({
    req(m$ps)
    plot(m$ps, plots = 3)
  })
  
  # plot 4
  output$ps.plot4 <- renderPlot({
    req(m$ps)
    plot(m$ps, plots = 4)
  })
  
  # plot 5
  output$ps.plot5 <- renderPlot({
    req(m$ps)
    plot(m$ps, plots = 5)
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
  
})