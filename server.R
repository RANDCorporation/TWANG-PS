shinyServer(function(input, output, session) {
  data(lalonde)
  df <- lalonde
  vars <- names(df)
  
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
  
  # write the formula
  output$formula <- renderText({ 
    if (!is.null(input$sel_treatment) & !is.null(input$covariates)) {
      paste0(input$sel_treatment, "~" , paste0(input$covariates, collapse = "+"))
    }
  })
  
  # for non integer parameters
  shrinkage <- reactive({as.numeric(input$shrinkage)})
  bag.fraction <- reactive({as.numeric(input$bag.fraction)})
  
  # store the results of the ps command
  ps.results <- reactiveValues()
  
  # let the user know something is happening
  observeEvent(input$run, {
    showModal(modalDialog(title = "TWANG", "Calculating propensity scores. Please wait.", footer = NULL, easyClose = FALSE))

    # generate the formula
    formula <- as.formula(paste0(input$sel_treatment, "~" , paste0(input$covariates, collapse = "+")))
    
    # run propensity score
    ps.results$ps <- ps(
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
    
    # close the modal
    removeModal()
  })
  
  # plot 1
  output$ps.plot1 <- renderPlot({
    req(ps.results$ps)
    plot(ps.results$ps, plots = 1)
  })
  
  # plot 2
  output$ps.plot2 <- renderPlot({
    req(ps.results$ps)
    plot(ps.results$ps, plots = 2)
  })
  
  # plot 3
  output$ps.plot3 <- renderPlot({
    req(ps.results$ps)
    plot(ps.results$ps, plots = 3)
  })
  
  # plot 4
  output$ps.plot4 <- renderPlot({
    req(ps.results$ps)
    plot(ps.results$ps, plots = 4)
  })
  
  # plot 5
  output$ps.plot5 <- renderPlot({
    req(ps.results$ps)
    plot(ps.results$ps, plots = 5)
  })
  
})