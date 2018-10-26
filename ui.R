suppressWarnings(library(shiny))
suppressWarnings(library(shinydashboard))

header <- dashboardHeader(disable = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home Page", tabName = "homepage", icon = icon("home")),
    menuItem("TWANG", tabName = "twang", icon = icon("bar-chart-o"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "homepage",
      box(
        width = NULL,
        h2("Introduction"),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
      )
    ),
    tabItem(
      tabName = "twang",
      box(
        width = 4,
        title = "Twang options",
        uiOutput("treatment"),
        selectInput("covariates", "Covariates", "", multiple = TRUE),
        numericInput("n.trees", "gbm iterations", 5000),
        numericInput("interaction.depth", "Interaction depth", 2),
        textInput("shrinkage", "Shrinkage", "0.01"),
        textInput("bag.fraction", "Bag fractions", "1.0"),
        numericInput("perm.test.iters", "Permutation test iterations", 0),
        numericInput("print.level", "Print level", 2),
        numericInput("interlim", "Max interations for direct optimizaiton", 1000),
        checkboxInput("verbose", "Verbose", value = FALSE),
        selectInput("estimand", "Estimand", choices = c("ATE", "ATT")),
        selectInput("stop.method", "Stop method", choices = c("es.mean", "ks.max"), selected = c("es.mean", "ks.max"), multiple = TRUE),
        checkboxInput("mulitnom", "Multinom", FALSE),
        actionButton("run", "Run Analysis", icon("paper-plane"), style="color: #fff; background-color: #663399;")
      ),
      box(
        width = 8,
        box(
          width = NULL,
          textOutput("formula")
        ),
        box(
          width = NULL,
          plotOutput("ps.plot1"),
          plotOutput("ps.plot2"),
          plotOutput("ps.plot3"),
          plotOutput("ps.plot4"),
          plotOutput("ps.plot5")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")