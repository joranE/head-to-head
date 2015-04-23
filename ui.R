library(shiny)
library(shinydashboard)
source("load_names.R")

shinyUI(dashboardPage(
    #Header
    dashboardHeader(title = "Statistical Skier"),
    
    #Sidebar
    dashboardSidebar(
      selectInput(inputId = "genderInput",
                  label = "Gender",
                  choices = c("Men","Women"),
                  selected = "Men"),
      selectizeInput(inputId = "nameInput1",
                  label = "First Athlete",
                  choices = "",
                  selected = ""),
      helpText("...versus..."),
      selectizeInput(inputId = "nameInput2",
                  label = "Opposing athletes",
                  choices = "",
                  selected = "",
                  multiple = TRUE,
                  options = list(maxItems = 3)),
      checkboxInput(inputId = "byTech",label = "Summarise by technique",value = FALSE),
      hr(),
      menuItem(
        menuSubItem(text = "All FIS Results",tabName = "allFIS"),
        menuSubItem(text = "Major International",tabName = "majInt"),
        text = "Charts",
        icon = icon("bar-chart")),
      hr(),
      helpText("sMPB: Standardized percent behind the median skier. This is only
               used for major international distance events.")
    ),
    
    #Body
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "allFIS",
          fluidRow(
            column(width = 6,
                   box(plotOutput("fis_dst"),
                       width = NULL,
                       title = "Head-to-Head Distance",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       status = "primary")),
            column(width = 6,
                   box(plotOutput("fis_spr"),
                       width = NULL,
                       title = "Head-to-Head Sprint",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       status = "primary"))
          ),
          fluidRow(
            box(plotOutput("win_loss_fis"),
                width = NULL,
                title = "Win-Loss Record",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary"))),
        
        tabItem(
          tabName = "majInt",
          fluidRow(
            column(width = 6,
                   box(plotOutput("maj_dst"),
                       width = NULL,
                       title = "Head-to-Head Distance",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       status = "primary")),
            column(width = 6,
                   box(plotOutput("maj_spr"),
                       width = NULL,
                       title = "Head-to-Head Sprint",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       status = "primary")
            )
          ),
    
          fluidRow(
            box(plotOutput("win_loss_maj"),
                width = NULL,
                title = "Win-Loss Record",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary")
          )
        )
      )
    )
  )
)

