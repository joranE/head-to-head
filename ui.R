library(shiny)
library(shinydashboard)
source("load_names.R")

shinyUI(dashboardPage(
    #Header
    dashboardHeader(title = "Statistical Skier"),
    
    #Sidebar
    dashboardSidebar(
      selectInput(inputId = "nameInput1",
                  label = "First Athlete",
                  choices = c("Choose first athlete..." = "",NAMES),
                  selected = NULL,
                  selectize = TRUE),
      helpText("...versus..."),
      selectInput(inputId = "nameInput2",
                  label = "Opposing athletes",
                  choices = "",
                  selected = "",
                  multiple = TRUE,
                  selectize = TRUE),
      checkboxInput(inputId = "byTech",label = "Summarise by technique",value = FALSE),
      hr(),
      menuItem(
        menuSubItem(text = "All FIS Results",tabName = "allFIS"),
        menuSubItem(text = "Major International",tabName = "majInt"),
        text = "Charts",
        icon = icon("bar-chart"))
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

