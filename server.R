library(shiny)
library(shinydashboard)
library(Cairo)
options(shiny.usecairo = TRUE)
source("helpers.R")
source("load_data.R")
source("load_names.R")

function(input, output, session) {
  
  get_names1 <- reactive({
    selected_gender <- input$genderInput
    NAMES[NAMES$gender == selected_gender,]
  })
  
  get_names2 <- reactive({
    selected_gender <- input$genderInput
    name1 <- input$nameInput1
    NAMES[NAMES$gender == selected_gender & NAMES$name != name1,]
  })
  
  #Update primary name choices based on gender selection
  observe({
    updateSelectInput(session,
                      inputId = "nameInput1",
                      choices = c("Select primary athlete..." = "",get_names1()$name))
  })
  
  #Update opponent name selections based on primary name selection
  observe({
    selected_name <- input$nameInput1
    updateSelectInput(session,
                      inputId = "nameInput2",
                      choices = c("",get_names2()$name))
  })
  
  output$fis_dst <- renderPlot({
    plot_fis_dst(ath1 = input$nameInput1,
                 ath2 = input$nameInput2,
                 by_tech = input$byTech)
  })
  
  output$fis_spr <- renderPlot({
    plot_fis_spr(ath1 = input$nameInput1,
                 ath2 = input$nameInput2,
                 by_tech = input$byTech)
  })
  
  output$maj_dst <- renderPlot({
    plot_maj_dst(ath1 = input$nameInput1,
                 ath2 = input$nameInput2,
                 by_tech = input$byTech)
  })
  
  output$maj_spr <- renderPlot({
    plot_maj_spr(ath1 = input$nameInput1,
                 ath2 = input$nameInput2,
                 by_tech = input$byTech)
  })
  
  output$win_loss_fis <- renderPlot({
    won_loss_plot(ath1 = input$nameInput1,
                  ath2 = input$nameInput2,
                  by_tech = input$byTech,
                  maj_int = FALSE)
  })
  
  output$win_loss_maj <- renderPlot({
    won_loss_plot(ath1 = input$nameInput1,
                  ath2 = input$nameInput2,
                  by_tech = input$byTech,
                  maj_int = TRUE)
  })
  
}
