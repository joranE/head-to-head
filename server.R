library(shiny)
library(shinydashboard)
source("load_data.R")
source("load_names.R")
source("helpers.R")

shinyServer(function(input, output, session) {
  
  #Update primary name choices based on gender selection
  observe({
    selected_gender <- input$genderInput
    updateSelectInput(session,
                      inputId = "nameInput1",
                      choices = c("Select primary athlete..." = "",NAMES$name[NAMES$gender == selected_gender]))
  })
  
  #Update opponent name selections based on primary name selection
  observe({
    selected_name <- input$nameInput1
    updateSelectInput(session,
                      inputId = "nameInput2",
                      choices = c("",NAMES$name[NAMES$name != selected_name & 
                                                         NAMES$gender == input$genderInput]))
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

)
