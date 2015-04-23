library(shiny)
library(shinydashboard)
source("load_data.R")
source("load_names.R")
source("helpers.R")

shinyServer(function(input, output, session) {

  #Update name selections based on nation selection
  observe({
    selected_name <- input$nameInput1
    updateSelectInput(session,
                      inputId = "nameInput2",
                      choices = c("",sort(unique(DATA$name[DATA$name != selected_name]))))
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
