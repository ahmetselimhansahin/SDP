# Bitirme deneme 3

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(markdown)
library(knitr)
library(tidyr)
library(maps)
library(ggmap)
library(geosphere)
library(ggrepel)
library(viridis)
library(nycflights13)
library(mapproj)

portfolio_data_v1 <- read.csv("data/portfolio_data_v1.csv", encoding="UTF-8", stringsAsFactors=FALSE)

portfolio <- portfolio_data_v1

colnames(portfolio) <- c("proje","masraf_merkezi","proje_kodu","tip","system","longtitude","latitude")

ui <- fluidPage(
  titlePanel("SDP App"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "var",
        label = "choose a variable to display",
        choices = c("tip","system"),
        selected = "tip"
      ),
      selectInput(
        "var2",
        "choose graph type",
        choices = c("bar","pie"),
        selected = "pie"
      )
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("text")
  )
  
))

server <- function(input, output, session) {
  
  condition <- reactive(isTRUE(input$var2 == "bar"))
  output$text <- renderText(condition())
  
  output$plot <- renderPlot({
    
    if(condition() == FALSE){
      ggplot(portfolio, aes(x = factor(1), fill = eval(parse(text = input$var)))) +
        geom_bar(width = 1) + coord_polar(theta = "y") + 
        ggtitle(paste("Status of ","'",input$var,"'")) + labs(fill = input$var)
      
    }
    else{
      portfolio %>% ggplot(aes(x=reorder(factor(proje_kodu), proje_kodu, function(x) length(x)), fill=eval(parse(text = input$var)))) + 
        geom_bar() + coord_flip() +
        labs(title=paste("Bar Chart of Projects Stacked by ","'",input$var,"'")) + 
        labs(x="Project Code", y="Count", fill = input$var)
    }
    
    
  })
  
  
  
    
    
  }
  

  
  


shinyApp(ui, server)