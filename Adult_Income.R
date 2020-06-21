#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

rm(list = ls())
gc()

ADULT <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                    sep = ",", header = F,
                    strip.white = TRUE)    # 데이터 불러올 때 데이터 앞뒤 공백 제거


names(ADULT) <- c("AGE", "WORK", "FINAL_WEIGHT", "EDU", "EDU_NUM",
                  "MARTIAL", "JOB", "RELATIONSHIP", "RACE", "SEX",
                  "CAP_GAIN", "CAP_LOSS", "HOURS", "COUNTRY", "INCOME")


n_total <- NROW(ADULT)
ADULT %>% glimpse()

# Define UI for application
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Text instructions
      HTML(paste("Enter a value between 1 and", n_total)),
      
      # Numeric input for sample size
      numericInput(inputId = "n",
                   label = "Sample size:",
                   value = 50,
                   min = 1, max = n_total,
                   step = 1),

      # Select variable for y-axis
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("AGE", "WORK", "FINAL_WEIGHT", "EDU", "EDU_NUM",
                              "MARTIAL", "JOB", "RELATIONSHIP", "RACE", "SEX",
                              "CAP_GAIN", "CAP_LOSS", "HOURS", "COUNTRY", "INCOME"),
                  selected = "INCOME"),

      # Select variable for x-axis
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("AGE", "WORK", "FINAL_WEIGHT", "EDU", "EDU_NUM",
                              "MARTIAL", "JOB", "RELATIONSHIP", "RACE", "SEX",
                              "CAP_GAIN", "CAP_LOSS", "HOURS", "COUNTRY", "INCOME"),
                  selected = "AGE"),
      
      # Select variable for x1-axis
      selectInput(inputId = "x1",
                  label = "X1-axis:",
                  choices = c("WORK", "EDU", "MARTIAL", "JOB", "RELATIONSHIP", "RACE", "SEX", "COUNTRY"),
                  selected = "WORK")

      # # Select variable for color
      # selectInput(inputId = "z",
      #             label = "Color by:",
      #             choices = c("WORK", "EDU",
      #                         "MARTIAL", "JOB", "RELATIONSHIP", "RACE", "SEX",
      #                         "COUNTRY", "INCOME"),
      #             selected = "WORK")

    ),

    # Output: Show data table
    mainPanel(
      DT::dataTableOutput(outputId = "ADULT", width = "100%"),
      plotOutput(outputId = "histogram"), 
      plotOutput(outputId = "barplot"),
      plotOutput(outputId = "densityplot", height = 200)
    )
  )
)

# Define server function 
server <- function(input, output) {
  
  
  # Create data table
  output$ADULT <- DT::renderDataTable({
    
    
    # Add a line with req(input$n) in the renderDataTable function in the server before movies_sample is calculated.
    req(input$n)
    
    ADULT_SAMPLE <- ADULT %>% 
      sample_n(input$n)
    DT::datatable(data = ADULT_SAMPLE, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  
  # Create histogram 
  output$histogram <- renderPlot({
    ggplot(data = ADULT, aes_string(x = input$x, 
                                    fill = input$y)) + 
      geom_histogram(stat = "bin")
  })
  
  # Create barplot
  output$barplot <- renderPlot({
    a <- ADULT %>%
      group_by(input$x1, input$y) %>%
      summarise(CNT = n())
    
  a %>% ggplot(aes_string(x = reorder(input$x1, CNT, fun = mean),
                        y = CNT, 
                        fill = input$y)) +
      geom_bar(stat = "identity", position = "fill") +
      coord_flip()
  })
  
  # Create Densityplot
  output$densityplot <- renderPlot({
    ggplot(data = ADULT, aes_string(x = input$x)) + 
      geom_density()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

