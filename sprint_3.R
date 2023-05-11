library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(ggtext)
library(RColorBrewer)

student_data <- read.csv("your-path\\student_data.csv")
student_scores <- read.csv("your-path\\student_scores.csv")

ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Attendance", tabName = "attendance", icon = icon("check-square")),
      menuItem("Hours spent on Moodle", tabName = "moodlehours", icon = icon("desktop")),
      menuItem("Exam Scores", tabName = "exams", icon = icon("book")),
      
      menuItem("Filters", icon = icon("sliders")),
      selectInput("student_id", label = "Select Student ID", 
                  choices = unique(na.omit(student_data$student_id)), 
                  selected = NULL, width = "100%"),
      
      selectInput("year", label = "Select Year",
                  choices = c("All", "2020", "2021"), 
                  selected = "All", width = "100%"),
      
      selectInput("module", label = "Select Module",
                  choices = unique(na.omit(student_scores$module)),
                  selected = NULL, width = "100%"),
      
      checkboxInput("show_trendline", label = "Show Trend Line", value = TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "attendance",
              plotOutput("attendance_plot")
      ),
      tabItem(tabName = "moodlehours",
              plotOutput("moodle_hours")
      ),
      tabItem(tabName = "exams",
              plotOutput("exam_scores")
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    student_data %>%
      mutate(date = as.Date(date, format = "%d/%m/%Y"),
             year_month = format(date, "%Y-%m"),
             month_name = format(date, "%b %Y")) %>%
      filter(student_id == input$student_id) %>%
      filter(input$year == "All" | substr(year_month, 1, 4) == input$year)
  })
  
  create_attendance_plot <- function(data, show_trend_line) {
    p <- ggplot(data, aes(x = year_month, y = attendance_per_month, group = student_id, color = student_id)) +
      geom_line()
    
    if (show_trend_line) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    p <- p + scale_x_discrete(limits = unique(data$year_month), labels = unique(data$month_name)) +
      labs(x = "Month", y = "Attendance per month(%)", color = "Student ID") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  }
  
  create_moodle_hours_plot <- function(data, show_trend_line) {
    p <- ggplot(data, aes(x = year_month, y = hours_on_moodle, group = student_id, color = student_id)) +
      geom_line()
    
    if (show_trend_line) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    p <- p + scale_x_discrete(limits = unique(data$year_month), labels = unique(data$month_name)) +
      labs(x = "Month", y = "Hours on Moodle(hrs)", color = "Student ID") +
      theme_minimal()  +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  }
  
  output$attendance_plot <- renderPlot({
    create_attendance_plot(filtered_data(), input$show_trendline)
  })
  
  output$moodle_hours <- renderPlot({
    create_moodle_hours_plot(filtered_data(), input$show_trendline)
  })
  
  output$exam_scores <- renderPlot({
    student_scores_filtered <- student_scores %>%
      filter(student_id == input$student_id, module == input$module) %>%
      select(student_id, module, year, ass_overall, ass_avg) %>%
      pivot_longer(cols = c("ass_overall", "ass_avg"), names_to = "type", values_to = "score")
    
    ggplot(student_scores_filtered, aes(x = type, y = score, fill = type)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5) +
      labs(x = "Assessment", y = "Score(%)", title = "Assessment Scores") +
      scale_fill_manual(values = c("ass_overall" = "blue", "ass_avg" = "red"), 
                        name = "Assessment Type", 
                        labels = c("Overall module score", "Student score")) +
      guides(fill = guide_legend(title = "Legend", 
                                 override.aes = list(shape = NA, linetype = "blank", size = 2),
                                 label.position = "bottom", label.hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(legend.position = "right") 
    
  })
  
  observeEvent(input$module, {
    input_module <<- input$module
  })
  
}




shinyApp(ui, server)