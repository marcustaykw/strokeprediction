pacman::p_load(tidyverse, tidymodels, shiny, shinydashboard)

model <- readRDS("finalized_stroke_model_28nov.RDS")

model$pre$mold$predictors %>% as_tibble()
model$pre$mold$predictors %>% colnames()

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Stroke Prediction App",
                  titleWidth = 320),
  
  dashboardSidebar(
    menuItem(
      "Stroke Prediction App",
      tabName = "stroke_tab",
      icon = icon("heart")
      )
    ),
  dashboardBody(
    tabItem(
      tabName = "stroke_tab",
      box(valueBoxOutput("stroke_prediction")),
      box(selectInput("gender",
                      label = "Gender",
                      choices = c("Male", "Female", "Other"))),
      box(selectInput("hypertension",
                      label = "Ever had Hypertension? (1 = Yes, 0 = No)",
                      choices = c("1", "0"))),
      box(selectInput("heart_disease",
                      label = "Ever had Heart Disease? (1 = Yes, 0 = No)",
                      choices = c("1", "0"))),
      box(selectInput("ever_married",
                      label = "Ever Married?",
                      choices = c("Yes", "No"))),
      box(selectInput("work_type",
                      label = "Work Type",
                      choices = c("children", "Govt_job", "Never_worked", "Private", "Self-employed"))),
      box(selectInput("Residence_type",
                      label = "Residence Type",
                      choices = c("Urban", "Rural"))),
      box(selectInput("smoking_status",
                      label = "Smoking Status",
                      choices = c("formerly smoked", "never smoked", "smokes", "Unknown"))),
      box(sliderInput("age",
                      label = "Age in Years",
                      min = 0, max = 82, value = 41)),
      box(sliderInput("avg_glucose_level",
                      label = "Average Glucose Level",
                      min = 55, max = 272, value = 158)),
      box(sliderInput("bmi",
                      label = "Body Mass Index",
                      min = 10, max = 98, value = 54))
    )
  )
)


server <- function(input, output)
{
  output$stroke_prediction <- renderValueBox({
    
    prediction <- predict(
      model,
      tibble("gender" = input$gender,
             "hypertension"= input$hypertension,
             "heart_disease" = input$heart_disease,
             "ever_married" = input$ever_married,
             "work_type" = input$work_type,
             "Residence_type" = input$Residence_type,
             "smoking_status" = input$smoking_status,
             "age" = input$age,
             "avg_glucose_level" = input$avg_glucose_level,
             "bmi" = input$bmi)
    )
    
    prediction_prob <- predict(
      model,
      tibble("gender" = input$gender,
             "hypertension"= input$hypertension,
             "heart_disease" = input$heart_disease,
             "ever_married" = input$ever_married,
             "work_type" = input$work_type,
             "Residence_type" = input$Residence_type,
             "smoking_status" = input$smoking_status,
             "age" = input$age,
             "avg_glucose_level" = input$avg_glucose_level,
             "bmi" = input$bmi),
      type = "prob"
    ) %>% 
      gather() %>% 
      arrange(desc(value)) %>% 
      dplyr::slice(1) %>% 
      select(value)
    
    prediction_color <- if_else(prediction$.pred_class == "1", "red", "green")
    
    prediction_statement <- if_else(prediction$.pred_class == "1", "YES", "NO")
    
    valueBox(
      value = paste0(round(100*prediction_prob$value, 0), "%"),
      subtitle = paste0("Will this patient/person have stroke?: ",
                        prediction_statement),
      color = prediction_color,
      icon = icon("heart")
    )
  })
}

shinyApp(ui, server)
