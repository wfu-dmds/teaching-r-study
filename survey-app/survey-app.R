## Deploy to wfudatasciencelab.shinyapps.io/learnr-demographics
library(shinysurveys)
library(shinyalert)
library(shiny)
library(rdrop2)
library(tibble)
library(shinyjs)
library(glue)

# define js function for opening urls in new tab/window
# based on https://stackoverflow.com/questions/41426016/shiny-open-multiple-browser-tabs
js_code <- "
shinyjs.openExperiment = function(url) {
  window.location.replace(url);
}
"

library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = js_code,
                         functions = c('openExperiment')),
  shinyalert::useShinyalert(),
  shinysurveys::surveyOutput(df = shinysurveys::teaching_r_questions,
                             survey_title = "Demographic Questions",
                             icon = icon("check"))
)

server <- function(input, output, session) {
  shinysurveys::renderSurvey(df = shinysurveys::teaching_r_questions)
  
  # Load dropbox authorization
  rdrop2::drop_auth(rdstoken = 'www/token.rds')
  
  # Function to save survey data to dropbox
  save_survey_data <- function(username) {
    
    data <- tibble::tribble(~username, ~question, ~response,
                            input$userID, "age", as.character(input$age),
                            input$userID, "gender", input$gender,
                            input$userID, "self_describe_gender", input$self_describe_gender,
                            input$userID, "education_attained", input$education_attained,
                            input$userID, "first_language", input$first_language,
                            input$userID, "first_language_other", input$first_language_other,
                            input$userID, "read_language", input$read_language,
                            input$userID, "learned_r", input$learned_r,
                            input$userID, "years_using_r", as.character(input$years_using_r),
                            input$userID, "learned_programming_not_r",input$learned_programming_not_r,
                            input$userID, "years_programming_not_r", input$years_programming_not_r,
                            input$userID, "completed_data_analysis", input$completed_data_analysis,
                            input$userID, "number_completed_data_analysis", input$number_completed_data_analysis
    )
    
    saveRDS(data, file = glue("survey_{username}.rds"))
    rdrop2::drop_upload(file = glue("survey_{username}.rds"),
                        path = "/D'Agostino McGowan Data Science Lab/survey_results/")
  }
  
  #save user-specific survey data to dropbox.
  observeEvent(input$submit, {
    shinyalert::shinyalert(title = "Redirecting to Modules Now...", 
                           type = "info",
                           showConfirmButton = FALSE)
    save_survey_data(username = input$userID)
    
    # Send to appropriate learnr modules
    if (grepl("_GA", input$userID)) {
      shinyjs::js$openExperiment(paste0("https://lucy.shinyapps.io/learnr-a/?user_id=", input$userID, "/"))
    } else if (grepl("_GB", input$userID)) {
      shinyjs::js$openExperiment(paste0("https://lucy.shinyapps.io/learnr-b/?user_id=", input$userID, "/"))
    }
    
  })
  
}

shinyApp(ui, server)