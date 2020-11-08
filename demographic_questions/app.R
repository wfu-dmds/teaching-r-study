library(shiny)
library(shinyjs)
library(rdrop2)
library(glue)
library(taskdesignr)

# define js function for opening urls in new tab/window
# based on https://stackoverflow.com/questions/41426016/shiny-open-multiple-browser-tabs
js_code <- "
shinyjs.openExperiment = function(url) {
  window.location.replace(url);
}
"

# Set the URL for where the experiment is located
experiment_url <- "https://jdtrat-apps.shinyapps.io/teaching_r_study/?user_id="

#establish SASS file
sass::sass(
  sass::sass_file("www/survey.scss"),
  output = "www/survey.css"
)

ui <- shiny::fillPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "survey.css")
  ),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = js_code,
                         functions = 'openExperiment'),
  div(class = "grid",
      div(class = "survey",
          h1("Demographic Questions"),
          taskdesignr::surveyOutput(df = taskdesignr::teaching_r_questions,
                                    icon = icon("check"))))
)

server <- function(input, output, session) {
  
  taskdesignr::renderSurvey(input = input, 
                            df = taskdesignr::teaching_r_questions,
                            session = session)
  
  # Load dropbox authorization
  rdrop2::drop_auth(rdstoken = 'token.rds')
  
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
                path = glue("teaching-r-study"))
  }
  
  # open user-specific link
  shinyjs::onclick("submit", 
                   js$openExperiment(paste0(experiment_url, input$userID, "/")))
  #save user-specific survey data to dropbox.
  observeEvent(input$submit, {save_survey_data(username = input$userID)})
  
}
  
shiny::shinyApp(ui = ui, server = server)



