library(taskdesignr)
library(shinyalert)

ui <- shiny::fluidPage(
  taskdesignr::surveyOutput(df = taskdesignr::teaching_r_questions)
)

server <- function(input, output, session) {
  taskdesignr::renderSurvey(input = input, 
                            df = taskdesignr::teaching_r_questions,
                            session = session)
  
  
  observeEvent(input$submit, {
    # The following is the link to use when redirecting.
    print(paste0("https://jdtrat-apps.shinyapps.io/taskdesignr_survey_user_tracking/?user_id=", input$userID))
})
  
}

shiny::shinyApp(ui = ui, server = server)