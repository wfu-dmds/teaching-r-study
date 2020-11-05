library(shiny)
library(shinyjs)
library(shinyalert)
source('www/generate_user_ids.R')

# define js function for opening urls in new tab/window
# based on https://stackoverflow.com/questions/41426016/shiny-open-multiple-browser-tabs
js_code <- "
shinyjs.openExperiment = function(url) {
  window.location.replace(url);
}
"

# Set the URL for where the experiment is located
experiment_url <- "https://jdtrat-apps.shinyapps.io/teaching_r_study_demographics/?user_id="

ui <- shinyUI(
    navbarPage(
        title = "Introduction to R",
        fluid = TRUE,
        theme = "style.css",
        collapsible = TRUE,
        tabPanel(
          shinyjs::useShinyjs(),
          shinyjs::extendShinyjs(text = js_code,
                                 functions = 'openExperiment'),
          shinyalert::useShinyalert(),
            "Home",
            tags$head(tags$script(
                HTML(
                    '
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '
                )
            )),
      fluidRow(
          h1("Learn R from the comfort of your console"),
          column(
              width = 4,
              HTML(
                  "<center>
            <span style='color:#80A8D7'>
            <i class='fas fa-window-restore fa-4x'></i>
            </span>"
              ),
            br(),
            br(),
            "We have created a six module course
           that introduces topics commonly used
           when conducting data analyses using R."
          ),
          column(
              width = 4,
              HTML(
                  "<center>
             <span style='color:#80A8D7'>
             <i class='fab fa-r-project fa-4x'></i>
             </span>"
              ),
             br(),
             br(),
             "These modules were created as part
          of a research study examining best
          practices for learning R - after
          clicking on the link below, you
          can optionally opt into participating
          in this study"
          ),
          column(
              width = 4,
              HTML(
                  "<center>
            <span style='color:#80A8D7'>
            <i class='fas fa-hand-holding-usd fa-4x'></i>
            </span>"
              ),
            br(),
            br(),
            "The first 150 participants who
           enroll in the study and complete the
          course will receive a $5 amazon gift card."
          ),
          
      ),
      br(),
      br(),
      includeHTML("home.html")
        ),
      tabPanel(
          "Consent",
          includeHTML("consent.html"),
          actionButton(
            inputId = "yes",
            label = "Yes, I agree to this study",
            icon = icon("check")
          ),
          actionButton(
            inputId = "no",
            label = "No, I do not agree to this study",
            onclick = "fakeClick('Home')"
          )
      )
    )
)


server <- function(input, output) {
  
  # Load dropbox authorization
  rdrop2::drop_auth(rdstoken = 'token.rds')
  
  vals <- reactiveValues()
  vals$group <- ifelse(runif(1) > 0.5, "A", "B")
  

  observeEvent(input$yes,{
    shinyalert::shinyalert(title = "Redirecting Now...", 
                           type = "info",
                           showConfirmButton = FALSE)
    user_id <- get_username(group = isolate(vals$group),
                            drop_path = "teaching-r-study/")
    shinyjs::js$openExperiment(paste0(experiment_url, user_id, "/"))
  })

}

shinyApp(ui = ui, server = server)